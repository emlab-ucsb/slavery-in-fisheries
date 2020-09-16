library(tictoc)
tic()
robustness_results_df <- tibble(year_assumption = sort(unique(main_df$years_until_caught))) %>% 
  crossing(tibble(include_vessel_characteristics = c(TRUE,FALSE))) %>%
  mutate(robustness_results = map2(year_assumption,
                                   include_vessel_characteristics,
         function(year_assumption,include_vessel_characteristics){


  if(is.na(year_assumption)) return()
  positive_vessels <- main_df %>%
    filter(offender == 1 & years_until_caught <= year_assumption) %>%
    dplyr::select(mmsi,fldb_vessel_id,year,offender)
  
  filtered_main_df <- main_df %>%
    # Only only include vessel-years from positives in the year prior to being caught
    # Otherwise exclude them from training - they have high potential for being positive
    dplyr::select(-fldb_vessel_id,-offender) %>%
    left_join(positive_vessels,by=c("mmsi","year")) %>%
    filter(!(mmsi %in% positive_vessels$mmsi & !offender)) %>%
    dplyr::select(-years_until_caught) %>%
    # Make binaries into 1s/0s
    mutate(offender = ifelse(is.na(offender),0,1),
           foc = ifelse(foc,1,0),
           iuu = ifelse(iuu,1,0))
  
  training_df <- filtered_main_df %>%
    # Add media source id
    # If it's not a known offender, create dummy ID for CV
    mutate(source_id = ifelse(offender != 1,
                              paste0("no_source_",row_number()),
                              source_id)) %>%
    # Add forced labor database vessel id
    # If it's not a known offender, create dummy ID for CV
    mutate(fldb_vessel_id = ifelse(offender != 1,
                                   paste0("no_fldb_info_",row_number()),
                                   fldb_vessel_id)) %>%
    # Make these columns factors
    mutate_at(
      vars("foc", "iuu", "gear", "flag", "year", "ais_type", "mmsi", "fldb_vessel_id", "year", "source_id", "offender"),
      list(as.factor))
  
  prediction_df <- main_df %>%
    dplyr::select(-fldb_vessel_id,-offender) %>%
    # Attach positive fldb_vessel_id to MMSIs, so we can see how these change over time
    left_join(positive_vessels %>% dplyr::select(-year),by=c("mmsi")) %>%
    mutate(offender = case_when(offender == 1 & years_until_caught == 1 ~ 1,
                                is.na(offender) ~ 0,
                                TRUE ~ 0)) %>%
    dplyr::select(-years_until_caught) %>%
    # Make these columns factors
    mutate_at(
      vars("foc", "iuu", "gear", "flag", "year", "ais_type", "mmsi", "fldb_vessel_id", "year", "source_id", "offender"),
      list(as.factor))
  
  fl_rec <- recipes::recipe(offender ~ ., data = head(training_df)) %>%
    recipes::update_role(mmsi, new_role = "id variable") %>%
    recipes::update_role(fldb_vessel_id, new_role = "id variable") %>%
    recipes::update_role(source_id, new_role = "id variable") %>%
    # Downsample unlabaled vessels
    # Skip=TRUE so that the testing dataset is not downsampled when baking
    # Do this at random every time
    #recipes::step_downsample("offender",under_ratio = tune(),skip=TRUE)  %>%
    recipes::step_knnimpute(ais_type,impute_with = c("gear","flag","length_m")) %>%
    # Box-Cox transformation on all numeric
    recipes::step_BoxCox(all_numeric()) %>%
    # Group flags with less than 1% into other
    recipes::step_other(flag, threshold = 0.01) %>%
    # Create dummy variables for factor columns like flag, ais_type
    recipes::step_dummy(all_predictors(), -all_numeric()) %>%
    # Remove near-zero variance numeric predictors
    recipes::step_nzv(all_predictors()) %>%
    # Remove numeric predictors that have correlation greater the 75%
    recipes::step_corr(all_numeric(), threshold = 0.75) %>%
    # Center all numeric predictors
    recipes::step_center(all_numeric()) %>%
    # Scale all numeric predictors
    recipes::step_scale(all_numeric())
  
  if(!include_vessel_characteristics) fl_rec <- fl_rec %>%
    recipes::update_role(engine_power_kw,tonnage_gt,length_m,crew_size,ais_type, new_role = "id variable")

  rf_mod <- rand_forest(
    mode = "classification",
    trees = 1000
  ) %>%
    set_engine("ranger")
  
  # Cross-validation
  # Ensure there is no splitting across source_id (and consequently fldb_vessel_id) across analysis and assessment datasets
  set.seed(101)
  cv_splits <- group_vfold_cv(training_df, 
                              group = source_id,
                              v = 10)
  
  # NA means no downsampling, so just the regular base classifier
  under_ratio_vec <- tibble(under_ratio = 1)
  # Number of bags
  bag_vec <- tibble(bag = seq(100))
  # Different models to try
  model_type_vec <- tibble(model_type = c("random_forest"))
  
  analysis_data <- cv_splits %>%
    mutate(# Create analysis dataset based on CV folds
      analysis = map(splits,~analysis(.x)),
      # Create assessment dataset based on CV folds
      assessment = map(splits,~assessment(.x))) %>%
    dplyr::select(-splits)
  
  model_runs <- crossing(under_ratio_vec,
                         model_type_vec,
                         bag_vec) %>%
    # If there's no downsampling, there's no bagging
    mutate(bagging = ifelse(is.na(under_ratio),FALSE,TRUE)) %>%
    # Don't need to bag if not doing downsampling
    filter(!(!bagging & bag >1)) %>%
    # Want different seed each time downsampling is done, for bagging
    mutate(seed = sample.int(10^5,n()))
  
  pb <- progress_estimated(nrow(model_runs) * nrow(cv_splits))
  
  predictions <- model_runs %>%
    mutate(recipe = map2(seed,bagging,function(x,y){
      # Add downsampling to pre-processing recipe if bagging
      ifelse(!y,
             tmp_recipe <- fl_rec,
             tmp_recipe <- fl_rec %>%
               step_downsample("offender",under_ratio=y,seed=x,skip=TRUE))
      return(tmp_recipe)
      # Create workflow based on random forest hyperparameter and downsampling pre-processing recipe
    }),
    workflow = map2(recipe,model_type,function(x,y){
      # Add either random forest or svm to worflow, along with appropriate recipe
      workflow() %>%
               add_model(rf_mod) %>%
               add_recipe(x)})) %>%
    # Remove unnecessary columns
    dplyr::select(-seed,-recipe) %>%
    # For each workflow, do entire model fitting process across all folds
    mutate(predict = map(workflow,function(x){
      analysis_data %>%
        mutate(predict = map2(analysis,assessment,function(y,z){
          tmp <- fit(x,y) %>%
            # Predict assessment data using fit
            predict(z, type = "prob") %>%
            # Add predictions to assessment data
            bind_cols(z) %>%
            # Select relevant columns
            dplyr::select(mmsi,year,offender,.pred_1)
          # Increment progress bar
          pb$tick()$print()
          return(tmp)
        })) %>%
        dplyr::select(id,predict) %>%
        unnest(predict)
    })) %>%
    dplyr::select(-workflow) %>%
    unnest(predict)
  
  bags_to_show <- max(bag_vec)
  bag_performance <- map_df(bags_to_show,function(x){
    tmp_performance <- predictions %>%
      filter(bag<=x) %>%
      # For each fold and hyperparameter set, take average prediction for each vessel-year (mmsi and year) across bags
      group_by(id,model_type,under_ratio,mmsi,year) %>%
      summarize(offender = offender[1],
                .pred_1 = mean(.pred_1,na.rm=TRUE)) %>%
      ungroup()  %>%
      # Make offender =1 reference level
      mutate(offender = relevel(as.factor(offender),"1")) %>%
      # Then we will calculate performance metrics, by threshold, for each fold and hyperparameter set
      group_by(id,model_type,under_ratio) %>%
      nest() %>%
      ungroup() %>%
      # Calculated metrics will include recall, modified_f1, and detection_prevalence
      mutate(threshold_perf = map(data,~threshold_perf_custom(.x,
                                                              truth = offender,
                                                              estimate = .pred_1,
                                                              thresholds=seq(0.01,0.99,0.01)))) %>%
      dplyr::select(-data) %>%
      unnest(threshold_perf) %>%
      # Now for each hyperparameter set, we will take the average performance across all folds
      group_by(model_type,under_ratio,.threshold,.metric) %>%
      summarize(mean_performance = mean(.estimate,na.rm=TRUE),
                sd_performance = sd(.estimate,na.rm=TRUE)) %>%
      ungroup() %>%
      group_by(model_type,under_ratio) %>%
      nest() %>%
      ungroup() %>%
      # Now we will find the optimial threshold for maximizing the modified F1 estimator
      # Maximize the mean
      mutate(optimal_threshold = map_dbl(data,~.x %>%
                                           filter(.metric=="modified_f1") %>%
                                           arrange(desc(mean_performance)) %>%
                                           slice(1) %>%
                                           .$.threshold),
             # optimized_metrics will contain metrics with optimized modified F1 estimator
             optimized_metrics = map2(data,optimal_threshold,~.x %>%
                                        filter(.threshold==.y))) %>% 
      unnest(optimized_metrics) %>%
      dplyr::select(-optimal_threshold,-data) %>%
      mutate(number_bags = x)
    print(paste(x,"complete"))
    tmp_performance
  })
  
  optimized_model <- tibble(model_type = "random_forest",
                            bagging = TRUE,
                            under_ratio = 1,
                            number_bags = max(bag_vec))
  
  cv_performance <- bag_performance %>% 
    mutate(bagging = ifelse(is.na(under_ratio),FALSE,TRUE)) %>%
    inner_join(optimized_model,by = c("model_type", "number_bags", "under_ratio","bagging"))
  
  optimized_model$.threshold <- cv_performance$.threshold[1]
  
  set.seed(101)
  # Define all model runs
  model_runs_final <- optimized_model %>%
    crossing(tibble(bag = seq(optimized_model$number_bags))) %>%
    mutate(seed = sample.int(10^5,n()))
  
  pb <- progress_estimated(nrow(model_runs_final))
  
  predictions_final <- model_runs_final %>%
    mutate(recipe = map2(seed,bagging,function(x,y){
      # Add downsampling to pre-processing recipe if bagging
      ifelse(!y,
             tmp_recipe <- fl_rec,
             tmp_recipe <- fl_rec %>%
               step_downsample("offender",under_ratio=y,seed=x,skip=TRUE))
      return(tmp_recipe)
      # Create workflow based on random forest hyperparameter and downsampling pre-processing recipe
    }),
    workflow = map2(recipe,model_type,function(x,y){
      # Add either random forest or svm to worflow, along with appropriate recipe
      workflow() %>%
               add_model(rf_mod %>%
                           set_args(importance = "impurity_corrected")) %>%
               add_recipe(x)})) %>%
    # Remove unnecessary columns
    dplyr::select(-seed,-recipe) %>%
    # For each workflow, do entire model fitting process across all folds
    mutate(fit = map(workflow,~fit(.x,training_df)),
           predict = map(fit,function(x){
             tmp <- x %>%
               # Predict assessment data using fit
               predict(prediction_df, type = "prob") %>%
               # Add predictions to assessment data
               bind_cols(prediction_df)%>%
               # Select relevant columns
               dplyr::select(mmsi,year,offender,.pred_1)
             # Increment progress bar
             pb$tick()$print()
             return(tmp)
           })) %>%
    dplyr::select(-workflow)
  
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  predictions_figures <- predictions_final %>%  
    dplyr::select(-fit) %>%
    unnest(predict) %>%
    # Switch factors to numeric for faster computation, then switch back
    mutate(offender = as.numeric.factor(offender),
           mmsi = as.numeric.factor(mmsi),
           year = as.numeric.factor(year)) %>%
    # Take average prediction for each vessel-year (mmsi and year) across bags
    group_by(mmsi,year) %>%
    summarize(offender = offender[1],
              .pred_1 = mean(.pred_1,na.rm=TRUE)) %>%
    ungroup() %>% 
    mutate(mmsi = as.factor(mmsi),
           year = as.factor(year)) %>%
    left_join(prediction_df %>%
                dplyr::select(-offender),
              by=c("mmsi","year")) %>%
    mutate(Country = countrycode(flag,"iso3c","country.name"))%>%
    # Use optimal threshold found above
    mutate(class = ifelse(.pred_1 >optimized_model$.threshold,1,0))%>% 
    mutate(Prediction = ifelse(class == 1,"Positive","Negative")) %>%
    mutate(Label = ifelse(offender == 0,"Unlabeled","Positive"))
  
  n_fun <- function(x){
    return(data.frame(y = max(x) + 0.05, label = paste0("n = ",prettyNum(length(x),big.mark=","))))
  }
  
  class_fig <- predictions_figures  %>%
    ggplot(aes(x=Prediction,y=.pred_1,fill=Label)) + 
    geom_boxplot(position = position_dodge(width = 0.75)) + 
    coord_flip() + 
    geom_hline(yintercept = optimized_model$.threshold,linetype=2) +
    labs(y = "Model risk score",
         x = "Model\nclassification",
         title = paste0("Year assumption: ",year_assumption))+
    stat_summary(fun.data = n_fun, geom = "label",position = position_dodge(width = 0.75),show.legend = FALSE) +
    ylim(c(0,1)) +
    disco::scale_fill_disco(palette = "vibrant", "Training data\nlabel",direction=1,alpha=1,
                            guide = guide_legend(reverse = TRUE) ) +
    theme_bw() +
    theme(axis.title.y = element_text(angle=0,vjust=0.5))

  return(list(cv_performance = cv_performance,
               predictions_figures = predictions_figures))
})) %>% 
  mutate(cv_performance = map(robustness_results,function(x){x$cv_performance}),
         predictions_figures = map(robustness_results,function(x){x$predictions_figures})) %>%
  dplyr::select(-robustness_results)
toc()

saveRDS(robustness_results_df,file="robustness_results_df.Rdata")

#robustness_results_df <- readRDS("robustness_results_df.Rdata")

robustness_cv <- robustness_results_df %>%
  dplyr::select(-predictions_figures) %>%
  unnest(cv_performance)

write_csv(robustness_cv,here::here("interim_data/s9_robustness_cv.csv"))


robustness_predictions_figures <- robustness_results_df %>%
  dplyr::select(-cv_performance) %>%
  unnest(predictions_figures)

write_csv(robustness_predictions_figures,here::here("interim_data/s10_robustness_predictions_figures.csv"))

robustness_final <- robustness_predictions_figures %>%
  group_by(year_assumption,include_vessel_characteristics) %>%
  nest() %>%
  mutate(results = map(data,function(x){
    
    tmp_pred <- x
    
    n_predicted_positive_vessel_years <- tmp_pred %>%
      filter(Prediction == "Positive") %>%
      nrow()
    
    n_predicted_positive_vessel_years_new <- tmp_pred %>%
      filter(Prediction == "Positive" & Label == "Unlabeled") %>%
      nrow()
    
    fraction_positive_vessel_years <- n_predicted_positive_vessel_years / nrow(tmp_pred)
    
    
    n_predicted_positive_vessels <- tmp_pred %>%
      filter(Prediction == "Positive") %>%
      distinct(mmsi) %>%
      nrow()
    
    fraction_positive_vessels <- n_predicted_positive_vessels / length(unique(tmp_pred$mmsi))
    
    n_positive_crew <- tmp_pred %>%
      filter(Prediction == "Positive") %>%
      distinct(mmsi,crew_size) %>%
      .$crew_size %>%
      sum()
    
    correct_positives <- tmp_pred %>%
      filter(Prediction == "Positive" & Label == "Positive") %>%
      nrow()
    
    fraction_correct_positives <- correct_positives / (tmp_pred %>%
                                                         filter( Label == "Positive") %>%
                                                         nrow())
    
    return(tibble(n_predicted_positive_vessel_years = n_predicted_positive_vessel_years,
                  n_predicted_positive_vessels = n_predicted_positive_vessels,
                  fraction_positive_vessels = fraction_positive_vessels,
                  fraction_positive_vessel_years = fraction_positive_vessel_years,
                  n_positive_crew = n_positive_crew,
                  fraction_correct_positives = fraction_correct_positives))
  })) %>%
  dplyr::select(-data) %>%
  unnest(results) 

write_csv(robustness_final,here::here("interim_data/s11_robustness_final.csv"))


robustness_cv <- read.csv(here::here("interim_data/s9_robustness_cv.csv"),stringsAsFactors = FALSE) %>%
  mutate(year_assumption = as.character(year_assumption))

robustness_predictions_figures <- read.csv(here::here("interim_data/s10_robustness_predictions_figures.csv"),stringsAsFactors = FALSE) %>%
  mutate(year_assumption = as.character(year_assumption))

robustness_final <- read.csv(here::here("interim_data/s11_robustness_final.csv"),stringsAsFactors = FALSE) %>%
  mutate(year_assumption = as.character(year_assumption))



robustness_cv_fig <- robustness_cv %>%
  mutate(error_min = pmax(mean_performance-sd_performance,0),
         error_max = ifelse(.metric=="recall",
                            pmin(mean_performance+sd_performance,1),
                            mean_performance+sd_performance))%>%
  mutate(.metric = case_when(.metric=="recall"~"Recall",
                             .metric=="modified_f1"~"Modified F1",
                             TRUE~"Detection\nprevalence")) %>%
  ggplot(aes(x = year_assumption,y=mean_performance,color=include_vessel_characteristics)) +
  geom_point(position=position_dodge(width=0.75)) +
  geom_errorbar(aes(ymin=error_min, ymax=error_max),position=position_dodge(width=0.75),width=0.5) + 
  facet_grid(.metric~.,scales="free_y",switch="y") +
  labs(x = "Positive training data set label year assumption",
       y = "Mean\nacross\nfolds")+ 
  theme_bw() +
  theme(strip.background =element_rect(fill=NA),
        axis.title.y = element_text(angle=0,vjust=0.5),
        strip.text.y.left = element_text(angle=0,vjust=0.5)) +
  disco::scale_color_disco("Include\nvessel\ncharacteristics",palette="muted") +
  ylim(c(0,NA))

robustness_cv_fig

#ggsave(here::here("output_figures/figure_s6.png"),robustness_cv_fig,width=7.5,height=5,device="png",dpi=300)


robustness_final_fig <- robustness_final%>%
  rename(`Fraction of correctly\nidentified true positives` = fraction_correct_positives,
         `Fraction of vessels\nidentified as positives` = fraction_positive_vessels,
         `Fraction of vessel-years\nidentified as positives` = fraction_positive_vessel_years,
         `Number of vessels\nidentified as positives` = n_predicted_positive_vessels,
         `Number of vessel-years\nidentified as positives` = n_predicted_positive_vessel_years,
         `Number of crew\nworking on\npositive vessels` = n_positive_crew) %>%
  pivot_longer(-c(year_assumption,include_vessel_characteristics)) %>%
  ggplot(aes(x = year_assumption,y=value,fill=include_vessel_characteristics)) +
  geom_bar(stat = "identity",position=position_dodge(width=1),color="black") +
  facet_grid(name~.,scales="free_y",switch="y") +
  labs(x = "Positive training data set label year assumption",
       y = "")+ 
  theme_bw() +
  theme(strip.background =element_rect(fill=NA),
        axis.title.y = element_text(angle=0,vjust=0.5),
        strip.text.y.left = element_text(angle=0,vjust=0.5)) +
  disco::scale_fill_disco("Include\nvessel\ncharacteristics",palette="muted") +
  scale_y_continuous(labels=scales::comma,limits = c(0,NA))

robustness_final_fig

#ggsave(here::here("output_figures/figure_s7.png"),robustness_final_fig,width=7.5,height=7.5,device="png",dpi=300)
