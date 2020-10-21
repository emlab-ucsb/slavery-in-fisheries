# Metadata  

## interim_data  

This directory contains interim files created during the analysis that can be used to generate all figures and statistics in the paper. These interim files are saved due to the long run times required for the analysis.  

### s4_final_model_predictions.csv  

This file contains final model predictions, as well as all model features (which are also found in `raw_data/s1_training_full.csv`. Each row contains a "vessel-year" observation which describes vessel characteristics and behavior statistics for a single year. These data were obtained from Global Fishing Watch. The data have the following schema:  

* `mmsi_anonymous`: Anonymized Maritime Mobile Service Identity (MMSI) unique vessel identifier, used to protect the identies of individual fishing vessels and avoid false or misleading accusations    
* `.seed`: Random number seed used to anonymize the MMSI  
* `.pred_1`: Raw risk model output score (continuous number from 0 to 1)  
* `class`: Predicted class (0 for low-risk of forced labor, 1 for high-risk of forced labor)
* `Prediction`: Predicted class (`Negative` for low-risk of forced labor, `Positive` for high-risk of forced labor)	
* `Label`: Original training dataset label (either `Positive` or `Unlabeled`)  
* `year`: Year of observation  
* `flag`: Flag the vessel flies.  
* `gear`: Predominant gear type used as classified by the GFW fishing algorithm. The study focuses on longliners, trawlers, and squid jiggers.  
* `engine_power_kw`: Vessel engine power (kw). Where available, this information comes from vessel registries. When registry information is not available, this information comes from the GFW vessel characteristics algorithm.  
* `tonnage_gt`: Vessel gross tonnage. Where available, this information comes from vessel registries. When registry information is not available, this information comes from the GFW vessel characteristics algorithm.  
* `length_m`: Vessel length (m). Where available, this information comes from vessel registries. When registry information is not available, this information comes from the GFW vessel characteristics algorithm. 
* `crew_size`: Number of crew on vessel. Where available, this information comes from vessel registries. When registry information is not available, this information comes from the GFW vessel characteristics algorithm.  
* `positions`: Number of AIS position messages.  
* `ais_type`: AIS device type, class A or B.
* `foc`: A boolean for whether or not a vessel flies a flag of convenience ([ITF 2019](https://www.itfglobal.org/en/sector/seafarers/flags-of-convenience))   
* `iuu`: A boolean for whether or not a vessel is on a Regional Fisheries Management Organisations (RFMO) IUU list or are subject to an INTERPOL Purple Notice ([Trygg Mat Tracking 2019](https://iuu-vessels.org)).
* `fldb_vessel_id`: Unique identifier created for the suspected forced labor vessel database   (`suspected_forced_labor_database.csv`) (this will be NA for vessel-years that do not have associated forced labor case)  
* `source_id`: Unique numeric ID for the media source from which a forced labor case report originated (this will be NA for vessel-years that do not have associated forced labor case)  
* `years_until_caught`: Number of years between the current year and the year the vessel was reported to be using forced labor (this will be 0 for years in which a vessel was caught for forced labor, a number >=1 for years prior to being caught, or NA for vessel-years that do not have associated forced labor case)  
* `hours`: Total active hours.  
* `fishing_hours`: Total fishing hours as classified by the GFW fishing algorithm.  
* `average_daily_fishing_hours`: Average daily fishing hours on days when fishing.  
* `fishing_hours_foreign_eez`:Total fishing hours in EEZs that are different than the vessel flag.  
* `fishing_hours_high_seas`: Total fishing hours in the high seas.  
* `distance_traveled_km`: Total distance traveled.  
* `max_distance_from_shore_km`: Maximum distance from shore (km).  
* `max_distance_from_port_km`: Maximum distance from any port (km).  
* `number_encounters`: Number of encounters with other vessels. Encounters are defined by events when two vessels are within 500 meters of each other, traveling less than 2 knots, and for a minimum duration of 2 hours. These encounters may represent transshipment, refueling, or transfer of supplies or crew ([Miller et al. 2018](https://doi.org/10.3389/fmars.2018.00240)).   
* `number_iuu_encounters`: Number of encounters with other vessels that are on a Regional Fisheries Management Organisations (RFMO) IUU list or are subject to an INTERPOL Purple Notice ([Trygg Mat Tracking 2019](https://iuu-vessels.org)).  
* `number_forced_labor_encounters`: Number of encounters with other vessels that are in the forced labor vessel database.  
* `average_encounter_duration_hours`: Average duration of encounters with other vessels. Encounters are defined as events when two vessels are within 500 meters of each other, traveling less than 2 knots, and for a minimum duration of 2 hours. These encounters may represent transshipment, refueling, or transfer of supplies or crew ([Miller et al. 2018](https://doi.org/10.3389/fmars.2018.00240)).  
* `gaps_24_hours`:  Number of AIS gaps greater than 24 hours. Gaps are defined as periods during which no AIS messages are received in areas of the ocean where AIS reception is expected. AIS coverage maps are calculated by determining the global vessel average number of daily position transmissions thinned to 5 minute intervals in 0.25x0.25 degree grid cells annually and for both A and B device classes. Gaps are defined as occurring when a vessel is at least 10km from shore and last transmitted in an area that has at least 10% daily AIS coverage for its given year and AIS device class.
* `number_voyages`: Total number of voyages between two ports.  
* `number_foreign_port_visits`: Number of visits to ports in countries that are different than the vessel flag.  
* `number_poc_port_visits`: Number of visits to ports in countries that had not ratified the Port State Measures Agreement (PSMA) at the time of port visit.  
* `average_voyage_duration_hours`: Average duration of all voyages. A voyage is defined as a trip between two ports.  
* `number_loitering_events`: Total number of loitering events. Loitering events are defined when a vessel is traveling less than 2 knots, at least 1 kilometer from shore, for at least one hour. These events may represent encounters if the second vessel is not broadcasting AIS.  
* `average_loitering_duration_hours`: Average duration of loitering events.  
* `offender`: Boolean for whether this vessel-year is classified as positive (1, e.g., a boat that has reported cases of forced labor) or unlabeld (0, e.g., a boat that does not have reported cases of forced labor)          

### s5_figure_3_data.csv  

This contains 2018 spatial fishing hours by gear type, including total fishing hours by vessels included in the analysis along with high-risk fishing hours, for 0.5x0.5 degree bins. These data were used to generate the map in Figure 3.

* `year`: Year 
* `gear`: Gear type category
* `lat_bin`: 0.5 latitude bin
* `lon_bin`: 0.5 degree longitude bin
* `fishing_kW_hours`: Fishing effort (kW-hours)
* `at_risk_fishing_kW_hours`: Fishing effort by high-risk vessels, as classified by the baseline model variation detailed in the manuscript (kW-hours)
* `fraction_at_risk_fishing_kW_hours`: Fraction of total fishing effort by high-risk vessels, as classified by the baseline model variation detailed in the manuscript   

### s6_figure_4_data.csv  

This 2018 port visit information by country and gear type, including total number of port visits by vessels included in the analysis along with high-risk port visits. These data were used to generate the map in Figure 4.

* `port_iso3`: Country ISO3 code 
* `year`: Year  
* `gear`: Gear type category 
* `total_visits`: Total number of visits by fishing vessels 
* `high_risk_visits`: Number of visits by high-risk fishing vessels, as classified by the baseline model variation detailed in the manuscript 
* `known_offender_visits`: Number of visits by fishing vessels contained in the suspected forced labor database 
* `fraction_high_risk_visits`: Fraction of visits by high-risk fishing vessels, as classified by the baseline model variation detailed in the manuscript 
* `psma_year`: Year in which the country ratified the Port State Measures Agreement (PSMA), if applicable 
* `psma_country`: Binary for whether or not the country has ratified the PSMA

### s7_figure_s3_data.csv  

These data summarize cross-validation model performance for different model variations and hyperparameters using their optimized thresholds. These data were used to generate Figure S2.

* `model_type`: Model type  
* `under_ratio`: Undersampling ratio  
* `.threshold`: Optimized cutoff threshold for determining high-risk vessel years  
* `.metric`: Cross-validation performance metric  
* `.mean_performance`: Mean metric performance across folds  
* `sd_performance`: Standard deviation metric performance across folds  
* `number_bags`: Number of bags  

### s8_figure_s5_data.csv  

These data summarize average feature importance across bags for the final model, which uses a random forest. These data were used to generate Figure S5.

* `Feature`:  Short-hand model feature code
* `Importance`:  Average feature importance across bags
* `indicator_name`:  Human-readable feature description  
* `indicator_type`:  Category for where data come from: directly observed using AIS data (`directly_observed`); inferred using the GFW fishing algorithm (`gfw_fishing_model`); or vessel characteristics that come from either a vessel registry or the GFW vessel characteristic algorithm (`registry_or_gfw_vessel_model`)  

### s9_robustness_cv.csv  

These data summarize cross-validation model performance for the model variations in the robustness checks using their optimized thresholds. These data were used to generate Figure S2.

* `year_assumption`: 1 is the baseline assumption and means that only the year prior to a vessel being reported for forced labor is labeled as positive, 2 means that the 2 years prior to being reported are labeled as positive, etc. A value of 6 means that all years prior to the vessel being reported are labeled as positive.  
* `include_vessel_characteristics`: Boolean for whether or not the model variation includes vessel characteristics as model features  
* `model_type`: Model type  
* `under_ratio`: Undersampling ratio  
* `.threshold`: Optimized cutoff threshold for determining high-risk vessel years  
* `.metric`: Cross-validation performance metric  
* `.mean_performance`: Mean metric performance across folds  
* `sd_performance`: Standard deviation metric performance across folds  
* `number_bags`: Number of bags  

### s10_robustness_predictions_figures.csv  

These data summarize anonymized final model predictions for robustness checks. These data were used to generate Figure S7.

* `mmsi_anonymous`: Anonymized Maritime Mobile Service Identity (MMSI) unique vessel identifier, used to protect the identies of individual fishing vessels and avoid false or misleading accusations    
* `year_assumption`: 1 is the baseline assumption and means that only the year prior to a vessel being reported for forced labor is labeled as positive, 2 means that the 2 years prior to being reported are labeled as positive, etc. A value of 6 means that all years prior to the vessel being reported are labeled as positive.  
* `include_vessel_characteristics`: Boolean for whether or not the model variation includes vessel characteristics as model features  
* `year`: Year of observation  
* `crew_size`: Number of crew on vessel. Where available, this information comes from vessel registries. When registry information is not available, this information comes from the GFW vessel characteristics algorithm.  
* `offender`: Boolean for whether this vessel-year is classified as positive (1, e.g., a boat that has reported cases of forced labor) or unlabeld (0, e.g., a boat that does not have reported cases of forced labor)          
* `flag`: Flag the vessel flies.  
* `gear`: Predominant gear type used as classified by the GFW fishing algorithm. The study focuses on longliners, trawlers, and squid jiggers.  
* `class`: Predicted class (0 for low-risk of forced labor, 1 for high-risk of forced labor)  
* `fldb_vessel_id`: Unique identifier created for the suspected forced labor vessel database   (`suspected_forced_labor_database.csv`) (this will be NA for vessel-years that do not have associated forced labor case)    


### s11_known_vessel_info.csv  

These data summarize how many vessel-years in our training dataset have known registry information for different vessel characteristics. 

* `total_vessel_years`: The total number of vessel-years in the training dataset  
* `known_length_m`: The number of vessel-years with known length from vessel registries  
* `known_tonnage_gt`: The number of vessel-years with known gross tonnage from vessel registries  
* `known_engine_power_kw`: The number of vessel-years with known engine power from vessel registries  
* `known_crew`: The number of vessel-years with known crew size from vessel registries  

### s12_figure_s1_data.csv

These data summarize the predicted class, by year, for each vessel in our forced labor vessel database using our baseline model. These data are used to help make Figure S1.

* `year`: Year  
* `class`: Predicted class (0 for low-risk of forced labor, 1 for high-risk of forced labor)  
* `mmsi`: Maritime Mobile Service Identity (MMSI) unique vessel identifier   
* `offender`: Boolean for whether this vessel-year is classified as positive (1, e.g., a boat that has reported cases of forced labor) or unlabeld (0, e.g., a boat that does not have reported cases of forced labor)          
* `fldb_vessel_id`: Unique identifier created for the suspected forced labor vessel database   (`suspected_forced_labor_database.csv`)  