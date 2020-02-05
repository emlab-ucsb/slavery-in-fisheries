#' Generate performance metrics across probability thresholds
#'
#' `threshold_perf_custom()` can take a set of class probability predictions
#' and determine performance characteristics across different values
#' of the probability threshold and any existing groups.
#'
#' Note that that the global option `yardstick.event_first` will be
#' used to determine which level is the event of interest. For more details,
#' see the Relevant level section of [yardstick::sens()].
#'
#' The currently calculated metrics are:
#' - [yardstick::recall()]
#' - [yardstick::detection_prevalence()]
#' - `modified_f1 = recall ^ 2 / detection_prevalence`
#'
#' @param .data A tibble, potentially grouped.
#'
#' @param truth The column identifier for the true two-class results
#' (that is a factor). This should be an unquoted column name.
#'
#' @param estimate The column identifier for the predicted class probabilities
#' (that is a numeric). This should be an unquoted column name.
#'
#' @param ... Currently unused.
#'
#' @param na_rm A single logical: should missing data be removed?
#'
#' @param thresholds A numeric vector of values for the probability
#' threshold. If unspecified, a series
#' of values between 0.5 and 1.0 are used. **Note**: if this
#' argument is used, it must be named.
#'
#' @return A tibble with columns: `.threshold`, `.estimator`, `.metric`,
#' `.estimate` and any existing groups.
#'
#' @examples
#' library(dplyr)
#' data("segment_logistic")
#'
#' # Set the threshold to 0.6
#' # > 0.6 = good
#' # < 0.6 = poor
#' threshold_perf_custom(segment_logistic, Class, .pred_good, thresholds = 0.6)
#'
#' # Set the threshold to multiple values
#' thresholds <- seq(0.5, 0.9, by = 0.1)
#'
#' segment_logistic %>%
#'   threshold_perf_custom(Class, .pred_good, thresholds)
#'
#' # ---------------------------------------------------------------------------
#'
#' # It works with grouped data frames as well
#' # Let's mock some resampled data
#' resamples <- 5
#'
#' mock_resamples <- resamples %>%
#'   replicate(
#'     expr = sample_n(segment_logistic, 100, replace = TRUE),
#'     simplify = FALSE
#'   ) %>%
#'   bind_rows(.id = "resample")
#'
#' resampled_threshold_perf_custom <- mock_resamples %>%
#'   group_by(resample) %>%
#'   threshold_perf_custom(Class, .pred_good, thresholds)
#'
#' resampled_threshold_perf_custom
#'
#' # Average over the resamples
#' resampled_threshold_perf_custom %>%
#'   group_by(.metric, .threshold) %>%
#'   summarise(.estimate = mean(.estimate))
#'
#' @export
threshold_perf_custom <- function(.data, ...) {
  UseMethod("threshold_perf_custom")
}

#' @rdname threshold_perf_custom
#' @importFrom tidyselect vars_select
#' @importFrom dplyr rename select mutate group_by do summarise
#' @importFrom dplyr %>% tibble ungroup
#' @importFrom stats na.omit
#' @export
threshold_perf_custom.data.frame <- function(.data,
                                      truth,
                                      estimate,
                                      thresholds = NULL,
                                      na_rm = TRUE,
                                      ...) {
  
  if (is.null(thresholds)) {
    thresholds <- seq(0.5, 1, length = 21)
  }
  
  nms   <- names(.data)
  obs   <- tidyselect::vars_select(nms, !!enquo(truth))
  probs <- tidyselect::vars_select(nms, !!enquo(estimate))
  rs_ch <- dplyr::group_vars(.data)
  
  rs_ch <- unname(rs_ch)
  
  obs_sym <- sym(obs)
  probs_sym <- sym(probs)
  
  if (length(rs_ch) == 0) {
    
    rs_ch <- NULL
    rs_id <- NULL
    
  } else {
    
    rs_id <- syms(rs_ch)
    
  }
  
  if (length(probs) > 1 | length(obs) > 1)
    stop("`truth` and `estimate` should only be single columns.",
         call. = FALSE)
  if (!inherits(.data[[obs]], "factor"))
    stop("`truth` should be a factor", call. = FALSE)
  if (length(levels(.data[[obs]])) != 2)
    stop("`truth` should be a 2 level factor", call. = FALSE)
  if (!is.numeric(.data[[probs]]))
    stop("`estimate` should be numericr", call. = FALSE)
  
  .data <- dplyr::rename(.data, truth = !!obs_sym, prob = !!probs_sym)
  
  if (!is.null(rs_id)) {
    .data <- dplyr::select(.data, truth, prob, !!!rs_id)
  } else {
    .data <- dplyr::select(.data, truth, prob)
  }
  
  if (na_rm) {
    .data <- na.omit(.data)
  }
  
  .data <- .data %>%
    expand_preds(
      threshold = thresholds,
      inc = c("truth", "prob", rs_ch)
    ) %>%
    mutate(
      alt_pred = recode_data(truth, prob, .threshold)
    )
  
  if (!is.null(rs_id)) {
    .data <- .data %>% group_by(!!!rs_id, .threshold)
  } else {
    .data <- .data %>% group_by(.threshold)
  }
  
  .data_metrics <- .data %>%
    two_class(truth, estimate = alt_pred)
  
  # Create the `distance` metric data frame
  # and add it modified_f1
  recall_vec <- .data_metrics %>%
    dplyr::filter(.metric == "recall") %>%
    dplyr::pull(.estimate)
  
  modified_f1 <- .data_metrics %>%
    dplyr::filter(.metric == "detection_prevalence") %>%
    dplyr::mutate(
      .metric = "modified_f1",
      # .estimate is detection_prevalence currently. this recodes as modified f1 score
      .estimate = recall_vec ^ 2 / .estimate
    )
  
  .data_metrics <- dplyr::bind_rows(.data_metrics, modified_f1)
  
  .data_metrics
}

#' @importFrom yardstick sens spec j_index metric_set
two_class <- function(...) {
  mets <- metric_set(recall,detection_prevalence)
  mets(...)
}

expand_preds <- function(.data, threshold, inc = NULL) {
  threshold <- unique(threshold)
  nth <- length(threshold)
  n_data <- nrow(.data)
  if (!is.null(inc))
    .data <- dplyr::select(.data, inc)
  .data <- .data[rep(1:nrow(.data), times = nth), ]
  .data$.threshold <- rep(threshold, each = n_data)
  .data
}

utils::globalVariables(
  c(
    ".",
    ".threshold",
    "alt_pred",
    "prob",
    "statistic",
    "value",
    ".metric",
    ".estimate",
    "distance"
  )
)

recode_data <- function(obs, prob, threshold) {
  lvl <- levels(obs)
  if (getOption("yardstick.event_first", default = TRUE)) {
    pred <- ifelse(prob >= threshold, lvl[1], lvl[2])
  } else {
    pred <- ifelse(prob >= threshold, lvl[2], lvl[1])
  }
  factor(pred, levels = lvl)
}

quote_collapse <- function(x, quote = "`", collapse = ", ") {
  paste(encodeString(x, quote = quote), collapse = collapse)
}

abort_default <- function(x, fn) {
  cls <- quote_collapse(class(x))
  msg <- paste0("No implementation of `", fn, "()` for object of class ", cls, ".")
  abort(msg)
}

# Check if a class_pred object came from an ordered factor
is_ordered_class_pred <- function(x) {
  attr(x, "ordered")
}

get_equivocal_label <- function(x) {
  attr(x, "equivocal")
}

is_ordered <- function(x) {
  UseMethod("is_ordered")
}

is_ordered.class_pred <- function(x) {
  is_ordered_class_pred(x)
}

is_ordered.default <- function(x) {
  is.ordered(x)
}