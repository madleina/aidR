##################################
#                                #
#   Filter data                  #
#                                #
##################################

#------------------------
# Public functions
#------------------------

#' Filter CGM values
#'
#' @param data_id A list with all data found for one individual. Filtering is
#'   only performed if the CGM data was cleaned and standardized.
#' @param truncate_min_cgm CGM values smaller than this threshold will be truncated. Default 40 mg/dL, if NULL, no filtering is performed.
#' @param truncate_max_cgm CGM values larger than this threshold will be truncated. Default 400 mg/dL, if NULL, no filtering is performed.
#' @param min_minutes_difference_cgm CGM values are downsampled to this time interval (in minutes). Default 5 minutes, if NULL, no downsampling is performed.
#'
#' @return A list with all data found for one individual, filtered, if necessary.
#' @examples
#' path <- system.file("extdata", "dexcom_clarity_example.csv", package = "aidR")
#' # Read data
#' data <- parse_data(id = "1", paths = path)
#' range(data$cgm$value)
#' length(data$cgm$value)
#' 
#' # Truncate to 40-400 mg/dL and downsample to 5-minute intervals
#' data_filtered <- filter_CGM(data)
#' range(data_filtered$cgm$value)
#' length(data_filtered$cgm$value)
#' 
#' # Truncate only, without downsampling
#' data_truncated <- filter_CGM(data, min_minutes_difference_cgm = NULL)
#' range(data_truncated$cgm$value)
#' length(data_truncated$cgm$value)
#' @export
filter_CGM <- function(data_id, truncate_min_cgm = 40, truncate_max_cgm = 400, min_minutes_difference_cgm = 5){
  if (is.null(data_id)){ return(data_id) }
  
  # Now: type-specific filters
  # CGM: only if cleaned
  if ("cgm" %in% names(data_id) && "value" %in% names(data_id$cgm)){ 
    data_id$cgm <- .filter_cgm.aidR(data_id$cgm, 
                                    truncate_min_cgm = truncate_min_cgm, 
                                    truncate_max_cgm = truncate_max_cgm, 
                                    min_minutes_difference_cgm = min_minutes_difference_cgm)
  }
  
  return(data_id)
}

#------------------------
# Helper functions
#------------------------

#' Filter CGM data
#'
#' @param cgm A data frame containing the cleaned CGM data for one individual.
#' @param truncate_min_cgm CGM values smaller than this threshold will be truncated. Default 40 mg/dL, if NULL, no filtering is performed.
#' @param truncate_max_cgm CGM values larger than this threshold will be truncated. Default 400 mg/dL, if NULL, no filtering is performed.
#' @param min_minutes_difference_cgm CGM values are downsampled to this time interval (in minutes). Default 5 minutes, if NULL, no downsampling is performed.
#'
#' @return A data frame with the filtered CGM data.
#' 
#' @keywords internal
.filter_cgm.aidR <- function(cgm, truncate_min_cgm = 40, truncate_max_cgm = 400, min_minutes_difference_cgm = 5){
  # Dummy check: Make sure CGM is in mg/dl
  mean_cgm <- mean(as.numeric(cgm$value), na.rm = TRUE)
  if (mean_cgm < 30) {
    stop("Mean glucose (", round(mean_cgm, digits = 1), ") < 30 - check units!")
  }
  
  # For unrealistically low/high CGM values: truncate them
  if (!is.null(truncate_min_cgm) && !is.na(truncate_min_cgm)) {
    cgm <- cgm %>% mutate(value = pmax(as.numeric(.data$value), truncate_min_cgm))
  }
  if (!is.null(truncate_max_cgm) && !is.na(truncate_max_cgm)) {
    cgm <- cgm %>% mutate(value = pmin(as.numeric(.data$value), truncate_max_cgm))
  }
  
  # Downsample CGM to 5 minute intervals
  # Reason: duplicate data streams
  if (!is.null(min_minutes_difference_cgm) && !is.na(min_minutes_difference_cgm)) {
    cgm <- cgm %>%
      arrange(.data$timestamp) %>%
      # round to the nearest 5 minute interval
      mutate(time_bin = floor_date(.data$timestamp, unit = paste0(min_minutes_difference_cgm, " minutes"))) %>%
      group_by(.data$time_bin) %>%
      slice_min(order_by = abs(as.numeric(difftime(.data$timestamp, .data$time_bin, units = "secs"))), 
                n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(-"time_bin")
  }
  
  return(cgm)
}

