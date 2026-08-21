##################################
#                                #
#  Checking data (completeness)  #
#                                #
##################################

#------------------------
# Public functions
#------------------------

#' Check if data for different types are available for at least one day
#'
#' @param id Character or numeric participant identifier.
#' @param data_id A list with all data found for one individual.
#' @param necessary_types A character vector with type names for which data should be available.
#' @param allow_total_insulin_instead_of_basal Logical, if \code{TRUE}, missing basal rate data will fall back to checking \code{total_insulin} before issuing a warning. Useful for Omnipod exports, where basal rates are commonly absent during loop mode.
#'
#' @return No return value, called for side effects. Issues a warning for every
#'   data type with missing data.
#' @export
check_completeness <- function(id, data_id,
                               necessary_types = c("cgm", "basal", "bolus", "carbs"),
                               allow_total_insulin_instead_of_basal = TRUE){
  if (is.null(necessary_types)){ return() }
  
  for (type in necessary_types){
    
    # Is the type present?
    type_found <- type %in% names(data_id)
    has_data <- FALSE
    
    # Now check that at least one day of data is available
    if (type_found){
      has_data <- nrow(data_id[[type]]) > 0
    }
    
    # If basal and allow_total_insulin_instead_of_basal: fall back to total_insulin
    # Reason: Omnipod does not report basal rates (only from manual mode) -> don't want to get warnings
    if (!has_data & type == "basal" & allow_total_insulin_instead_of_basal & "total_insulin" %in% names(data_id)){
      has_data <- nrow(data_id$total_insulin) > 0
      type_found <- has_data
    }
    
    if (!type_found){
      warning("Id ", id, ": No data of type '", type, "' found!")
      next # No need to check further
    }
    
    if (!has_data){
      warning("Id ", id, ": No data of type '", type, "' found for any day!")
    }
  }
}

#' Check if data for different types are available for a range of dates
#'
#' @param id Character or numeric participant identifier.
#' @param data_id A list with all data found for one individual.
#' @param first_date A character string representing the first date for which data should be available.
#' @param last_date A character string representing the last date (included) for which data should be available.
#' @param necessary_types A character vector with type names for which data should be available.
#' @param allow_total_insulin_instead_of_basal Logical, if \code{TRUE}, missing basal rate data will fall back to checking \code{total_insulin} before issuing a warning. Useful for Omnipod exports, where basal rates are commonly absent during loop mode.
#'
#' @return No return value, called for side effects. Issues a warning for every
#'   data type with missing days.
#' @export
check_completeness_range <- function(id, data_id, first_date, last_date, 
                                     necessary_types = c("cgm", "basal", "bolus", "carbs"),
                                     allow_total_insulin_instead_of_basal = TRUE){
  if (is.null(necessary_types)){ return() }
  
  for (type in necessary_types){
    
    # Is the type present?
    type_found <- type %in% names(data_id)
    missing <- c()
    
    # Now check for completeness of data
    if (!is.null(data_id[[type]]) && !"timestamp" %in% names(data_id[[type]])){
      stop("No column 'timestamp' in data of type '", type, "'! Was the data cleaned and standardized?")
    }
    
    expected <- seq(date(first_date), date(last_date), by = "day")
    if (type_found){
      missing  <- expected[!expected %in% date(data_id[[type]]$timestamp)]
    }
    
    # If basal and allow_total_insulin_instead_of_basal: check if data on total_insulin is present
    # Reason: Omnipod does not report basal rates (only from manual mode) -> don't want to get warnings because of this
    if ((!type_found | length(missing) > 0) & type == "basal" & allow_total_insulin_instead_of_basal & "total_insulin" %in% names(data_id)){
      # Note: check within missing as an id might have both basal and total insulin
      # -> missing dates are those for which we don't have basal nor total_insulin
      missing  <- missing[!missing %in% date(data_id$total_insulin$timestamp)]
      type_found <- TRUE
    }
    
    if (!type_found){
      warning("Id ", id, ": No data of type '", type, "' found!")
      next # No need to check further
    }
    
    if (length(missing) > 0){
      str <- ifelse(length(missing) == 1, "day", "days")
      warning("Id ", id, ": No data of type '", type, "' found for ", str, " ", .format_date_ranges.aidR(missing), "!")
    }
  }
}

#------------------------
# Helper functions
#------------------------

#' Format date ranges
#'
#' @param dates A vector with dates.
#'
#' @return A string with all consecutive dates collapsed into ranges.
#' 
#' @keywords internal
.format_date_ranges.aidR <- function(dates) {
  dates <- sort(dates)
  diffs <- c(1, diff(as.numeric(dates)))
  groups <- cumsum(diffs != 1)
  
  ranges <- tapply(dates, groups, function(g) {
    if (length(g) == 1) as.character(g)
    else paste0(min(g), " - ", max(g))
  })
  
  paste0(unname(ranges), collapse = ", ")
}

