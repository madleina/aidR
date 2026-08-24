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
#'
#' @return No return value, called for side effects. Issues a warning for every
#'   data type with missing data.
#' @export
check_completeness <- function(id, data_id,
                               necessary_types = c("cgm", "basal", "bolus", "carbs")){
  if (is.null(necessary_types)){ return() }
  
  for (type in necessary_types){
    
    # Is the type present?
    type_found <- type %in% names(data_id)
    has_data <- FALSE
    
    # Now check that at least one day of data is available
    if (type_found){
      has_data <- nrow(data_id[[type]]) > 0
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
#'
#' @return No return value, called for side effects. Issues a warning for every
#'   data type with missing days.
#' @export
check_completeness_range <- function(id, data_id, first_date, last_date, 
                                     necessary_types = c("cgm", "basal", "bolus", "carbs")){
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

#' Check that the summed insulin matches the daily totals reported by an export
#'
#' Compares the insulin summed over the individual data points with the daily
#' total reported by the export itself. Only daily totals with
#' \code{source == "reported"} are checked: where aidR summed the total itself
#' (\code{source == "summed"}), the two agree by construction. In practice this
#' means Glooko (basal and bolus) and Tandem Source (basal only).
#'
#' A large mismatch means that the individual data points do not cover the whole
#' day. The most common cause are AID systems that record basal rates for manual
#' mode only (e.g. Omnipod on Glooko): the basal insulin delivered in automated
#' mode is missing from the basal rates, but is included in the reported total.
#'
#' The first and the last day of an export are usually only partially covered.
#' They are therefore skipped, unless the basal rates show that the day is
#' covered from midnight to midnight (up to \code{midnight_tol_h}).
#'
#' @param id Character or numeric participant identifier.
#' @param data_id A list with all data found for one individual.
#' @param rel_tol Numeric, the relative tolerance of the comparison.
#' @param abs_tol Numeric, the absolute tolerance of the comparison, in units of
#'   insulin. A day is flagged when the absolute difference exceeds both
#'   \code{abs_tol} and \code{rel_tol} times the reported total.
#'
#' @return Invisibly, a data frame with one row per checked day, holding the
#'   summed and the reported total and whether the two agree. Called for its
#'   side effects: a warning is issued for every day where they do not.
#' @export
check_insulin_totals <- function(id, data_id, rel_tol = 0.1, abs_tol = 1){
  res <- lapply(c("basal", "bolus"), function(type){
    .check_insulin_total.aidR(id, data_id, type, rel_tol, abs_tol)
  })
  
  invisible(bind_rows(res))
}

#------------------------
# Helper functions
#------------------------

#' Compare the summed and the reported daily insulin of one data type
#'
#' @param id Character or numeric participant identifier.
#' @param data_id A list with all data found for one individual.
#' @param type Either \code{"basal"} or \code{"bolus"}.
#' @param rel_tol Numeric, the relative tolerance of the comparison.
#' @param abs_tol Numeric, the absolute tolerance of the comparison.
#'
#' @return A data frame with one row per checked day, or \code{NULL} if there is
#'   nothing to compare.
#'
#' @keywords internal
.check_insulin_total.aidR <- function(id, data_id, type, rel_tol, abs_tol){
  total_type <- paste0("total_", type)
  reported <- data_id[[total_type]]
  
  # Nothing in reported -> can not compare
  if (is.null(reported) || nrow(reported) == 0 || !("source" %in% names(reported))){ return(NULL) }
  
  # Only reported totals carry information: summed ones agree by construction
  reported <- reported[reported$source == "reported" & !is.na(reported[[total_type]]), , drop = FALSE]
  if (nrow(reported) == 0){ return(NULL) }
  
  # No entries in basal, whereas there is data in total_basal -> warn!
  if (is.null(data_id[[type]]) || nrow(data_id[[type]]) == 0){
    warning("Id ", id, ": No data for ", type, " insulin found - this does not match the reported '",
            total_type, "' (",
            .format_date_ranges.aidR(reported$date), ")!")
    return(NULL)
  }

  # Warn about days reported more than once with differing values,
  # which happens when two overlapping exports were parsed for the same id
  conflicting <- .conflicting_dates.aidR(reported$date, reported[[total_type]])
  if (length(conflicting) > 0){
    warning("Id ", id, ": Conflicting values of '", total_type, "' reported for ",
            ifelse(length(conflicting) == 1, "day", "days"), " ",
            .format_date_ranges.aidR(conflicting), "!")
    reported <- reported[!reported$date %in% conflicting, , drop = FALSE]
  }
  reported <- reported[!duplicated(reported$date), , drop = FALSE]
  if (nrow(reported) == 0){ return(NULL) }
  
  # Sum over the individual data points
  summed <- if (type == "basal"){
    .total_basal_per_day.aidR(data_id$basal)
  } else {
    .total_bolus_per_day.aidR(data_id$bolus)
  }
  
  cmp <- data.frame(
    id     = id,
    type   = type,
    date   = reported$date,
    summed = summed[[total_type]][match(reported$date, summed$date)],
    reported = reported[[total_type]]
  )
  # A day without a single data point sums to zero, not to NA
  cmp$summed[is.na(cmp$summed)] <- 0
  
  cmp$within_tolerance <- abs(cmp$summed - cmp$reported) <=
    pmax(abs_tol, rel_tol * abs(cmp$reported))
  rownames(cmp) <- NULL
  
  bad <- cmp$date[!cmp$within_tolerance]
  if (length(bad) > 0){
    warning("Id ", id, ": The summed ", type, " insulin does not match the reported '",
            total_type, "' for ", ifelse(length(bad) == 1, "day", "days"), " ",
            .format_date_ranges.aidR(bad), "!")
  }
  
  return(cmp)
}

#' Find dates that appear more than once with differing values
#'
#' @param dates A vector of dates.
#' @param values A numeric vector of the same length as \code{dates}.
#'
#' @return A vector with the dates that carry more than one distinct value.
#'
#' @keywords internal
.conflicting_dates.aidR <- function(dates, values){
  n_distinct <- tapply(round(values, 6), as.character(dates), function(x) length(unique(x)))
  conflicting <- names(n_distinct)[n_distinct > 1]
  
  return(sort(as.Date(conflicting)))
}

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

