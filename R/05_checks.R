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
#' Accepts either the output of \code{\link{parse_data}} for a single individual
#' or the output of \code{\link{merge_all}} for multiple individuals. Every
#' individual found in the data is checked separately.
#'
#' @param data A list with all data found for one individual (the output of
#'   \code{\link{parse_data}}) or for multiple individuals (the output of
#'   \code{\link{merge_all}}). Requires cleaned and standardized data.
#' @param necessary_types A character vector with type names for which data should be available.
#'
#' @return No return value, called for side effects. Issues a warning for every
#'   individual and data type with missing data.
#' @examples
#' # A mylife export contains no CGM and no basal data
#' path <- system.file("extdata", "mylife_example.csv", package = "aidR")
#' data <- parse_data(id = "1", paths = path)
#' check_completeness(data)
#'
#' # Only require bolus and carbohydrate data
#' check_completeness(data, necessary_types = c("bolus", "carbs"))
#' @export
check_completeness <- function(data,
                               necessary_types = c("cgm", "basal", "bolus", "carbs")){
  if (is.null(necessary_types)){ return(invisible(NULL)) }
  
  for (id in .ids_in_data.aidR(data)){
    .check_completeness_id.aidR(id, .subset_by_id.aidR(data, id), necessary_types)
  }
  
  invisible(NULL)
}

#' Check if data for different types are available for a range of dates
#'
#' Accepts either the output of \code{\link{parse_data}} for a single individual
#' or the output of \code{\link{merge_all}} for multiple individuals. Every
#' individual found in the data is checked separately.
#'
#' @param data A list with all data found for one individual (the output of
#'   \code{\link{parse_data}}) or for multiple individuals (the output of
#'   \code{\link{merge_all}}). Requires cleaned and standardized data.
#' @param first_date A character string representing the first date for which data should be available.
#' @param last_date A character string representing the last date (included) for which data should be available.
#' @param necessary_types A character vector with type names for which data should be available.
#'
#' @return No return value, called for side effects. Issues a warning for every
#'   individual and data type with missing days.
#' @examples
#' # The example export covers 2026-06-22 only
#' path <- system.file("extdata", "glooko_example.zip", package = "aidR")
#' data <- parse_data(id = "1", paths = path)
#' check_completeness_range(data, first_date = "2026-06-21", last_date = "2026-06-23")
#' @export
check_completeness_range <- function(data, first_date, last_date, 
                                     necessary_types = c("cgm", "basal", "bolus", "carbs")){
  if (is.null(necessary_types)){ return(invisible(NULL)) }
  
  expected <- seq(date(first_date), date(last_date), by = "day")
  
  for (id in .ids_in_data.aidR(data)){
    .check_completeness_range_id.aidR(id, .subset_by_id.aidR(data, id), expected, necessary_types)
  }
  
  invisible(NULL)
}

#' Check that the summed insulin matches the daily totals reported by an export
#'
#' Compares the insulin summed over the individual data points with the daily
#' total reported by the export itself. Only daily totals with
#' \code{source == "reported"} are checked: where aidR summed the total itself
#' (\code{source == "summed"}), the two agree by construction. In practice this
#' means Glooko (basal and bolus) and Tandem Source (basal only).
#'
#' Accepts either the output of \code{\link{parse_data}} for a single individual
#' or the output of \code{\link{merge_all}} for multiple individuals. Every
#' individual found in the data is checked separately.
#'
#' By default, only the basal insulin is checked, which is where the mismatches
#' described below arise. Pass \code{types = c("basal", "bolus")} to check the
#' bolus insulin as well.
#'
#' A large mismatch means that the individual data points do not cover the whole
#' day. The most common cause are AID systems that record basal rates for manual
#' mode only (e.g. Omnipod on Glooko): the basal insulin delivered in automated
#' mode is missing from the basal rates, but is included in the reported total.
#'
#' Note that the first and the last day of an export are usually only partially
#' covered, so a mismatch on those days is expected.
#'
#' @param data A list with all data found for one individual (the output of
#'   \code{\link{parse_data}}) or for multiple individuals (the output of
#'   \code{\link{merge_all}}). Requires cleaned and standardized data.
#' @param types Character vector with the types of insulin that are checked, any
#'   of \code{"basal"} and \code{"bolus"}.
#' @param rel_tol Numeric, the relative tolerance of the comparison.
#' @param abs_tol Numeric, the absolute tolerance of the comparison, in units of
#'   insulin. A day is flagged when the absolute difference exceeds both
#'   \code{abs_tol} and \code{rel_tol} times the reported total.
#'
#' @return Invisibly, a data frame with one row per checked individual, day and
#'   type, holding the summed and the reported total and whether the two agree.
#'   Called for its side effects: a warning is issued for every day where they
#'   do not.
#' @examples
#' path <- system.file("extdata", "glooko_example.zip", package = "aidR")
#' data <- parse_data(id = "1", paths = path)
#' check_insulin_totals(data)
#'
#' # Check bolus insulin as well, and keep the comparison
#' res <- check_insulin_totals(data, types = c("basal", "bolus"))
#' res
#' @export
check_insulin_totals <- function(data, types = "basal", rel_tol = 0.2, abs_tol = 1){
  if ((any(!(types %in% c("basal", "bolus"))))){
    stop("Invalid types '", paste0(types, collapse = ", "), "'. Must be 'basal' or 'bolus' or both.")
  }
  
  res <- list()
  for (id in .ids_in_data.aidR(data)){
    data_id <- .subset_by_id.aidR(data, id)
    for (type in types){
      res[[length(res) + 1]] <- .check_insulin_total.aidR(id, data_id, type, rel_tol, abs_tol)
    }
  }
  
  invisible(bind_rows(res))
}

#------------------------
# Helper functions
#------------------------

#' All participant identifiers found in a list of standardized data
#'
#' Collects the ids of every data frame that carries an \code{id} column, so
#' that the data of a single individual (the output of \code{\link{parse_data}})
#' and the data of multiple individuals (the output of \code{\link{merge_all}})
#' can be treated alike.
#'
#' @param data A list with all data found for one or several individuals.
#'
#' @return A character vector with the ids found, in the order in which they
#'   appear. \code{NA_character_} if no id could be determined (e.g. because
#'   every data frame is empty), so that the data is still checked once, then as
#'   an individual of unknown id.
#'
#' @keywords internal
.ids_in_data.aidR <- function(data){
  if (!inherits(data, "list")){
    stop("Require a list with the data of one or several individuals, e.g. the output of 
         parse_data() or merge_all().", call. = FALSE)
  }
  
  ids <- unlist(lapply(data, function(df){
    if (is.null(df) || !is.data.frame(df) || !("id" %in% names(df))){ return(NULL) }
    as.character(df$id)
  }), use.names = FALSE)
  
  ids <- unique(ids[!is.na(ids)])
  if (length(ids) == 0){ return(NA_character_) }
  
  return(ids)
}

#' Keep the rows of one individual only, across all data types
#'
#' @param data A list with all data found for one or several individuals.
#' @param id Character participant identifier, or \code{NA} to leave the data
#'   untouched (the id could not be determined, see \code{\link{.ids_in_data.aidR}}).
#'
#' @return The input list, with every data frame reduced to the rows of
#'   \code{id}. Data frames without an \code{id} column are returned unchanged.
#'
#' @keywords internal
.subset_by_id.aidR <- function(data, id){
  if (is.na(id)){ return(data) }
  
  lapply(data, function(df){
    if (is.null(df) || !is.data.frame(df) || !("id" %in% names(df))){ return(df) }
    df[!is.na(df$id) & as.character(df$id) == id, , drop = FALSE]
  })
}

#' Prefix identifying an individual in a warning message
#'
#' @param id Character participant identifier, or \code{NA} if it is unknown.
#'
#' @return A string, empty if \code{id} is \code{NA}.
#'
#' @keywords internal
.id_prefix.aidR <- function(id){
  if (length(id) != 1 || is.na(id)){ return("") }
  
  paste0("Id ", id, ": ")
}

#' Check if data for different types are available for at least one day, for one individual
#'
#' @param id Character participant identifier, or \code{NA} if it is unknown.
#' @param data_id A list with all data found for one individual.
#' @param necessary_types A character vector with type names for which data should be available.
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
.check_completeness_id.aidR <- function(id, data_id, necessary_types){
  for (type in necessary_types){
    df <- data_id[[type]]
    
    # Is the type present?
    if (is.null(df)){
      warning(.id_prefix.aidR(id), "No data of type '", type, "' found!", call. = FALSE)
      next # No need to check further
    }
    
    # Now check that at least one day of data is available
    if (nrow(df) == 0){
      warning(.id_prefix.aidR(id), "No data of type '", type, "' found for any day!", call. = FALSE)
    }
  }
}

#' Check if data for different types are available for a range of dates, for one individual
#'
#' @param id Character participant identifier, or \code{NA} if it is unknown.
#' @param data_id A list with all data found for one individual.
#' @param expected A vector of dates for which data should be available.
#' @param necessary_types A character vector with type names for which data should be available.
#'
#' @return No return value, called for side effects.
#'
#' @keywords internal
.check_completeness_range_id.aidR <- function(id, data_id, expected, necessary_types){
  for (type in necessary_types){
    df <- data_id[[type]]
    
    # Is the type present?
    if (is.null(df)){
      warning(.id_prefix.aidR(id), "No data of type '", type, "' found!", call. = FALSE)
      next # No need to check further
    }
    
    # Now check for completeness of data
    if (nrow(df) > 0 && !("timestamp" %in% names(df))){
      stop("No column 'timestamp' in data of type '", type, "'! Was the data cleaned and standardized?",
           call. = FALSE)
    }
    
    missing <- if (nrow(df) == 0) expected else expected[!expected %in% date(df$timestamp)]
    
    if (length(missing) > 0){
      str <- ifelse(length(missing) == 1, "day", "days")
      warning(.id_prefix.aidR(id), "No data of type '", type, "' found for ", str, " ", 
              .format_date_ranges.aidR(missing), "!", call. = FALSE)
    }
  }
}

#' Compare the summed and the reported daily insulin of one data type
#'
#' @param id Character participant identifier, or \code{NA} if it is unknown.
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
    warning(.id_prefix.aidR(id), "No data for ", type, " insulin found - this does not match the reported '",
            total_type, "' (",
            .format_date_ranges.aidR(reported$date), ")!", call. = FALSE)
    return(NULL)
  }

  # A day can be reported more than once, with differing values, when two
  # overlapping exports were parsed for the same id -> keep the largest value,
  # which is the one of the export that covers the full day
  reported <- reported[order(reported$date, -reported[[total_type]]), , drop = FALSE]
  reported <- reported[!duplicated(reported$date), , drop = FALSE]
  
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
    warning(.id_prefix.aidR(id), "The summed ", type, " insulin does not match the reported '",
            total_type, "' for ", ifelse(length(bad) == 1, "day", "days"), " ",
            .format_date_ranges.aidR(bad), "!", call. = FALSE)
  }
  
  return(cmp)
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
