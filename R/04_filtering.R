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
#' path <- system.file("extdata", "tandem_source_example.zip", package = "aidR")
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

#' Filter carbohydrate entries
#'
#' Removes carbohydrate entries that were overwritten by another entry.
#'
#' In the bolus calculator, carbs may be entered, but the bolus may not be
#' delivered (or cancelled), and the carbs may then be entered again, with a
#' bolus shortly after. Some platforms (e.g. Glooko) export all of these
#' entries.
#'
#' A carbohydrate entry is removed if
#' \itemize{
#'   \item its bolus is zero (found via \code{link_id})
#'   \item another carbohydrate entry of the same \code{format} (or sharing
#'     one, for combined formats such as \code{"glooko/tidepool"}), with a
#'     different \code{link_id}, lies at most \code{max_minutes_overwrite}
#'     minutes before or after it.
#'   \item it is not flagged as a hypo treatment (\code{is_hypo_treatment}).
#' }
#' Slowly absorbed carbohydrates merged during cleaning (linked entries with an
#' \code{estimated_absorption_duration}, see \code{\link{clean.glooko}}) are
#' never removed and do not count as another entry. The zero bolus linked to a
#' removed entry is removed from the bolus data as well.
#'
#' @param data_id A list with all data found for one individual. Filtering is
#'   only performed if the carbohydrate and bolus data were cleaned and
#'   standardized.
#' @param max_minutes_overwrite Maximum time difference (in minutes) between
#'   an overwritten entry and another entry. Default 2 minutes. If NULL,
#'   nothing is removed.
#'
#' @return A list with all data found for one individual, filtered, if
#'   necessary. Removed rows are stored in the attribute \code{removed}, see
#'   \code{removed_entries()}.
#' @examples
#' path <- system.file("extdata", "glooko_example.zip", package = "aidR")
#' data <- parse_data(id = "1", paths = path)
#' nrow(data$carbs)
#'
#' data_filtered <- filter_carbs(data)
#' nrow(data_filtered$carbs)
#' removed_entries(data_filtered)
#' @export
filter_carbs <- function(data_id, max_minutes_overwrite = 2){
  if (is.null(data_id)) { return(data_id) }
  
  # Carbs and bolus: only if cleaned
  if (!(all(c("carbs", "bolus") %in% names(data_id)) &&
        all(c("format", "timestamp", "link_id") %in% names(data_id$carbs)) &&
        all(c("total", "link_id") %in% names(data_id$bolus)))){
    return(data_id)
  }
  
  if (!is.null(max_minutes_overwrite) && !is.na(max_minutes_overwrite)) {
    data_id <- .filter_overwritten_carbs.aidR(data_id, max_minutes_overwrite = max_minutes_overwrite)
  }
  
  return(data_id)
}

#' Get all entries removed during cleaning and filtering
#'
#' Rows can be removed while cleaning (e.g. merged slowly absorbed
#' carbohydrates in \code{\link{clean.glooko}}) and by filters (e.g.
#' overwritten carbohydrate entries in \code{\link{filter_carbs}}).
#'
#' @param data_id A list with all data found for one individual.
#'
#' @return A named list with one data frame per data type, holding the removed
#'   rows and the column \code{removed_reason}. \code{NULL} if nothing was
#'   removed.
#' @examples
#' path <- system.file("extdata", "glooko_example.zip", package = "aidR")
#' data <- filter_carbs(parse_data(id = "1", paths = path))
#' removed_entries(data)
#' @export
removed_entries <- function(data_id){
  attr(data_id, "removed")
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

#' Remove carbohydrate entries that were overwritten by another entry
#'
#' See \code{filter_carbs()} for filtering rules. The zero bolus rows are removed
#' from the bolus data as well, and all removed rows are logged.
#'
#' @param data_id A list with all data found for one individual.
#' @param max_minutes_overwrite Maximum time difference (in minutes) between
#'   the overwritten entry and another entry.
#'
#' @return \code{data_id} without the overwritten entries.
#'
#' @keywords internal
.filter_overwritten_carbs.aidR <- function(data_id, max_minutes_overwrite = 2){
  # Find indices that are overwritten
  ix <- .find_overwritten_carbs.aidR(data_id$carbs, data_id$bolus, max_minutes_overwrite)
  if (length(ix$carbs) == 0) { return(data_id) }
  
  carbs <- data_id$carbs
  bolus <- data_id$bolus
  
  # Store all carbs that will be removed in separate data frame
  # then add it to data_id (under attribute 'removed')
  removed <- carbs[ix$carbs, , drop = FALSE]
  removed$matched_to_timestamp <- carbs$timestamp[ix$close_carbs]
  removed$matched_to_value     <- carbs$value[ix$close_carbs]
  data_id <- .log_removed.aidR(data_id, "carbs", removed,
                               reason = "overwritten carbohydrate entry")
  data_id <- .log_removed.aidR(data_id, "bolus", bolus[ix$bolus, , drop = FALSE],
                               reason = "zero bolus of overwritten carbohydrate entry")
  
  # Actually remove carbs and bolus from data
  data_id$carbs <- carbs[-ix$carbs, , drop = FALSE]
  if (length(ix$bolus) > 0) { data_id$bolus <- bolus[-ix$bolus, , drop = FALSE] }
  
  return(data_id)
}

#' Find carbohydrate entries that were overwritten by another entry
#'
#' @param carbs A data frame containing the cleaned carbohydrate data for one individual.
#' @param bolus A data frame containing the cleaned bolus data for one individual.
#' @param max_minutes_overwrite Maximum time difference (in minutes) between
#'   the overwritten entry and another entry.
#'
#' @return A list with the row indices of the overwritten carbs
#'   (\code{carbs}), of the closest other entry of each (\code{close_carbs})
#'   and of the zero bolus rows to remove (\code{bolus}).
#'
#' @keywords internal
.find_overwritten_carbs.aidR <- function(carbs, bolus, max_minutes_overwrite = 2){
  # Get bolus delivered for each carb entry (sum over all rows with link_id);
  # skip boluses not associated to a carb entry. 
  bolus_total <- tapply(as.numeric(bolus$total), bolus$link_id, sum)
  total <- unname(bolus_total[carbs$link_id])
  
  # Merged slowly absorbed carbs: never overwritten and never overwrite others
  is_slow <- if ("estimated_absorption_duration" %in% names(carbs)) {
    !is.na(carbs$link_id) & !is.na(carbs$estimated_absorption_duration)
  } else rep(FALSE, nrow(carbs))
  
  # Candidates: zero bolus and never hypo treatments
  is_hypo <- if ("is_hypo_treatment" %in% names(carbs)) carbs$is_hypo_treatment %in% TRUE else rep(FALSE, nrow(carbs))
  candidates <- which(total %in% 0 & !is_hypo & !is_slow)
  
  # There must be another entry of the same format close in time
  # (formats may be combined after merging near-duplicates, e.g. "glooko/tidepool")
  formats <- strsplit(as.character(carbs$format), "/", fixed = TRUE)
  rm_c <- integer(0)
  close_c <- integer(0)
  # Loop over all candidates
  for (i in candidates){
    # Calculate time difference between all carb entries and the candidate
    diff_time <- abs(as.numeric(difftime(carbs$timestamp, carbs$timestamp[i], units = "mins")))
    # Condition: same format, within time threshold and a different entry (other link_id)
    same_format <- vapply(formats, function(f) any(f %in% formats[[i]]), logical(1))
    other <- which(same_format & diff_time <= max_minutes_overwrite &
                     !(carbs$link_id %in% carbs$link_id[i]) & !is_slow)
    if (length(other) > 0){
      # Found at least one override (take closest one in time) -> remove candidate
      rm_c <- c(rm_c, i)
      close_c <- c(close_c, other[which.min(diff_time[other])])
    }
  }
  
  # Find index of bolus to remove (= zero bolus)
  rm_b <- which(bolus$link_id %in% carbs$link_id[rm_c])
  list(carbs = rm_c, close_carbs = close_c, bolus = rm_b)
}


