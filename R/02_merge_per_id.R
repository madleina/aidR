##################################
#                                #
#   Merge data for each id       #
#                                #
##################################

#------------------------
# Helper functions
#------------------------

#' Determine the most general atomic type among a set of classes
#'
#' Given the classes of a single column as it appears across several data
#' frames, returns the name of the most general atomic type that can hold all
#' of them, following the hierarchy
#' \code{logical < integer < numeric < character}.
#' If any supplied class is not one of the known atomic types (e.g.
#' \code{Date}), \code{"character"} is returned.
#'
#' @param classes Character vector of class names (typically the first element
#'   of \code{class()} for the same column across multiple files).
#'
#' @return A length-one character string, one of \code{"logical"},
#'   \code{"integer"}, \code{"numeric"} or \code{"character"}.
#'
#' @keywords internal
.most_general_class.aidR <- function(classes) {
  rank_map <- c(logical = 1L, integer = 2L, numeric = 3L, double = 3L, character = 4L)
  ranks <- rank_map[classes]
  
  # If any class isn't in the map (Date, POSIXct, factor, ...) and the
  # column disagrees across files, the only safe common type is character.
  if (anyNA(ranks)) return("character")
  
  canonical <- c("1" = "logical", "2" = "integer", "3" = "numeric", "4" = "character")
  canonical[[as.character(max(ranks))]]
}

#' Coerce a vector to a target atomic type
#'
#' Wrapper around the \code{as.*} functions, dispatching on
#' a target type name as returned by \code{\link{.most_general_class.aidR}}. An
#' unrecognised target leaves the vector unchanged.
#'
#' @param x A vector to coerce.
#' @param target Length-one character string naming the target type: one of
#'   \code{"logical"}, \code{"integer"}, \code{"numeric"} or
#'   \code{"character"}.
#'
#' @return \code{x} coerced to \code{target}, or \code{x} unchanged if
#'   \code{target} is not recognised.
#'
#' @keywords internal
.coerce_to.aidR <- function(x, target) {
  switch(target,
         logical   = as.logical(x),
         integer   = as.integer(x),
         numeric   = as.numeric(x),
         character = as.character(x),
         x)
}

#' Merge all data available for one individual, separately per data type
#'
#' @param data_id A list with all data found for one individual, with one entry
#'   per parsed file.
#'
#' @return A list with all data found for one individual, merged for each data
#'   type and sorted in time. Identical entries are removed, ignoring the \code{format}
#'   column (e.g. the same data downloaded from two platforms is kept once, with the
#'   \code{format} of its first occurrence). Entries where all measurement columns are NA are removed.
#'   For \code{total_basal} and \code{total_bolus}, only the largest value is kept for days that occur more than once.
#'   Near-duplicates from different formats are removed.
#' @keywords internal
.merge_per_id.aidR <- function(data_id){
  if (is.null(data_id)){ return(data_id) }
  
  # Merge data (by type, e.g. cgm, basal, ...) across all parsed files
  data_id <- .merge_by_type.aidR(data_id)

  # Sort by timestamp
  data_id <- .sort_by_time.aidR(data_id)
  
  # Remove entries that are exactly the same (per data type, ignoring the 'format' column)
  for (i in 1:length(data_id)){
    # (e.g. same data downloaded from two platforms) -> keep the first occurrence
    data_id[[i]] <- data_id[[i]] %>% distinct(across(-any_of(c("format"))), .keep_all = TRUE)
    
    # Remove entries where all relevant columns are NA
    data_id[[i]] <- data_id[[i]] %>%
      filter(!dplyr::if_all(!any_of(c("id", "format", "timestamp", "timezone_offset", "unit", "link_id")), is.na))
  }

  # total_basal and total_bolus:
  # A day can be reported more than once, with differing values, when two
  # overlapping exports were parsed for the same id -> keep the largest value,
  # which is the one of the export that covers the full day
  for (total_type in intersect(c("total_basal", "total_bolus"), names(data_id))){
    df <- data_id[[total_type]]
    df <- df[order(df$date, -df[[total_type]]), , drop = FALSE]
    data_id[[total_type]] <- df[!duplicated(df$date), , drop = FALSE]
  }
  
  # Finally filter on entries that were exported by two platforms, but with slightly different values or timestamps
  data_id <- .filter_near_duplicates.aidR(data_id)

  return(data_id)
}

#' Remove near-duplicate entries from different formats
#'
#' The same entry exported by two platforms may differ slightly, e.g. in the
#' timestamp (rounding), in the value (rounding) or because one platform
#' provides more details (e.g. a \code{label}). Two entries of the same data
#' type are near-duplicates if they
#' \itemize{
#'   \item come from different formats
#'   \item have the same \code{unit}
#'   \item lie at most \code{max_minutes} apart
#'   \item differ in value by at most the tolerance of the data type
#'     (\code{carbs}: 1 g, \code{SMBG}: 1 mg/dL, \code{bolus}: 0.05 U,
#'     \code{basal}: 0.05 U/h).
#' }
#' Basal rates change frequently, so that neighbouring entries often have 
#' similar rates. For \code{basal}, two entries must therefore lie at most 
#' \code{max_minutes_basal} apart and their \code{duration} must differ by at 
#' most \code{tolerance_basal_duration}. Entries with a missing \code{duration} 
#' are never considered near-duplicates.
#' Of each pair of duplicates, one entry is kept: the one with a \code{link_id},
#' otherwise the one with more information. Its timestamp and value are used,
#' and its missing information (e.g. a \code{label}) is filled from the other
#' entry. If information was filled, both formats are joined with a \code{/}
#' (e.g. \code{"glooko/tidepool"}).
#'
#' A warning is given if the entries of two formats match better when shifted
#' by one hour, see \code{\link{.check_time_shift.aidR}}.
#'
#' @param data_id A list with all data found for one individual (cleaned), with one entry
#'   per data type.
#' @param max_minutes Maximum time difference (in minutes) between two
#'   duplicates.
#' @param tolerance_carbs Tolerance for detecting duplicate carb entries, default 1 g.
#' @param tolerance_SMBG Tolerance for detecting duplicate SMBG entries, default 1 mg/dL.
#' @param tolerance_bolus Tolerance for detecting duplicate bolus entries, default 0.05 U.
#' @param tolerance_basal Tolerance for detecting duplicate basal rates, default 0.05 U/h.
#' @param tolerance_basal_duration Tolerance for the \code{duration} of duplicate
#'   basal entries, in hours, default 1 minute.
#' @param max_minutes_basal Maximum time difference (in minutes) between two
#'   duplicate basal entries.
#'
#' @return \code{data_id} without the duplicates. Removed rows are logged
#'   in the attribute \code{removed}, see \code{\link{removed_entries}}.
#'
#' @keywords internal
.filter_near_duplicates.aidR <- function(data_id, max_minutes = 5,
                                         tolerance_carbs = 1,
                                         tolerance_SMBG = 1,
                                         tolerance_bolus = 0.05,
                                         tolerance_basal = 0.05,
                                         tolerance_basal_duration = 1/60,
                                         max_minutes_basal = 2
                                         ){
  # col/tol: value column and its tolerance; col2/tol2: optional second column that must match as well
  tolerances <- list(carbs = list(col = "value", tol = tolerance_carbs, max_minutes = max_minutes),
                     SMBG  = list(col = "value", tol = tolerance_SMBG,  max_minutes = max_minutes),
                     bolus = list(col = "total", tol = tolerance_bolus, max_minutes = max_minutes),
                     basal = list(col = "rate",  tol = tolerance_basal, max_minutes = max_minutes_basal,
                                  col2 = "duration", tol2 = tolerance_basal_duration))
  
  for (type in intersect(names(tolerances), names(data_id))){
    df  <- data_id[[type]]
    tl  <- tolerances[[type]]
    col <- tl$col
    
    # Only de-duplicate if data was cleaned
    if (!all(c("format", "timestamp", col, tl$col2) %in% names(df))) { next }
    
    # Warn if two formats seem to be shifted by one hour (e.g. daylight saving time)
    .check_time_shift.aidR(df, type, col, tl$tol, tl$max_minutes, col2 = tl$col2, tol2 = tl$tol2)
    
    # Find indices of duplicates
    ix <- .find_near_duplicates.aidR(df, col, tl$tol, tl$max_minutes, col2 = tl$col2, tol2 = tl$tol2)
    if (length(ix$remove) == 0) { next }
    
    # Log the entries to remove
    removed <- df[ix$remove, , drop = FALSE]
    removed$matched_to_timestamp <- df$timestamp[ix$keep]
    removed$matched_to_value     <- df[[col]][ix$keep]
    removed$matched_to_format    <- df$format[ix$keep]
    data_id <- .log_removed.aidR(data_id, type, removed,
                                 reason = "near-duplicate of entry from another format")
    
    # Borrow information: fill NA columns of the kept entry from the removed one
    borrowed <- rep(FALSE, length(ix$keep))
    for (c in setdiff(names(df), c("id", "format", "timestamp", "timezone_offset", col, "unit"))){
      fill <- is.na(df[[c]][ix$keep]) & !is.na(df[[c]][ix$remove])
      df[[c]][ix$keep[fill]] <- df[[c]][ix$remove[fill]]
      borrowed <- borrowed | fill
    }
    # Combine the formats of entries that borrowed information
    df$format[ix$keep[borrowed]] <- paste(df$format[ix$keep[borrowed]], 
                                          df$format[ix$remove[borrowed]], sep = "/")
    
    # Now actually remove
    data_id[[type]] <- df[-ix$remove, , drop = FALSE]
  }
  
  return(data_id)
}

#' Warn if the entries of two formats are shifted by one hour
#'
#' Exports of overlapping time periods may be shifted by one hour, e.g. because
#' of daylight saving time or time zone settings. Their entries are then not
#' detected as near-duplicates. For each pair of formats, the number of
#' near-duplicates is counted with the timestamps as they are, and with the
#' timestamps of one format shifted by +/- 1 hour. A warning is given if a
#' shift yields at least \code{min_matches} near-duplicates, and more than
#' twice as many as without the shift. Nothing is changed in the data.
#'
#' @param df A data frame with the cleaned data of one data type.
#' @param type Name of the data type (used in the warning).
#' @param col Name of the column holding the value.
#' @param tol Maximum difference in value between two near-duplicates.
#' @param max_minutes Maximum time difference (in minutes) between two
#'   near-duplicates.
#' @param min_matches Minimum number of near-duplicates with the shift to give
#'   a warning.
#' @param col2 Optional name of a second column that must match as well, or \code{NULL}.
#' @param tol2 Maximum difference in \code{col2} between two near-duplicates.
#'
#' @return \code{TRUE} (invisibly) if a warning was given, \code{FALSE} otherwise.
#'
#' @keywords internal
.check_time_shift.aidR <- function(df, type, col, tol, max_minutes = 5, min_matches = 5,
                                   col2 = NULL, tol2 = NULL){
  formats <- unique(df$format)
  if (length(formats) < 2) { return(invisible(FALSE)) }
  
  shifted <- FALSE
  for (pair in combn(formats, 2, simplify = FALSE)){
    sub <- df[df$format %in% pair, , drop = FALSE]
    # Number of near-duplicates if the second format is shifted by 'shift' minutes
    n_matches <- function(shift){
      is_second <- sub$format == pair[2]
      sub$timestamp[is_second] <- sub$timestamp[is_second] + shift * 60
      sub <- sub[order(sub$timestamp), , drop = FALSE]
      length(.find_near_duplicates.aidR(sub, col, tol, max_minutes, col2 = col2, tol2 = tol2)$remove)
    }
    n_0 <- n_matches(0)
    n_shift <- max(n_matches(-60), n_matches(60))
    if (n_shift >= min_matches && n_shift > 2 * n_0){
      warning("Id ", df$id[1], ": ", type, " entries of '", pair[1], "' and '", pair[2],
              "' match better when shifted by 1 hour (", n_shift, " vs. ", n_0, " entries). ",
              "Check the time zone / daylight saving time settings of the exports.", call. = FALSE)
      shifted <- TRUE
    }
  }
  
  invisible(shifted)
}

#' Find near-duplicate entries from different formats
#'
#' @param df A data frame with the cleaned data of one data type, sorted by
#'   \code{timestamp}.
#' @param col Name of the column holding the value.
#' @param tol Maximum difference in value between two near-duplicates.
#' @param max_minutes Maximum time difference (in minutes) between two
#'   near-duplicates.
#' @param col2 Optional name of a second column that must match as well (e.g.
#'   the \code{duration} of basal entries), or \code{NULL}.
#' @param tol2 Maximum difference in \code{col2} between two near-duplicates.
#'
#' @return A list with the row indices of the entries to remove
#'   (\code{remove}) and of the entry kept instead of each (\code{keep}).
#'
#' @keywords internal
.find_near_duplicates.aidR <- function(df, col, tol, max_minutes = 5, col2 = NULL, tol2 = NULL){
  n <- nrow(df)
  none <- list(remove = integer(0), keep = integer(0))
  if (n < 2 || length(unique(df$format)) < 2) return(none)
  
  value <- as.numeric(df[[col]])
  unit  <- if ("unit" %in% names(df)) df$unit else rep(NA, n)
  # Second column: always matches if not given
  value2 <- if (!is.null(col2)) as.numeric(df[[col2]]) else rep(0, n)
  tol2   <- if (!is.null(col2)) tol2 else 0
  
  # Find all candidate pairs: look ahead while within max_minutes (df is sorted by time)
  pairs <- list()
  # Loop over all pairs (from next until end)
  for (i in 1:(n-1)){
    for (j in (i + 1):n){
      diff_time <- as.numeric(difftime(df$timestamp[j], df$timestamp[i], units = "mins"))
      if (is.na(diff_time) || diff_time > max_minutes) { break }
      # Must be different format, same unit and (almost) the same value (and second value, if given)
      if (df$format[i] != df$format[j] && identical(unit[i], unit[j]) &&
          isTRUE(abs(value[i] - value[j]) <= tol) && isTRUE(abs(value2[i] - value2[j]) <= tol2)){
        pairs[[length(pairs) + 1]] <- c(i = i, j = j, diff_time = diff_time, diff_value = abs(value[i] - value[j]))
      }
    }
  }
  if (length(pairs) == 0) { return(none) }
  pairs <- as.data.frame(do.call(rbind, pairs))
  
  # Get amount of information per entry: has a link_id, and number of non-NA columns (apart from the matched ones)
  has_link <- if ("link_id" %in% names(df)) !is.na(df$link_id) else rep(FALSE, n)
  n_info   <- rowSums(!is.na(df[, setdiff(names(df), c("id", "format", "timestamp", "timezone_offset", col, "unit")), drop = FALSE]))
  
  # Match one-to-one, closest pairs first
  pairs <- pairs[order(pairs$diff_time, pairs$diff_value), , drop = FALSE]
  used <- rep(FALSE, n)
  remove <- integer(0)
  keep <- integer(0)
  for (k in 1:nrow(pairs)){
    i <- pairs$i[k]
    j <- pairs$j[k]
    if (used[i] || used[j]) next
    used[c(i, j)] <- TRUE
    # Keep i (the first one) unless j provides more information
    keep_j <- has_link[j] > has_link[i] || (has_link[j] == has_link[i] && n_info[j] > n_info[i])
    remove <- c(remove, if (keep_j) i else j)
    keep   <- c(keep,   if (keep_j) j else i)
  }
  
  return(list(remove = remove, keep = keep))
}

#' Merge all data available for one individual and one data type
#'
#' @param data_id A list with all data found for one individual, with one entry
#'   per parsed file.
#'
#' @return A list with all data found for one individual, with one entry per data
#'   type. Rows removed during cleaning (attribute \code{removed} of each file)
#'   are merged by type as well and stored in the attribute \code{removed}.
#' 
#' @keywords internal
.merge_by_type.aidR <- function(data_id){
  # Merge by type
  # Note: data is a list() with one entry per filename
  # Each 'filename' can either be 
  # - a list with individual data types (classes cgm, basal, bolus, ...), e.g. for Tidepool and CareLink
  # - a list with a single entry only (class cgm / basal / bolus / ...), e.g. for Glooko and YourLoops
  
  filenames <- names(data_id)
  
  # Collect all data frames grouped by type across all files
  by_type <- list()
  removed_by_type <- list()
  
  for (filename in filenames){
    data_id_file <- data_id[[filename]]
    
    # Collect rows removed during cleaning (e.g. merged slowly absorbed carbs from Glooko)
    removed_file <- attr(data_id_file, "removed")
    for (type in names(removed_file)){
      # Make sure types match those of the data (see below)
      df <- removed_file[[type]] %>%
        mutate(across(where(is.character), \(x) type.convert(x, as.is = TRUE)))
      removed_by_type[[type]] <- c(removed_by_type[[type]], list(df))
    }
    
    for (type in names(data_id_file)){
      df <- data_id_file[[type]]
      
      if (is.null(df) || nrow(df) == 0) next
      
      # Make sure types match
      df <- df %>%
        mutate(across(where(is.character), \(x) type.convert(x, as.is = TRUE)))
      
      by_type[[type]] <- c(by_type[[type]], list(df))
    }
  }
  
  merged <- lapply(by_type, function(dfs) {
    dfs <- .harmonise_column_types.aidR(dfs)
    bind_rows(dfs)
  })
  
  # Same rows can be removed from overlapping exports -> keep them once
  if (length(removed_by_type) > 0){
    attr(merged, "removed") <- lapply(removed_by_type, function(dfs) {
      removed <- bind_rows(.harmonise_column_types.aidR(dfs)) %>%
        distinct(across(-any_of(c("format"))), .keep_all = TRUE)
      if ("link_id" %in% names(removed)){
        removed$link_id <- as.character(removed$link_id)
      }
      removed
    })
  }
  
  return(merged)
}

#' Sort data frames of all data types according to timestamps
#'
#' @param data_id A list with all data found for one individual, with one entry
#'   per data type.
#'
#' @return A list with all data found for one individual, sorted in time.
#' 
#' @keywords internal
.sort_by_time.aidR <- function(data_id){
  # Loop over all types
  for (i in 1:length(data_id)){
    if ("timestamp" %in% names(data_id[[i]])){
      data_id[[i]] <- data_id[[i]] %>% 
        arrange(timestamp)
    }
  }
  
  return(data_id)
}


