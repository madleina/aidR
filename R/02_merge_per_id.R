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
#' a target type name as returned by \code{\link{.most_general_class}}. An
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

#' Harmonise column types across a list of data frames
#'
#' Prepares a list of data frames for row-binding by resolving column type
#' mismatches. For every column present in more than one data frame, the
#' classes are compared: if they all agree the column is left untouched,
#' otherwise every occurrence of the column is coerced to the most general 
#' common type.
#'
#' @param dfs A list of data frames.
#'
#' @return The input list with mismatched columns coerced to a common type
#'
#' @keywords internal
.harmonise_column_types.aidR <- function(dfs) {
  all_cols <- unique(unlist(lapply(dfs, names)))
  
  for (col in all_cols) {
    # first class of this column in each df that actually has it
    classes <- vapply(dfs, function(df) {
      if (col %in% names(df)) class(df[[col]])[1] else NA_character_
    }, character(1))
    present <- classes[!is.na(classes)]
    
    # all files agree (or only one file has it) -> leave untouched
    if (length(unique(present)) <= 1) next
    
    target <- .most_general_class.aidR(present)
    
    dfs <- lapply(dfs, function(df) {
      if (col %in% names(df)) df[[col]] <- .coerce_to.aidR(df[[col]], target)
      df
    })
  }
  
  dfs
}

#' Merge all data available for one individual, separately per data type.
#'
#' @param data_id A list with all data found for one individual.
#'
#' @return A list with all data found for one individual, merged for each data type. Identical entries are removed.
#' @keywords internal
.merge_per_id.aidR <- function(data_id){
  if (is.null(data_id)){ return(data_id) }
  
  # Merge data (by type, e.g. cgm, basal, ...) across all parsed files
  data_id <- .merge_by_type.aidR(data_id)

  # Sort by timestamp
  data_id <- .sort_by_time.aidR(data_id)
  
  # Remove entries that are exactly the same (per data type)
  for (i in 1:length(data_id)){
    data_id[[i]] <- data_id[[i]] %>% distinct()
  }
  
  return(data_id)
}

#' Merge all data available for one individual and one data type.
#'
#' @param data_id A list with all data found for one individual.
#'
#' @return A list with all data found for one individual, merged for each data type.
#' 
#' @keywords internal
.merge_by_type.aidR <- function(data_id){
  # Merge by type
  # Note: data is a list() with one entry per filename
  # Each 'filename' can either be 
  # - a list with individual data types (classes cgm, basal, bolus, ...), e.g. for Tidepool and Carelink
  # - a list with a single entry only (class cgm / basal / bolus / ...), e.g. for Glooko and Yourloops
  
  filenames <- names(data_id)
  
  # Collect all data frames grouped by type across all files
  by_type <- list()
  
  for (filename in filenames){
    data_id_file <- data_id[[filename]]
    
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
  
  return(merged)
}

#' Sort data frames of all data types according to timestamps.
#'
#' @param data_id A list with all data found for one individual.
#'
#' @return A list with all data found for one individual, sorted in time
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


