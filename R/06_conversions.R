##################################
#                                #
#  Conversion to other formats   #
#                                #
##################################

#------------------------
# Public functions
#------------------------

#' Convert CGM data to a format compatible with the R-package iglu
#'
#' @param data A list with all data found for one individual (the output of
#'   \code{\link{parse_data}}) or for multiple individuals (the output of
#'   \code{\link{merge_all}}). Requires cleaned and standardized CGM data.
#'
#' @return A data frame with columns \code{id}, \code{time} and \code{gl},
#'   compatible with the R-package iglu.
#' @examples
#' path <- system.file("extdata", "dexcom_clarity_example.csv", package = "aidR")
#' data <- parse_data(id = "1", paths = path)
#' iglu_df <- get_iglu_format(data)
#' iglu::plot_glu(iglu_df)
#' @export
get_iglu_format <- function(data){
  if (!inherits(data, "list") || 
      !("cgm" %in% names(data)) || 
      !("value" %in% names(data$cgm))){
    stop("Require a list with attribute 'cgm'. 
         Please make sure parse_data() was run with clean = TRUE.")
  }
  
  cgm <- data$cgm[, c("id", "timestamp", "value")]
  iglu_df <- iglu::process_data(cgm, id = "id", timestamp = "timestamp", glu = "value")
  return(iglu_df)
}

#' Convert CGM data to a format compatible with the R-package cgmquantify
#'
#' Builds the data frame that the metric functions of the R-package
#' \href{https://CRAN.R-project.org/package=cgmquantify}{cgmquantify}
#' expect, with columns \code{Time}, \code{glucose}, \code{Date}, \code{time_of_day} and
#' \code{type_of_event}. This is the same layout that \code{cgmquantify::readfile()}
#' produces from a Dexcom export.
#'
#' Note that cgmquantify always related to a single subject.
#'
#' @param data A list with all data found for one individual (the output of
#'   \code{\link{parse_data}}) or for multiple individuals (the output of
#'   \code{\link{merge_all}}). Requires cleaned and standardized CGM data.
#' @param id Character or numeric participant identifier, naming the individual
#'   to convert. Can be omitted if \code{data} holds the CGM data of a single
#'   individual.
#'
#' @return A data frame with columns \code{Time} (\code{POSIXct}), \code{glucose}
#'   (mg/dL), \code{Date}, \code{time_of_day} and \code{type_of_event}
#'   (\code{1} above 180 mg/dL, \code{-1} below 70 mg/dL and \code{0} in
#'   between), sorted by time and compatible with the R-package cgmquantify.
#' @examplesIf rlang::is_installed("cgmquantify")
#' path <- system.file("extdata", "dexcom_clarity_example.csv", package = "aidR")
#' data <- parse_data(id = "1", paths = path)
#' cgmquantify_df <- get_cgmquantify_format(data)
#' cgmquantify::summary_glucose(cgmquantify_df)
#' @export
get_cgmquantify_format <- function(data, id = NULL){
  if (!inherits(data, "list") || 
      !("cgm" %in% names(data)) || 
      !("value" %in% names(data$cgm))){
    stop("Require a list with attribute 'cgm'. 
         Please make sure parse_data() was run with clean = TRUE.")
  }
  
  cgm <- data$cgm
  
  # cgmquantify summarizes one individual at a time -> pick exactly one
  all_ids <- unique(as.character(cgm$id))
  if (is.null(id)){
    if (length(all_ids) > 1){
      stop("CGM data of more than one individual found (", paste(all_ids, collapse = ", "), 
           "), but the cgmquantify metrics are defined for a single individual. 
           Please provide the individual to convert via 'id'.")
    }
    id <- all_ids
  } else {
    id <- as.character(id)
    if (length(id) != 1){ stop("Require a single 'id', got ", length(id), ".") }
    if (!(id %in% all_ids)){ stop("No CGM data found for id ", id, "!") }
  }
  cgm <- cgm[as.character(cgm$id) == id, , drop = FALSE]
  
  # The cgmquantify metrics assume mg/dL (should never happen)
  units <- unique(as.character(cgm$unit))
  units <- units[!is.na(units)]
  if (length(units) > 0 && !all(units == "mg/dL")){
    stop("Id ", id, ": Expected CGM data in mg/dL, found ", paste(units, collapse = ", "), 
         ". The cgmquantify metrics assume mg/dL.")
  }
  
  # Remove NA 
  n_missing <- sum(is.na(cgm$value))
  if (n_missing > 0){
    warning("Id ", id, ": Dropped ", n_missing, " CGM ", 
            ifelse(n_missing == 1, "value", "values"), " that are NA!")
    cgm <- cgm[!is.na(cgm$value), , drop = FALSE]
  }
  
  # Sort time
  cgm <- cgm[order(cgm$timestamp), , drop = FALSE]
  
  # Date and time of day are derived from the local (wall-clock) time, 
  # independently of the timezone the timestamps are tagged with
  value <- as.numeric(cgm$value)
  cgmquantify_df <- data.frame(
    Time          = cgm$timestamp,
    glucose       = value,
    Date          = as.Date(format(cgm$timestamp, "%Y-%m-%d")),
    time_of_day   = hms::as_hms(format(cgm$timestamp, "%H:%M:%S")),
    type_of_event = as.numeric(ifelse(value > 180, 1, ifelse(value < 70, -1, 0)))
  )
  rownames(cgmquantify_df) <- NULL
  
  return(cgmquantify_df)
}

#' Write AID data (CGM, basal, bolus, carbs and SMBG, where available) to a JSON file in DIAX format
#'
#' Accepts either the output of \code{\link{parse_data}} for a single
#' individual or the output of \code{\link{merge_all}} for
#' multiple individuals. One JSON file following the
#' \href{https://github.com/Center-for-Diabetes-Technology/DIAX}{DIAX}
#' specification is written per individual.
#'
#' @param data A list with data of one individual (output of \code{parse_data()})
#'   or of multiple individuals (output of \code{merge_all()}).
#' @param prefix Character string prepended to the id to build each output
#'   filename, e.g. \code{prefix = "output/id_"} writes
#'   \code{"output/id_1.json"} for id \code{"1"}.
#' @param ids Character or numeric vector with the individual(s) to write. 
#'   Defaults to all ids found in \code{data}.
#'
#' @return No return value, called for side effects: one JSON file is written per id.
#' @examples
#' path <- system.file("extdata", "glooko_example.zip", package = "aidR")
#' data <- parse_data(id = "1", paths = path)
#'
#' out_dir <- tempfile("diax_")
#' dir.create(out_dir)
#' write_DIAX_format(data, prefix = file.path(out_dir, "id_"))
#' list.files(out_dir)
#'
#' unlink(out_dir, recursive = TRUE)
#' @export
write_DIAX_format <- function(data, prefix, ids = NULL){
  if (!inherits(data, "list")){
    stop("Require a list with attribute 'cgm'.
         Please make sure parse_data() was run with clean = TRUE.")
  }

  # Check which types are present
  type_map <- .diax_series_map.aidR()
  types_present <- intersect(names(type_map), names(data))
  if (length(types_present) == 0){
    stop("None of the recognized data types (",
         paste(names(type_map), collapse = ", "), ") were found in 'data'.")
  }
  data <- data[types_present]

  # Get ids for which we want to write a json file
  all_ids <- unique(unlist(lapply(data, function(df) as.character(df$id))))
  if (is.null(ids)){ ids <- all_ids }
  ids <- as.character(ids)
  missing_ids <- setdiff(ids, all_ids)
  if (length(missing_ids) > 0){
    warning("No data found for id(s): ", paste(missing_ids, collapse = ", "))
    ids <- setdiff(ids, missing_ids)
  }

  # Loop over all ids and write file
  for (id in ids){
    data_id <- lapply(data, function(df) df[as.character(df$id) == id, , drop = FALSE])
    diax <- .diax_list.aidR(id, data_id)
    filename <- paste0(prefix, id, ".json")
    writeLines(toJSON(diax, indent = 2), filename)
  }

  invisible(NULL)
}

#------------------------
# Helper functions
#------------------------

#' Mapping from aidR data types to DIAX field names
#'
#' @return A named list, mapping aidR type names to DIAX field names.
#'
#' @keywords internal
.diax_series_map.aidR <- function(){
  list(cgm = "cgm", bolus = "bolus", basal = "basal_rate", carbs = "carbs", SMBG = "smbg")
}

#' Name of the column holding the series value, per aidR data type
#'
#' @return A named list, mapping aidR type names to column names.
#'
#' @keywords internal
.diax_value_col.aidR <- function(){
  list(cgm = "value", bolus = "total", basal = "rate", carbs = "value", SMBG = "value")
}

#' Name of the column holding the device name, per aidR data type
#'
#' @return A named list, mapping aidR type names to column names (\code{NA} if
#'   no device is recorded for that type).
#'
#' @keywords internal
.diax_device_col.aidR <- function(){
  list(cgm = "sensor_name", bolus = "pump_name", basal = "pump_name",
       carbs = NA, SMBG = "sensor_name")
}

#' Format timestamps according to the DIAX ISO 8601 convention
#'
#' Per the DIAX specification, timestamps are \code{"Y-m-d H:M:S"} when the
#' timezone is unknown, or \code{"Y-m-d H:M:S Z"} (offset appended, no colon)
#' when it is known.
#'
#' @param timestamp A vector of timestamps, in local time.
#' @param timezone_offset A numeric vector (same length as \code{timestamp})
#'   with the UTC offset in hours, or \code{NA} where the offset is unknown.
#'
#' @return A character vector of formatted timestamps, e.g.
#'   \code{"2025-09-27 14:02:20 -0400"} when the offset is known, or
#'   \code{"2025-09-27 14:02:20"} when it is not.
#'
#' @keywords internal
.format_diax_time.aidR <- function(timestamp, timezone_offset){
  time_str <- format(timestamp, "%Y-%m-%d %H:%M:%S")

  sign <- ifelse(timezone_offset < 0, "-", "+")
  offset_h <- abs(timezone_offset)
  hh <- floor(offset_h)
  mm <- round((offset_h - hh) * 60)
  offset_str <- sprintf("%s%02d%02d", sign, hh, mm)

  ifelse(is.na(timezone_offset), time_str, paste(time_str, offset_str))
}

#' Build one DIAX time/value series from a standardized aidR data frame
#'
#' @param type One of \code{"cgm"}, \code{"bolus"}, \code{"basal"}, \code{"carbs"} or \code{"SMBG"}.
#' @param df The corresponding standardized aidR data frame.
#'
#' @return A list with entries \code{time} and \code{value}.
#'
#' @keywords internal
.diax_series.aidR <- function(type, df){
  # Sort in time
  df <- df |> 
    arrange(timestamp)
  
  # Get column with values
  value_col <- .diax_value_col.aidR()[[type]]

  list(
    time = .format_diax_time.aidR(df$timestamp, df$timezone_offset),
    value = as.numeric(df[[value_col]])
  )
}

#' Determine the device name(s) recorded for one data type
#'
#' @param type One of \code{"cgm"}, \code{"bolus"}, \code{"basal"}, \code{"carbs"} or \code{"SMBG"}.
#' @param df The corresponding standardized aidR data frame.
#'
#' @return A character string with the device name(s), comma-separated if more
#'   than one is present, or \code{NA} if no device is recorded.
#'
#' @keywords internal
.diax_device.aidR <- function(type, df){
  device_col <- .diax_device_col.aidR()[[type]]
  if (is.na(device_col) || !(device_col %in% names(df))){
    return(NA)
  }

  devices <- unique(as.character(df[[device_col]]))
  devices <- devices[!is.na(devices)]
  if (length(devices) == 0){
    return(NA)
  }

  paste(sort(devices), collapse = ", ")
}

#' Describe the timestamp convention used, for the DIAX metadata
#'
#' The description depends on whether the timezone offset is known for all, some
#' or none of the data points.
#'
#' @param data_id A list with the standardized aidR data frames present for
#'   one individual, named by aidR type (\code{cgm}, \code{bolus}, ...).
#'
#' @return A named list to be used as the \code{time} entry of the DIAX metadata.
#'
#' @keywords internal
.diax_metadata_time.aidR <- function(data_id){
  # Time: with or without timezone
  all_local <- all(sapply(data_id, function(df) all(is.na(df$timezone_offset))))
  some_local <- any(sapply(data_id, function(df) any(is.na(df$timezone_offset))))
  
  if (all_local){
    return(list(unit = "Y-m-d H:M:S",
                description = paste(
                  "Timestamps for each data point, in local time (unknown timezone)."
                )))
  } else if (some_local){
    return(list(unit = c("Y-m-d H:M:S", "Y-m-d H:M:S Z"),
                description = paste(
                  "Timestamps for each data point, in local time.",
                  "Appended with the UTC offset (Z) where known, e.g. '2025-09-27 14:02:20 -0400';",
                  "without it otherwise, e.g. '2025-09-27 14:02:20'."
                )))
  } else {
    return(list(unit = "Y-m-d H:M:S Z",
                description = 
                  "Timestamps for each data point, in local time. Appended with the UTC offset (Z)."
                ))
  }
}

#' Build the \code{metadata} section of a DIAX list
#'
#' @param data_id A list with the standardized aidR data frames present for
#'   one individual, named by aidR type (\code{cgm}, \code{bolus}, ...).
#'
#' @return A named list to be used as the \code{metadata} entry of the DIAX list.
#'
#' @keywords internal
.diax_metadata.aidR <- function(data_id){
  descriptions <- list(
    cgm   = "Continuous Glucose Monitoring (CGM) data",
    bolus = "Insulin bolus data, meal and correction, in units",
    basal = "Basal insulin delivery data",
    carbs = "User announced carbohydrate intake associated with bolus",
    SMBG  = "Self-Monitored Blood Glucose (SMBG) data"
  )
  units <- list(cgm = "mg/dL", bolus = "U", basal = "U/h", carbs = "g", SMBG = "mg/dL")
  type_map <- .diax_series_map.aidR()

  metadata <- list(
    unique_id = "id number of the subject",
    time = .diax_metadata_time.aidR(data_id)
  )

  for (type in names(data_id)){
    entry <- list(unit = units[[type]], description = descriptions[[type]])
    device <- .diax_device.aidR(type, data_id[[type]])
    if (!is.na(device)){
      entry$device <- device
    }
    metadata[[type_map[[type]]]] <- entry
  }

  metadata
}

#' Build a full DIAX list for one individual
#'
#' @param id Character or numeric participant identifier, stored as \code{unique_id}.
#' @param data_id A list with the standardized aidR data frames for one
#'   individual, named by aidR type (\code{cgm}, \code{bolus}, ...).
#'
#' @return A list following the DIAX specification.
#'
#' @keywords internal
.diax_list.aidR <- function(id, data_id){
  type_map <- .diax_series_map.aidR()

  has_data <- vapply(data_id, function(df) !is.null(df) && nrow(df) > 0, logical(1))
  data_id <- data_id[has_data]

  diax <- list(unique_id = id)
  for (type in names(data_id)){
    diax[[type_map[[type]]]] <- .diax_series.aidR(type, data_id[[type]])
  }
  diax$metadata <- .diax_metadata.aidR(data_id)

  return(diax)
}








