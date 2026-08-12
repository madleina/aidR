##################################
#                                #
#   Read Yourloops data          #
#                                #
##################################

#------------------------
# Public functions
#------------------------

#' Read file from a Yourloops export
#'
#' @param id Character or numeric participant identifier.
#' @param filename Character string corresponding to the filename of the Yourloops export.
#'
#' @return An instance of class \code{yourloops}. NULL if the data was not parsed.
read_yourloops <- function(id, filename) {
  # Get file type
  file_type <- .get_yourloops_file_type.aidR(filename)

  # Read file
  data <- list()
  data[[file_type]] <- .read_yourloops_files.aidR(filename)

  class(data) <- "yourloops"

  return(data)
}

#' Format and clean the Yourloops data to keep relevant columns only.
#'
#' @param data An instance of class \code{yourloops}, wrapping a named list with data of a particular data type.
#' @param ... Additional arguments passed to methods.
#'
#' @return A named list with names \code{cgm}, \code{basal}, \code{bolus}, or \code{carbs}, or other names if data is of another type.
#' @export
clean.yourloops <- function(data, ...) {
  if (is.null(data)) {
    return(NULL)
  }
  if (!("yourloops" %in% class(data))) {
    stop("Expected Yourloops format.")
  }

  # Format if CGM, basal, bolus and carb data (no SMBG given)
  if (names(data) == "cgm") {
    return(list(cgm = .format_cgm_yourloops.aidR(data$cgm)))
  } else if (names(data) == "basal") {
    return(list(basal = .format_basal_yourloops.aidR(data$basal)))
  } else if (names(data) == "bolus") {
    return(list(bolus = .format_bolus_yourloops.aidR(data$bolus)))
  } else if (names(data) == "meals") {
    return(list(carbs = .format_carbs_yourloops.aidR(data$meals)))
  } else if (names(data) == "rescuecarbs") {
    return(list(carbs = .format_rescuecarbs_yourloops.aidR(data$rescuecarbs)))
  }

  # All other formats: just return the way they are
  return(data)
}

#------------------------
# Helper functions
#------------------------

#' Check if a file corresponds to a file from a Yourloops export.
#'
#' @param filename Character string with the filename to be checked.
#'
#' @return A logical value: \code{TRUE} if the file is a file from a Yourloops export.
#'
#' @keywords internal
.is_yourloops_format.aidR <- function(filename) {
  file_type <- .get_yourloops_file_type.aidR(filename)
  return(!is.null(file_type))
}

#' A lookup with Yourloop file characteristics.
#'
#' @return A list with data types and their expected headers.
#'
#' @keywords internal
.get_yourloops_file_lookup.aidR <- function() {
  yourloop_files <- list()

  yourloop_files$alarms <- c("timestamp", "timezoneOffSet", "inputTime", "type", "level", "code", "acknowledge_status", "updateTimestamp")
  yourloop_files$basal <- c("timestamp", "timezoneOffSet", "duration_in_milliseconds", "rate", "deliveryType")
  yourloop_files$bolus <- c("timestamp", "timezoneOffSet", "type", "delivered", "required", "insulinOnBoard", "prescriptor", "biphasicId", "part")
  yourloop_files$cgm <- c("timestamp", "timezoneOffSet", "value", "units", "cgmModel")
  yourloop_files$meals <- c("timestamp", "timezoneOffSet", "inputTimestamp", "carbsValue", "carbUnit", "is_input_meal_fat", "bolus.type", "bolus.delivered", "bolus.insulinOnBoard", "bolus.prescriptor", "recommended.net")
  yourloop_files$physicalActivities <- c("timestamp", "timezoneOffSet", "duration.value", "duration.unit", "reportedIntensity", "inputTimestamp")
  yourloop_files$rescuecarbs <- c("timestamp", "timezoneOffSet", "confirmedCarbs", "confirmedCarbsUnits", "prescriptor", "recommendedCarbs", "recommendedCarbsUnits")
  yourloop_files$settings_cgm <- c("apiVersion", "endOfLifeTransmitterDate", "expirationDate", "manufacturer", "name", "swVersionTransmitter", "transmitterId")
  yourloop_files$settings_current_parameters <- c("name", "value", "unit", "effectiveDate")
  yourloop_files$settings_device <- c("imei", "name", "manufacturer", "operatingSystem", "osVersion", "smartphoneModel")
  yourloop_files$settings_pump <- c("expirationDate", "manufacturer", "name", "swVersion", "serialNumber")
  yourloop_files$settings_security_basals <- c("timestamp", "timezoneOffset", "rate", "start")

  return(yourloop_files)
}

#' Get file type of a Yourloops export.
#'
#' @param filename Character string with the filename to be checked.
#'
#' @return A character string if file type could be determined, NULL there was no match. If multiple files types matched, an error is thrown.
#'
#' @keywords internal
.get_yourloops_file_type.aidR <- function(filename) {
  # Note: always in English
  # Check number of columns and headers

  # Try opening file
  file <- tryCatch(
    read.csv(filename),
    error = function(e) {
      return(NULL)
    },
    warning = function(e) {
      return(NULL)
    }
  )

  lookup <- .get_yourloops_file_lookup.aidR()

  # Try to find appropriate file
  match <- which(sapply(lookup, function(x) {
    return(length(x) == ncol(file) && all(x == names(file)))
  }))

  if (length(match) == 0) {
    # no match
    return(NULL)
  } else if (length(match) == 1) {
    return(names(match))
  } else {
    # more than one match -> should never happen as colnames of yourloop files are unique
    stop("More than one match in Yourloop files - should never happen, check lookup!")
  }
}

#' Read a file from a Yourloops export
#'
#' @param filename A character string giving the path to the file.
#'
#' @return A data frame containing the contents of the specified sheet, or
#'   \code{NULL} if the sheet does not exist in the file.
#' @keywords internal
.read_yourloops_files.aidR <- function(filename) {
  # Open file(s)
  f <- read.csv(filename)
  return(f)
}

#' Format timestamp to get local (wall-clock) time of a Yourloops export
#'
#' @param file A data frame.
#'
#' @return A data frame where the timezone has been formatted to local time.
#' @keywords internal
.format_timestamp_yourloops.aidR <- function(file) {
  # Format timestamp and add timezone (important!)
  # timestamp is given in Zulu (UTC) time, to get local time, we add timezone
  
  if ("timestamp" %in% names(file) && "timezoneOffSet" %in% names(file)) {
    file$timestamp <- as_datetime(file$timestamp) + minutes(file$timezoneOffSet)
  }
  return(file)
}


#------------------------
# Format CGM
#------------------------

#' Format and clean CGM data from Yourloops
#'
#' @param cgm A data frame containing the CGM data from a Yourloops file.
#'
#' @return A data frame containing the formatted and cleaned CGM data.
#' @keywords internal
.format_cgm_yourloops.aidR <- function(cgm) {
  # Format timestamp
  cgm <- .format_timestamp_yourloops.aidR(cgm)
  
  # Convert to mg/dL, if necessary
  cgm[cgm$units != "mg/dL"] <- cgm[cgm$units != "mg/dL"] * 18.018

  cgm <- data.frame(
    timestamp = cgm$timestamp,
    timezone_offset = -cgm$timezoneOffSet / 60, # take -offset as we've added it to timestamp before
    value = cgm$value,
    unit = "mg/dL",
    pump_name = cgm$cgmModel
  )

  return(cgm)
}

#------------------------------
# Functions for basal
#------------------------------

#' Format and clean basal data from Yourloops
#'
#' @param basal A data frame containing the basal data from a Yourloops file.
#'
#' @return A data frame containing the formatted and cleaned basal data.
#' @keywords internal
.format_basal_yourloops.aidR <- function(basal) {
  # Format timestamp
  basal <- .format_timestamp_yourloops.aidR(basal)
  
  basal <- basal %>%
    mutate(
      duration = .data$duration_in_milliseconds / (1000 * 3600), # convert to hours
      unit = "U/h",
      pump_name = NA # not given in file
    ) %>%
    mutate(timezone_offset = -.data$timezoneOffSet / 60) |> # take -offset as we've added it to timestamp before
    select("timestamp", "timezone_offset", "duration", "rate", "unit", "pump_name")
  
  return(basal)
}

#------------------------------
# Functions for bolus
#------------------------------

#' Format and clean bolus data from Yourloops
#'
#' @param bolus A data frame containing the bolus data from a Yourloops file.
#'
#' @return A data frame containing the formatted and cleaned bolus data.
#' @keywords internal
.format_bolus_yourloops.aidR <- function(bolus) {
  # Format timestamp
  bolus <- .format_timestamp_yourloops.aidR(bolus)
  
  # Note: biphasic boluses are basically two individual normal boluses
  # -> treat them separate, but still flag them as "biphasic"
  bolus <- bolus %>%
    mutate(
      timezone_offset = -.data$timezoneOffSet / 60, # take -offset as we've added it to timestamp before
      normal = .data$delivered,
      total = .data$normal,
      extended = NA,
      unit = "U",
      duration_extended = NA,
      pump_name = NA
    ) %>%
    select("timestamp", "timezone_offset", "type", "total", "normal", 
           "extended", "unit", "duration_extended", "pump_name")
  
  return(bolus)
}

#------------------------------
# Functions for carbs
#------------------------------

#' Format and clean carbs data from Yourloops
#'
#' @param carbs A data frame containing the carbs data from a Yourloops file.
#'
#' @return A data frame containing the formatted and cleaned carbs data.
#' @keywords internal
.format_carbs_yourloops.aidR <- function(carbs) {
  # Format timestamp
  carbs <- .format_timestamp_yourloops.aidR(carbs)
  
  carbs <- carbs %>%
    mutate(
      timezone_offset = -.data$timezoneOffSet / 60, # take -offset as we've added it to timestamp before
      carbs = .data$carbsValue,
      unit = "g",
      label = paste0("is_input_meal_fat_", .data$is_input_meal_fat),
      estimated_absorption_duration = NA,
      is_hypo_treatment = NA
    ) %>%
    select("timestamp", "timezone_offset", "carbs", "unit", "label", "estimated_absorption_duration", "is_hypo_treatment")
  
  return(carbs)
}

#------------------------------
# Functions for rescuecarbs
#------------------------------

#' Format and clean rescuecarbs data from Yourloops
#'
#' @param carbs A data frame containing the rescuecarbs data from a Yourloops file.
#'
#' @return A data frame containing the formatted and cleaned rescuecarbs data.
#' @keywords internal
.format_rescuecarbs_yourloops.aidR <- function(carbs) {
  # Format timestamp
  carbs <- .format_timestamp_yourloops.aidR(carbs)
  
  carbs <- carbs %>%
    mutate(
      timezone_offset = -.data$timezoneOffSet / 60, # take -offset as we've added it to timestamp before
      carbs = .data$confirmedCarbs,
      unit = "g",
      label = "rescuecarbs",
      estimated_absorption_duration = NA,
      is_hypo_treatment = TRUE
    ) %>%
    select("timestamp", "timezone_offset", "carbs", "unit", "label", "estimated_absorption_duration", "is_hypo_treatment")
  
  return(carbs)
}




