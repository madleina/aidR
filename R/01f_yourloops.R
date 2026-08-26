##################################
#                                #
#   Read YourLoops data          #
#                                #
##################################

#------------------------
# Public functions
#------------------------

#' Read a file from a YourLoops export
#'
#' @param id Character or numeric participant identifier.
#' @param filename Character string with the filename of the file to be read.
#'
#' @return An instance of class \code{yourloops}, wrapping a named list with the
#'   data of one data type.
#'
#' @keywords internal
read_yourloops <- function(id, filename) {
  # Get file type
  file_type <- .get_yourloops_file_type.aidR(filename)

  # Read file
  data <- list()
  data[[file_type]] <- .read_yourloops_files.aidR(filename)

  class(data) <- "yourloops"

  return(data)
}

#' Format and clean the YourLoops data to keep relevant columns only
#'
#' @param data An instance of class \code{yourloops}, wrapping a named list with data of a particular data type.
#' @param id Character or numeric participant identifier.
#' @param ... Additional arguments passed to methods.
#'
#' @return A named list with the cleaned data, named according to the data type
#'   of \code{data}: \code{cgm}, \code{basal}, \code{bolus} or \code{carbs} (from
#'   meals or from rescue carbohydrates). \code{NULL} for all other data types,
#'   which are not cleaned. Note that a YourLoops export contains no SMBG data.
#' @export
clean.yourloops <- function(data, id, ...) {
  if (is.null(data)) {
    return(NULL)
  }
  if (!("yourloops" %in% class(data))) {
    stop("Expected YourLoops format.")
  }

  # Format if CGM, basal, bolus and carb data (no SMBG given)
  if (names(data) == "cgm") {
    return(list(cgm = .format_cgm_yourloops.aidR(id, data$cgm)))
  } else if (names(data) == "basal") {
    basal <- .format_basal_yourloops.aidR(id, data$basal)
    return(list(basal = basal,
                total_basal = .total_basal_per_day.aidR(basal)))
  } else if (names(data) == "bolus") {
    bolus <- .format_bolus_yourloops.aidR(id, data$bolus)
    return(list(bolus = bolus,
                total_bolus = .total_bolus_per_day.aidR(bolus)))
  } else if (names(data) == "meals") {
    return(list(carbs = .format_carbs_yourloops.aidR(id, data$meals)))
  } else if (names(data) == "rescuecarbs") {
    return(list(carbs = .format_rescuecarbs_yourloops.aidR(id, data$rescuecarbs)))
  }

  # All other formats: don't clean, return NULL
  return(NULL)
}

#------------------------
# Helper functions
#------------------------

#' Check if a file comes from a YourLoops export
#'
#' @param filename Character string with the filename to be checked.
#'
#' @return A logical value: \code{TRUE} if the file comes from a YourLoops export.
#'
#' @keywords internal
.is_yourloops_format.aidR <- function(filename) {
  file_type <- .get_yourloops_file_type.aidR(filename)
  return(!is.null(file_type))
}

#' A lookup with YourLoops file characteristics
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

#' Get the data type of a file from a YourLoops export
#'
#' @param filename Character string with the filename to be checked.
#'
#' @return A character string with the data type, or \code{NULL} if there was no
#'   match. Throws an error if multiple data types matched.
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
    # more than one match -> should never happen as colnames of YourLoops files are unique
    stop("More than one match in YourLoops files - should never happen, check lookup!")
  }
}

#' Read a file from a YourLoops export
#'
#' @param filename Character string with the filename of the file to be read.
#'
#' @return A data frame containing the contents of the file.
#' @keywords internal
.read_yourloops_files.aidR <- function(filename) {
  # Open file(s)
  f <- read.csv(filename)
  return(f)
}

#' Format timestamps to get local (wall-clock) time of a YourLoops export
#'
#' Timestamps are given in Zulu (UTC) time; the timezone offset is added to
#' obtain local time. Data frames without these two columns are left unchanged.
#'
#' @param file A data frame obtained from reading a YourLoops file.
#'
#' @return A data frame where the timestamps have been converted to local time.
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

#' Format and clean CGM data from a YourLoops export
#'
#' @param id Character or numeric participant identifier.
#' @param cgm A data frame containing the CGM data from a YourLoops file.
#'
#' @return A data frame containing the formatted and cleaned CGM data.
#' @keywords internal
.format_cgm_yourloops.aidR <- function(id, cgm) {
  # Format timestamp
  cgm <- .format_timestamp_yourloops.aidR(cgm)
  
  # Convert to mg/dL, if necessary
  cgm[cgm$units != "mg/dL"] <- cgm[cgm$units != "mg/dL"] * 18.018

  cgm <- data.frame(
    id = id,
    format = "yourloops",
    timestamp = cgm$timestamp,
    timezone_offset = cgm$timezoneOffSet / 60,
    value = cgm$value,
    unit = "mg/dL",
    sensor_name = cgm$cgmModel
  )

  return(cgm)
}

#------------------------------
# Functions for basal
#------------------------------

#' Format and clean basal data from a YourLoops export
#'
#' @param id Character or numeric participant identifier.
#' @param basal A data frame containing the basal data from a YourLoops file.
#'
#' @return A data frame containing the formatted and cleaned basal data.
#' @keywords internal
.format_basal_yourloops.aidR <- function(id, basal) {
  # Format timestamp
  basal <- .format_timestamp_yourloops.aidR(basal)
  
  basal <- basal %>%
    mutate(
      id = id,
      format = "yourloops",
      duration = .data$duration_in_milliseconds / (1000 * 3600), # convert to hours
      unit = "U/h",
      pump_name = NA # not given in file
    ) %>%
    mutate(timezone_offset = .data$timezoneOffSet / 60) |>
    select("id", "format", "timestamp", "timezone_offset", "duration", "rate", "unit", "pump_name")
  
  return(basal)
}

#------------------------------
# Functions for bolus
#------------------------------

#' Format and clean bolus data from a YourLoops export
#'
#' Biphasic boluses are treated as two individual normal boluses, but are still
#' flagged as biphasic in the \code{type} column.
#'
#' @param id Character or numeric participant identifier.
#' @param bolus A data frame containing the bolus data from a YourLoops file.
#'
#' @return A data frame containing the formatted and cleaned bolus data.
#' @keywords internal
.format_bolus_yourloops.aidR <- function(id, bolus) {
  # Format timestamp
  bolus <- .format_timestamp_yourloops.aidR(bolus)
  
  # Note: biphasic boluses are basically two individual normal boluses
  # -> treat them separate, but still flag them as "biphasic"
  bolus <- bolus %>%
    mutate(
      id = id,
      format = "yourloops",
      timezone_offset = .data$timezoneOffSet / 60,
      # normal and extended are never NA: where the export records no amount,
      # the portion is zero
      normal = coalesce(.data$delivered, 0),
      total = .data$normal,
      extended = 0,
      unit = "U",
      duration_extended = NA,
      pump_name = NA
    ) %>%
    select("id", "format", "timestamp", "timezone_offset", "type", "total", "normal", 
           "extended", "unit", "duration_extended", "pump_name")
  
  return(bolus)
}

#------------------------------
# Functions for carbs
#------------------------------

#' Format and clean carbohydrate data from a YourLoops export
#'
#' @param id Character or numeric participant identifier.
#' @param carbs A data frame containing the meal data from a YourLoops file.
#'
#' @return A data frame containing the formatted and cleaned carbohydrate data.
#' @keywords internal
.format_carbs_yourloops.aidR <- function(id, carbs) {
  # Format timestamp
  carbs <- .format_timestamp_yourloops.aidR(carbs)
  
  carbs <- carbs %>%
    mutate(
      id = id,
      format = "yourloops",
      timezone_offset = .data$timezoneOffSet / 60,
      value = .data$carbsValue,
      unit = "g",
      label = paste0("is_input_meal_fat_", .data$is_input_meal_fat),
      estimated_absorption_duration = NA,
      is_hypo_treatment = NA
    ) %>%
    select("id", "format", "timestamp", "timezone_offset", "value", "unit", 
           "label", "estimated_absorption_duration", "is_hypo_treatment")
  
  return(carbs)
}

#------------------------------
# Functions for rescuecarbs
#------------------------------

#' Format and clean rescue carbohydrate data from a YourLoops export
#'
#' @param id Character or numeric participant identifier.
#' @param carbs A data frame containing the rescue carbohydrate data from a YourLoops file.
#'
#' @return A data frame containing the formatted and cleaned carbohydrate data,
#'   flagged as hypoglycemia treatment.
#' @keywords internal
.format_rescuecarbs_yourloops.aidR <- function(id, carbs) {
  # Format timestamp
  carbs <- .format_timestamp_yourloops.aidR(carbs)
  
  carbs <- carbs %>%
    mutate(
      id = id,
      format = "yourloops",
      timezone_offset = .data$timezoneOffSet / 60,
      value = .data$confirmedCarbs,
      unit = "g",
      label = "rescuecarbs",
      estimated_absorption_duration = NA,
      is_hypo_treatment = TRUE
    ) %>%
    select("id", "format", "timestamp", "timezone_offset", "value", "unit", 
           "label", "estimated_absorption_duration", "is_hypo_treatment")
  
  return(carbs)
}




