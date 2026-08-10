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

  # Format if CGM, basal, bolus and carb data
  if (names(data) == "cgm") {
    return(list(cgm = .format_cgm_yourloops.aidR(data$cgm)))
  } else if (names(data) == "basal") {
    return(list(basal = .format_basal_yourloops.aidR(data$basal)))
  } else if (names(data) == "bolus") {
    return(list(bolus = .format_bolus_yourloops.aidR(data$bolus)))
  } else if (names(data) == "meals") {
    return(list(carbs = .format_carbs_yourloops.aidR(data$meals)))
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

  # Format timestamp and add timezone (important!)
  # timestamp is given in Zulu (UTC) time, to get local time, we add timezone
  if ("timestamp" %in% names(f) && "timezoneOffSet" %in% names(f)) {
    f$timestamp <- as_datetime(f$timestamp) + minutes(f$timezoneOffSet)

    # Remove offset to avoid future confusion
    f <- f %>% select(-"timezoneOffSet")
  }

  return(f)
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
  # Convert to mg/dL, if necessary
  cgm[cgm$units != "mg/dL"] <- cgm[cgm$units != "mg/dL"] * 18.018

  cgm <- data.frame(
    timestamp = cgm$timestamp,
    value = cgm$value
  )

  return(cgm)
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
  bolus <- bolus %>%
    mutate(
      sub_type = case_when(
        .data$type == "normal" ~ "standard",
        .data$type == "biphasic" ~ "biphasic",
        TRUE ~ "normal"
      ),
      duration = NA,
      extended = NA,
      normal = case_when(
        .data$sub_type == "standard" ~ .data$delivered,
        .data$sub_type == "biphasic" ~ .data$delivered,
      ),
    ) %>%
    select("timestamp", "type", "sub_type", "duration", "extended", "normal")

  return(bolus)
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
  basal <- basal %>%
    mutate(
      duration = .data$duration_in_milliseconds / (1000 * 60), # convert to minutes
      amount = .data$duration / 60 * .data$rate,
      delivery_type = .data$deliveryType,
    ) %>%
    select("timestamp", "duration", "amount", "rate")

  return(basal)
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
  carbs <- carbs %>%
    mutate(
      label = paste0("is_input_meal_fat_", .data$is_input_meal_fat),
      carbs_grams = .data$carbsValue,
      estimated_absorption_duration = NA
    ) %>%
    select("timestamp", "carbs_grams", "label", "estimated_absorption_duration")

  return(carbs)
}
