##################################
#                                #
#   Read Tandem Source data      #
#                                #
##################################

#------------------------
# Public functions
#------------------------

#' Read file from a Tandem Source export
#'
#' @param id Character or numeric participant identifier.
#' @param filename Character string corresponding to the filename of the Tandem Source export.
#'
#' @return An instance of class \code{tandem_source}. NULL if the data was not parsed.
read_tandem_source <- function(id, filename) {
  # Get file type
  file_type <- .get_tandem_source_file_type.aidR(filename)

  # Read file
  data <- list()
  data[[file_type]] <- .read_tandem_source_files.aidR(filename)

  class(data) <- "tandem_source"

  return(data)
}

#' Format and clean the Tandem Source data to keep relevant columns only.
#'
#' @param data An instance of class \code{tandem_source}, wrapping a named list with data of a particular data type.
#' @param ... Additional arguments passed to methods.
#'
#' @return A named list with names \code{cgm}, \code{basal}, \code{bolus}, or \code{carbs}, or other names if data is of another type.
#' @export
clean.tandem_source <- function(data, ...) {
  if (is.null(data)) {
    return(NULL)
  }
  if (!("tandem_source" %in% class(data))) {
    stop("Expected Tandem Source format.")
  }

  # Format if CGM, basal, bolus and carb data
  if (names(data) == "cgm") {
    return(list(cgm = .format_cgm_tandem_source.aidR(data$cgm)))
  } else if (names(data) == "basal") {
    return(list(basal = .format_basal_tandem_source.aidR(data$basal)))
  } else if (names(data) == "bolus") {
    return(list(bolus = .format_bolus_tandem_source.aidR(data$bolus),
                carbs = .format_carbs_tandem_source.aidR(data$bolus)))
  } else if (names(data) == "bg") {
    return(list(SMBG = .format_bg_tandem_source.aidR(data$bg)))
  }

  # All other formats: just return the way they are
  return(data)
}

#------------------------
# Helper functions
#------------------------

#' Check if a file corresponds to a file from a Tandem Source export.
#'
#' @param filename Character string with the filename to be checked.
#'
#' @return A logical value: \code{TRUE} if the file is a file from a Tandem Source export.
#'
#' @keywords internal
.is_tandem_source_format.aidR <- function(filename) {
  file_type <- .get_tandem_source_file_type.aidR(filename)
  return(!is.null(file_type))
}

#' A lookup with Tandem Source file characteristics.
#'
#' @return A list with data types and their expected headers.
#'
#' @keywords internal
.get_tandem_source_file_lookup.aidR <- function() {
  tandem_source_files <- list()

  tandem_source_files$basal <- c("Device Type", "Serial Number",	"Event Date Time", "Commanded Basal Dose (units of insulin)")
  tandem_source_files$bolus <- c("Type", "Bolus Type", "Bolus Delivery Method", "BG", "Serial Number", "Completion Date Time", "Insulin Delivered", "Food Delivered", "Correction Delivered", "Completion Status Desc", "Bolex Start Date Time", "Bolex Completion Date Time", "Bolex Insulin Delivered", "Bolex Completion Status Desc", "Standard Percent", "Duration (mins)", "Carb Size", "Target BG", "Correction Factor",	"Carb Ratio")
  tandem_source_files$cgm <- c("Device Type", "Serial Number", "Description", "Event Date Time", "Readings")
  tandem_source_files$bg <- c("Device Type", "Serial Number", "Description", "Event Date Time", "BG", "Note")
  tandem_source_files$hourly_basal <- c("Serial Number", "Event Date", "12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM")
  
  return(tandem_source_files)
}

#' Get file type of a Tandem Source export.
#'
#' @param filename Character string with the filename to be checked.
#'
#' @return A character string if file type could be determined, NULL there was no match. If multiple files types matched, an error is thrown.
#'
#' @keywords internal
.get_tandem_source_file_type.aidR <- function(filename) {
  # Note: always in English
  # Check number of columns and headers

  # Try opening file
  file <- tryCatch(
    read.csv(filename, check.names = F),
    error = function(e) {
      return(NULL)
    },
    warning = function(e) {
      return(NULL)
    }
  )

  lookup <- .get_tandem_source_file_lookup.aidR()

  # Try to find appropriate file
  match <- which(sapply(lookup, function(x) {
    if (length(x) != ncol(file)) return(FALSE)
    all(mapply(function(pat, nm) startsWith(nm, pat), x, names(file)))
  }))

  if (length(match) == 0) {
    # no match
    return(NULL)
  } else if (length(match) == 1) {
    return(names(match))
  } else {
    # more than one match -> should never happen as colnames are unique
    stop("More than one match in Tandem Source files - should never happen, check lookup!")
  }
}

#' Read a file from a Tandem Source export
#'
#' @param filename A character string giving the path to the file.
#'
#' @return A data frame containing the contents of the specified file.
#' @keywords internal
.read_tandem_source_files.aidR <- function(filename) {
  # Open file(s)
  f <- read.csv(filename, check.names = F)
  return(f)
}

#------------------------
# Format CGM
#------------------------

#' Format and clean CGM data from Tandem Source
#'
#' @param cgm A data frame containing the CGM data from a Tandem Source file.
#'
#' @return A data frame containing the formatted and cleaned CGM data.
#' @keywords internal
.format_cgm_tandem_source.aidR <- function(cgm) {

  # 5th column contains CGM readings
  # Convert units, if necessary
  if (grepl("mmol/L", names(cgm)[5])){
    cgm$value <- cgm[,5] * 18.018
  } else if (grepl("mg/dL", names(cgm)[5])){
    cgm$value <- cgm[,5]
  } else {
    stop(paste0("Unknown CGM units in column ", names(cgm)[5], "!"))
  }
  
  cgm <- data.frame(
    timestamp = as_datetime(cgm$`Event Date Time`),
    timezone_offset = NA,
    value = cgm$value,
    unit = "mg/dL",
    sensor_name = cgm$`Device Type`
  )

  return(cgm)
}

#------------------------------
# Functions for basal
#------------------------------

#' Format and clean basal data from Tandem Source
#'
#' @param basal A data frame containing the basal data from a Tandem Source file.
#'
#' @return A data frame containing the formatted and cleaned basal data.
#' @keywords internal
.format_basal_tandem_source.aidR <- function(basal) {
  
  basal <- basal |> 
    mutate(timestamp = as_datetime(.data$`Event Date Time`)) |> 
    arrange(timestamp) |> 
    mutate(timezone_offset = NA,
           duration = as.numeric(difftime(lead(timestamp), timestamp, units = "hours")),
           rate = .data$`Commanded Basal Dose (units of insulin)` / duration,
           unit = "U/h") |> 
    rename(pump_name = "Device Type") |> 
    select("timestamp", "timezone_offset", "duration", "rate", "unit", "pump_name")
  
  return(basal)
}

#------------------------------
# Functions for bolus
#------------------------------

#' Format and clean bolus data from Tandem Source
#'
#' @param bolus A data frame containing the bolus data from a Tandem Source file.
#'
#' @return A data frame containing the formatted and cleaned bolus data.
#' @keywords internal
.format_bolus_tandem_source.aidR <- function(bolus) {
  
  # Decide if a bolus is normal / dual wave / square wave
  bolus <- bolus |> 
    mutate(type = case_when(
      # normal if delivery method is not extended (can be Standard or Auto)
      .data$`Bolus Delivery Method` != "Extended" ~ "normal",
      # dual_wave if delivery method is extended and standard > 0
      .data$`Bolus Delivery Method` == "Extended" & .data$`Standard Percent` > 0 ~ "dual_wave",
      # square_wave if delivery method is extended and standard == 0
      .data$`Bolus Delivery Method` == "Extended" & .data$`Standard Percent` == 0 ~ "square_wave",
      # default: normal
      TRUE ~ "normal"
    ))
  
  # Add units of total bolus, as well as units of normal and extended bolus
  bolus <- bolus |> 
    mutate(
      total = .data$`Insulin Delivered`,
      extended = .data$`Bolex Insulin Delivered`,
      normal   = .data$total - coalesce(.data$extended, 0)
    )
  
  # Select relevant columns
  bolus <- bolus |> 
    mutate(timestamp = as_datetime(.data$`Completion Date Time`),
           timezone_offset = NA,
           duration_extended = .data$`Duration (mins)` / 60,
           unit = "U",
           pump_name = NA
           ) |> 
    select("timestamp", "timezone_offset", "type", "total", "normal", 
           "extended", "unit", "duration_extended", "pump_name")
  
  return(bolus)
}

#------------------------------
# Functions for carbs
#------------------------------

#' Format and clean carbs data from Tandem Source
#'
#' @param carbs A data frame containing the carbs data from a Tandem Source file.
#'
#' @return A data frame containing the formatted and cleaned carbs data.
#' @keywords internal
.format_carbs_tandem_source.aidR <- function(carbs) {
  carbs <- carbs |> 
    filter(.data$`Carb Size` > 0) |> 
    mutate(timestamp = as_datetime(.data$`Completion Date Time`),
           timezone_offset = NA,
           carbs = .data$`Carb Size`,
           unit = "g",
           label = NA,
           estimated_absorption_duration = NA,
           is_hypo_treatment = NA) |> 
    select("timestamp", "timezone_offset", "carbs", "unit", "label", "estimated_absorption_duration", "is_hypo_treatment")
  
  return(carbs)
}

#------------------------------
# Functions for SMBG
#------------------------------

#' Format and clean SMBG data from Tandem Source
#'
#' @param cgm A data frame containing the SMBG data from a Tandem Source file.
#'
#' @return A data frame containing the formatted and cleaned SMBG data.
#' @keywords internal
.format_bg_tandem_source.aidR <- function(bg) {
  
  # 5th column contains BG readings
  # Convert units, if necessary
  if (grepl("mmol/L", names(bg)[5])){
    bg$value <- bg[,5] * 18.018
  } else if (grepl("mg/dL", names(bg)[5])){
    bg$value <- bg[,5]
  } else {
    stop(paste0("Unknown BG units in column ", names(bg)[5], "!"))
  }
  
  bg <- data.frame(
    timestamp = as_datetime(bg$`Event Date Time`),
    timezone_offset = NA,
    value = bg$value,
    unit = "mg/dL",
    sensor_name = bg$`Device Type`
  )
  
  return(bg)
}





