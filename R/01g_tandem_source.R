##################################
#                                #
#   Read Tandem Source data      #
#                                #
##################################

#------------------------
# Public functions
#------------------------

#' Read a file from a Tandem Source export
#'
#' @param id Character or numeric participant identifier.
#' @param filename Character string with the filename of the file to be read.
#'
#' @return An instance of class \code{tandem_source}, wrapping a named list with
#'   the data of one data type.
#'
#' @keywords internal
read_tandem_source <- function(id, filename) {
  # Get file type
  file_type <- .get_tandem_source_file_type.aidR(filename)

  # Read file
  data <- list()
  data[[file_type]] <- .read_tandem_source_files.aidR(filename)

  class(data) <- "tandem_source"

  return(data)
}

#' Format and clean the Tandem Source data to keep relevant columns only
#'
#' @param data An instance of class \code{tandem_source}, wrapping a named list with data of a particular data type.
#' @param id Character or numeric participant identifier.
#' @param ... Additional arguments passed to methods.
#'
#' @return A named list with the cleaned data, named according to the data type
#'   of \code{data}: \code{cgm}, \code{basal}, \code{bolus}, \code{carbs} and
#'   \code{total_bolus} (the latter three are extracted from the bolus file),
#'   \code{total_basal} (extracted from the hourly basal file) or \code{SMBG}.
#'   \code{NULL} for all other data types, which are not cleaned.
#' @keywords internal
#' @exportS3Method
clean.tandem_source <- function(data, id, ...) {
  if (is.null(data)) {
    return(NULL)
  }
  if (!("tandem_source" %in% class(data))) {
    stop("Expected Tandem Source format.")
  }

  # Format if CGM, basal, bolus and carb data
  if (names(data) == "cgm") {
    return(list(cgm = .format_cgm_tandem_source.aidR(id, data$cgm)))
  } else if (names(data) == "basal") {
    return(list(basal = .format_basal_tandem_source.aidR(id, data$basal)))
  } else if (names(data) == "bolus") {
    bolus <- .format_bolus_tandem_source.aidR(id, data$bolus)
    return(list(bolus = bolus,
                carbs = .format_carbs_tandem_source.aidR(id, data$bolus),
                total_bolus = .total_bolus_per_day.aidR(bolus)))
  } else if (names(data) == "hourly_basal") {
    return(list(total_basal = .format_total_basal_tandem_source.aidR(id, data$hourly_basal)))
  } else if (names(data) == "bg") {
    return(list(SMBG = .format_bg_tandem_source.aidR(id, data$bg)))
  }

  # All other formats: don't clean, return NULL
  return(NULL)
}

#------------------------
# Helper functions
#------------------------

#' Check if a file comes from a Tandem Source export
#'
#' @param filename Character string with the filename to be checked.
#'
#' @return A logical value: \code{TRUE} if the file comes from a Tandem Source export.
#'
#' @keywords internal
.is_tandem_source_format.aidR <- function(filename) {
  file_type <- .get_tandem_source_file_type.aidR(filename)
  return(!is.null(file_type))
}

#' A lookup with Tandem Source file characteristics
#'
#' @return A named list with data types and their expected headers. Each element
#'   is a list of one or more alternative headers, as Tandem Source uses
#'   different column names in different exports (e.g. \code{12 AM ... 11 PM}
#'   versus \code{00 ... 23} for the hourly basal file).
#'
#' @keywords internal
.get_tandem_source_file_lookup.aidR <- function() {
  tandem_source_files <- list()

  tandem_source_files$basal <- list(c("Device Type", "Serial Number",	"Event Date Time", "Commanded Basal Dose (units of insulin)"))
  tandem_source_files$bolus <- list(c("Type", "Bolus Type", "Bolus Delivery Method", "BG", "Serial Number", "Completion Date Time", "Insulin Delivered", "Food Delivered", "Correction Delivered", "Completion Status Desc", "Bolex Start Date Time", "Bolex Completion Date Time", "Bolex Insulin Delivered", "Bolex Completion Status Desc", "Standard Percent", "Duration (mins)", "Carb Size", "Target BG", "Correction Factor",	"Carb Ratio"))
  tandem_source_files$cgm <- list(c("Device Type", "Serial Number", "Description", "Event Date Time", "Readings"))
  tandem_source_files$bg <- list(c("Device Type", "Serial Number", "Description", "Event Date Time", "BG", "Note"))
  # The hourly basal file comes in two flavours: hours can either be named
  # "12 AM" ... "11 PM" or "00" ... "23"
  tandem_source_files$hourly_basal <- list(
    c("Serial Number", "Event Date", "12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM", "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM"),
    c("Serial Number", "Event Date", "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23")
  )
  
  return(tandem_source_files)
}

#' Get the data type of a file from a Tandem Source export
#'
#' @param filename Character string with the filename to be checked.
#'
#' @return A character string with the data type, or \code{NULL} if there was no
#'   match. Throws an error if multiple data types matched.
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
  if (is.null(file)) {
    return(NULL)
  }
  if (ncol(file) == 0) {
    return(NULL)
  }

  lookup <- .get_tandem_source_file_lookup.aidR()

  # Try to find appropriate file: a data type matches if any of its alternative
  # headers matches
  match <- which(sapply(lookup, function(headers) {
    any(sapply(headers, function(x) {
      if (length(x) != ncol(file)) return(FALSE)
      all(mapply(function(pat, nm) startsWith(nm, pat), x, names(file)))
    }))
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
#' @param filename Character string with the filename of the file to be read.
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

#' Format and clean CGM data from a Tandem Source export
#'
#' @param id Character or numeric participant identifier.
#' @param cgm A data frame containing the CGM data from a Tandem Source file.
#'
#' @return A data frame containing the formatted and cleaned CGM data.
#' @keywords internal
.format_cgm_tandem_source.aidR <- function(id, cgm) {

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
    id = id, 
    format = "tandem_source",
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

#' Format and clean basal data from a Tandem Source export
#'
#' Tandem Source reports the insulin delivered since the previous entry; the rate
#' is obtained by dividing it by the duration until the next entry.
#'
#' @param id Character or numeric participant identifier.
#' @param basal A data frame containing the basal data from a Tandem Source file.
#'
#' @return A data frame containing the formatted and cleaned basal data.
#' @keywords internal
.format_basal_tandem_source.aidR <- function(id, basal) {
  
  basal <- basal |> 
    mutate(timestamp = as_datetime(.data$`Event Date Time`)) |> 
    arrange(timestamp) |> 
    mutate(id = id, 
           format = "tandem_source",
           timezone_offset = NA,
           duration = as.numeric(difftime(lead(timestamp), timestamp, units = "hours")),
           rate = .data$`Commanded Basal Dose (units of insulin)` / .data$duration,
           unit = "U/h") |> 
    rename(pump_name = "Device Type") |> 
    select("id", "format", "timestamp", "timezone_offset", "duration", "rate", "unit", "pump_name")
  
  return(basal)
}

#' Total basal insulin per day, as reported by a Tandem Source export
#'
#' Tandem Source reports the basal insulin delivered in each hour of the day, in
#' one file with one row per day and one column per hour. Summing over the 24
#' hourly columns gives the total basal insulin delivered on that day.
#'
#' @param id Character or numeric participant identifier.
#' @param hourly_basal A data frame containing the hourly basal data from a
#'   Tandem Source file.
#'
#' @return A data frame with columns \code{id}, \code{format}, \code{date},
#'   \code{total_basal} and \code{source}, holding one row per day.
#' @keywords internal
.format_total_basal_tandem_source.aidR <- function(id, hourly_basal) {
  if (is.null(hourly_basal) || nrow(hourly_basal) == 0){ return(NULL) }
  
  # First two columns are the serial number and the date, the remaining 24 are
  # the hours of the day (12 AM ... 11 PM)
  hours <- hourly_basal[, 3:ncol(hourly_basal), drop = FALSE]
  hours <- as.data.frame(lapply(hours, as.numeric))
  
  res <- data.frame(
    id = id,
    format = "tandem_source",
    date = date(as_datetime(hourly_basal$`Event Date`)),
    total_basal = apply(hours, 1, .sum_or_na.aidR),
    source = "reported"
  )
  
  # Note: there may be several entries per day if more than one pump was used
  res <- res %>% 
    group_by(.data$id, .data$format, .data$date, .data$source) %>% 
    summarise(total_basal = .sum_or_na.aidR(.data$total_basal), .groups = "drop") %>% 
    as.data.frame() %>% 
    select("id", "format", "date", "total_basal", "source")
  
  return(res[order(res$date), ])
}

#------------------------------
# Functions for bolus
#------------------------------

#' Format and clean bolus data from a Tandem Source export
#'
#' @param id Character or numeric participant identifier.
#' @param bolus A data frame containing the bolus data from a Tandem Source file.
#'
#' @return A data frame containing the formatted and cleaned bolus data.
#' @keywords internal
.format_bolus_tandem_source.aidR <- function(id, bolus) {
  
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
      # normal and extended are never NA: where the export records no amount,
      # the portion is zero
      extended = coalesce(.data$`Bolex Insulin Delivered`, 0),
      total    = coalesce(.data$`Insulin Delivered`, .data$extended),
      normal   = .data$total - .data$extended
    )
  
  # Select relevant columns
  bolus <- bolus |> 
    mutate(id = id, 
           format = "tandem_source",
           timestamp = as_datetime(.data$`Completion Date Time`),
           timezone_offset = NA,
           duration_extended = .data$`Duration (mins)` / 60,
           unit = "U",
           pump_name = NA
           ) |> 
    select("id", "format", "timestamp", "timezone_offset", "type", "total", "normal", 
           "extended", "unit", "duration_extended", "pump_name")
  
  return(bolus)
}

#------------------------------
# Functions for carbs
#------------------------------

#' Format and clean carbohydrate data from a Tandem Source export
#'
#' @param id Character or numeric participant identifier.
#' @param carbs A data frame containing the bolus data from a Tandem Source file,
#'   which holds the carbohydrates entered into the bolus calculator.
#'
#' @return A data frame containing the formatted and cleaned carbohydrate data.
#' @keywords internal
.format_carbs_tandem_source.aidR <- function(id, carbs) {
  carbs <- carbs |> 
    filter(.data$`Carb Size` > 0) |> 
    mutate(id = id, 
           format = "tandem_source",
           timestamp = as_datetime(.data$`Completion Date Time`),
           timezone_offset = NA,
           value = .data$`Carb Size`,
           unit = "g",
           label = NA,
           estimated_absorption_duration = NA,
           is_hypo_treatment = NA) |> 
    select("id", "format", "timestamp", "timezone_offset", "value", "unit", 
           "label", "estimated_absorption_duration", "is_hypo_treatment")
  
  return(carbs)
}

#------------------------------
# Functions for SMBG
#------------------------------

#' Format and clean SMBG data from a Tandem Source export
#'
#' @param id Character or numeric participant identifier.
#' @param bg A data frame containing the SMBG data from a Tandem Source file.
#'
#' @return A data frame containing the formatted and cleaned SMBG data.
#' @keywords internal
.format_bg_tandem_source.aidR <- function(id, bg) {
  
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
    id = id, 
    format = "tandem_source",
    timestamp = as_datetime(bg$`Event Date Time`),
    timezone_offset = NA,
    value = bg$value,
    unit = "mg/dL",
    sensor_name = bg$`Device Type`
  )
  
  return(bg)
}





