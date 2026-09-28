##################################
#                                #
#   Read Dexcom Clarity data     #
#                                #
##################################

#------------------------
# Public functions
#------------------------

#' Read a file from a Dexcom clarity export
#'
#' @param id Character or numeric participant identifier.
#' @param filename Character string with the filename of the file to be read.
#'
#' @return An instance of class \code{dexcom_clarity}, wrapping a named list with
#'   the CGM data.
#'
#' @keywords internal
read_dexcom_clarity <- function(id, filename) {
  # Read file (always CGM)
  data <- list()
  data$cgm <- .read_dexcom_clarity_files.aidR(filename)
  
  class(data) <- "dexcom_clarity"

  return(data)
}

#' Format and clean the Dexcom Clarity data to keep relevant columns only
#'
#' @param data An instance of class \code{dexcom_clarity}, wrapping a named list with CGM data.
#' @param id Character or numeric participant identifier.
#' @param ... Additional arguments passed to methods.
#'
#' @return A named list with one element, \code{cgm}, containing the cleaned
#'   CGM data.
#' @export
clean.dexcom_clarity <- function(data, id, ...) {
  if (is.null(data)) {
    return(NULL)
  }
  if (!("dexcom_clarity" %in% class(data))) {
    stop("Expected Dexcom Clarity format.")
  }

  # Format CGM data
  if (length(names(data)) != 1 | (length(names(data)) == 1 & names(data) != "cgm")) {
    stop("Unexpected data type ", paste0(names(data)), " for Dexcom Clarity! Support 'cgm' only.")
  } else {
    return(list(cgm = .format_cgm_dexcom_clarity.aidR(id, data$cgm)))
  }
}

#------------------------
# Helper functions
#------------------------

#' Find the separator used in a Dexcom Clarity export
#'
#' @param filename Character string with the filename to be checked.
#'
#' @return A character string with the separator (\code{";"} or \code{","}), or
#'   \code{NULL} if the file does not come from a Dexcom Clarity export.
#'
#' @keywords internal
.get_sep_dexcom_clarity.aidR <- function(filename) {
  # Try both ; and , as a separator (both can occur, depending on the language)
  for (sep in c(";", ",")) {
    ok <- tryCatch({
      data <- read.csv(filename, sep = sep, nrows = 50)
      ncol(data) == 14 &&
        grepl("Dexcom", data[4, 6], ignore.case = TRUE) &&
        any(grepl("EGV", data[, 3]))
    }, error = function(e) FALSE)
    
    if (isTRUE(ok)) return(sep)
  }
  NULL
}

#' Check if a file comes from a Dexcom Clarity export
#'
#' @param filename Character string with the filename to be checked.
#'
#' @return A logical value: \code{TRUE} if the file comes from a Dexcom Clarity export.
#'
#' @keywords internal
.is_dexcom_clarity_format.aidR <- function(filename) {
  !is.null(.get_sep_dexcom_clarity.aidR(filename))
}

#' Read a file from a Dexcom Clarity export
#'
#' The separator (\code{";"} or \code{","}) is detected automatically, as both
#' can occur, depending on the language of the export. Glucose values reported
#' in mmol/L are converted to mg/dL.
#'
#' @param filename Character string with the filename of the file to be read.
#'
#' @return A data frame with the CGM data, with the columns \code{timestamp},
#'   \code{device_id}, \code{value} (in mg/dL) and \code{transmitter_id}.
#' @keywords internal
.read_dexcom_clarity_files.aidR <- function(filename){
  sep <- .get_sep_dexcom_clarity.aidR(filename)
  if (is.null(sep)) {
    stop("Could not determine the separator (';' or ',') of Dexcom Clarity file ", filename, "!")
  }
  data <- read.csv(filename, sep = sep, check.names = F)
  
  # 3rd column contains event type (but is language specific, so directly take this column)
  egv <- data[,3] == "EGV"
  # Keep columns: timestamp, event type, device id, glucose value, transmitter id
  cgm <- data[egv, c(2, 7, 8, 14)]
  colnames(cgm) = c("timestamp", "device_id", "value", "transmitter_id")

  # Values that are out of range will turn into NA
  cgm$value = suppressWarnings(as.numeric(cgm$value))
  cgm$timestamp <- as_datetime(cgm$timestamp)
  
  # Convert units, if necessary
  if (grepl("mmol/L", names(data)[8])){
    cgm$value <- cgm$value * 18.018
  }
  # Else assume it's mg/dL (default)
  
  return(cgm)
}

#------------------------
# Format CGM
#------------------------

#' Format and clean CGM data from a Dexcom Clarity export
#'
#' @param id Character or numeric participant identifier.
#' @param cgm A data frame containing the CGM data from a Dexcom Clarity file.
#'
#' @return A data frame containing the formatted and cleaned CGM data.
#' @keywords internal
.format_cgm_dexcom_clarity.aidR <- function(id, cgm) {
  cgm <- data.frame(
    id = id, 
    format = "dexcom_clarity",
    timestamp = as_datetime(cgm$timestamp),
    timezone_offset = NA,
    value = cgm$value,
    unit = "mg/dL",
    sensor_name = cgm$device_id
  )

  return(cgm)
}
