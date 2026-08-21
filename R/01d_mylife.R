##################################
#                                #
#   Read mylife data             #
#                                #
##################################

#------------------------
# Public functions
#------------------------

#' Read a file from a mylife export
#'
#' @param id Character or numeric participant identifier.
#' @param filename Character string with the filename of the file to be read.
#'
#' @return An instance of class \code{mylife}, wrapping a named list with one
#'   entry per entry type found in the file. \code{NULL} if the file is empty.
#'
#' @keywords internal
read_mylife <- function(id, filename){
  # Read the file line by line
  lines <- read.csv(filename, encoding = "UTF-8")
  lines <- lines[,1]
  if (length(lines) == 0){ return(NULL) }
  
  # Keep only lines that contain a date
  keep <- grepl("^.*;[0-9]{2}\\.[0-9]{2}\\.[0-9]{2};", lines)
  res <- data.frame(timestamp = as_datetime(sum(keep)), 
                    type = character(sum(keep)),
                    amount = numeric(sum(keep)),
                    units = character(sum(keep)),
                    information = character(sum(keep)),
                    note = character(sum(keep)))
  count <- 1
  for (i in 1:length(lines)){
    if (!keep[i]) next
    j <- strsplit(gsub("\\\"", "", lines[i]), ";")[[1]]
    res$timestamp[count] <- parse_date_time(paste(j[2], j[3]), c("dmy HM", "dmY HM"))
    res$type[count] <- j[4]
    res$amount[count] <- j[5]
    res$units[count] <- j[6]
    res$information[count] <- j[7]
    res$note <- j[8]
    count <- count + 1
  }
  res$type[res$type == ""] <- NA
  res$amount[res$amount == ""] <- NA
  res$units[res$units == ""] <- NA
  res$information[res$information == ""] <- NA
  res$note[res$note == ""] <- NA
  res$amount[res$amount == "N/A"] <- NA
  
  # Split by type
  data <- split(res, res$type)

  class(data) <- "mylife"
  
  return(data)
}

#' Format and clean the mylife data to keep relevant columns only
#'
#' The entry types are identified by their unit, since their labels are
#' language-specific. Note that a mylife export contains no CGM and no basal data.
#'
#' @param data An instance of class \code{mylife}, wrapping a named list with data of different types.
#' @param id Character or numeric participant identifier.
#' @param ... Additional arguments passed to methods.
#'
#' @return A named list with the cleaned data, with entries \code{bolus},
#'   \code{carbs} and \code{SMBG}. Entries are \code{NULL} if the export holds
#'   no data of that type.
#' @export
clean.mylife <- function(data, id, ...){
  if (is.null(data)){ return(NULL) }
  if (!("mylife" %in% class(data))){ stop("Expected mylife format.") }
  
  # Remove class attribute for ease
  data <- unclass(data)
  
  # find all bolus-, carb- and SMBG-related entries
  # Note: language-specific -> use unit (g) to identify them
  bolus_in_list <- sapply(data, function(x) return(all(grepl("^U", x$units))))
  carbs_in_list <- sapply(data, function(x) return(all(grepl("^g ", x$units))))
  smbg_in_list <- sapply(data, function(x) return(all(grepl("^mg/dL", x$units) | grepl("^mmol/L", x$units))))
  
  data_new <- list()
  data_new$bolus <- .clean_mylife_bolus.aidR(id, bind_rows(data[bolus_in_list]))
  data_new$carbs <- .clean_mylife_carbs.aidR(id, bind_rows(data[carbs_in_list]))
  data_new$SMBG <- .clean_mylife_SMBG.aidR(id, bind_rows(data[smbg_in_list]))
  
  return(data_new)
}

#------------------------
# Helper functions
#------------------------

#' Check if a file comes from a mylife export
#'
#' @param filename Character string with the filename to be checked.
#'
#' @return A logical value: \code{TRUE} if the file comes from a mylife export.
#'
#' @keywords internal
.is_mylife_format.aidR <- function(filename){
  tryCatch({
    lines <- read.csv(filename, encoding = "UTF-8")
    header <- colnames(lines)[1]
    # Expect eight columns, separated by .
    # Do not check content as this is language-specific
    if (length(strsplit(header, "\\.")[[1]]) != 8){ return(FALSE) }
    TRUE
  }, 
  warning = function(w) FALSE,
  error = function(e) FALSE)
}

#------------------------
# Format bolus
#------------------------

#' Format and clean bolus data from a mylife export
#'
#' @param id Character or numeric participant identifier.
#' @param bolus A data frame containing the bolus data from a mylife file.
#'
#' @return A data frame containing the formatted and cleaned bolus data.
#'   \code{NULL} if there is no bolus data.
#'
#' @keywords internal
.clean_mylife_bolus.aidR <- function(id, bolus){
  if (is.null(bolus) | nrow(bolus) == 0){ return(NULL) }
  
  bolus <- bolus %>% 
    mutate(id = id,
           format = "mylife",
           timestamp = as_datetime(.data$timestamp),
           timezone_offset = NA,
           type = "normal",
           total = as.numeric(gsub(pattern = ",", replacement = ".", .data$amount)),
           normal = .data$total,
           extended = NA,
           unit = "U",
           duration_extended = NA,
           pump_name = NA,
           ) %>% 
  select("id", "format", "timestamp", "timezone_offset", "type", "total", "normal", 
         "extended", "unit", "duration_extended", "pump_name")
    
  return(bolus)
}

#' Format and clean carbohydrate data from a mylife export
#'
#' @param id Character or numeric participant identifier.
#' @param carbs A data frame containing the carbohydrate data from a mylife file.
#'
#' @return A data frame containing the formatted and cleaned carbohydrate data.
#'   \code{NULL} if there is no carbohydrate data.
#'
#' @keywords internal
.clean_mylife_carbs.aidR <- function(id, carbs){
  if (is.null(carbs) | nrow(carbs) == 0){ return(NULL) }
  
  carbs <- carbs %>% 
    mutate(id = id,
           format = "mylife",
           timestamp = as_datetime(.data$timestamp),
           timezone_offset = NA,
           value = as.numeric(gsub(pattern = ",", replacement = ".", .data$amount)),
           estimated_absorption_duration = as.numeric(str_extract(.data$information, "(?<=\\s)[\\d.]+(?=h)")),
           is_hypo_treatment = grepl("HypoTreatment", .data$information),
           unit = "g"
           ) %>% 
    rename(label = "type") %>% 
    select("id", "format", "timestamp", "timezone_offset", "value", "unit", 
           "label", "estimated_absorption_duration", "is_hypo_treatment")
  
  return(carbs)
}

#' Format and clean SMBG data from a mylife export
#'
#' @param id Character or numeric participant identifier.
#' @param SMBG A data frame containing the SMBG data from a mylife file.
#'
#' @return A data frame containing the formatted and cleaned SMBG data.
#'   \code{NULL} if there is no SMBG data.
#'
#' @keywords internal
.clean_mylife_SMBG.aidR <- function(id, SMBG){
  if (is.null(SMBG) | nrow(SMBG) == 0){ return(NULL) }
  
  # Make sure SMBG is in mg/dl
  SMBG$amount[SMBG$units == "mmol/L"] <- as.numeric(SMBG$amount[SMBG$units == "mmol/L"]) * 18.018
  
  # Select columns: timestamp and value
  SMBG <- SMBG %>%
    mutate(id = id,
           format = "mylife",
           timestamp = as_datetime(.data$timestamp),
           timezone_offset = NA,
           value = as.numeric(gsub(pattern = ",", replacement = ".", .data$amount)),
           unit = "mg/dL",
           sensor_name = NA) %>% 
  select("id", "format", "timestamp", "timezone_offset", "value", "unit", "sensor_name")
  
  return(SMBG)
}


