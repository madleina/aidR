##################################
#                                #
#   Read Mylife data             #
#                                #
##################################

#------------------------
# Public functions
#------------------------

#' Read file from a Mylife export
#'
#' @param id Character or numeric participant identifier.
#' @param filename Character string corresponding to the filename of the Mylife export.
#'
#' @return An instance of class \code{mylife}.
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

#' Format and clean the Mylife data to keep relevant columns only.
#'
#' @param data An instance of class \code{mylife}, wrapping a named list with data of different types.
#' @param ... Additional arguments passed to methods.
#'
#' @return A named list with names \code{bolus} and \code{carbs}, and other names for data of other types.
#' @export
clean.mylife <- function(data, ...){
  if (is.null(data)){ return(NULL) }
  if (!("mylife" %in% class(data))){ stop("Expected Mylife format.") }
  
  # Remove class attribute for ease
  data <- unclass(data)
  
  # find all bolus- and carb-related entries
  # Note: language-specific -> use unit (g) to identify them
  bolus_in_list <- sapply(data, function(x) return(all(grepl("^U", x$units))))
  carbs_in_list <- sapply(data, function(x) return(all(grepl("^g ", x$units))))
  
  data$bolus <- .clean_mylife_bolus.aidR(bind_rows(data[bolus_in_list]))
  data$carbs <- .clean_mylife_carbs.aidR(bind_rows(data[carbs_in_list]))

  return(data)
}

#------------------------
# Helper functions
#------------------------

#' Check if a file corresponds to a file from a Mylife export.
#'
#' @param filename Character string with the filename to be checked.
#'
#' @return A logical value: \code{TRUE} if the file is a file from a Mylife export.
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

#' Format and clean bolus entries of a Mylife export.
#'
#' @param bolus A data frame containing the bolus data from a Mylife file.
#'
#' @return A data frame containing the formatted and cleaned bolus data.
#'
#' @keywords internal
.clean_mylife_bolus.aidR <- function(bolus){
  bolus <- bolus %>% 
    mutate(timestamp = as_datetime(.data$timestamp),
           timezone_offset = NA,
           type = "normal",
           total = as.numeric(gsub(pattern = ",", replacement = ".", .data$amount)),
           normal = .data$total,
           extended = NA,
           unit = "U",
           duration_extended = NA,
           pump_name = NA,
           ) %>% 
  select("timestamp", "timezone_offset", "type", "total", "normal", 
         "extended", "unit", "duration_extended", "pump_name")
    
  return(bolus)
}

#' Format and clean carbohydrate entries of a Mylife export.
#'
#' @param carbs A data frame containing the carbohydrate data from a Mylife file.
#'
#' @return A data frame containing the formatted and cleaned carbohydrate data.
#'
#' @keywords internal
.clean_mylife_carbs.aidR <- function(carbs){
  carbs <- carbs %>% 
    mutate(timestamp = as_datetime(.data$timestamp),
           timezone_offset = NA,
           carbs = as.numeric(gsub(pattern = ",", replacement = ".", .data$amount)),
           estimated_absorption_duration = as.numeric(str_extract(.data$information, "(?<=\\s)[\\d.]+(?=h)")),
           is_hypo_treatment = grepl("HypoTreatment", .data$information),
           unit = "g"
           ) %>% 
    rename(label = "type") %>% 
    select("timestamp", "timezone_offset", "carbs", "unit", "label", "estimated_absorption_duration", "is_hypo_treatment")
  
  return(carbs)
}


