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
  # Read file
  data <- .read_tandem_source_file.aidR(filename)

  # Add class name
  if (!is.null(data)) {
    class(data) <- "tandem_source"
  }
  
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
#'   \code{total_bolus} (the latter three are extracted from the bolus file;
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
    # Each row is a bolus and the carbs entered for it: link both
    data$bolus <- .rename_bolus_tandem_source.aidR(data$bolus)
    link_id <- .link_bolus_carbs_tandem_source.aidR(id, data$bolus)
    bolus <- .format_bolus_tandem_source.aidR(id, data$bolus, link_id)
    return(list(bolus = bolus,
                carbs = .format_carbs_tandem_source.aidR(id, data$bolus, link_id),
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
  if (is.null(file)){ return(FALSE) }
   
  tandem_files <- .get_tandem_source_file_lookup.aidR()
   
   # Loop over all file types in lookup
   for (i in 1:length(tandem_files)) {
     file_type <- tandem_files[[i]]
     
     # Check if it is this file type
     if (.is_tandem_source_file_type.aidR(filename, file_type$expected_num_cols, file_type$numeric_cols, file_type$non_numeric_cols, file_type$expected_keyword)) {
       return(TRUE)
     }
   }
   return(FALSE)
}

#' Check if a Tandem Source file corresponds to a particular data type
#'
#' @param filename Character string with the filename to be checked.
#' @param expected_num_cols A numeric value with the expected number of columns of that file.
#' @param numeric_cols A numeric vector corresponding to the column indices that contain numeric values.
#' @param non_numeric_cols A numeric vector corresponding to the column indices that should not contain numeric values.
#' @param expected_keyword A list with keys \code{name} and \code{col_ix}, corresponding to a keyword that should be present in a particular column.
#'   If \code{name} holds several keywords, at least one of them must be present.
#'
#' @return A logical value: \code{TRUE} if the file matches the data type format.
#'   \code{FALSE} for empty files, whose columns can not be checked.
#'
#' @keywords internal
.is_tandem_source_file_type.aidR <- function(filename, expected_num_cols, numeric_cols, non_numeric_cols = c(), expected_keyword = NULL) {
  # Note: language may be different -> can not check headers directly
  # Instead check if there are 'expected_num_cols' columns,
  # if the expected numeric columns are indeed numeric
  # and if a given column contains a particular keyword
  
  f <- read.csv(filename, check.names = FALSE)
  
  # Check if number of columns matches
  if (ncol(f) != expected_num_cols) {
    return(FALSE)
  }
  
  # Check if a certain column contains a keyword (at least one of them, if several are given)
  if (length(expected_keyword$name) > 0) {
    values  <- unique(f[, expected_keyword$col_ix])
    matches <- vapply(expected_keyword$name,
                      \(keyword) any(grepl(keyword, values, ignore.case = TRUE)), logical(1))
    if (!any(matches)) {
      return(FALSE)
    }
  }
  
  # If file is empty: we can not check for values anyways
  if (nrow(f) == 0) {
    return(FALSE)
  }
  
  # Check if numeric columns match
  for (c in numeric_cols) {
    is_numeric <- .is_numeric_col.aidR(f[, c])
    if (!is_numeric) {
      return(FALSE)
    }
  }
  
  # Check if non-numeric columns match
  for (c in non_numeric_cols) {
    if (all(is.na(f[, c]))) {
      next
    } # can not check
    is_numeric <- .is_numeric_col.aidR(f[, c])
    if (is_numeric) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

#' A lookup with Tandem Source file characteristics
#'
#' @return A list with data types and their expected file formats, including the
#' number of columns, the column indices of the time- and numeric columns and
#' expected keywords in the header.
#'
#' @keywords internal
.get_tandem_source_file_lookup.aidR <- function() {
  # Note: tried to find keywords that are relatively common across languages (Bolus, Tandem, t:slim X2), but cannot exclude that there are languages
  # where those are named differently
  # In such cases, this lookup will fail!
  
  tandem_files <- list()
  
  # 4 columns
  tandem_files$basal <- list(expected_num_cols = 4, time_cols = 3, date_cols = c(), numeric_cols = 4, expected_keyword = list(name = c("Tandem", "t:slim X2"), col_ix = 1), non_numeric_cols = 1)
  
  # 5 columns
  tandem_files$cgm <- list(expected_num_cols = 5, time_cols = 4, date_cols = c(), numeric_cols = 5, expected_keyword = list(name = c("Tandem", "t:slim X2"), col_ix = 1), non_numeric_cols = c(1, 3))
  
  # 6 columns
  tandem_files$bg <- list(expected_num_cols = 6, time_cols = 4, date_cols = c(), numeric_cols = 5, expected_keyword = list(name = c("Tandem", "t:slim X2"), col_ix = 1), non_numeric_cols = c(1, 3))
  
  # 20 columns
  tandem_files$bolus <- list(expected_num_cols = 20, time_cols = 6, date_cols = c(), numeric_cols = c(4, 7:9, 15:20), expected_keyword = list(name = "Bolus", col_ix = 1), non_numeric_cols = c(1:3, 10))
  
  # 27 columns
  tandem_files$hourly_basal <- list(expected_num_cols = 26, time_cols = c(), date_cols = 2, numeric_cols = c(3:26), expected_keyword = list(), non_numeric_cols = c())
  
  return(tandem_files)
}

#' Format any file from a Tandem Source export by configuring timestamps
#'
#' @param filename Character string with the filename to be formatted.
#' @param time_cols A numeric vector corresponding to the column indices that contain timestamps to be formatted.
#' @param date_cols A numeric vector corresponding to the column indices that contain dates to be formatted.
#'
#' @return A data frame with the content of the file and properly formatted time- and numeric columns.
#'
#' @keywords internal
.format_tandem_source_file.aidR <- function(filename, time_cols, date_cols) {
  # Open file
  f <- read.csv(filename, check.names = FALSE)
  
  # Format timestamp
  f <- f %>%
    mutate(across(all_of(time_cols), ~ parse_date_time(.x, orders = c("Ymd HM", "Ymd HMS", "dmy HM", "dmy HMS"), tz = "UTC")))
  
  # Format date
  f <- f %>%
    mutate(across(all_of(date_cols), ~ as.Date(.x)))
  
  return(f)
}

#' Read the data from a Tandem file
#'
#' @param filename Character string with the filename to be parsed.
#'
#' @return A named list with a single entry, named after the data type of the
#'   file and holding a data frame with formatted timestamps and numeric columns.
#'   \code{NULL} if the file is empty. Throws an error if the data type of a
#'   non-empty file could not be determined.
#'
#' @keywords internal
.read_tandem_source_file.aidR <- function(filename) {
  # Get lookup
  tandem_files <- .get_tandem_source_file_lookup.aidR()
  
  # Loop over all file types in lookup
  for (i in 1:length(tandem_files)) {
    file_type <- tandem_files[[i]]
    
    # Check if it is this file type
    if (.is_tandem_source_file_type.aidR(filename, file_type$expected_num_cols, file_type$numeric_cols, file_type$non_numeric_cols, file_type$expected_keyword)) {
      data <- list()
      data[[names(tandem_files)[i]]] <- .format_tandem_source_file.aidR(filename, file_type$time_cols, file_type$date_cols)
      return(data)
    }
  }
  
  # Could not match file type
  # This is ok if the file is empty, otherwise throw
  f <- read.csv(filename, check.names = FALSE)
  if (nrow(f) == 0) {
    return(NULL)
  }
  stop("Unrecognized file type for Tandem Source: ", filename)
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
  if (grepl("mmol/L", names(cgm)[5], ignore.case = TRUE)){
    cgm$value <- cgm[,5] * 18.018
  } else if (grepl("mg/dL", names(cgm)[5], ignore.case = TRUE)){
    cgm$value <- cgm[,5]
  } else {
    stop(paste0("Unknown CGM units in column ", names(cgm)[5], "!"))
  }
  
  cgm <- data.frame(
    id = id, 
    format = "tandem_source",
    timestamp = as_datetime(cgm[,4]),
    timezone_offset = NA,
    value = cgm$value,
    unit = "mg/dL",
    sensor_name = cgm[,1] # Device Name
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
    mutate(timestamp = as_datetime(.data[[names(basal)[3]]])) |> 
    arrange(timestamp) |> 
    mutate(id = id, 
           format = "tandem_source",
           timezone_offset = NA,
           duration = as.numeric(difftime(lead(timestamp), timestamp, units = "hours")),
           rate = .data[[names(basal)[4]]] / .data$duration,
           unit = "U/h") |> 
    rename(pump_name = all_of(names(basal)[1])) |>
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
    date = date(as_datetime(hourly_basal[,2])),
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

#' Rename bolus data from a Tandem Source export for easier handling
#'
#' @param bolus A data frame containing the bolus data from a Tandem Source file.
#'
#' @return A data frame containing the renamed bolus data.
#' @keywords internal
.rename_bolus_tandem_source.aidR <- function(bolus){
  # rename, for easier use
  names(bolus) <- c("Type", "Bolus Type", "Bolus Delivery Method", "BG (mg/dL)",
                    "Serial Number", "Completion Date Time", "Insulin Delivered",
                    "Food Delivered", "Correction Delivered", "Completion Status Desc",
                    "Bolex Start Date Time", "Bolex Completion Date Time", 
                    "Bolex Insulin Delivered", "Bolex Completion Status Desc",
                    "Standard Percent", "Duration (mins)", "Carb Size",
                    "Target BG (mg/dL)", "Correction Factor", "Carb Ratio")
  return(bolus)
}

#' Format and clean bolus data from a Tandem Source export
#'
#' @param id Character or numeric participant identifier.
#' @param bolus A data frame containing the bolus data from a Tandem Source file.
#' @param link_id A character vector with one key per row of \code{bolus},
#'   linking the bolus to the carbohydrate entry of the same row.
#'   Default \code{NA}.
#'
#' @return A data frame containing the formatted and cleaned bolus data.
#' @keywords internal
.format_bolus_tandem_source.aidR <- function(id, bolus, link_id = NA) {
  # Decide if a bolus is normal / dual wave / square wave
  bolus <- bolus |> 
    mutate(type = case_when(
      # normal if duration is zero
      .data$`Duration (mins)` == 0 ~ "normal",
      # dual_wave if duration is > 0 and standard > 0
      .data$`Duration (mins)` > 0 & .data$`Standard Percent` > 0 ~ "dual_wave",
      # square_wave if duration > 0 and standard == 0
      .data$`Duration (mins)` > 0 & .data$`Standard Percent` == 0 ~ "square_wave",
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
           pump_name = NA,
           link_id = link_id
           ) |> 
    select("id", "format", "timestamp", "timezone_offset", "type", "total", "normal", 
           "extended", "unit", "duration_extended", "pump_name", "link_id")
  
  return(bolus)
}

#------------------------------
# Functions for carbs
#------------------------------

#' Link boluses and carbohydrate entries of a Tandem Source bolus file
#'
#' Each row of the bolus file corresponds to a bolus together with the 
#' carbohydrates entered for it in the bolus calculator. Rows with carbohydrates 
#' get a \code{link_id}, built from the completion time, the carbohydrates, and 
#' the insulin delivered.
#'
#' @param id Character or numeric participant identifier.
#' @param bolus A data frame containing the bolus data from a Tandem Source file.
#'
#' @return A character vector with one key per row of \code{bolus}, \code{NA}
#'   for rows without carbohydrates.
#' @keywords internal
.link_bolus_carbs_tandem_source.aidR <- function(id, bolus) {
  .create_link_id.aidR(id, as_datetime(bolus$`Completion Date Time`), bolus$`Carb Size`,
                       bolus$`Insulin Delivered`, bolus$`Bolex Insulin Delivered`,
                       linked = bolus$`Carb Size` > 0)
}

#' Format and clean carbohydrate data from a Tandem Source export
#'
#' @param id Character or numeric participant identifier.
#' @param carbs A data frame containing the bolus data from a Tandem Source file,
#'   which holds the carbohydrates entered into the bolus calculator.
#' @param link_id A character vector with one key per row of \code{carbs},
#'   linking the carbohydrate entry to the bolus of the same row.
#'   Default \code{NA}.
#'
#' @return A data frame containing the formatted and cleaned carbohydrate data.
#' @keywords internal
.format_carbs_tandem_source.aidR <- function(id, carbs, link_id = NA) {
  carbs <- carbs |> 
    mutate(link_id = link_id) |> 
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
           "label", "estimated_absorption_duration", "is_hypo_treatment", "link_id")
  
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
  if (grepl("mmol/L", names(bg)[5], ignore.case = TRUE)){
    bg$value <- bg[,5] * 18.018
  } else if (grepl("mg/dL", names(bg)[5], ignore.case = TRUE)){
    bg$value <- bg[,5]
  } else {
    stop(paste0("Unknown BG units in column ", names(bg)[5], "!"))
  }
  
  bg <- data.frame(
    id = id, 
    format = "tandem_source",
    timestamp = as_datetime(bg[,4]),
    timezone_offset = NA,
    value = bg$value,
    unit = "mg/dL",
    sensor_name = bg[,1]
  )
  
  return(bg)
}





