##################################
#                                #
#   Read Glooko data             #
#                                #
##################################

#------------------------
# Public functions
#------------------------

#' Read a file from a Glooko export
#'
#' @param id Character or numeric participant identifier.
#' @param filename Character string with the filename of the file to be read.
#'
#' @return An instance of class \code{glooko}, wrapping a named list with the
#'   data of one data type. \code{NULL} if the file is empty.
#'
#' @keywords internal
read_glooko <- function(id, filename) {
  # Read, if possible
  data <- .read_glooko_file.aidR(filename)

  # Add class name
  if (!is.null(data)) {
    class(data) <- "glooko"
  }

  return(data)
}

#' Format and clean the Glooko data to keep relevant columns only
#' 
#' @param data An instance of class \code{glooko}, wrapping a named list with data of a particular data type.
#' @param id Character or numeric participant identifier.
#' @param ... Additional arguments passed to methods.
#'
#' @return A named list with the cleaned data, named according to the data type
#'   of \code{data}: \code{cgm}, \code{basal}, \code{bolus} and \code{carbs}
#'   (both are extracted from the bolus file), \code{total_basal} and
#'   \code{total_bolus} (both are extracted from the daily aggregated insulin
#'   file) or \code{SMBG}. \code{NULL} for all other data types, which are not
#'   cleaned.
#' @export
clean.glooko <- function(data, id, ...) {
  if (is.null(data)) {
    return(NULL)
  }
  if (!("glooko" %in% class(data))) {
    stop("Expected Glooko format.")
  }

  # Format, if necessary
  if (names(data) == "cgm") {
    return(list(cgm = .format_cgm_glooko.aidR(id, data)))
  } else if (names(data) == "basal") {
    return(list(basal = .format_basal_glooko.aidR(id, data)))
    return(data)
  } else if (names(data) == "bolus") {
    return(list(
      bolus = .format_bolus_glooko.aidR(id, data),
      carbs = .format_carbs_glooko.aidR(id, data)
    ))
  } else if (names(data) == "total_insulin") {
    return(list(
      total_basal = .format_total_basal_glooko.aidR(id, data),
      total_bolus = .format_total_bolus_glooko.aidR(id, data)
    ))
  } else if (names(data) == "SMBG") {
    return(list(SMBG = .format_SMBG_glooko.aidR(id, data)))
  } 
  
  # All other formats: don't clean, return NULL
  return(NULL)
}

#------------------------
# Helper functions
#------------------------

#' Check if a file comes from a Glooko export
#'
#' @param filename Character string with the filename to be checked.
#'
#' @return A logical value: \code{TRUE} if the file comes from a Glooko export.
#'
#' @keywords internal
.is_glooko_format.aidR <- function(filename) {
  # Note: language may vary -> check for structure
  # first line is expected to be
  # Name:XXX,Date Range:y-m-d - y-m-d (but this is language specific)
  # -> test for:
  # ...:...,...:date - date
  # allowed date formats: y-m-d or d.m.y or d/m/y

  # Try opening it
  first_line <- tryCatch(
    readLines(filename, n = 1, warn = FALSE, encoding = "UTF-8"),
    error = function(e) {
      return(FALSE)
    },
    warning = function(e) {
      return(FALSE)
    }
  )
  if (first_line == FALSE) {
    return(FALSE)
  }
  if (length(first_line) == 0) {
    return(FALSE)
  }

  # Split at comma, expect two columns
  cols <- strsplit(first_line, ",")[[1]]
  if (length(cols) != 2) {
    return(FALSE)
  }

  # First col: anything:anything  (name field)
  name_ok <- grepl("^.+:.+$", cols[1])

  # Second col: anything:<date> - <date>
  # Dates can be dd/mm/yyyy, dd.mm.yyyy, or yyyy-mm-dd
  date_pattern <- "\\d{2}[./-]\\d{2}[./-]\\d{2,4}|\\d{4}-\\d{2}-\\d{2}"
  range_ok <- grepl(
    paste0("^.+:(", date_pattern, ") - (", date_pattern, ")$"),
    cols[2]
  )

  return(name_ok && range_ok)
}

#' Format any file from a Glooko export by configuring timestamps and numeric values
#'
#' @param filename Character string with the filename to be formatted.
#' @param time_cols A numeric vector corresponding to the column indices that contain timestamps to be formatted.
#' @param numeric_cols A numeric vector corresponding to the column indices that contain numeric values to be formatted.
#'
#' @return A data frame with the content of the file and properly formatted time- and numeric columns.
#'
#' @keywords internal
.format_glooko_file.aidR <- function(filename, time_cols, numeric_cols) {
  # Open file and skip first line (Glooko identifier)
  f <- read.csv(filename, skip = 1)

  # Format timestamp
  f <- f %>%
    mutate(across(all_of(time_cols), ~ parse_date_time(.x, orders = c("Ymd HM", "Ymd HMS", "dmy HM", "dmy HMS"), tz = "UTC")))

  # Format numeric values
  for (c in numeric_cols) {
    f[, c] <- as.numeric(gsub(pattern = ",", replacement = ".", f[, c]))
  }

  return(data.frame(f))
}

#' Check if a vector can be converted to numeric values
#'
#' @param vec Character vector to be checked.
#'
#' @return A logical value: \code{TRUE} if the vector can be converted to numeric values.
#'
#' @keywords internal
.is_numeric_col.aidR <- function(vec) {
  # Replace , by .
  vec <- gsub(pattern = ",", replacement = ".", vec)
  vec[!is.na(vec) & vec == ""] <- NA
  # Now check if it can be converted to numeric
  converted <- suppressWarnings(as.numeric(vec))
  not_numeric <- any(is.na(converted) & !is.na(vec)) # TRUE only if NAs were newly introduced

  return(!not_numeric)
}

#' Check if a Glooko file corresponds to a particular data type
#'
#' @param filename Character string with the filename to be checked.
#' @param expected_num_cols A numeric value with the expected number of columns of that file.
#' @param numeric_cols A numeric vector corresponding to the column indices that contain numeric values.
#' @param non_numeric_cols A numeric vector corresponding to the column indices that should not contain numeric values.
#' @param expected_keyword A list with keys \code{name} and \code{col_ix}, corresponding to a keyword that should be present in a particular column.
#'
#' @return A logical value: \code{TRUE} if the file matches the data type format.
#'   \code{FALSE} for empty files, whose columns can not be checked.
#'
#' @keywords internal
.is_glooko_file_type.aidR <- function(filename, expected_num_cols, numeric_cols, non_numeric_cols = c(), expected_keyword = NULL) {
  # Note: language may be different -> can not check headers directly
  # Instead check if there are 'expected_num_cols' columns,
  # if the expected numeric columns are indeed numeric
  # and if a given column contains a particular keyword

  f <- read.csv(filename, skip = 1)

  # Check if number of columns matches
  if (ncol(f) != expected_num_cols) {
    return(FALSE)
  }

  # Check if a certain column contains a keyword
  if (!is.null(expected_keyword)) {
    if (!grepl(expected_keyword$name, names(f)[expected_keyword$col_ix], ignore.case = T)) {
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

#' A lookup with Glooko file characteristics
#'
#' @return A list with data types and their expected file formats, including the
#' number of columns, the column indices of the time- and numeric columns and
#' expected keywords in the header.
#'
#' @keywords internal
.get_glooko_file_lookup.aidR <- function() {
  # Note: tried to find keywords that are relatively common across languages (CGM, Insulin, (g)), but cannot exclude that there are languages
  # where those are named differently
  # In such cases, this lookup will fail!

  glooko_files <- list()

  # 2 columns
  glooko_files$carbs_manual <- list(expected_num_cols = 2, time_cols = 1, numeric_cols = 2, expected_keyword = list(name = "(g)", col_ix = 2), non_numeric_cols = c())
  glooko_files$notes <- list(expected_num_cols = 2, time_cols = 1, numeric_cols = c(), non_numeric_cols = c())

  # 3 columns
  glooko_files$cgm <- list(expected_num_cols = 3, time_cols = 1, numeric_cols = 2, expected_keyword = list(name = "CGM", col_ix = 2), non_numeric_cols = c())
  glooko_files$alarm <- list(expected_num_cols = 3, time_cols = 1, numeric_cols = c(), non_numeric_cols = c())

  # 4 columns
  glooko_files$SMBG <- list(expected_num_cols = 4, time_cols = 1, numeric_cols = 2, non_numeric_cols = 3)
  glooko_files$manual_insulin <- list(expected_num_cols = 4, time_cols = 1, numeric_cols = 3, expected_keyword = list(name = "Insulin", col_ix = 4), non_numeric_cols = c())
  glooko_files$medication <- list(expected_num_cols = 4, time_cols = 1, numeric_cols = 3, non_numeric_cols = c(2, 4))

  # 5 columns
  glooko_files$total_insulin <- list(expected_num_cols = 5, time_cols = 1, numeric_cols = 2:4, expected_keyword = list(name = "Insulin", col_ix = 3), non_numeric_cols = c())
  glooko_files$exercise <- list(expected_num_cols = 5, time_cols = 1, numeric_cols = 4:5, non_numeric_cols = c())

  # 7, 8, 9 columns
  glooko_files$basal <- list(expected_num_cols = 7, time_cols = 1, numeric_cols = 3:6, expected_keyword = list(name = "Insulin", col_ix = 2), non_numeric_cols = c())
  glooko_files$food <- list(expected_num_cols = 8, time_cols = 1, numeric_cols = 3, non_numeric_cols = c())
  glooko_files$bolus <- list(expected_num_cols = 9, time_cols = 1, numeric_cols = 3:8, expected_keyword = list(name = "Insulin", col_ix = 2), non_numeric_cols = c())

  return(glooko_files)
}

#' Read the data from a Glooko file
#'
#' @param filename Character string with the filename to be parsed.
#'
#' @return A named list with a single entry, named after the data type of the
#'   file and holding a data frame with formatted timestamps and numeric columns.
#'   \code{NULL} if the file is empty. Throws an error if the data type of a
#'   non-empty file could not be determined.
#'
#' @keywords internal
.read_glooko_file.aidR <- function(filename) {
  # Get lookup
  glooko_files <- .get_glooko_file_lookup.aidR()

  # Loop over all file types in lookup
  for (i in 1:length(glooko_files)) {
    file_type <- glooko_files[[i]]

    # Check if it is this file type
    if (.is_glooko_file_type.aidR(filename, file_type$expected_num_cols, file_type$numeric_cols, file_type$non_numeric_cols, file_type$expected_keyword)) {
      data <- list()
      data[[names(glooko_files)[i]]] <- .format_glooko_file.aidR(filename, file_type$time_cols, file_type$numeric_cols)
      return(data)
    }
  }

  # Could not match file type
  # This is ok if the file is empty, otherwise throw
  f <- read.csv(filename, skip = 1)
  if (nrow(f) == 0) {
    return(NULL)
  }
  stop("Unrecognized file type for Glooko: ", filename)
}

#------------------------
# Format CGM
#------------------------

#' Format and clean CGM data from a Glooko export
#'
#' @param id Character or numeric participant identifier.
#' @param data A data frame containing the CGM data from a Glooko file.
#'
#' @return A data frame containing the formatted and cleaned CGM data.
#' @keywords internal
.format_cgm_glooko.aidR <- function(id, data) {
  data <- data.frame(unclass(data))

  # CGM values are always given in second column (column name may vary with language)

  # Convert to mg/dL, if necessary
  if (grepl("mmol", names(data)[2])) {
    data[, 2] <- data[, 2] * 18.018
  } else if (!(grepl("mg", names(data)[2]))) {
    # CGM not provided in mmol/l nor in mg/dl -> throw
    stop("Unknown unit: '", names(data)[2], "' for CGM data.")
  }

  data <- data.frame(
    id = id,
    format = "glooko",
    timestamp = data[, 1],
    timezone_offset = NA,
    value = data[, 2],
    unit = "mg/dL",
    sensor_name = NA # never given in Glooko file
  )

  return(data)
}

#------------------------------
# Functions for basal
#------------------------------

#' Format and clean basal data from a Glooko export
#'
#' @param id Character or numeric participant identifier.
#' @param data A data frame containing the basal data from a Glooko file.
#'
#' @return A data frame containing the formatted and cleaned basal data.
#' @keywords internal
.format_basal_glooko.aidR <- function(id, data) {
  data <- data.frame(unclass(data))

  # Rename columns (for different languages)
  names(data) <- c(
    "Timestamp", "Insulin Type", "Duration (minutes)",
    "Percentage (%)", "Rate", "Insulin Delivered (U)", "Serial number"
  )

  # Select relevant columns, rename
  data <- data %>%
    mutate(id = id,
           format = "glooko",
           timezone_offset = NA,
           duration = .data$`Duration (minutes)` / 60, # in hours
           unit = "U/h", # rate
           pump_name = NA # pump name: not known (!= Serial number)
    ) %>% 
    rename(
      timestamp = "Timestamp",
      rate = "Rate"
    ) %>%
    select("id", "format", "timestamp", "timezone_offset", "duration", "rate",
           "unit", "pump_name")
  
  return(data)
}

#------------------------------
# Functions for bolus
#------------------------------

#' Format and clean bolus data from a Glooko export
#'
#' @param id Character or numeric participant identifier.
#' @param data A data frame containing the bolus data from a Glooko file.
#'
#' @return A data frame containing the formatted and cleaned bolus data.
#' @keywords internal
.format_bolus_glooko.aidR <- function(id, data) {
  data <- data.frame(unclass(data))

  # Rename columns (for different languages)
  names(data) <- c(
    "timestamp", "Insulin Type", "Blood Glucose Input",
    "Carbs Input", "Carbs Ratio", "Insulin Delivered (U)",
    "Initial Delivery (U)", "Extended Delivery (U)", "Serial number"
  )

  # Decide if a bolus is normal / dual wave / square wave
  data <- data %>%
    mutate(
      type = case_when(
        # normal if initial and extended delivery are both NA
        is.na(.data$`Initial Delivery (U)`) & is.na(.data$`Extended Delivery (U)`) ~ "normal",
        # dual if initial and extended delivery are both > 0
        .data$`Initial Delivery (U)` > 0 & .data$`Extended Delivery (U)` > 0 ~ "dual_wave",
        # square if initial delivery is 0 and extended delivery is > 0
        .data$`Initial Delivery (U)` == 0 & .data$`Extended Delivery (U)` > 0 ~ "square_wave",
        # default: normal
        TRUE ~ "normal"
      )
    )
  
  # Add units of total bolus, as well as units of normal and extended bolus
  data <- data |> 
    mutate(
      extended = ifelse(.data$type != "normal",
                        .data$`Extended Delivery (U)`,
                        NA
      ),
      normal = case_when(
        type == "normal" ~ .data$`Insulin Delivered (U)`,
        type == "dual_wave" ~ .data$`Initial Delivery (U)`,
        type == "square_wave" ~ 0
      ),
      total = coalesce(.data$extended, 0) + coalesce(.data$normal, 0),
    )
  
  # Select relevant columns
  data <- data |> 
    mutate(
      id = id,
      format = "glooko",
      duration_extended = NA, # not given (also not for extended bolus)
      timezone_offset = NA, # not given
      pump_name = NA, # not given
      unit = "U"
    ) %>%
    select("id", "format", "timestamp", "timezone_offset", "type", "total", 
           "normal", "extended", "unit", "duration_extended", "pump_name")
  
  return(data)
}

#------------------------------
# Functions for carbs
#------------------------------

#' Format and clean carbohydrate data from a Glooko export
#'
#' @param id Character or numeric participant identifier.
#' @param data A data frame containing the bolus data from a Glooko file, which
#'   holds the carbohydrates entered into the bolus calculator.
#'
#' @return A data frame containing the formatted and cleaned carbohydrate data.
#' @keywords internal
.format_carbs_glooko.aidR <- function(id, data) {
  data <- data.frame(unclass(data))
  
  # 4th column in entered carbs. Filter on non-zero carbs
  data <- data.frame(id = id,
                     format = "glooko",
                     timestamp = data[, 1],
                     timezone_offset = NA,
                     value = data[, 4])
  
  data <- data %>%
    filter(.data$value > 0) %>%
    mutate(
      unit = "g",
      label = NA,
      estimated_absorption_duration = NA,
      is_hypo_treatment = NA
    )
  
  return(data)
}

#------------------------------
# Functions for total insulin
#------------------------------

#' Format and clean daily aggregated insulin data from a Glooko export
#'
#' @param id Character or numeric participant identifier.
#' @param data A data frame containing the daily aggregated insulin data from a Glooko file.
#'
#' @return A data frame containing the formatted and cleaned total insulin data.
#' @keywords internal
.format_total_insulin_glooko.aidR <- function(id, data) {
  data <- data.frame(unclass(data))

  names(data) <- c(
    "Timestamp", "Total Bolus (U)", "Total Insulin (U)",
    "Total Basal (U)", "Serial number"
  )

  data <- data %>%
    mutate(
      id = id,
      format = "glooko",
      date = date(.data$Timestamp),
      total_bolus = .data$`Total Bolus (U)`,
      total_insulin = .data$`Total Insulin (U)`,
      total_basal = .data$`Total Basal (U)`
    ) %>%
    select("id", "format", "date", "total_bolus", 
           "total_basal", "total_insulin")

  return(data)
}

#' Total basal insulin per day, as reported by a Glooko export
#'
#' @param id Character or numeric participant identifier.
#' @param data A data frame containing the daily aggregated insulin data from a Glooko file.
#'
#' @return A data frame with columns \code{id}, \code{format}, \code{date},
#'   \code{total_basal} and \code{source}, holding one row per day.
#' @keywords internal
.format_total_basal_glooko.aidR <- function(id, data) {
  .format_total_insulin_glooko.aidR(id, data) %>% 
    mutate(source = "reported") %>% 
    select("id", "format", "date", "total_basal", "source")
}

#' Total bolus insulin per day, as reported by a Glooko export
#'
#' @param id Character or numeric participant identifier.
#' @param data A data frame containing the daily aggregated insulin data from a Glooko file.
#'
#' @return A data frame with columns \code{id}, \code{format}, \code{date},
#'   \code{total_bolus} and \code{source}, holding one row per day.
#' @keywords internal
.format_total_bolus_glooko.aidR <- function(id, data) {
  .format_total_insulin_glooko.aidR(id, data) %>% 
    mutate(source = "reported") %>% 
    select("id", "format", "date", "total_bolus", "source")
}

#------------------------------
# Functions for SMBG (BG)
#------------------------------

#' Format and clean SMBG data from a Glooko export
#'
#' @param id Character or numeric participant identifier.
#' @param data A data frame containing the SMBG data from a Glooko file.
#'
#' @return A data frame containing the formatted and cleaned SMBG data.
#' @keywords internal
.format_SMBG_glooko.aidR <- function(id, data) {
  data <- data.frame(unclass(data))

  # BG values are always given in second column (column name may vary with language)
  # Convert to mg/dL, if necessary
  if (grepl("mmol", names(data)[2])) {
    data[, 2] <- data[, 2] * 18.018
  } else if (!(grepl("mg", names(data)[2]))) {
    # BG not provided in mmol/l nor in mg/dl -> throw
    stop("Unknown unit: '", names(data)[2], "' for BG data.")
  }

  data <- data.frame(
    id = id,
    format = "glooko",
    timestamp = data[, 1],
    timezone_offset = NA,
    value = data[, 2],
    unit = "mg/dL",
    sensor_name = NA # never given in Glooko file
  )
  
  return(data)
}

