##################################
#                                #
#   Read Glooko data             #
#                                #
##################################

#------------------------
# Public functions
#------------------------

#' Read file from a Glooko export
#'
#' @param id Character or numeric participant identifier.
#' @param filename Character string corresponding to the filename of the Glooko export.
#'
#' @return An instance of class \code{glooko}. NULL if the data was not parsed.
read_glooko <- function(id, filename) {
  # Read, if possible
  data <- .read_glooko_file.aidR(filename)

  # Add class name
  if (!is.null(data)) {
    class(data) <- "glooko"
  }

  return(data)
}

#' Format and clean the Glooko data to keep relevant columns only.
#'
#' @param data An instance of class \code{glooko}, wrapping a named list with data of a particular data type.
#' @param ... Additional arguments passed to methods.
#'
#' @return A named list with names \code{cgm}, \code{basal}, \code{bolus}, or \code{carbs}, or other names if data is of another type.
#' @export
clean.glooko <- function(data, ...) {
  if (is.null(data)) {
    return(NULL)
  }
  if (!("glooko" %in% class(data))) {
    stop("Expected Glooko format.")
  }

  # Format, if necessary
  if (names(data) == "cgm") {
    return(list(cgm = .format_cgm_glooko.aidR(data)))
  } else if (names(data) == "basal") {
    return(list(basal = .format_basal_glooko.aidR(data)))
    return(data)
  } else if (names(data) == "bolus") {
    return(list(
      bolus = .format_bolus_glooko.aidR(data),
      carbs = .format_carbs_glooko.aidR(data)
    ))
  } else if (names(data) == "total_insulin") {
    return(list(total_insulin = .format_total_insulin_glooko.aidR(data)))
  } else if (names(data) == "SMBG") {
    return(list(SMBG = .format_SMBG_glooko.aidR(data)))
  } else if (names(data) == "alarm") {
    return(list(alarm = .format_alarms_glooko.aidR(data)))
  } else if (names(data) == "manual_insulin") {
    return(list(manual_insulin = .format_manual_insulin_glooko.aidR(data)))
  } else if (names(data) == "medication") {
    return(list(medication = .format_medication_glooko.aidR(data)))
  } else if (names(data) == "food") {
    return(list(food = .format_food_glooko.aidR(data)))
  } else if (names(data) == "notes") {
    return(list(notes = .format_notes_glooko.aidR(data)))
  } else if (names(data) == "exercise") {
    return(list(exercise = .format_exercise_glooko.aidR(data)))
  }

  # Note: there are more data types but they have always been empty so far
  # -> throw if a non-empty one occurs, so we can check how to format it

  stop("Not implemented: ", names(data))
}

#------------------------
# Helper functions
#------------------------

#' Check if a file corresponds to a file from a Glooko export.
#'
#' @param filename Character string with the filename to be checked.
#'
#' @return A logical value: \code{TRUE} if the file is a file from a Glooko export.
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

#' Format any file from a Glooko export by configuring timestamps and numeric values.
#'
#' @param filename Character string with the filename to be formatted.
#' @param time_cols A numeric vector corresponding to the column indices that contain timestamps to be formatted.
#' @param numeric_cols A numeric vector corresponding to the column indices that contain numeric values to be formatted.
#'
#' @return A data frame with the content of the filename and properly formatted time- and numeric columns.
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
#' @param vec Character vector to be checked
#'
#' @return A logical value: \code{TRUE} if the vector can be converted to numeric values
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

#' Check if a Glooko file corresponds to a particular data type.
#'
#' @param filename Character string with the filename to be checked.
#' @param expected_num_cols The expected number of columns of that file.
#' @param numeric_cols A numeric vector corresponding to the column indices that contain numeric values.
#' @param non_numeric_cols A numeric vector corresponding to the column indices that should not contain numeric values.
#' @param expected_keyword A list with keys \code{name} and \code{col_ix}, corresponding to a keyword that should be present in a particular column.
#'
#' @return A logical value: \code{TRUE} if the file matches the data type format.
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

#' A lookup with Glooko file characteristics.
#'
#' @return A list with data types and their expected file formats, including the number of columns, the column indices of the time- and
#' numeric columns and expected keywords in the header.
#'
#' @keywords internal
.get_glooko_file_lookup.aidR <- function() {
  # Note: tried to find keywords that are relatively common across languages (CGM, Insulin, (g)), but cannot exclude that there are languages
  # where those are named differently
  # In such cases, this lookup will fail!

  glooko_files <- list()

  # 2 columns
  glooko_files$carbs <- list(expected_num_cols = 2, time_cols = 1, numeric_cols = 2, expected_keyword = list(name = "(g)", col_ix = 2), non_numeric_cols = c())
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

#' Read the data from a Glooko file.
#'
#' @param filename Character string with the filename to be parsed.
#'
#' @return A data frame with the content found in the file, with formatted timestamps and numeric columns. NULL if the file could not be parsed
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

#' Format and clean CGM data from Glooko.
#'
#' @param data A data frame containing the CGM data from a Glooko file.
#'
#' @return A data frame containing the formatted and cleaned CGM data.
#' @keywords internal
.format_cgm_glooko.aidR <- function(data) {
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

#' Format and clean basal data from Glooko.
#'
#' @param data A data frame containing the basal data from a Glooko file.
#'
#' @return A data frame containing the formatted and cleaned basal data.
#' @keywords internal
.format_basal_glooko.aidR <- function(data) {
  data <- data.frame(unclass(data))

  # Rename columns (for different languages)
  names(data) <- c(
    "Timestamp", "Insulin Type", "Duration (minutes)",
    "Percentage (%)", "Rate", "Insulin Delivered (U)", "Serial number"
  )

  # Note: keep all "suspend" events for consistency (no insulin delivered)
  data <- data %>%
    mutate(
      amount = .data$`Duration (minutes)` / 60 * .data$Rate, # Calculate basal insulin as duration (mins) / 60 * rate [units/hour]
      delivery_type = .data$`Insulin Type`,
      timezone_offset = NA
    ) %>%
    rename(
      timestamp = .data$Timestamp,
      duration = .data$`Duration (minutes)`,
      rate = .data$Rate,
      percent = .data$`Percentage (%)`
    ) %>%
    select("timestamp", "timezone_offset", "duration", "amount", "rate")
  
  # Add sensor name: not known for Glooko (!= Serial number)
  data$sensor_name <- NA

  return(data)
}

#------------------------------
# Functions for bolus
#------------------------------

#' Format and clean bolus data from Glooko.
#'
#' @param data A data frame containing the bolus data from a Glooko file.
#'
#' @return A data frame containing the formatted and cleaned bolus data.
#' @keywords internal
.format_bolus_glooko.aidR <- function(data) {
  data <- data.frame(unclass(data))

  # Rename columns (for different languages)
  names(data) <- c(
    "Timestamp", "Insulin Type", "Blood Glucose Input",
    "Carbs Input", "Carbs Ratio", "Insulin Delivered (U)",
    "Initial Delivery (U)", "Extended Delivery (U)", "Serial number"
  )

  data <- data %>%
    rename(timestamp = .data$Timestamp) %>%
    mutate(
      sub_type = case_when(
        is.na(.data$`Initial Delivery (U)`) & is.na(.data$`Extended Delivery (U)`) ~ "standard",
        .data$`Initial Delivery (U)` > 0 & .data$`Extended Delivery (U)` > 0 ~ "dual_wave",
        .data$`Initial Delivery (U)` == 0 & .data$`Extended Delivery (U)` > 0 ~ "square_wave",
        TRUE ~ "normal"
      ),
      duration = NA,
      extended = ifelse(.data$sub_type != "standard",
        .data$`Extended Delivery (U)`,
        NA
      ),
      normal = case_when(
        sub_type == "standard" ~ .data$`Insulin Delivered (U)`,
        sub_type == "dual_wave" ~ .data$`Initial Delivery (U)`,
        sub_type == "square_wave" ~ 0
      ),
      type = NA,
      timezone_offset = NA
    ) %>%
    select("timestamp", "timezone_offset", "type", "sub_type", "duration", "extended", "normal")

  # Add sensor name: not known for Glooko (!= Serial number)
  data$sensor_name <- NA
  
  return(data)
}

#------------------------------
# Functions for total insulin
#------------------------------

#' Format and clean total insulin data from Glooko.
#'
#' @param data A data frame containing the total insulin data from a Glooko file.
#'
#' @return A data frame containing the formatted and cleaned total insulin data.
#' @keywords internal
.format_total_insulin_glooko.aidR <- function(data) {
  data <- data.frame(unclass(data))

  names(data) <- c(
    "Timestamp", "Total Bolus (U)", "Total Insulin (U)",
    "Total Basal (U)", "Serial number"
  )

  data <- data %>%
    mutate(
      total_bolus = .data$`Total Bolus (U)`,
      total_insulin = .data$`Total Insulin (U)`,
      total_basal = .data$`Total Basal (U)`,
      timezone_offset = NA
    ) %>%
    rename(timestamp = .data$Timestamp) %>%
    select("timestamp", "timezone_offset", "total_bolus", "total_insulin", "total_basal")

  return(data)
}

#------------------------------
# Functions for carbs
#------------------------------

#' Format and clean carbohydrate data from Glooko.
#'
#' @param data A data frame containing the carbohydrate data from a Glooko file.
#'
#' @return A data frame containing the formatted and cleaned carbohydrate data.
#' @keywords internal
.format_carbs_glooko.aidR <- function(data) {
  data <- data.frame(unclass(data))

  # 4th column in entered carbs. Filter on non-zero carbs
  data <- data.frame(timestamp = data[, 1],
                     timezone_offset = NA,
                     carbs = data[, 4])

  data <- data %>%
    filter(.data$carbs > 0) %>%
    mutate(
      label = NA,
      estimated_absorption_duration = NA
    )

  return(data)
}

#------------------------------
# Functions for SMBG (BG)
#------------------------------

#' Format and clean SMBG (bg sheet) data from Glooko.
#'
#' @param data A data frame containing the SMBG data from a Glooko file.
#'
#' @return A data frame containing the formatted and cleaned SMBG data.
#' @keywords internal
.format_SMBG_glooko.aidR <- function(data) {
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
    timestamp = data[, 1],
    timezone_offset = NA,
    value = data[, 2],
    manual_reading = data[, 3]
  )

  return(data)
}

#------------------------------
# Functions for alarms
#------------------------------

#' Format and clean alarm data from Glooko.
#'
#' @param data A data frame containing the alarm data from a Glooko file.
#'
#' @return A data frame containing the formatted and cleaned alarm data.
#' @keywords internal
.format_alarms_glooko.aidR <- function(data) {
  data <- data.frame(unclass(data))

  # drop serial number
  data <- data.frame(
    timestamp = data[, 1],
    timezone_offset = NA,
    name = data[, 2]
  )

  return(data)
}

#------------------------------
# Functions for manual insulin
#------------------------------

#' Format and clean manual insulin data from Glooko.
#'
#' @param data A data frame containing the manual insulin data from a Glooko file.
#'
#' @return A data frame containing the formatted and cleaned manual insulin data.
#' @keywords internal
.format_manual_insulin_glooko.aidR <- function(data) {
  data <- data.frame(unclass(data))

  # drop serial number
  data <- data.frame(
    timestamp = data[, 1],
    timezone_offset = NA,
    name = data[, 2],
    value = data[, 3],
    insulin_type = data[, 4]
  )

  return(data)
}

#------------------------------
# Functions for medication
#------------------------------

#' Format and clean medication data from Glooko.
#'
#' @param data A data frame containing the medication data from a Glooko file.
#'
#' @return A data frame containing the formatted and cleaned medication data.
#' @keywords internal
.format_medication_glooko.aidR <- function(data) {
  data <- data.frame(unclass(data))

  # drop serial number
  data <- data.frame(
    timestamp = data[, 1],
    timezone_offset = NA,
    name = data[, 2],
    value = data[, 3],
    medication_type = data[, 4]
  )

  return(data)
}

#------------------------------
# Functions for food
#------------------------------

#' Format and clean food data from Glooko.
#'
#' @param data A data frame containing the food data from a Glooko file.
#'
#' @return A data frame containing the formatted and cleaned food data.
#' @keywords internal
.format_food_glooko.aidR <- function(data) {
  data <- data.frame(unclass(data))

  # Note: ignoring fat, proteins, calories and portions for now
  # consider adding them

  # 3th column is carbs
  data <- data.frame(
    timestamp = data[, 1],
    timezone_offset = NA,
    label = data[, 2],
    carbs = data[, 3]
  )

  # Filter on non-zero carbs
  data <- data %>%
    filter(.data$carbs > 0) %>%
    mutate(estimated_absorption_duration = NA)

  return(data)
}

#------------------------------
# Functions for notes
#------------------------------

#' Format and clean notes data from Glooko.
#'
#' @param data A data frame containing the notes data from a Glooko file.
#'
#' @return A data frame containing the formatted and cleaned notes data.
#' @keywords internal
.format_notes_glooko.aidR <- function(data) {
  data <- data.frame(unclass(data))

  data <- data.frame(
    timestamp = data[, 1],
    timezone_offset = NA,
    note = data[, 2]
  )

  return(data)
}

#------------------------------
# Functions for exercise
#------------------------------

#' Format and clean exercise data from Glooko.
#'
#' @param data A data frame containing the exercise data from a Glooko file.
#'
#' @return A data frame containing the formatted and cleaned exercise data.
#' @keywords internal
.format_exercise_glooko.aidR <- function(data) {
  data <- data.frame(unclass(data))

  data <- data.frame(
    timestamp = data[, 1],
    timezone_offset = NA,
    name = data[, 2],
    intensity = data[, 3],
    duration_minutes = data[, 4],
    burned_calories = data[, 5]
  )

  return(data)
}
