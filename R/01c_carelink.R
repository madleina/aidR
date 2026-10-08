##################################
#                                #
#   Read CareLink data           #
#                                #
##################################

#------------------------
# Public functions
#------------------------

#' Read a file from a CareLink export
#'
#' @param id Character or numeric participant identifier.
#' @param filename Character string with the filename of the file to be read.
#'
#' @return An instance of class \code{carelink}, wrapping a named list with the
#'   data of the different types found in the file.
#'
#' @keywords internal
read_carelink <- function(id, filename) {
  # Read raw: 3 data frames with (1) many different informations, (2) daily aggregated auto (closed-loop) insulin and (3) CGM measurement data.
  raw <- .read_carelink_raw.aidR(id, filename)
  dfs_new <- raw$dfs

  # First df: contains all sorts of info -> split
  SMBG <- .get_carelink_SMBG.aidR(dfs_new[[1]])
  scheduled_basal_rates <- .get_carelink_scheduled_basal_rates.aidR(dfs_new[[1]], raw$pump_name)
  temp_basal <- .get_carelink_temp_basal.aidR(dfs_new[[1]], raw$pump_name)
  bolus <- .get_carelink_bolus.aidR(dfs_new[[1]], raw$pump_name)
  prime <- .get_carelink_prime.aidR(dfs_new[[1]])
  alerts <- .get_carelink_alerts.aidR(dfs_new[[1]])
  corr_bolus_info <- .get_carelink_correction_bolus_info.aidR(dfs_new[[1]])
  bwz <- .get_carelink_BWZ.aidR(dfs_new[[1]])
  sensor_calibration <- .get_carelink_sensor_calibration.aidR(dfs_new[[1]])
  insulin_action_curve <- .get_carelink_insulin_action_curve_over_time.aidR(dfs_new[[1]])
  device_changes <- .get_carelink_device_changes.aidR(dfs_new[[1]])

  # Match bolus and BWZ
  bolus <- .match_carelink_BWZ_bolus.aidR(bolus, bwz)

  data <- list(
    SMBG = SMBG, scheduled_basal_rates = scheduled_basal_rates, temp_basal = temp_basal,
    bolus = bolus, prime = prime, alerts = alerts, corr_bolus_info = corr_bolus_info,
    bwz = bwz, sensor_calibration = sensor_calibration, insulin_action_curve = insulin_action_curve,
    device_changes = device_changes
  )

  # Check if we missed any data while splitting
  .check_if_missing_columns_with_data.aidR(dfs_new[[1]], subset = bind_rows(data))

  # Second df: contains aggregated auto (closed-loop) insulin (per day)
  aggr_auto_insulin <- .get_carelink_aggr_auto_insulin.aidR(dfs_new[[2]])
  .check_if_missing_columns_with_data.aidR(dfs_new[[2]], subset = aggr_auto_insulin)
  data$aggr_auto_insulin <- aggr_auto_insulin

  # Third df: contains sensor (CGM) data
  data$cgm <- .get_carelink_CGM.aidR(dfs_new[[3]], raw$sensor_name)
  data$sensor_exceptions <- .get_carelink_sensor_exceptions.aidR(dfs_new[[3]])

  class(data) <- "carelink"

  return(data)
}

#' Format and clean the CareLink data to keep relevant columns only
#'
#' @param data An instance of class \code{carelink}, wrapping a named list with data of different types.
#' @param id Character or numeric participant identifier.
#' @param ... Additional arguments passed to methods.
#'
#' @return A named list with the cleaned data, with entries \code{cgm},
#'   \code{bolus}, \code{carbs} (extracted from the bolus wizard
#'   entries) and \code{SMBG}.
#' @keywords internal
#' @exportS3Method
clean.carelink <- function(data, id, ...) {
  if (is.null(data)) {
    return(NULL)
  }
  if (!("carelink" %in% class(data))) {
    stop("Expected CareLink format.")
  }

  # Remove class attribute for ease
  data <- unclass(data)

  # Format cgm, bolus and carbs to unit format
  data_new <- list()
  data_new$cgm <- .clean_carelink_CGM.aidR(id, data$cgm)
  data_new$bolus <- .clean_carelink_bolus.aidR(id, data$bolus)
  data_new$carbs <- .clean_carelink_carbs.aidR(id, data$bwz)
  data_new$SMBG <- .clean_carelink_SMBG.aidR(id, data$SMBG)

  # Daily bolus total: not reported by CareLink -> sum the standardized data.
  # Note: the only daily aggregate CareLink reports is the auto (closed-loop)
  # insulin of the "Aggregated Auto Insulin Data" section, which is not used here
  # (see data$aggr_auto_insulin of the uncleaned data).
  data_new$total_bolus <- .total_bolus_per_day.aidR(data_new$bolus)

  return(data_new)
}

#------------------------
# Helper functions
#------------------------

#' Read the raw content of a CareLink file, split into its sections
#'
#' @param id Character or numeric participant identifier.
#' @param filename Character string with the filename of the file to be read.
#'
#' @return A list with entries \code{dfs}, \code{sensor_name} and \code{pump_name}.
#'   \code{dfs} is a list of three data frames containing (1) pump and sensor
#'   events of many different types, (2) daily aggregated auto (closed-loop)
#'   insulin and (3) CGM measurement data. \code{sensor_name} and
#'   \code{pump_name} are the device
#'   names read from the file header, \code{NA} if the header is absent.
#'
#' @keywords internal
.read_carelink_raw.aidR <- function(id, filename) {
  # Open file
  data <- .open_carelink_file.aidR(filename)

  # Split by section (-------)
  dfs <- split(data, f = cumsum(grepl("-------", data$V1)))

  sensor_name <- NA
  pump_name <- NA
  if (length(dfs) > 3) {
    # First df: contains patient info and versions (not always available)
    # Read pump name
    pump_name <- as.character(dfs[[1]][1, ][8])

    # Read sensor name
    sensor_name <- as.character(dfs[[1]][3, ][8])

    # Not relevant for further analysis
    dfs <- dfs[2:length(dfs)]
  }

  dfs_new <- list()
  for (df in dfs) {
    # Get type name
    name <- df[1, ][1]
    name <- paste0(name[name != "" & name != "NA" & !grepl("-----", name)], collapse = "_")

    if (length(name) != 1) {
      stop(paste0("Id ", id, ", file '", filename, "': Failed to parse name of data frame from header '", df[1, ], "'."))
    }

    # Get column names
    col_names <- df[2, ]

    # Remove empty lines
    df <- as.data.frame(df[df$V1 != "", ])

    # Split properly
    df <- data.frame(x = df[-(1:2), ])
    names(df) <- col_names

    # Set NAs
    df[df == ""] <- NA
    df[df == "NA"] <- NA

    # Remove empty lines (again, relevant if ; is used as delimiter)
    df <- df[!is.na(df$Index), ]

    # Format timestamp
    df <- df %>%
      mutate(timestamp = .convert_timestamp_carelink.aidR(
        timestamp = paste0(.data$Date, " ", .data$Time),
        date_val  = .data$Date,
        time_val  = .data$Time
      )) %>%
      mutate(`New Device Time` = .convert_timestamp_carelink.aidR(.data$`New Device Time`)) %>%
      select(-"Date", -"Time") %>%
      relocate("timestamp", .after = "Index")

    # Reformat all digits with "," to "."
    df <- df %>%
      mutate(across(4:ncol(df), ~ ifelse(
        grepl("^[-+]?[0-9]+(,[0-9]+)?$", .), # is probably a number
        as.numeric(gsub(",", ".", .)),
        .
      )))

    # Convert all mmol/L to mg/dL
    df <- .convert_BG_units_carelink.aidR(df)

    dfs_new[[name]] <- df
  }

  if (length(dfs_new) %% 3 != 0) {
    stop("Expected a multiple of 3 sub-frames, found ", length(dfs_new))
  }

  if (length(dfs_new) > 3) {
    dfs_new <- .match_types_before_merging.aidR(dfs_new)
    # Can happen if there are many datapoints -> starts to split (or duplicate)
    # Merge 1 with 4, 2 with 5, 3 with 6 etc
    dfs_new <- lapply(1:3, function(i) {
      bind_rows(dfs_new[seq(i, length(dfs_new), by = 3)]) %>%
        arrange(desc(timestamp))
    })
  }

  if (length(dfs_new) != 3) {
    stop("Expected 3 sub-frames, found ", length(dfs_new))
  }

  return(list(dfs = dfs_new, sensor_name = sensor_name, pump_name = pump_name))
}

#' Check if a file comes from a CareLink export
#'
#' CareLink is used for Medtronic MiniMed devices only, which is what the check
#' looks for in the file header.
#'
#' @param filename Character string with the filename to be checked.
#'
#' @return A logical value: \code{TRUE} if the file comes from a CareLink export.
#'
#' @keywords internal
.is_carelink_format.aidR <- function(filename) {
  tryCatch(
    {
      # CareLink: only for Medtronic
      data <- .open_carelink_file.aidR(filename)
      if (any(grepl("MiniMed", data[1, ]))) {
        return(TRUE)
      }
      FALSE
    },
    warning = function(w) FALSE,
    error = function(e) FALSE
  )
}

#' Open a CareLink file
#'
#' Works if the file is in xlsx or csv format, throws an error otherwise.
#'
#' @param filename Character string with the filename to be opened.
#'
#' @return A data frame with a single column \code{V1}, holding each line of the
#'   file as a semicolon-separated string.
#'
#' @keywords internal
.open_carelink_file.aidR <- function(filename) {
  if (file_ext(filename) == "xlsx") {
    data <- read_excel(filename, col_names = F, trim_ws = F, .name_repair = "minimal")
    names(data)[1] <- "V1"
  } else if (file_ext(filename) == "csv") {
    sep <- .detect_csv_separator.aidR(filename)
    data <- read.csv(filename, skip = 0, check.names = F, header = F, sep = sep)
  } else {
    stop("Unknown file extension for CareLink: ", file_ext(filename), ".")
  }
  return(data)
}

#' Convert timestamps from an Excel file formatted as fractional days
#'
#' @param date_val The fractional value for the date.
#' @param time_val The fractional value for the time of day.
#'
#' @return A timestamp in POSIXct, or \code{NULL} if the values are not numeric
#'   and must be parsed as strings instead.
#'
#' @keywords internal
.convert_excel_datetime.aidR <- function(date_val, time_val) {
  # If numeric, convert from Excel serial format
  if (is.numeric(date_val) || suppressWarnings(!is.na(as.numeric(date_val)))) {
    date_part <- as.Date(as.numeric(date_val), origin = "1899-12-30")
    time_part <- as.numeric(time_val) * 86400 # fractional day -> seconds
    return(as.POSIXct(as.numeric(date_part) * 86400 + time_part, origin = "1970-01-01", tz = "UTC"))
  }

  # Otherwise, fall through to string parsing
  return(NULL)
}

#' Convert timestamps from an Excel file formatted as serial numbers
#'
#' @param serial The serial value.
#'
#' @return A timestamp in POSIXct.
#'
#' @keywords internal
.convert_excel_serial.aidR <- function(serial) {
  serial <- as.numeric(serial)
  date_part <- floor(serial)
  time_part <- serial - date_part
  as.POSIXct(
    as.numeric(as.Date(date_part, origin = "1899-12-30")) * 86400 + time_part * 86400,
    origin = "1970-01-01", tz = "UTC"
  )
}

#' Convert timestamps of a CareLink file to POSIXct timestamps
#'
#' @param timestamp The timestamp from the file.
#' @param date_val The fractional value for the date, used if timestamp can not be parsed directly.
#' @param time_val The fractional value for the time of day, used if timestamp can not be parsed directly.
#'
#' @return A vector of timestamps in POSIXct.
#'
#' @keywords internal
.convert_timestamp_carelink.aidR <- function(timestamp, date_val = NULL, time_val = NULL) {
  if (all(is.na(timestamp))) {
    return(timestamp)
  }
  if (length(timestamp) == 0 || all(trimws(as.character(timestamp)) == "")) {
    return(as.POSIXct(character(0), tz = "UTC"))
  }

  # Case 1: separate Date + Time columns, both numeric (Excel serials)
  if (!is.null(date_val) && !is.null(time_val)) {
    result <- tryCatch(
      .convert_excel_datetime.aidR(date_val[1], time_val[1]),
      error = function(e) NULL
    )
    if (!is.null(result)) {
      return(do.call(c, lapply(seq_along(date_val), function(i) {
        .convert_excel_datetime.aidR(date_val[i], time_val[i])
      })))
    }
  }

  # Case 2: single combined Excel serial (e.g. New Device Time)
  if (suppressWarnings(any(!is.na(as.numeric(timestamp))))) {
    return(.convert_excel_serial.aidR(timestamp))
  }

  # Case 3: string parsing fallback
  dt <- parse_date_time(as.character(timestamp), c("dmy HMS", "dmY HMS", "dmy HM", "dmY HM", "Ymd HMS"), tz = "UTC")
  if (any(is.na(dt) & !is.na(timestamp))) stop("Unknown date format")
  return(dt)
}

#' Check that no relevant columns were missed when splitting a data frame
#'
#' Throws an error if there are columns with non-NA entries in \code{df} that are
#' not present in \code{subset}.
#'
#' @param df The full data frame.
#' @param subset The subsetted data frame.
#'
#' @return Called for its side effects. Returns \code{NULL} invisibly.
#'
#' @keywords internal
.check_if_missing_columns_with_data.aidR <- function(df, subset) {
  # Go over entries of columns in df that are NOT present in subset -> check if there is any non-NA entry there
  for (i in 1:nrow(subset)) {
    these_rows_other_cols <- df[df$Index %in% subset$Index, !(names(df) %in% names(subset))]

    for (j in 1:ncol(these_rows_other_cols)) {
      if (any(!is.na(these_rows_other_cols[, j]))) {
        stop("Found column ", names(these_rows_other_cols)[j], " with non-NA entries that is not present in subset!")
      }
    }
  }
}

#' Convert blood glucose units to mg/dL in a CareLink file
#'
#' @param df A data frame obtained from reading a CareLink file.
#'
#' @return A data frame, with all mmol/L columns converted to mg/dL and renamed
#'   accordingly.
#'
#' @keywords internal
.convert_BG_units_carelink.aidR <- function(df) {
  # Loop over all columns, check if unit is mmol/L and convert to mg/dL
  for (i in 1:ncol(df)) {
    if (grepl("mmol/L", names(df)[i])) {
      # Convert to mg/dL
      df[, i] <- as.numeric(df[, i]) * 18.018
      # Rename
      names(df)[i] <- gsub("mmol/L", "mg/dL", names(df)[i])
    }
  }
  return(df)
}

#' Match the column types of multiple data frames
#'
#' Empty data frames are replaced by a zero-row data frame with the column types
#' of the first non-empty data frame.
#'
#' @param dfs_new A list of data frames.
#'
#' @return A list of data frames, with harmonized types.
#'
#' @keywords internal
.match_types_before_merging.aidR <- function(dfs_new) {
  # Store schema from a non-empty df before splitting
  schema_df <- dfs_new[[which(sapply(dfs_new, nrow) > 0)[1]]][0, ] # 0 rows, correct types

  # Replace empty dfs with the schema df
  dfs_new <- lapply(dfs_new, function(df) {
    if (nrow(df) == 0) schema_df else df
  })

  return(dfs_new)
}

#' Detect the separator of a CSV file
#'
#' Detects whether a CSV file uses a comma or semicolon as the column
#' separator.
#'
#' @param file Path to the CSV file.
#'
#' @return The detected separator as a character string.
#'
#' @keywords internal
.detect_csv_separator.aidR <- function(file) {
  line <- readLines(file, n = 1, warn = FALSE)

  separators <- c(",", ";")
  counts <- vapply(
    separators,
    function(separator) {
      lengths(regmatches(line, gregexpr(separator, line, fixed = TRUE)))
    },
    integer(1)
  )

  if (all(counts == 0)) {
    stop("Could not detect CSV separator.")
  }

  separators[which.max(counts)]
}

#------------------------
# Functions for parsing
#------------------------

#' Parse all entries corresponding to SMBG in a CareLink export
#'
#' @param df A data frame obtained from reading a CareLink file.
#'
#' @return A data frame with the corresponding entries.
#'
#' @keywords internal
.get_carelink_SMBG.aidR <- function(df) {
  SMBG <- df %>%
    select("Index", "timestamp", "BG Source", "BG Reading (mg/dL)", "Linked BG Meter ID") %>%
    filter(if_any(-c(1:2), ~ !is.na(.) & . != ""))

  .check_if_missing_columns_with_data.aidR(df, SMBG)

  return(SMBG)
}

#' Parse all entries corresponding to the scheduled basal rates in a CareLink export
#'
#' @param df A data frame obtained from reading a CareLink file.
#' @param pump_name A character string with the name of the pump, \code{NA} if unknown.
#'
#' @return A data frame with the corresponding entries.
#'
#' @keywords internal
.get_carelink_scheduled_basal_rates.aidR <- function(df, pump_name) {
  scheduled_basal_rates <- df %>%
    select("Index", "timestamp", "Basal Rate (U/h)", "Suspend", "Rewind") %>%
    filter(if_any(-c(1:2), ~ !is.na(.) & . != ""))

  # Calculate duration to next entry
  scheduled_basal_rates <- scheduled_basal_rates %>%
    arrange(desc(.data$timestamp)) %>%
    mutate(
      next_time = lag(.data$timestamp),
      duration_h = as.numeric(difftime(.data$next_time, .data$timestamp, units = "hours"))
    ) %>%
    select(-"next_time")

  .check_if_missing_columns_with_data.aidR(df, scheduled_basal_rates)

  # Add pump name column
  scheduled_basal_rates$pump_name <- rep(as.character(pump_name), nrow(scheduled_basal_rates))

  return(scheduled_basal_rates)
}

#' Parse all entries corresponding to temporary basal rates in a CareLink export
#'
#' @param df A data frame obtained from reading a CareLink file.
#' @param pump_name A character string with the name of the pump, \code{NA} if unknown.
#'
#' @return A data frame with the corresponding entries.
#'
#' @keywords internal
.get_carelink_temp_basal.aidR <- function(df, pump_name) {
  temp_basal <- df %>%
    select("Index", "timestamp", "Temp Basal Amount", "Temp Basal Type", "Temp Basal Duration (h:mm:ss)", "Preset Temp Basal Name") %>%
    filter(if_any(-c(1:2), ~ !is.na(.) & . != ""))

  .check_if_missing_columns_with_data.aidR(df, temp_basal)

  # Add pump name column
  temp_basal$pump_name <- rep(pump_name, nrow(temp_basal))

  return(temp_basal)
}

#' Parse all entries corresponding to bolus deliveries in a CareLink export
#'
#' @param df A data frame obtained from reading a CareLink file.
#' @param pump_name A character string with the name of the pump, \code{NA} if unknown.
#'
#' @return A data frame with the corresponding entries.
#'
#' @keywords internal
.get_carelink_bolus.aidR <- function(df, pump_name) {
  bolus <- df %>%
    select(
      "Index", "timestamp", "Bolus Type", "Bolus Volume Selected (U)", "Bolus Volume Delivered (U)",
      "Bolus Duration (h:mm:ss)", "Bolus Number", "Bolus Cancellation Reason", "Preset Bolus", "Bolus Source"
    ) %>%
    filter(if_any(-c(1:2), ~ !is.na(.) & . != ""))

  # Remove duplicate rows (all entries are present 2x, once with delivered and once without delivered)
  bolus <- bolus %>%
    arrange(.data$timestamp) %>%
    group_by(across(-c("Bolus Volume Delivered (U)", "timestamp", "Index"))) %>%
    mutate(
      # Mark groups of timestamps within <1 min difference
      time_group = cumsum(c(TRUE, diff(.data$timestamp) >= dminutes(1)))
    ) %>%
    group_by(across(-c("Bolus Volume Delivered (U)", "timestamp", "Index")), .data$time_group, .add = TRUE) %>%
    summarise(
      Index = min(.data$Index, na.rm = T),
      timestamp = min(.data$timestamp, na.rm = TRUE),
      `Bolus Volume Delivered (U)` = coalesce(.data$`Bolus Volume Delivered (U)`[!is.na(.data$`Bolus Volume Delivered (U)`)][1], .data$`Bolus Volume Delivered (U)`[1]),
      .groups = "drop_last"
    ) %>%
    ungroup() %>%
    select(-"time_group") %>%
    relocate("Index", "timestamp", .before = "Bolus Type") %>%
    relocate("Bolus Volume Delivered (U)", .after = "Bolus Volume Selected (U)") %>%
    arrange(.data$timestamp)

  .check_if_missing_columns_with_data.aidR(df, bolus)

  # Add pump name column
  bolus$pump_name <- pump_name

  return(bolus)
}

#' Parse all entries corresponding to prime events in a CareLink export
#'
#' @param df A data frame obtained from reading a CareLink file.
#'
#' @return A data frame with the corresponding entries.
#'
#' @keywords internal
.get_carelink_prime.aidR <- function(df) {
  prime <- df %>%
    select("Index", "timestamp", "Prime Type", "Prime Volume Delivered (U)", "Estimated Reservoir Volume after Fill (U)") %>%
    filter(if_any(-c(1:2), ~ !is.na(.) & . != ""))

  .check_if_missing_columns_with_data.aidR(df, prime)
  return(prime)
}

#' Parse all entries corresponding to alerts in a CareLink export
#'
#' @param df A data frame obtained from reading a CareLink file.
#'
#' @return A data frame with the corresponding entries.
#'
#' @keywords internal
.get_carelink_alerts.aidR <- function(df) {
  alerts <- df %>%
    select("Index", "timestamp", "Alert", "User Cleared Alerts") %>%
    filter(if_any(-c(1:2), ~ !is.na(.) & . != ""))

  .check_if_missing_columns_with_data.aidR(df, alerts)
  return(alerts)
}

#' Parse all entries corresponding to correction boluses in a CareLink export
#'
#' @param df A data frame obtained from reading a CareLink file.
#'
#' @return A data frame with the corresponding entries.
#'
#' @keywords internal
.get_carelink_correction_bolus_info.aidR <- function(df) {
  if (!("SmartGuard Correction Bolus Feature" %in% names(df))) {
    return(data.frame())
  }
  corr_bolus_info <- df %>%
    select("Index", "timestamp", "SmartGuard Correction Bolus Feature") %>%
    filter(if_any(-c(1:2), ~ !is.na(.) & . != ""))

  .check_if_missing_columns_with_data.aidR(df, corr_bolus_info)
  return(corr_bolus_info)
}

#' Parse all entries corresponding to the bolus wizard in a CareLink export
#'
#' @param df A data frame obtained from reading a CareLink file.
#'
#' @return A data frame with the corresponding entries.
#'
#' @keywords internal
.get_carelink_BWZ.aidR <- function(df) {
  # The names of the carbohydrate columns depend on the carbohydrate unit set on
  # the pump (grams or exchanges) -> select whichever variant is present
  carb_cols <- unlist(lapply(.carelink_carb_variants.aidR(), function(v) c(v$ratio, v$input)))

  bwz <- df %>%
    select(
      "Index", "timestamp", "BWZ Estimate (U)", "BWZ Target High BG (mg/dL)", "BWZ Target Low BG (mg/dL)",
      any_of(carb_cols), "BWZ Insulin Sensitivity (mg/dL/U)",
      "BWZ BG/SG Input (mg/dL)", "BWZ Correction Estimate (U)", "BWZ Food Estimate (U)",
      "BWZ Active Insulin (U)", "BWZ Status", "BWZ Unabsorbed Insulin Total (U)",
      "Final Bolus Estimate", "Scroll Step Size"
    ) %>%
    filter(if_any(-c(1:2), ~ !is.na(.) & . != ""))

  .check_if_missing_columns_with_data.aidR(df, bwz)
  return(bwz)
}

#' The carbohydrate column names used in a CareLink export
#'
#' CareLink names the bolus wizard carbohydrate columns after the carbohydrate
#' unit configured on the pump: grams ("BWZ Carb Input (grams)" and
#' "BWZ Carb Ratio (g/U)") or exchanges ("BWZ Carb Input (exchanges)" and
#' "BWZ Carb Ratio (U/Ex)").
#'
#' @return A list with one entry per carbohydrate unit, each a list with the
#'   column names of the carbohydrate input (\code{input}) and the carbohydrate
#'   ratio (\code{ratio}), as well as the corresponding unit (\code{unit}).
#'
#' @keywords internal
.carelink_carb_variants.aidR <- function() {
  list(
    list(input = "BWZ Carb Input (grams)", ratio = "BWZ Carb Ratio (g/U)", unit = "g"),
    list(input = "BWZ Carb Input (exchanges)", ratio = "BWZ Carb Ratio (U/Ex)", unit = "exchanges")
  )
}

#' Find the carbohydrate columns present in a CareLink export
#'
#' @param df A data frame containing the bolus wizard entries from a CareLink file.
#'
#' @return A list with the column names of the carbohydrate input (\code{input})
#'   and the carbohydrate ratio (\code{ratio}), as well as the corresponding
#'   unit (\code{unit}).
#'
#' @keywords internal
.get_carelink_carb_variant.aidR <- function(df) {
  variants <- .carelink_carb_variants.aidR()
  found <- Filter(function(v) v$input %in% names(df), variants)

  if (length(found) == 0) {
    stop(
      "Found no carbohydrate column in the bolus wizard entries! Expected one of: ",
      paste(sapply(variants, function(v) v$input), collapse = ", ")
    )
  }
  if (length(found) > 1) {
    stop(
      "Found more than one carbohydrate column in the bolus wizard entries: ",
      paste(sapply(found, function(v) v$input), collapse = ", ")
    )
  }

  return(found[[1]])
}

#' Match bolus wizard entries to bolus entries from a CareLink export
#'
#' @param bolus A data frame containing bolus entries.
#' @param bwz A data frame containing bolus wizard entries.
#'
#' @return A data frame containing bolus entries, with an extra column indicating the index of a matching bolus wizard entry.
#'
#' @keywords internal
.match_carelink_BWZ_bolus.aidR <- function(bolus, bwz) {
  bolus$ix_in_bwz <- NA
  for (i in 1:nrow(bolus)) {
    amount_match <- bwz$`BWZ Estimate (U)` == bolus$`Bolus Volume Selected (U)`[i]
    time_match <- abs(difftime(bwz$timestamp, bolus$timestamp[i])) < seconds(30)
    match <- which(amount_match & time_match)

    if (length(match) == 1) {
      bolus$ix_in_bwz[i] <- match
    } else if (length(match) > 1) {
      # take the closer one
      match <- match[which.min(abs(difftime(bwz$timestamp[match], bolus$timestamp[i])))]
      bolus$ix_in_bwz[i] <- match
    }
    # Else no match: Is ok
  }
  return(bolus)
}

#' Parse all entries corresponding to sensor calibration in a CareLink export
#'
#' @param df A data frame obtained from reading a CareLink file.
#'
#' @return A data frame with the corresponding entries.
#'
#' @keywords internal
.get_carelink_sensor_calibration.aidR <- function(df) {
  if (!("Sensor Calibration BG (mg/dL)") %in% names(df)) {
    return(NULL)
  }

  sensor <- df %>%
    select("Index", "timestamp", "Sensor Calibration BG (mg/dL)", "Sensor Calibration Rejected Reason") %>%
    filter(if_any(-c(1:2), ~ !is.na(.) & . != ""))

  .check_if_missing_columns_with_data.aidR(df, sensor)
  return(sensor)
}

#' Parse all entries corresponding to insulin action curve over time in a CareLink export
#'
#' @param df A data frame obtained from reading a CareLink file.
#'
#' @return A data frame with the corresponding entries.
#'
#' @keywords internal
.get_carelink_insulin_action_curve_over_time.aidR <- function(df) {
  iac <- df %>%
    select("Index", "timestamp", "Insulin Action Curve Time") %>%
    filter(if_any(-c(1:2), ~ !is.na(.) & . != ""))

  .check_if_missing_columns_with_data.aidR(df, iac)
  return(iac)
}

#' Parse all entries corresponding to device changes in a CareLink export
#'
#' @param df A data frame obtained from reading a CareLink file.
#'
#' @return A data frame with the corresponding entries.
#'
#' @keywords internal
.get_carelink_device_changes.aidR <- function(df) {
  changes <- df %>%
    select("Index", "timestamp", "BLE Network Device") %>%
    filter(if_any(-c(1:2), ~ !is.na(.) & . != ""))

  .check_if_missing_columns_with_data.aidR(df, changes)
  return(changes)
}

#' Parse all entries corresponding to the daily aggregated auto (closed-loop) insulin in a CareLink export
#'
#' @param df A data frame obtained from reading a CareLink file.
#'
#' @return A data frame with the corresponding entries.
#'
#' @keywords internal
.get_carelink_aggr_auto_insulin.aidR <- function(df) {
  agg_insulin <- df %>%
    select("Index", "timestamp", "Bolus Type", "Bolus Volume Selected (U)", "Bolus Volume Delivered (U)", "Bolus Source") %>%
    filter(if_any(-c(1:2), ~ !is.na(.) & . != ""))

  .check_if_missing_columns_with_data.aidR(df, agg_insulin)
  return(agg_insulin)
}

#' Parse all entries corresponding to CGM data in a CareLink export
#'
#' @param df A data frame obtained from reading a CareLink file.
#' @param sensor_name A character string with the name of the sensor, \code{NA} if unknown.
#'
#' @return A data frame with the corresponding entries.
#'
#' @keywords internal
.get_carelink_CGM.aidR <- function(df, sensor_name) {
  # So far, all Event Markers were Start or End of the day -> we can ignore those
  # But check if there are any others, in case this becomes important
  if (!all(df$`Event Marker` == "Start of the day" | df$`Event Marker` == "End of the day", na.rm = T)) {
    stop("Unexpected event markers! Check if we should keep this column")
  }

  # ISIG = raw data number that the sensor extracts -> run through MM’s algorithm to get sensor glucose number
  CGM <- df %>%
    select("Index", "timestamp", "Sensor Glucose (mg/dL)", "ISIG Value") %>%
    filter(if_any(-c(1:2), ~ !is.na(.) & . != ""))

  # Add sensor name column
  CGM$sensor_name <- sensor_name

  return(CGM)
}

#' Parse all entries corresponding to sensor exceptions in a CareLink export
#'
#' @param df A data frame obtained from reading a CareLink file.
#'
#' @return A data frame with the corresponding entries.
#'
#' @keywords internal
.get_carelink_sensor_exceptions.aidR <- function(df) {
  sensor_exceptions <- df %>%
    select("Index", "timestamp", "Sensor Exception") %>%
    filter(if_any(-c(1:2), ~ !is.na(.) & . != ""))

  return(sensor_exceptions)
}

#------------------------
# Functions for cleaning
#------------------------

#' Format and clean CGM data from a CareLink export
#'
#' @param id Character or numeric participant identifier.
#' @param cgm A data frame containing the CGM data from a CareLink file.
#'
#' @return A data frame containing the formatted and cleaned CGM data.
#'
#' @keywords internal
.clean_carelink_CGM.aidR <- function(id, cgm) {
  # only keep timestamp and value

  cgm <- cgm %>%
    select("timestamp", "Sensor Glucose (mg/dL)", "sensor_name") %>%
    rename(value = "Sensor Glucose (mg/dL)") |>
    mutate(
      id = id,
      format = "carelink",
      .before = "timestamp"
    ) |>
    mutate(timezone_offset = NA, .after = "timestamp") |>
    mutate(unit = "mg/dL", .before = "sensor_name")

  return(cgm)
}

#' Format and clean bolus data from a CareLink export
#'
#' @param id Character or numeric participant identifier.
#' @param bolus A data frame containing the bolus data from a CareLink file.
#'
#' @return A data frame containing the formatted and cleaned bolus data.
#'
#' @keywords internal
.clean_carelink_bolus.aidR <- function(id, bolus) {
  # Decide if a bolus is normal / square wave (dual wave does not exist in Minimed)
  bolus <- bolus %>%
    mutate(
      type = case_when(
        # normal if no duration
        is.na(.data$`Bolus Duration (h:mm:ss)`) ~ "normal",
        # square wave if with duration
        !is.na(.data$`Bolus Duration (h:mm:ss)`) ~ "square_wave",
        # default: normal
        TRUE ~ "normal"
      )
    )

  # Add units of total bolus, as well as units of normal and extended bolus
  bolus <- bolus %>%
    mutate(
      id = id,
      format = "carelink",
      total = as.numeric(.data$`Bolus Volume Delivered (U)`),
      extended = if_else(.data$type == "normal", 0, .data$total),
      normal = if_else(.data$type == "normal", .data$total, 0),
      duration_extended = if (all(is.na(.data$`Bolus Duration (h:mm:ss)`))) {
        NA
      } else {
        time_length(hms(as.character(.data$`Bolus Duration (h:mm:ss)`)), unit = "hour")
      },
      unit = "U",
      timezone_offset = NA,
      pump_name = NA
    ) %>%
    select(
      "id", "format", "timestamp", "timezone_offset", "type", "total", "normal",
      "extended", "unit", "duration_extended", "pump_name"
    )

  # Remove all boluses where nothing was delivered
  # Reason: all boluses appear 2x, once with bolus selected and once with delivered
  bolus <- bolus |>
    filter(!is.na(.data$total))

  return(bolus)
}

#' Format and clean carbohydrate data from a CareLink export
#'
#' @param id Character or numeric participant identifier.
#' @param bwz A data frame containing the bolus wizard entries from a CareLink
#'   file, which hold the carbohydrates entered into the bolus calculator.
#'
#' @return A data frame containing the formatted and cleaned carbohydrate data.
#'
#' @keywords internal
.clean_carelink_carbs.aidR <- function(id, bwz) {
  # Extract carb info from bolus wizard: reported either in grams or in exchanges,
  # depending on the carbohydrate unit set on the pump
  variant <- .get_carelink_carb_variant.aidR(bwz)
  if (variant$unit != "g") {
    message(
      "Id ", id, ": Carbohydrates are reported in ", variant$unit,
      " and not in grams - the exchange size is not part of the export, ",
      "so they are kept as is.
            Please make sure to convert to grams using the correct exchange size."
    )
  }

  carbs <- bwz %>%
    mutate(
      id = id,
      format = "carelink",
      timezone_offset = NA,
      value = as.numeric(.data[[variant$input]]),
      unit = variant$unit,
      label = NA,
      estimated_absorption_duration = NA,
      is_hypo_treatment = NA
    ) %>%
    select(
      "id", "format", "timestamp", "timezone_offset", "value", "unit",
      "label", "estimated_absorption_duration", "is_hypo_treatment"
    )

  return(carbs)
}

#' Format and clean SMBG data from a CareLink export
#'
#' @param id Character or numeric participant identifier.
#' @param SMBG A data frame containing the SMBG data from a CareLink file.
#'
#' @return A data frame containing the formatted and cleaned SMBG data.
#'
#' @keywords internal
.clean_carelink_SMBG.aidR <- function(id, SMBG) {
  SMBG <- SMBG %>%
    mutate(
      id = id,
      format = "carelink",
      timezone_offset = NA,
      value = as.numeric(.data$`BG Reading (mg/dL)`),
      unit = "mg/dL",
      sensor_name = NA
    ) %>%
    select(
      "id", "format", "timestamp", "timezone_offset", "value",
      "unit", "sensor_name"
    )

  return(SMBG)
}
