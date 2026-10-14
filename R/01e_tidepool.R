##################################
#                                #
#   Read Tidepool data           #
#                                #
##################################

#------------------------
# Public functions
#------------------------

#' Read a file from a Tidepool export
#'
#' @param id Character or numeric participant identifier.
#' @param filename Character string with the filename of the file to be read.
#'
#' @return An instance of class \code{tidepool}, wrapping a named list with one
#'   entry per sheet of the export. Entries of optional sheets that are absent
#'   from the file are \code{NULL}.
#'
#' @keywords internal
read_tidepool <- function(id, filename){
  # basal and bolus should always exist
  # Note: guess_max is set to a higher value to ensure proper parsing of sparse columns
  # Note: name_repair is set to minimal to silence messages about columns without names
  basal <- read_excel(filename, sheet = "Basal", guess_max = 100000, .name_repair = "minimal")
  bolus <- read_excel(filename, sheet = "Bolus", guess_max = 100000, .name_repair = "minimal")
  
  # Read optional sheets
  cgm <- .read_tidepool_sheet.aidR(filename, sheet = "CGM") # can sometimes be missing
  device_event <- .read_tidepool_sheet.aidR(filename, sheet = "Device Event")
  food <- .read_tidepool_sheet.aidR(filename, sheet = "Food")
  bolus_calculator <- .read_tidepool_sheet.aidR(filename, sheet = "Bolus Calculator")
  basal_schedules <- .read_tidepool_sheet.aidR(filename, sheet = "Basal Schedules")
  bg_targets <- .read_tidepool_sheet.aidR(filename, sheet = "BG Targets")
  carb_ratios <- .read_tidepool_sheet.aidR(filename, sheet = "Carb Ratios")
  insulin_sensitivities <- .read_tidepool_sheet.aidR(filename, sheet = "Insulin Sensitivities")
  SMBG <- .read_tidepool_sheet.aidR(filename, sheet = "SMBG")
  upload <- .read_tidepool_sheet.aidR(filename, sheet = "Upload")
  
  data <- list(cgm = cgm, basal = basal, bolus = bolus, device_event = device_event, 
               food = food, bolus_calculator = bolus_calculator,
               basal_schedules = basal_schedules, bg_targets = bg_targets, 
               carb_ratios = carb_ratios, insulin_sensitivities = insulin_sensitivities,
               SMBG = SMBG, upload = upload)
  class(data) <- "tidepool"

  return(data)
}

#' Format and clean the Tidepool data to keep relevant columns only
#'
#' @param data An instance of class \code{tidepool}, wrapping a named list with data of different types.
#' @param id Character or numeric participant identifier.
#' @param ... Additional arguments passed to methods.
#'
#' @return A named list with the cleaned data, with entries \code{cgm},
#'   \code{basal}, \code{bolus}, \code{SMBG} and \code{carbs}. Carbohydrates are
#'   taken from the Food sheet or, if that sheet is absent, from the Bolus
#'   Calculator sheet. Only in the latter case are \code{bolus} and
#'   \code{carbs} linked by \code{link_id}; otherwise it is \code{NA}.
#' @keywords internal
#' @exportS3Method
clean.tidepool <- function(data, id, ...){
  if (is.null(data)){ return(NULL) }
  if (!("tidepool" %in% class(data))){ stop("Expected Tidepool format.") }
  
  # Remove class attribute for ease
  data <- unclass(data)
  
  # Format CGM, basal, bolus and carb data
  data_new <- list()
  data_new$cgm <- .format_cgm_tidepool.aidR(id, data$cgm)
  data_new$basal <- .format_basal_tidepool.aidR(id, data$basal)
  data_new$SMBG <- .format_SMBG_tidepool.aidR(id, data$SMBG)
  
  # Link boluses and carbs: only possible via the Bolus Calculator sheet
  link <- NULL
  if (is.null(data$food) && !is.null(data$bolus_calculator)){
    link <- .link_bolus_carbs_tidepool.aidR(id, data$bolus, data$bolus_calculator)
  }
  data_new$bolus <- .format_bolus_tidepool.aidR(id, data$bolus, link$bolus)
  
  if (!is.null(data$food) & !is.null(data$bolus_calculator)){
    stop(paste0("Found both food and bolus calculator data. Figure out which one to keep."))
  } else if (!is.null(data$food)){
    data_new$carbs <- .format_food_tidepool.aidR(id, data$food)
  } else if (!is.null(data$bolus_calculator)){
    data_new$carbs <- .format_bolus_calculator_tidepool.aidR(id, data$bolus_calculator, link$bolus_calculator)
  } else {
    stop(paste0("No data on carbohydrates available!"))
  }

  # Daily insulin totals: not reported by Tidepool -> sum the standardized data
  data_new$total_basal <- .total_basal_per_day.aidR(data_new$basal)
  data_new$total_bolus <- .total_bolus_per_day.aidR(data_new$bolus)
  
  # Found everything
  return(data_new)
}

#------------------------
# Helper functions
#------------------------

#' Check if a file comes from a Tidepool export
#'
#' @param filename Character string with the filename to be checked.
#'
#' @return A logical value: \code{TRUE} if the file comes from a Tidepool export.
#'
#' @keywords internal
.is_tidepool_format.aidR <- function(filename){
  if (is.na(excel_format(filename))) {
    return(FALSE)
  }
  tryCatch({
    sheets <- excel_sheets(filename)
    all(c("Basal", "Bolus") %in% sheets)
  }, error = function(e) FALSE)
}

#' Read a sheet from a Tidepool Excel file
#'
#' @param filename Character string with the filename of the file to be read.
#' @param sheet Character string with the name of the sheet to be read.
#'
#' @return A data frame containing the contents of the specified sheet, or
#'   \code{NULL} if the sheet does not exist in the file.
#' @keywords internal
.read_tidepool_sheet.aidR <- function(filename, sheet){
  if (sheet %in% excel_sheets(filename)){
    return(read_excel(filename, sheet = sheet, guess_max = 100000, .name_repair = "minimal"))
  } else {
    return(NULL)
  }
}

#------------------------
# Format CGM
#------------------------

#' Format and clean CGM data from a Tidepool export
#'
#' @param id Character or numeric participant identifier.
#' @param cgm A data frame containing the CGM data from a Tidepool file.
#'
#' @return A data frame containing the formatted and cleaned CGM data.
#' @keywords internal
.format_cgm_tidepool.aidR <- function(id, cgm){
  if (is.null(cgm)){ return(NULL) }
  
  # Make sure CGM is in mg/dl
  cgm$Value[cgm$Units == "mmol/L"] <- cgm$Value[cgm$Units == "mmol/L"] * 18.018

  # Check if we can use local time
  if (all(is.na(cgm$`Local Time`))){
    stop("Local time is NA.")
  }
  
  # Check if we can find g7 indicator for sensor name
  sensor_name <- rep(NA, nrow(cgm))
  if ("Payload" %in% names(cgm)){
    for (i in 1:nrow(cgm)){
      if (is.na(cgm$Payload[i])){ next }
      y <- fromJSON(cgm$Payload[i])
      if ("g7" %in% names(y)){
        if (y$g7){ sensor_name[i] <- "Dexcom G7" }
      }
    }
  }
  
  # Select columns: timestamp and value
  cgm <- cgm %>%
    select("Local Time", "Timezone Offset", "Value") %>%
    mutate(id = id,
           format = "tidepool",
           `Local Time` = as_datetime(.data$`Local Time`),
           `Timezone Offset` = .data$`Timezone Offset` / 60) %>% 
    rename(value = "Value", timestamp = "Local Time", timezone_offset = "Timezone Offset") |> 
    mutate(unit = "mg/dL",
           sensor_name = sensor_name) |> 
    select("id", "format", "timestamp", "timezone_offset", "value", "unit", "sensor_name")

  return(cgm)
}

#------------------------
# Format basal
#------------------------

#' Format and clean basal data from a Tidepool export
#'
#' @param id Character or numeric participant identifier.
#' @param basal A data frame containing the basal data from a Tidepool file.
#'
#' @return A data frame containing the formatted and cleaned basal data.
#' @keywords internal
.format_basal_tidepool.aidR <- function(id, basal){
  # Select relevant columns, format time, rename
  basal <- basal %>%
    mutate(id = id,
           format = "tidepool",
           pump_name = NA, # not given in sheet
           duration = .data$`Duration (mins)` / 60, # in hours
           unit = "U/h") |> # of rate
    select("id", "format", "Local Time", "Timezone Offset", "duration", "Rate", 
           "unit", "Delivery Type", "pump_name") %>%
    mutate(`Local Time` = as_datetime(.data$`Local Time`),
           `Timezone Offset` = .data$`Timezone Offset` / 60) %>% 
    rename(
      timestamp       = "Local Time", 
      timezone_offset = "Timezone Offset",
      rate            = "Rate",
      delivery_type   = "Delivery Type"
    )
  
  # Remove very short basal rates (artefacts from Loop)
  # filter out rates that last < 1s
  basal <- basal |> 
    filter(.data$duration >= 1/3600)
  
  # Set rate for suspend to zero
  # Relevant for open-source AIDs (e.g. Loop), where this is NA
  basal$rate[basal$delivery_type == "suspend"] <- 0

  # Dealing with delivery types
  # If "temp" coexists with "automated" and/or "suspend" in the same timestamp, 
  # keep only the "temp" row(s); otherwise keep everything.
  # Relevant for Open-source AIDs: may contain duplicates
  basal <- basal %>%
    group_by(.data$timestamp) %>%
    dplyr::filter(
      if (!any(is.na(.data$delivery_type)) &&
          any(.data$delivery_type == "temp") &&
          any(.data$delivery_type %in% c("automated", "suspend"))) {
        .data$delivery_type == "temp"
      } else {
        TRUE
      }
    ) %>%
    ungroup() |> 
  select(-"delivery_type")
  
  return(basal)
}

#------------------------
# Format bolus
#------------------------

#' Format and clean bolus data from a Tidepool export
#'
#' @param id Character or numeric participant identifier.
#' @param bolus A data frame containing the bolus data from a Tidepool file.
#' @param link_id A character vector with one key per row of \code{bolus},
#'   linking the bolus to its carbohydrate entries. \code{NULL} if unknown, 
#'   which results in \code{NA}.
#'
#' @return A data frame containing the formatted and cleaned bolus data.
#' @keywords internal
.format_bolus_tidepool.aidR <- function(id, bolus, link_id = NULL){
  if (is.null(link_id)){ link_id <- NA }
  
  # Format time
  bolus <- bolus %>%
    mutate(`Local Time` = as_datetime(.data$`Local Time`),
           `Timezone Offset` = .data$`Timezone Offset` / 60
           )
    
  # Decide if a bolus is normal / dual wave / square wave
  bolus <- bolus %>%
    mutate(type = case_when(
             !("Sub Type" %in% names(bolus)) ~ "normal",
             # normal if there extended = 0
             is.na(.data$Extended) | .data$Extended == 0 ~ "normal",
             # dual_wave if normal > 0 and extended > 0
             .data$Extended > 0 & .data$Normal > 0 ~ "dual_wave",
             # square wave if normal = 0 and extended > 0
             .data$Extended > 0 & (is.na(.data$Normal) | .data$Normal == 0) ~ "square_wave",
             TRUE ~ "normal"
           )
    )
  
  # Add units of total bolus, as well as units of normal and extended bolus
  bolus <- bolus %>%
    mutate(normal = ifelse(is.na(.data$Normal), 0, .data$Normal),
           extended = ifelse(is.na(.data$Extended), 0, .data$Extended),
           total = coalesce(.data$extended, 0) + coalesce(.data$normal, 0)
    )
  
  # Select relevant columns
  bolus <- bolus %>%
    mutate(id = id,
           format = "tidepool",
           unit = "U",
           pump_name = NA,
           duration_extended = .data$`Duration (mins)` / 60,
           link_id = link_id) |> 
    rename(timestamp = "Local Time", 
           timezone_offset = "Timezone Offset") |> 
    select("id", "format", "timestamp", "timezone_offset", "type", "total", 
           "normal", "extended", "unit", "duration_extended", "pump_name", "link_id")
  
  return(bolus)
}

#------------------------
# Format food
#------------------------

#' Format and clean carbohydrate data from the Food sheet of a Tidepool export
#'
#' @param id Character or numeric participant identifier.
#' @param food A data frame containing the food data from a Tidepool file.
#'
#' @return A data frame containing the formatted and cleaned carbohydrate data.
#' @keywords internal
.format_food_tidepool.aidR <- function(id, food){
  # Extract net carbs and estimated absorption duration
  food$value <- sapply(food$Nutrition, .extract_net_carbs_tidepool.aidR)
  food$estimated_absorption_duration <- sapply(1:nrow(food), .extract_estimated_absorption_duration_tidepool.aidR, food)
  
  food <- food %>%
    mutate(id = id,
           format = "tidepool",
           timestamp = as_datetime(.data$`Local Time`),
           timezone_offset = .data$`Timezone Offset` / 60,
           estimated_absorption_duration = .data$estimated_absorption_duration / 3600, # convert seconds to hours 
           unit = "g",
           is_hypo_treatment = NA,
           link_id = NA) %>% # cannot be linked to boluses
    rename(label = "Name") |> 
    select("id", "format", "timestamp", "timezone_offset", "value", "unit", 
           "label", "estimated_absorption_duration", "is_hypo_treatment", "link_id")
  
  return(food)
}


#' Extract the net carbohydrate amount from the \code{Nutrition} column of the Food sheet
#'
#' @param json_str A character string in JSON format, containing the net carbohydrate amount.
#'
#' @return A numeric value with the net carbohydrate amount.
#' @keywords internal
.extract_net_carbs_tidepool.aidR <- function(json_str) {
  parsed <- fromJSON(json_str)
  
  if (length(parsed) == 0 | any(!("carbohydrate" %in% names(parsed)))){
    stop("Unknown type: ", parsed)
  }
  parsed <- parsed$carbohydrate
  
  if (length(names(parsed)) != 2 | any(names(parsed) != c("net", "units"))){
    stop("Unknown names: ", json_str)
  }
  if (parsed$units != "grams"){
    stop("Unknown unit: ", parsed$units)
  }
  
  return(parsed$net)
}

#' Extract the estimated absorption duration from the Food sheet of a Tidepool export
#'
#' The duration is either part of the JSON string in the \code{Nutrition} column,
#' or part of the JSON string in the \code{Payload} column.
#'
#' @param row The current row index to parse.
#' @param data The full data frame with the food data from a Tidepool file.
#'
#' @return A numeric value with the estimated absorption duration in seconds,
#'   \code{NA} if not found.
#' @keywords internal
.extract_estimated_absorption_duration_tidepool.aidR <- function(row, data){
  # Either given in "Nutrition" as part of json string
  parsed <- fromJSON(data$Nutrition[row])
  
  if ("estimatedAbsorptionDuration" %in% names(parsed)){
    return(parsed$estimatedAbsorptionDuration)
  }
  
  # Or given in Payload column as part of long json string
  if ("Payload" %in% names(data)){
    parsed <- fromJSON(data$Payload[row])
    exists <- grepl("AbsorptionTime", names(parsed))
    if (any(exists)){
      return(as.numeric(parsed[exists]))
    }
  }
  
  return(NA)
}

#------------------------
# Format bolus calculator
#------------------------

#' Link boluses and carbohydrate entries of a Tidepool export
#'
#' Use the column \code{Bolus} of the Bolus Calculator sheet and the column 
#' \code{Id} of the Bolus sheet to match bolus and carbohydrate entries. Create 
#' a new identifier based on local time, normal and extended amount, and 
#' duration as for all other formats.
#'
#' @param id Character or numeric participant identifier.
#' @param bolus A data frame containing the bolus data from a Tidepool file.
#' @param bolus_calculator A data frame containing the bolus calculator data
#'   from a Tidepool file.
#'
#' @return A list with entries \code{bolus} and \code{bolus_calculator},
#'   holding one key per row of the respective data frame (\code{NA} for rows
#'   without a link). \code{NULL} if the columns needed are missing.
#' @keywords internal
.link_bolus_carbs_tidepool.aidR <- function(id, bolus, bolus_calculator){
  if (!("Id" %in% names(bolus)) || !all(c("Bolus", "Carb Input") %in% names(bolus_calculator))){
    return(NULL)
  }
  
  # Only calculator entries with carbs and a bolus that is part of the Bolus sheet
  has_carbs <- (bolus_calculator$`Carb Input` > 0) %in% TRUE
  linked_calc <- has_carbs & !is.na(bolus_calculator$Bolus) & bolus_calculator$Bolus %in% bolus$Id
  
  # Key built from the content of the bolus (columns that are absent are NA)
  col <- function(name) if (name %in% names(bolus)) bolus[[name]] else rep(NA, nrow(bolus))
  key <- .create_link_id.aidR(id, as_datetime(col("Local Time")), col("Normal"), col("Extended"),
                              col("Duration (mins)"),
                              linked = bolus$Id %in% bolus_calculator$Bolus[linked_calc])
  
  # Calculator entries get the key of their bolus (several entries may share one bolus)
  list(
    bolus = key,
    bolus_calculator = ifelse(linked_calc, key[match(bolus_calculator$Bolus, bolus$Id)], NA)
  )
}

#' Format and clean carbohydrate data from the Bolus Calculator sheet of a Tidepool export
#'
#' @param id Character or numeric participant identifier.
#' @param bolus_calculator A data frame containing the bolus calculator data from a Tidepool file.
#' @param link_id A character vector with one key per row of
#'   \code{bolus_calculator}, linking the carbohydrate entry to its bolus. 
#'   \code{NULL} (the default) if unknown, which results in \code{NA}.
#'
#' @return A data frame containing the formatted and cleaned carbohydrate data.
#'   \code{NULL} if no carbohydrates were entered.
#' @keywords internal
.format_bolus_calculator_tidepool.aidR <- function(id, bolus_calculator, link_id = NULL){
  if (is.null(link_id)){ link_id <- NA }
  
  bolus_calculator <- bolus_calculator %>%
    mutate(link_id = link_id) %>%
    filter(.data$`Carb Input` > 0) %>%
    mutate(id = id,
           format = "tidepool",
           timestamp = as_datetime(.data$`Local Time`),
           timezone_offset = .data$`Timezone Offset` / 60,
           unit = "g",
           label = NA,
           estimated_absorption_duration = NA,
           is_hypo_treatment = NA
           ) %>% 
    rename(value = "Carb Input") |> 
    select("id", "format", "timestamp", "timezone_offset", "value", "unit", 
           "label", "estimated_absorption_duration", "is_hypo_treatment", "link_id")
  
  if (nrow(bolus_calculator) == 0){ return(NULL) }
  
  return(bolus_calculator)
}

#------------------------
# Format SMBG
#------------------------

#' Format and clean SMBG data from a Tidepool export
#'
#' @param id Character or numeric participant identifier.
#' @param SMBG A data frame containing the SMBG data from a Tidepool file.
#'
#' @return A data frame containing the formatted and cleaned SMBG data.
#'   \code{NULL} if the export contains no SMBG sheet.
#' @keywords internal
.format_SMBG_tidepool.aidR <- function(id, SMBG){
  if (is.null(SMBG)){ return(NULL) }
  
  # Make sure SMBG is in mg/dl
  SMBG$Value[SMBG$Units == "mmol/L"] <- SMBG$Value[SMBG$Units == "mmol/L"] * 18.018
  
  # Check if we can use local time
  if (all(is.na(SMBG$`Local Time`))){
    stop("Local time is NA.")
  }
  
  # Select columns: timestamp and value
  SMBG <- SMBG %>%
    mutate(id = id,
           format = "tidepool",
           timestamp = as_datetime(.data$`Local Time`),
           timezone_offset = .data$`Timezone Offset` / 60,
           value = .data$Value,
           unit = "mg/dL",
           sensor_name = NA) %>% 
    select("id", "format", "timestamp", "timezone_offset", "value", "unit", "sensor_name")
  
  return(SMBG)
}
