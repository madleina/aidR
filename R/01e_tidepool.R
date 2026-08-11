##################################
#                                #
#   Read tidepool data           #
#                                #
##################################

#------------------------
# Public functions
#------------------------

#' Read file from a Tidepool export
#'
#' @param id Character or numeric participant identifier.
#' @param filename Character string corresponding to the filename of the Tidepool export.
#'
#' @return An instance of class \code{tidepool}.
read_tidepool <- function(id, filename){
  # CGM, basal, and bolus should always exist
  # Note: guess_max is set to a higher value to ensure proper parsing of sparse columns
  # Note: name_repair is set to minimal to silence messages about columns without names
  cgm <- read_excel(filename, sheet = "CGM", guess_max = 100000, .name_repair = "minimal")
  basal <- read_excel(filename, sheet = "Basal", .name_repair = "minimal")
  bolus <- read_excel(filename, sheet = "Bolus", .name_repair = "minimal")
  
  # Read optional sheets
  device_event <- .read_tidepool_sheet.aidR(filename, sheet = "Device Event")
  food <- .read_tidepool_sheet.aidR(filename, sheet = "Food")
  bolus_calculator <- .read_tidepool_sheet.aidR(filename, sheet = "Bolus Calculator")
  basal_schedules <- .read_tidepool_sheet.aidR(filename, sheet = "Basal Schedules")
  bg_targets <- .read_tidepool_sheet.aidR(filename, sheet = "BG Targets")
  carb_ratios <- .read_tidepool_sheet.aidR(filename, sheet = "Carb Ratios")
  insulin_sensitivities <- .read_tidepool_sheet.aidR(filename, sheet = "Insulin Sensitivities")
  smbg <- .read_tidepool_sheet.aidR(filename, sheet = "SMBG")
  upload <- .read_tidepool_sheet.aidR(filename, sheet = "Upload")
  
  data <- list(cgm = cgm, basal = basal, bolus = bolus, device_event = device_event, 
               food = food, bolus_calculator = bolus_calculator,
               basal_schedules = basal_schedules, bg_targets = bg_targets, 
               carb_ratios = carb_ratios, insulin_sensitivities = insulin_sensitivities,
               smbg = smbg, upload = upload)
  class(data) <- "tidepool"

  return(data)
}

#' Format and clean the Tidepool data to keep relevant columns only.
#'
#' @param data An instance of class \code{tidepool}, wrapping a named list with data of different types.
#' @param ... Additional arguments passed to methods.
#'
#' @return A named list with names \code{cgm}, \code{basal}, \code{bolus}, and \code{carbs}, and other names for data of other types.
#' @export
clean.tidepool <- function(data, ...){
  if (is.null(data)){ return(NULL) }
  if (!("tidepool" %in% class(data))){ stop("Expected Tidepool format.") }
  
  # Remove class attribute for ease
  data <- unclass(data)
  
  # Format CGM, basal, bolus and carb data
  data$cgm <- .format_cgm_tidepool.aidR(data$cgm)
  data$basal <- .format_basal_tidepool.aidR(data$basal)
  data$bolus <- .format_bolus_tidepool.aidR (data$bolus)
  
  if (!is.null(data$food) & !is.null(data$bolus_calculator)){
    stop(paste0("Found both food and bolus calculator data. Figure out which one to keep."))
  } else if (!is.null(data$food)){
    data$carbs <- .format_food_tidepool.aidR(data$food)
  } else if (!is.null(data$bolus_calculator)){
    data$carbs <- .format_bolus_calculator_tidepool.aidR(data$bolus_calculator)
  } else {
    stop(paste0("No data on carbohydrates available!"))
  }

  # Found everything
  return(data)
}

#------------------------
# Helper functions
#------------------------

#' Check if a file corresponds to a file from a Tidepool export.
#'
#' @param filename Character string with the filename to be checked.
#'
#' @return A logical value: \code{TRUE} if the file is a file from a Tidepool export.
#'
#' @keywords internal
.is_tidepool_format.aidR <- function(filename){
  if (is.na(excel_format(filename))) {
    return(FALSE)
  }
  tryCatch({
    read_excel(filename, n_max = 0, sheet = "CGM", .name_repair = "minimal")
    TRUE
  }, error = function(e) FALSE)
}

#' Read a sheet from a Tidepool Excel file
#'
#' @param filename A character string giving the path to the Excel file.
#' @param sheet A character string giving the name of the sheet to read.
#'
#' @return A data frame containing the contents of the specified sheet, or
#'   \code{NULL} if the sheet does not exist in the file.
#' @keywords internal
.read_tidepool_sheet.aidR <- function(filename, sheet){
  if (sheet %in% excel_sheets(filename)){
    return(read_excel(filename, sheet = sheet, .name_repair = "minimal"))
  } else {
    return(NULL)
  }
}

#------------------------
# Format CGM
#------------------------

#' Format and clean CGM data from Tidepool
#'
#' @param cgm A data frame containing the CGM data from a Tidepool file.
#'
#' @return A data frame containing the formatted and cleaned CGM data.
#' @keywords internal
.format_cgm_tidepool.aidR <- function(cgm){
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
      y <- fromJSON(cgm$Payload[i])
      if ("g7" %in% names(y)){
        if (y$g7){ sensor_name[i] <- "Dexcom G7" }
      }
    }
  }
  
  # Select columns: timestamp and value
  cgm <- cgm %>%
    select("Local Time", "Timezone Offset", "Value") %>%
    mutate(`Local Time` = as_datetime(.data$`Local Time`),
           `Timezone Offset` = .data$`Timezone Offset` / 60) %>% 
    rename(value = "Value", timestamp = "Local Time", timezone_offset = "Timezone Offset") |> 
    mutate(unit = "mg/dL",
           sensor_name = sensor_name)

  return(cgm)
}

#------------------------
# Format basal
#------------------------

#' Format and clean basal data from Tidepool
#'
#' @param basal A data frame containing the basal data from a Tidepool file.
#'
#' @return A data frame containing the formatted and cleaned basal data.
#' @keywords internal
.format_basal_tidepool.aidR <- function(basal){
  basal <- basal %>%
    mutate(
      amount = .data$`Duration (mins)` / 60 * .data$`Rate`,
    ) %>%
    select("Local Time", "Timezone Offset", "Duration (mins)", "amount", "Rate", "Delivery Type") %>%
    mutate(`Local Time` = as_datetime(.data$`Local Time`),
           `Timezone Offset` = .data$`Timezone Offset` / 60) %>% 
    rename(
      timestamp     = "Local Time", 
      timezone_offset = "Timezone Offset",
      duration      = "Duration (mins)", 
      rate          = "Rate",
      delivery_type = "Delivery Type"
    )
  
  # Set rate and amount for suspend to zero
  # Relevant for open-source AIDs (e.g. Loop), where this is NA
  basal$rate[basal$delivery_type == "suspend"] <- 0
  basal$amount[basal$delivery_type == "suspend"] <- 0
  
  # Dealing with delivery types
  # If there is automated and temp at the same time
  # Take only automated
  # Relevant for Open-source AIDs: may contain duplicates
  basal <- basal %>%
    group_by(.data$timestamp) %>%
    dplyr::filter(
      # Check if there is a combination of type "temp" and "automated" within the same timestamp
      if (!any(is.na(.data$delivery_type)) & any(.data$delivery_type == "temp") & any(.data$delivery_type == "automated")) {
        # If the above condition is true, keep only rows with type "temp"
        .data$delivery_type == "temp"
      } else {
        # Otherwise, keep all rows as they are
        TRUE
      }
    ) %>%
    ungroup() %>% 
    select(-"delivery_type")
  
  # Add pump name to basal: Not known from exports
  basal$pump_name <- NA
  
  return(basal)
}

#------------------------
# Format bolus
#------------------------

#' Format and clean bolus data from Tidepool
#'
#' @param bolus A data frame containing the bolus data from a Tidepool file.
#'
#' @return A data frame containing the formatted and cleaned bolus data.
#' @keywords internal
.format_bolus_tidepool.aidR <- function(bolus){
  # Classify: normal, dual_wave and square_wave
  bolus <- bolus %>%
    mutate(sub_type = case_when(
             !("Sub Type" %in% names(bolus)) ~ "standard",
             is.na(.data$Extended) | .data$Extended == 0 ~ "standard",
             .data$Extended > 0 & .data$Normal > 0 ~ "dual_wave",
             .data$Extended > 0 & (is.na(.data$Normal) | .data$Normal == 0) ~ "square_wave",
             TRUE ~ "standard"
           )
    ) %>%
    select("Local Time", "Timezone Offset", "Sub Type", "sub_type", "Duration (mins)", "Extended", "Normal") %>%
    mutate(`Local Time` = as_datetime(.data$`Local Time`),
           `Timezone Offset` = .data$`Timezone Offset` / 60) %>% 
    rename(timestamp = "Local Time", 
           timezone_offset = "Timezone Offset",
           duration = "Duration (mins)", 
           extended = "Extended", 
           normal = "Normal", 
           type = "Sub Type")
  
  # Add pump name to bolus: Not known from exports
  bolus$pump_name <- NA
  
  return(bolus)
}

#------------------------
# Format food
#------------------------

#' Format and clean food data from Tidepool
#'
#' @param food A data frame containing the food data from a Tidepool file.
#'
#' @return A data frame containing the formatted and cleaned food data.
#' @keywords internal
.format_food_tidepool.aidR <- function(food){
  # Extract net carbs and estimated absorption duration
  food$carbs_grams <- sapply(food$Nutrition, .extract_net_carbs_tidepool.aidR)
  food$estimated_absorption_duration <- sapply(1:nrow(food), .extract_estimated_absorption_duration_tidepool.aidR, food)
  
  food <- food %>%
    select("Local Time", "Timezone Offset", "carbs_grams", "Name", "estimated_absorption_duration") %>%
    mutate(`Local Time` = as_datetime(.data$`Local Time`),
           `Timezone Offset` = .data$`Timezone Offset` / 60,
           estimated_absorption_duration = .data$estimated_absorption_duration / 60) %>% # convert seconds to minutes 
    rename(timestamp = "Local Time", 
           timezone_offset = "Timezone Offset",
           label = "Name")
  
  return(food)
}


#' Extract net carbohydrate amount from json string given in "Nutrition" column of Food sheet
#'
#' @param json_str A character string in json format, containing the net carbohydrate amount
#'
#' @return A numeric value with the net carbohydrate amount
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

#' Extract estimated absorption duration from json string given in Bolus Calculator sheet
#'
#' @param row The current row index to parse
#' @param data The full data frame
#'
#' @return A numeric value with the estimated absorption duration, NA if not found.
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

#' Format and clean carbohydrates from bolus calculator data from Tidepool
#'
#' @param bolus_calculator A data frame containing the bolus calculator data from a Tidepool file.
#'
#' @return A data frame containing the formatted and cleaned carbohydrates from bolus calculator data.
#' @keywords internal
.format_bolus_calculator_tidepool.aidR <- function(bolus_calculator){
  
  bolus_calculator <- bolus_calculator %>%
    filter(.data$`Carb Input` > 0) %>%
    mutate(label = NA) %>%
    mutate(`Local Time` = as_datetime(.data$`Local Time`),
           `Timezone Offset` = .data$`Timezone Offset` / 60) %>% 
    select("Local Time", "Timezone Offset", "Carb Input", "label") %>%
    rename(timestamp = "Local Time", 
           timezone_offset = "Timezone Offset",
           carbs_grams = "Carb Input")
  
  if (nrow(bolus_calculator) == 0){ return(NULL) }
  
  bolus_calculator$estimated_absorption_duration <- NA
  
  return(bolus_calculator)
}



