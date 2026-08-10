## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(aidR)
library(dplyr)

## -----------------------------------------------------------------------------
path <- system.file("extdata", "1_cycle_1_arm_1_ms_aid_data_1.xlsx", package = "aidR")

data_101 <- parse_data(id = "101", paths = path)
str(data_101, max.level = 1)
str(data_101[[1]], max.level = 1)

## -----------------------------------------------------------------------------
merged_101 <- merge_per_id(data_101)
str(merged_101, max.level = 1)
head(merged_101$cgm)

## -----------------------------------------------------------------------------
nrow(merged_101$cgm)

clean_101 <- remove_duplicates(merged_101)
nrow(clean_101$cgm)

## -----------------------------------------------------------------------------
check_completeness("101", clean_101)

## -----------------------------------------------------------------------------
date_range <- range(as.Date(clean_101$cgm$timestamp))
date_range

check_completeness_range(
  "101", clean_101,
  first_date = as.character(date_range[1]),
  last_date  = as.character(date_range[2])
)

## -----------------------------------------------------------------------------
# For illustration, we reuse the same sample export for a second participant
data_102  <- parse_data(id = "102", paths = path)
clean_102 <- remove_duplicates(merge_per_id(data_102))

all_data <- list("101" = clean_101, "102" = clean_102)
combined <- merge_all(all_data)

str(combined, max.level = 1)
head(combined$cgm)

## -----------------------------------------------------------------------------
combined$cgm %>%
  filter(id == "101") %>%
  head()

## -----------------------------------------------------------------------------
combined$cgm %>%
  filter(timestamp >= min(timestamp), timestamp < min(timestamp) + lubridate::ddays(3)) %>%
  nrow()

## -----------------------------------------------------------------------------
combined$cgm %>%
  filter(value < 70) %>%
  head()

## -----------------------------------------------------------------------------
combined$bolus %>%
  filter(type == "automated") %>%
  head()

## ----eval = FALSE-------------------------------------------------------------
# ids   <- c("101", "102")           # participant IDs
# paths <- list(                      # one or more export paths per participant
#   "101" = "path/to/participant_101_export.zip",
#   "102" = "path/to/participant_102_export.zip"
# )
# 
# all_data <- list()
# for (id in ids) {
#   parsed          <- parse_data(id, paths[[id]])
#   merged          <- merge_per_id(parsed)
#   all_data[[id]]  <- remove_duplicates(merged)
# 
#   check_completeness(id, all_data[[id]])
# }
# 
# combined <- merge_all(all_data)

