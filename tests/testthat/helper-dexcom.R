# Helpers for round-tripping CGM data through the Dexcom readers of other packages

#' Write CGM readings to a Dexcom-style CSV
#'
#' One fixture serves both readers that aidR is tested against:
#'
#' * `cgmquantify::readfile()` picks the columns
#'   `Timestamp (YYYY-MM-DDThh:mm:ss)` and `Glucose Value (mg/dL)` (which
#'   `read.csv()` mangles into the names it actually indexes) and ends with
#'   `tail(df, -11)` to strip Dexcom's metadata block. Exactly 11 header rows
#'   are therefore written, so that it returns exactly the readings passed here.
#' * `iglu::read_raw_data(sensor = "dexcom")` drops every row whose
#'   `Event Type` is not `"EGV"` instead, and takes the first column matching
#'   "glucose" as the glucose value.
#'
#' The glucose values are written with 17 significant digits rather than through
#' `as.character()`, which `write.csv()` would use and which keeps only 15. aidR
#' converts CGM data to mg/dL, so a value can carry conversion noise far below
#' any relevant digit (e.g. 97.999999999999986), and at 15 digits that collapses
#' to "98" - numerically irrelevant, but enough to move a reading across a bin
#' boundary and make e.g. `iglu::hist_roc()` differ.
#'
#' Note that 17 digits still does not make the round-trip bit-exact: R parses
#' decimal strings up to one ulp low (`as.numeric("96.000000000000014")` returns
#' exactly 96), in `read.csv()` and in the parser alike. A CSV round-trip in R
#' therefore cannot preserve an arbitrary double, and the comparisons against
#' the readers are tolerance-based, as `expect_equal()` is by default.
#'
#' @param time A vector of timestamps.
#' @param glucose A numeric vector of glucose values, in mg/dL.
#' @param path Character string with the file to write.
write_dexcom_csv <- function(time, glucose, path){
  # The 11 metadata rows of a Dexcom export, which carry no reading
  header <- data.frame(
    event_type = c("FirstName", "LastName", "Device", "SerialNumber",
                   "TransmitterId", "TransmitterTime", "AlertSettings",
                   "AlertSettings", "AlertSettings", "AlertSettings",
                   "AlertSettings"),
    timestamp  = NA_character_,
    glucose    = NA_character_
  )
  readings <- data.frame(
    event_type = "EGV",
    timestamp  = format(time, "%Y-%m-%dT%H:%M:%S"),
    glucose    = sprintf("%.17g", as.numeric(glucose))
  )

  out <- rbind(header, readings)
  names(out) <- c("Event Type", "Timestamp (YYYY-MM-DDThh:mm:ss)",
                  "Glucose Value (mg/dL)")
  utils::write.csv(out, path, row.names = FALSE, na = "")

  invisible(path)
}

#' Run the session in UTC for the duration of a test
#'
#' `cgmquantify::readfile()` parses the Dexcom timestamps with `as.POSIXct()`
#' without a timezone, i.e. in the session timezone, and then derives `Date` with
#' `as.Date()`, which converts to UTC first. Its `Date` column therefore shifts
#' by a day around midnight whenever the session is not in UTC, while `aidR`
#' derives the date from the local (wall-clock) time. Forcing UTC makes the two
#' directly comparable. Not needed for `iglu::read_raw_data()`, which takes the
#' timezone as an argument.
local_utc <- function(envir = parent.frame()){
  old_tz <- Sys.getenv("TZ", unset = NA)
  Sys.setenv(TZ = "UTC")
  withr::defer({
    if (is.na(old_tz)){ Sys.unsetenv("TZ") } else { Sys.setenv(TZ = old_tz) }
  }, envir = envir)
}

#' The cgmquantify functions that summarize a CGM data frame
#'
#' All of them take the data frame as their only mandatory argument and return a
#' numeric value or a data frame.
cgmquantify_metrics <- function(){
  c("interdaysd", "interdaycv", "intradaysd", "intradaycv",
    "TIR", "TOR", "POR", "MGE", "MGN", "J_index",
    "LBGI", "HBGI", "LBGI_HBGI", "eA1c", "GMI", "summary_glucose")
}

#' The iglu functions that summarize a CGM data frame
#'
#' All exported iglu functions whose first argument is \code{data}, that run on a
#' single subject without further arguments, and that return a value rather than
#' a plot, i.e. every metric, profile and transformation function.
#'
#' \code{agp()} is not among them: it returns a patchwork of the plots that
#' \code{plot_agp()} and \code{plot_daily()} draw and the numbers that
#' \code{agp_metrics()} reports - all three are compared separately - and its
#' grid grobs are named from a global counter, so two calls never compare equal.
iglu_metrics <- function(){
  c("above_percent", "active_percent", "adrr", "agp_metrics",
    "all_metrics", "auc", "below_percent", "CGMS2DayByDay", "cogi", "conga",
    "cv_glu", "cv_measures", "ea1c", "episode_calculation", "gmi", "grade",
    "grade_eugly", "grade_hyper", "grade_hypo", "gri", "gvp", "hbgi",
    "hyper_index", "hypo_index", "igc", "in_range_percent",
    "iqr_glu", "j_index", "lbgi", "m_value", "mad_glu", "mag", "mage",
    "mean_glu", "median_glu", "modd", "pgs", "quantile_glu", "range_glu",
    "roc", "sd_glu", "sd_measures", "sd_roc", "summary_glu")
}

#' The iglu functions that draw a CGM data frame
#'
#' Compared through \code{ggplot2::layer_data()}, i.e. on the data they draw,
#' rather than as objects.
iglu_plots <- function(){
  c("plot_glu", "plot_daily", "plot_agp", "plot_ranges", "hist_roc")
}
