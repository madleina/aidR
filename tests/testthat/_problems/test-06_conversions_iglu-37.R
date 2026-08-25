# Extracted from test-06_conversions_iglu.R:37

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "aidR", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
path <- system.file("extdata", "tidepool_example.xlsx", package = "aidR")
data <- parse_data(id = "1", paths = path)
converted <- get_iglu_format(data)
csv <- withr::local_tempfile(fileext = ".csv")
write_dexcom_csv(converted$time, converted$gl, csv)
from_file <- iglu::read_raw_data(csv, sensor = "dexcom", id = "1", tz = "UTC")
from_file$id <- converted$id
for (metric in iglu_metrics()){
    fun <- getExportedValue("iglu", metric)
    result_converted <- suppressWarnings(fun(converted))
    result_from_file <- suppressWarnings(fun(from_file))
    expect_equal(result_converted, result_from_file, info = metric)
  }
