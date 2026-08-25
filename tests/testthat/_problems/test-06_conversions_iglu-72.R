# Extracted from test-06_conversions_iglu.R:72

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "aidR", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
path_1 <- system.file("extdata", "tidepool_example.xlsx", package = "aidR")
path_2 <- system.file("extdata", "carelink_example.csv", package = "aidR")
data <- merge_all(list("1" = parse_data(id = "1", paths = path_1),
                         "2" = parse_data(id = "2", paths = path_2)))
converted <- get_iglu_format(data)
expect_equal(names(converted), c("id", "time", "gl"))
expect_equal(nrow(converted), nrow(data$cgm))
