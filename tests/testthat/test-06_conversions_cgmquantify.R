test_that("get_cgmquantify_format() reproduces the data frame of cgmquantify::readfile()", {
  skip_if_not_installed("cgmquantify")
  local_utc()

  path <- system.file("extdata", "tidepool_example.xlsx", package = "aidR")
  data <- parse_data(id = "1", paths = path)
  converted <- get_cgmquantify_format(data)

  # Write the same CGM data to a Dexcom-style CSV and read it back with cgmquantify
  csv <- withr::local_tempfile(fileext = ".csv")
  write_dexcom_csv(converted$Time, converted$glucose, csv)
  from_file <- cgmquantify::readfile(csv)

  expect_equal(names(from_file), names(converted))
  expect_equal(nrow(from_file), nrow(converted))

  # readfile() parses the timestamps without a timezone, so its 'tzone' attribute
  # is unset (i.e. session-local), whereas aidR tags its wall-clock timestamps as
  # UTC. Under TZ=UTC the two denote the same instants and the same wall clock,
  # only the attribute differs -> compare both explicitly.
  expect_equal(as.numeric(from_file$Time), as.numeric(converted$Time))
  expect_equal(format(from_file$Time, "%Y-%m-%dT%H:%M:%S"),
               format(converted$Time, "%Y-%m-%dT%H:%M:%S"))

  # All remaining columns must match exactly, compared one by one so that a
  # failure names the offending column
  for (column in setdiff(names(converted), "Time")){
    expect_equal(from_file[[column]], converted[[column]], info = column)
  }
})

test_that("every cgmquantify metric agrees between the CSV and the conversion", {
  skip_if_not_installed("cgmquantify")
  local_utc()

  path <- system.file("extdata", "tidepool_example.xlsx", package = "aidR")
  data <- parse_data(id = "1", paths = path)
  converted <- get_cgmquantify_format(data)

  csv <- withr::local_tempfile(fileext = ".csv")
  write_dexcom_csv(converted$Time, converted$glucose, csv)
  from_file <- cgmquantify::readfile(csv)

  for (metric in cgmquantify_metrics()){
    fun <- getExportedValue("cgmquantify", metric)
    expect_equal(fun(converted), fun(from_file), info = metric)
  }
})

test_that("plot_glucose() draws the same data from the CSV and the conversion", {
  skip_if_not_installed("cgmquantify")
  skip_if_not_installed("ggplot2")
  local_utc()

  path <- system.file("extdata", "tidepool_example.xlsx", package = "aidR")
  data <- parse_data(id = "1", paths = path)
  converted <- get_cgmquantify_format(data)

  csv <- withr::local_tempfile(fileext = ".csv")
  write_dexcom_csv(converted$Time, converted$glucose, csv)
  from_file <- cgmquantify::readfile(csv)

  # cgmquantify builds the plot with aes(x = df$time_of_day, y = df$glucose),
  # which ggplot2 warns about - the warnings come from cgmquantify, not from aidR
  plot_converted <- suppressWarnings(ggplot2::layer_data(cgmquantify::plot_glucose(converted)))
  plot_from_file <- suppressWarnings(ggplot2::layer_data(cgmquantify::plot_glucose(from_file)))

  expect_equal(plot_converted, plot_from_file)
})

test_that("get_cgmquantify_format() requires an id as soon as several are present", {
  path_1 <- system.file("extdata", "tidepool_example.xlsx", package = "aidR")
  path_2 <- system.file("extdata", "carelink_example.csv", package = "aidR")
  data <- merge_all(list("1" = parse_data(id = "1", paths = path_1),
                         "2" = parse_data(id = "2", paths = path_2)))

  expect_error(get_cgmquantify_format(data), "more than one individual")
  expect_error(get_cgmquantify_format(data, id = "99"), "No CGM data found")
  expect_error(get_cgmquantify_format(data, id = c("1", "2")), "single 'id'")

  for (id in c("1", "2")){
    converted <- get_cgmquantify_format(data, id = id)
    expect_equal(nrow(converted), sum(as.character(data$cgm$id) == id))
  }
})

test_that("get_cgmquantify_format() rejects uncleaned data and non-mg/dL units", {
  path <- system.file("extdata", "carelink_example.csv", package = "aidR")

  expect_error(get_cgmquantify_format(parse_data(id = "1", paths = path, clean = FALSE)),
               "clean = TRUE")

  data <- parse_data(id = "1", paths = path)
  data$cgm$unit <- "mmol/L"
  expect_error(get_cgmquantify_format(data), "mg/dL")
})

test_that("get_cgmquantify_format() drops missing glucose values with a warning", {
  path <- system.file("extdata", "carelink_example.csv", package = "aidR")
  data <- parse_data(id = "1", paths = path)
  n_all <- nrow(data$cgm)

  data$cgm$value[c(1, 5, 10)] <- NA
  expect_warning(converted <- get_cgmquantify_format(data), "Dropped 3 CGM values")
  expect_equal(nrow(converted), n_all - 3)
  expect_false(anyNA(converted$glucose))
})

test_that("get_cgmquantify_format() returns the source CGM data, sorted by time", {
  # The round-trip tests above write the CSV from the conversion itself, so they
  # cannot catch an error in the content or the order of the conversion: both
  # sides would carry it. This test compares against the source data instead.
  path <- system.file("extdata", "tidepool_example.xlsx", package = "aidR")
  data <- parse_data(id = "1", paths = path)
  converted <- get_cgmquantify_format(data)

  cgm <- data$cgm[order(data$cgm$timestamp), , drop = FALSE]

  expect_equal(nrow(converted), nrow(cgm))
  expect_false(is.unsorted(converted$Time))
  expect_equal(as.numeric(converted$Time), as.numeric(cgm$timestamp))
  expect_equal(converted$glucose, as.numeric(cgm$value))

  # Date and time of day are the wall clock of the timestamp
  expect_equal(converted$Date, as.Date(format(cgm$timestamp, "%Y-%m-%d")))
  expect_equal(converted$time_of_day, hms::as_hms(format(cgm$timestamp, "%H:%M:%S")))

  # type_of_event marks the readings above 180 mg/dL and below 70 mg/dL
  expect_true(all(converted$type_of_event[converted$glucose > 180] == 1))
  expect_true(all(converted$type_of_event[converted$glucose < 70] == -1))
  expect_true(all(converted$type_of_event[converted$glucose >= 70 &
                                            converted$glucose <= 180] == 0))
})
