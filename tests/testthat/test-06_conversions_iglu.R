test_that("get_iglu_format() reproduces the data frame of iglu::read_raw_data()", {
  path <- system.file("extdata", "tidepool_example.xlsx", package = "aidR")
  data <- parse_data(id = "1", paths = path)
  converted <- get_iglu_format(data)

  # Write the same CGM data to a Dexcom-style CSV and read it back with iglu
  csv <- withr::local_tempfile(fileext = ".csv")
  write_dexcom_csv(converted$time, converted$gl, csv)
  from_file <- iglu::read_raw_data(csv, sensor = "dexcom", id = "1", tz = "UTC")

  expect_equal(names(from_file), names(converted))
  expect_equal(nrow(from_file), nrow(converted))

  # read_raw_data() carries the id through as the character string it was given,
  # while process_data() converts it to the type of the underlying column
  expect_equal(as.character(from_file$id), as.character(converted$id))
  expect_equal(from_file$time, converted$time)
  expect_equal(from_file$gl, converted$gl)
})

test_that("every iglu metric agrees between the CSV and the conversion", {
  path <- system.file("extdata", "tidepool_example.xlsx", package = "aidR")
  data <- parse_data(id = "1", paths = path)
  converted <- get_iglu_format(data)

  csv <- withr::local_tempfile(fileext = ".csv")
  write_dexcom_csv(converted$time, converted$gl, csv)
  from_file <- iglu::read_raw_data(csv, sensor = "dexcom", id = "1", tz = "UTC")

  # The id is the only column that differs in type, and several metrics report it
  from_file$id <- converted$id

  for (metric in iglu_metrics()){
    fun <- getExportedValue("iglu", metric)
    result_converted <- suppressWarnings(fun(converted))
    result_from_file <- suppressWarnings(fun(from_file))
    expect_equal(result_converted, result_from_file, info = metric)
  }
})

test_that("the iglu plotting functions draw the same data from the CSV and the conversion", {
  skip_if_not_installed("ggplot2")

  path <- system.file("extdata", "tidepool_example.xlsx", package = "aidR")
  data <- parse_data(id = "1", paths = path)
  converted <- get_iglu_format(data)

  csv <- withr::local_tempfile(fileext = ".csv")
  write_dexcom_csv(converted$time, converted$gl, csv)
  from_file <- iglu::read_raw_data(csv, sensor = "dexcom", id = "1", tz = "UTC")
  from_file$id <- converted$id

  for (plot_fun in iglu_plots()){
    fun <- getExportedValue("iglu", plot_fun)
    layers_converted <- suppressWarnings(ggplot2::layer_data(fun(converted)))
    layers_from_file <- suppressWarnings(ggplot2::layer_data(fun(from_file)))
    expect_equal(layers_converted, layers_from_file, info = plot_fun)
  }
})

test_that("get_iglu_format() returns the source CGM data of every individual", {
  # The round-trip tests above write the CSV from the conversion itself, so they
  # cannot catch an error in the content or the order of the conversion: both
  # sides would carry it. This test compares against the source data instead.
  path_1 <- system.file("extdata", "tidepool_example.xlsx", package = "aidR")
  path_2 <- system.file("extdata", "carelink_example.csv", package = "aidR")
  data <- merge_all(list("1" = parse_data(id = "1", paths = path_1),
                         "2" = parse_data(id = "2", paths = path_2)))
  converted <- get_iglu_format(data)

  expect_equal(names(converted), c("id", "time", "gl"))
  expect_equal(nrow(converted), nrow(data$cgm))

  # Unlike cgmquantify, iglu handles several individuals itself
  expect_equal(sort(unique(as.character(converted$id))), c("1", "2"))

  for (id in c("1", "2")){
    cgm <- data$cgm[as.character(data$cgm$id) == id, , drop = FALSE]
    cgm <- cgm[order(cgm$timestamp), , drop = FALSE]
    subset <- converted[as.character(converted$id) == id, , drop = FALSE]

    expect_equal(nrow(subset), nrow(cgm), info = id)
    expect_false(is.unsorted(subset$time))
    expect_equal(as.numeric(subset$time), as.numeric(cgm$timestamp), info = id)
    expect_equal(subset$gl, as.numeric(cgm$value), info = id)
  }
})

test_that("get_iglu_format() rejects uncleaned data", {
  path <- system.file("extdata", "carelink_example.csv", package = "aidR")

  expect_error(get_iglu_format(parse_data(id = "1", paths = path, clean = FALSE)),
               "clean = TRUE")
})
