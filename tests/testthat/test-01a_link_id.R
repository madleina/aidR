test_that(".create_link_id.aidR() builds unique keys from the event content", {
  ts <- as.POSIXct(c("2026-07-01 08:00:00", "2026-07-01 08:00:00", "2026-07-01 12:00:00"), tz = "UTC")
  key <- .create_link_id.aidR("1", "glooko", ts, c(40, 40, 0), linked = c(TRUE, TRUE, FALSE))

  # Identical events are numbered; unlinked events are NA
  expect_equal(key, c("1|glooko|2026-07-01 08:00:00|40|1", "1|glooko|2026-07-01 08:00:00|40|2", NA))
  expect_false(anyDuplicated(na.omit(key)) > 0)

  # Same content gives the same key, independently of other rows
  expect_equal(.create_link_id.aidR("1", "glooko", ts[1], 40), key[1])

  expect_equal(.create_link_id.aidR("1", "glooko", character(0)), character(0))
  expect_error(.create_link_id.aidR("1", "glooko", 1:2, 1:3))
})

test_that("parse_data() links boluses and carbs where the export allows it", {
  for (file in c("glooko_example.zip", "tandem_source_example.zip", "tidepool_example.xlsx")){
    data <- suppressMessages(parse_data("1", system.file("extdata", file, package = "aidR")))
    linked <- data$carbs[!is.na(data$carbs$link_id), ]
    expect_gt(nrow(linked), 0)
    expect_false(anyDuplicated(linked$link_id) > 0)
    # Every linked carb entry has its bolus, at the same time
    ix <- match(linked$link_id, data$bolus$link_id)
    expect_false(anyNA(ix))
    expect_equal(data$bolus$timestamp[ix], linked$timestamp)
  }

  for (file in c("carelink_example.csv", "mylife_example.csv", "yourloops_example.zip")){
    data <- suppressMessages(parse_data("1", system.file("extdata", file, package = "aidR")))
    expect_true(all(is.na(data$bolus$link_id)))
    expect_true(all(is.na(data$carbs$link_id)))
    expect_type(data$carbs$link_id, "character")
  }
})

test_that("Tidepool links are built from the bolus content, not from the Tidepool Id", {
  bolus <- data.frame(`Local Time` = c("2026-07-01 08:00:00", "2026-07-01 12:00:00", "2026-07-01 18:00:00"),
                      Normal = c(4, 2, 3), Extended = NA, `Duration (mins)` = NA,
                      Id = c("a", "b", "c"), check.names = FALSE)
  calc <- data.frame(Bolus = c("a", "a", "c", "x"), `Carb Input` = c(30, 10, 0, 20), check.names = FALSE)
  link <- .link_bolus_carbs_tidepool.aidR("1", bolus, calc)

  expect_equal(link$bolus, c("1|tidepool|2026-07-01 08:00:00|4|NA|NA|1", NA, NA))
  # Both carb entries share the key of bolus "a"; no carbs or unknown bolus: NA
  expect_equal(link$bolus_calculator, c(link$bolus[1], link$bolus[1], NA, NA))
})

test_that("parse_data() keeps the links when the same export is parsed twice", {
  path <- system.file("extdata", "glooko_example.zip", package = "aidR")
  once  <- suppressMessages(parse_data("1", path))
  twice <- suppressMessages(parse_data("1", c(path, path)))
  expect_equal(twice$carbs$link_id, once$carbs$link_id)
  expect_equal(twice$bolus$link_id, once$bolus$link_id)
})
