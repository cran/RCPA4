test_that("invalid plot messages preserve lists and can be suppressed", {
  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot.new()
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE)
  on.exit(options(old), add = TRUE)
  dv <- factor(c("Yes", "No", "Yes", "No"))
  iv <- factor(c("A", "A", "B", "B"))

  expect_message(
    crosstabC(dv, iv, plot = "invalid", table = FALSE),
    "Here are your options:\n  - TRUE\n  - line\n  - bar\n  - mosaic\n"
  )
  expect_output(
    suppressMessages(crosstabC(dv, iv, plot = "invalid", table = FALSE)),
    NA
  )
  expect_message(
    compmeansC(seq_along(iv), iv, plot = "invalid", table = FALSE),
    "Here are your options:\n  - TRUE\n  - line\n  - bar\n  - points\n"
  )
})

test_that("invalid responses include their choices in the error condition", {
  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot.new()
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE)
  on.exit(options(old), add = TRUE)
  dv <- factor(c("Yes", "No", "Yes", "No"), levels = c("Yes", "No"))
  iv <- factor(c("A", "A", "B", "B"))

  expect_output(
    expect_error(
      crosstabC(dv, iv, plot.response = "invalid"),
      "Here are your options:\n  - Yes\n  - No\nPlease correct"
    ),
    NA
  )
  expect_error(
    testpropsC(x1 = dv, x2 = 0.5, response = "invalid", plot = FALSE),
    "Here are your options:\n  - Yes\n  - No\nPlease correct"
  )
  expect_error(
    transformC("dummy", dv, confirm = FALSE),
    "Here are your options:\n  - Yes\n  - No\nPlease complete"
  )
  expect_error(
    transformC("dummy", dv, response = "invalid", confirm = FALSE),
    'These are the values of dv:\n  - "Yes"\n  - "No"\nPlease correct'
  )
})

test_that("small expected chi-square counts produce a suppressible warning", {
  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot.new()
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE)
  on.exit(options(old), add = TRUE)
  dv <- factor(c("Yes", "No", "Yes", "No"))
  iv <- factor(c("A", "A", "B", "B"))

  expect_warning(
    utils::capture.output(crosstabC(dv, iv, chisq = TRUE, plot = FALSE)),
    "At least one expected frequency is less than 5"
  )
  expect_silent(suppressWarnings(
    utils::capture.output(crosstabC(dv, iv, chisq = TRUE, plot = FALSE))
  ))
})

test_that("import status and no-mode information are suppressible messages", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE)
  on.exit(options(old), add = TRUE)
  file <- tempfile(fileext = ".csv")
  on.exit(unlink(file), add = TRUE)
  source_data <- data.frame(value = 1:3)
  utils::write.csv(source_data, file, row.names = FALSE)

  expect_message(getC(file, confirm = FALSE), "File selected: .*\nFile type:     csv")
  expect_silent(imported <- suppressMessages(getC(file, confirm = FALSE)))
  expect_equal(imported, source_data)
  expect_message(wtd_mode(1:3), "There is no modal value")
  expect_silent(mode <- suppressMessages(wtd_mode(1:3)))
  expect_identical(mode, "None")
})
