test_that("describeC returns the same invisible results without table output", {
  old <- options(RCPA4.verbose = TRUE, RCPA4.slow = FALSE)
  on.exit(options(old), add = TRUE)
  x <- c(1, 2, 4, 8)

  expect_output(displayed <- withVisible(describeC(x)), "Descriptive Statistics")
  expect_silent(hidden <- withVisible(describeC(x, table = FALSE)))
  expect_identical(hidden$value, displayed$value)
  expect_false(hidden$visible)
  expect_false(displayed$visible)

  expect_output(
    positional <- describeC(x, NULL, NULL, 2, FALSE),
    "Descriptive Statistics"
  )
  expect_identical(positional, suppressMessages(describeC(x, digits = 2, table = FALSE)))
})

test_that("sortC returns the same invisible results without table output", {
  old <- options(RCPA4.verbose = TRUE, RCPA4.slow = FALSE)
  on.exit(options(old), add = TRUE)
  id <- c("A", "B", "C", "D")
  by <- c(1, 4, 4, 2)
  thenby <- c(1, 2, 3, 4)

  expect_output(
    displayed <- withVisible(sortC(id, by, thenby = thenby, limit = 3)),
    "Sorted Case-Level Information"
  )
  expect_silent(hidden <- withVisible(sortC(id, by, thenby = thenby, limit = 3, table = FALSE)))
  expect_identical(hidden$value, displayed$value)
  expect_identical(as.character(hidden$value$id), c("C", "B", "D"))
  expect_false(hidden$visible)
  expect_false(displayed$visible)

  expect_output(
    positional <- sortC(id, by, NULL, NULL, FALSE, 2, 3, FALSE, FALSE),
    "Sorted Case-Level Information"
  )
  expect_identical(positional, sortC(id, by, descending = FALSE, limit = 2, table = FALSE))
})

test_that("table FALSE skips long-table prompts and HTML export", {
  old <- options(RCPA4.verbose = TRUE, RCPA4.slow = TRUE)
  on.exit(options(old), add = TRUE)
  testthat::local_mocked_bindings(
    ask = function(...) stop("Unexpected confirmation prompt"),
    print_blocksC = function(...) stop("Unexpected HTML export"),
    slightpause = function(...) stop("Unexpected display pause"),
    .package = "RCPA4"
  )
  id <- seq_len(25)
  by <- rev(id)
  expect_silent(sorted <- sortC(id, by, table = FALSE, printC = TRUE))
  expect_equal(nrow(sorted), 25L)
  expect_identical(sorted$id, id)
  expect_silent(described <- describeC(by, table = FALSE, printC = TRUE))
  expect_s3_class(described, "describeC_table")
})

test_that("class guidance remains a suppressible message with table FALSE", {
  x <- c("10", "20", "10")
  expect_message(describeC(x, table = FALSE), "numeric-looking factor levels")
  expect_silent(result <- suppressMessages(describeC(x, table = FALSE)))
  expect_s3_class(result, "describeC_table")
})
