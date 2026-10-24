test_that("table FALSE suppresses variance tables but retains the test results", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE)
  on.exit(options(old), add = TRUE)
  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot.new()
  x1 <- c(1, 2, 3, 4, 5)
  x2 <- c(2, 4, 6, 8, 10)

  for (weights in list(NULL, c(1, 2, 1, 2, 1))) {
    args <- list(x1 = x1, x2 = x2, var.test = TRUE)
    if (!is.null(weights)) args$w <- weights
    expect_output(
      displayed <- do.call(testmeansC, c(args, list(plot = FALSE))),
      "Equal Variances Test"
    )
    expect_silent(
      hidden <- withVisible(do.call(testmeansC, c(args, list(table = FALSE, plot = TRUE))))
    )
    expect_false(hidden$visible)
    expect_s3_class(hidden$value$variance_test_table, "data.frame")
    expect_identical(hidden$value$variance_test_table, displayed$variance_test_table)
  }
})
