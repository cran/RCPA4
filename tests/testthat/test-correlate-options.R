test_that("correlateC restores scipen after reporting inferential statistics", {
  old <- options(scipen = 7, RCPA4.verbose = FALSE, RCPA4.slow = FALSE)
  on.exit(options(old), add = TRUE)
  variables <- list(c(1, 2, 4, 5, 7, 8), c(2, 1, 5, 3, 8, 6), c(3, 5, 2, 8, 4, 9))

  expect_silent(result <- quiet_run(correlateC(variables, stats = TRUE, plot = FALSE)))
  expect_identical(getOption("scipen"), 7L)
  expect_true(is.list(result))
  expect_true(all(c("correlation", "std.err", "t.value", "p.value") %in% names(result)))
})

test_that("correlateC restores scipen when table rendering fails", {
  old <- options(scipen = 7, RCPA4.verbose = FALSE, RCPA4.slow = FALSE)
  on.exit(options(old), add = TRUE)
  variables <- list(c(1, 2, 4, 5, 7, 8), c(2, 1, 5, 3, 8, 6), c(3, 5, 2, 8, 4, 9))
  original_render_table <- getFromNamespace("render_table", "RCPA4")
  testthat::local_mocked_bindings(
    render_table = function(blocks, x, caption = NULL, ...) {
      if (identical(caption, "Standard Errors of Correlation Coefficients")) {
        expect_identical(getOption("scipen"), 999L)
        stop("Simulated rendering failure")
      }
      original_render_table(blocks, x, caption = caption, ...)
    },
    .package = "RCPA4"
  )

  expect_error(quiet_run(correlateC(variables, stats = TRUE, plot = FALSE)),
               "Simulated rendering failure")
  expect_identical(getOption("scipen"), 7L)
})
