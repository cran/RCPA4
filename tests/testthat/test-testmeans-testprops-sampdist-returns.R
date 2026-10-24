test_that("testmeansC returns stable-key list with optional variance table", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)
  x_a <- c(1, 2, 3, 4, 5)
  x_b <- c(2, 3, 4, 5, 6)
  
  out <- quiet_run(
    testmeansC(
      x1 = x_a,
      x2 = x_b,
      var.test = TRUE,
      plot = FALSE,
      printC = FALSE
    )
  )
  
  expect_s3_class(out, "testmeansC_result")
  expect_true(is.list(out))
  expect_identical(
    sort(names(out)),
    sort(c("descriptive_table", "test_stats_table", "ci_table", "variance_test_table"))
  )
  expect_s3_class(out[["descriptive_table"]], "data.frame")
  expect_s3_class(out[["test_stats_table"]], "data.frame")
  expect_s3_class(out[["ci_table"]], "data.frame")
  expect_s3_class(out[["variance_test_table"]], "data.frame")
  expect_true(is.list(attr(out, "rcpa4_tables", exact = TRUE)))
})


test_that("testmeansC keeps stable keys with NULL optional table when disabled", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)
  x_a <- c(1, 2, 3, 4, 5)
  x_b <- c(2, 3, 4, 5, 6)
  
  out <- quiet_run(
    testmeansC(
      x1 = x_a,
      x2 = x_b,
      plot = FALSE,
      printC = FALSE
    )
  )
  
  expect_s3_class(out, "testmeansC_result")
  expect_true(is.list(out))
  expect_null(out[["variance_test_table"]])
})

test_that("testmeansC uses numeric factor labels rather than factor codes", {
  dv <- factor(c("10", "20", "30", "40"))
  iv <- factor(c("A", "A", "B", "B"))

  out <- quiet_run(testmeansC(dv = dv, iv = iv, plot = FALSE, printC = FALSE))

  expect_equal(as.numeric(out$descriptive_table["Mean", ]), c(15, 35))
})

test_that("testmeansC uses numeric factor labels in the x1/x2 interface", {
  x1 <- factor(c("10", "20", "30"))
  x2 <- factor(c("20", "30", "40"))

  out <- quiet_run(testmeansC(x1 = x1, x2 = x2, plot = FALSE, printC = FALSE))

  expect_equal(as.numeric(out$descriptive_table["Mean", ]), c(20, 30))
})


test_that("testpropsC returns stable-key list", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)
  
  x1 <- factor(c("Yes", "No", "Yes", "Yes", "No", "Yes"))
  x2 <- factor(c("Yes", "Yes", "No", "Yes", "No", "No"))
  
  out <- quiet_run(
    testpropsC(
      x1 = x1,
      x2 = x2,
      response = "Yes",
      plot = FALSE,
      printC = FALSE
    )
  )
  
  expect_s3_class(out, "testpropsC_result")
  expect_true(is.list(out))
  expect_identical(
    sort(names(out)),
    sort(c("descriptive_table", "z_test_table", "ci_table"))
  )
  expect_s3_class(out[["descriptive_table"]], "data.frame")
  expect_s3_class(out[["z_test_table"]], "data.frame")
  expect_s3_class(out[["ci_table"]], "data.frame")
  expect_true(is.list(attr(out, "rcpa4_tables", exact = TRUE)))
})


test_that("sampdistC returns CI-shaped data frame and supports ci_table key access", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)
  
  out <- quiet_run(
    sampdistC(
      stat = 10,
      se = 1,
      ci = 95,
      table = FALSE,
      printC = FALSE
    )
  )
  
  expect_s3_class(out, "sampdistC_result")
  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("Lower Bound", "Point Estimate", "Upper Bound"))
  expect_s3_class(out[["ci_table"]], "data.frame")
  expect_true(is.list(attr(out, "rcpa4_tables", exact = TRUE)))
})


test_that("sampdistC keeps stable ci_table key with NULL when ci disabled", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)
  
  out <- quiet_run(
    sampdistC(
      stat = 10,
      se = 1,
      ci = FALSE,
      printC = FALSE
    )
  )
  
  expect_s3_class(out, "sampdistC_result")
  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("Lower Bound", "Point Estimate", "Upper Bound"))
  expect_equal(nrow(out), 0)
  expect_null(out[["ci_table"]])
  expect_true(is.list(attr(out, "rcpa4_tables", exact = TRUE)))
})
