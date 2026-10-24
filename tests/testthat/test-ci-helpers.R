test_that("CImean handles confidence level input and returns CI table", {
  ci_tbl <- quiet_run(CImean(states$vep20_turnout, conf = 0.95, plot = FALSE, printC = FALSE))
  
  expect_s3_class(ci_tbl, "data.frame")
  expect_named(ci_tbl, c("Lower Bound", "Point Estimate", "Upper Bound"))
  expect_equal(nrow(ci_tbl), 1)
  
  expect_error(
    quiet_run(CImean(states$vep20_turnout, conf = 120, plot = FALSE, printC = FALSE)),
    "should not exceed 100"
  )
})

test_that("CIprop handles confidence level input and returns CI table", {
  ci_tbl <- quiet_run(CIprop(states$south, conf = 0.95, plot = FALSE, printC = FALSE))
  
  expect_s3_class(ci_tbl, "data.frame")
  expect_named(ci_tbl, c("Lower Bound", "Point Estimate", "Upper Bound"))
  expect_true(nrow(ci_tbl) >= 1)
  
  expect_error(
    quiet_run(CIprop(states$south, conf = 40, plot = FALSE, printC = FALSE)),
    "confidence level should be percentage"
  )
})

test_that("CImean uses numeric factor labels rather than factor codes", {
  x <- factor(c("10", "20", "30"))

  ci_tbl <- quiet_run(CImean(x, conf = 95, plot = FALSE, printC = FALSE))

  expect_equal(ci_tbl[["Point Estimate"]], 20)
})
