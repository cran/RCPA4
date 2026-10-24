test_that("boxplotC supports grouped and single-variable plots", {
  expect_no_error(
    quiet_run(boxplotC(dv = states$vep20_turnout, iv = states$region, printC = FALSE))
  )
  
  expect_no_error(
    quiet_run(boxplotC(dv = states$vep20_turnout, printC = FALSE))
  )
  
  expect_no_error(
    quiet_run(boxplotC(dv = states$vep20_turnout, iv = states$region, w = states$population, printC = FALSE))
  )
  
  expect_no_error(
    quiet_run(boxplotC(dv = states$vep20_turnout, iv = states$region, box.col = "gray40", boxcol = "black", printC = FALSE))
  )
  
  out <- quiet_run(boxplotC(dv = states$vep20_turnout, iv = states$region, printC = FALSE))
  expect_s3_class(out, "boxplot")
  expect_true(is.list(out))
})

test_that("boxplotC restores key par settings", {
  old_par <- graphics::par(c("mar", "mfrow", "xpd", "ask"))
  
  quiet_run(boxplotC(dv = states$vep20_turnout, iv = states$region, printC = FALSE))
  quiet_run(boxplotC(dv = states$vep20_turnout, printC = FALSE))
  
  new_par <- graphics::par(c("mar", "mfrow", "xpd", "ask"))
  expect_equal(new_par, old_par)
})

test_that("boxplotC uses numeric factor labels rather than factor codes", {
  dv <- factor(c("10", "20", "30", "40"))
  iv <- factor(c("A", "A", "B", "B"))

  out <- quiet_run(boxplotC(dv = dv, iv = iv, printC = FALSE))

  expect_equal(out$stats[3, ], c(15, 35))
})
