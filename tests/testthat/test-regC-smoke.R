test_that("regC basic unweighted model runs and returns lm object", {
  fit <- quiet_run(
    regC(states$vep20_turnout ~ states$hs_or_more, plot.res = FALSE, printC = FALSE)
  )
  
  expect_s3_class(fit, "lm")
  expect_true(nrow(summary(fit)$coefficients) >= 2)
})

test_that("regC VIF diagnostics do not print when table is FALSE", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE)
  on.exit(options(old), add = TRUE)
  grDevices::pdf(file = NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_silent(fit <- regC(vep20_turnout ~ hs_or_more + adv_or_more,
                            data = states, vif = TRUE, table = FALSE,
                            plot.res = TRUE, printC = FALSE))
  expect_s3_class(fit, "lm")

  expect_silent(single <- suppressMessages(
    regC(vep20_turnout ~ hs_or_more, data = states, vif = TRUE,
         table = FALSE, plot.res = TRUE, printC = FALSE)
  ))
  expect_s3_class(single, "lm")
})
