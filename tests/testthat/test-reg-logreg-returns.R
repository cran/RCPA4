test_that("regC returns augmented model with stable auxiliary table access", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)
  
  fit <- quiet_run(
    regC(
      states$vep20_turnout ~ states$hs_or_more,
      anova = TRUE,
      plot.res = FALSE,
      printC = FALSE
    )
  )
  
  expect_s3_class(fit, "regC_result")
  expect_s3_class(fit, "lm")
  expect_true(is.list(attr(fit, "rcpa4_tables", exact = TRUE)))
  
  expect_true(is.numeric(fit[["coefficients"]]))
  expect_s3_class(fit[["model_fit_table"]], "data.frame")
  expect_s3_class(fit[["anova_table"]], "data.frame")
  expect_null(fit[["norm_test_table"]])
})


test_that("logregC returns augmented model with stable auxiliary table access", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)
  
  fit <- quiet_run(
    logregC(
      world$icc_treaty ~ world$eiu_democ_score,
      fit.stats = TRUE,
      classify = TRUE,
      plot.res = FALSE,
      printC = FALSE
    )
  )
  
  expect_s3_class(fit, "logregC_result")
  expect_s3_class(fit, "glm")
  expect_true(is.list(attr(fit, "rcpa4_tables", exact = TRUE)))
  
  expect_true(is.numeric(fit[["coefficients"]]))
  expect_true(is.matrix(fit[["deviance_residuals_table"]]))
  expect_s3_class(fit[["model_fit_table"]], "data.frame")
  expect_true(is.matrix(fit[["classification_table"]]))
  expect_null(fit[["anova_table"]])
})

test_that("regression coefficient tables retain conventional P-value formatting", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)

  x <- seq(-2, 2, length.out = 100)
  y <- 3 + 10 * x + rep(c(-0.5, 0.5), 50)
  ols <- quiet_run(regC(y ~ x, plot.res = FALSE, printC = FALSE))
  expect_match(ols[["coef_table"]]["x", "Pr(>|t|)"], "^<0\\.001$")
  expect_match(
    ols[["model_fit_table"]]$Value[ols[["model_fit_table"]]$Statistic == "Model P-value"],
    "^<\\.001$"
  )

  logit_data <- data.frame(
    x = rep(seq(-2, 2, length.out = 20), each = 5)
  )
  logit_data$y <- as.integer(logit_data$x + rep(c(-1.2, -0.4, 0, 0.4, 1.2), 20) > 0)
  logit <- quiet_run(logregC(y ~ x, data = logit_data, fit.stats = TRUE,
                             plot.res = FALSE, printC = FALSE))
  expect_match(logit[["coef_table"]]["x", "Pr(>|z|)"], "^<0\\.001$")
  expect_match(
    logit[["model_fit_table"]]$Value[logit[["model_fit_table"]]$Statistic == "p-Value of Chi2"],
    "^<\\.001$"
  )
})
