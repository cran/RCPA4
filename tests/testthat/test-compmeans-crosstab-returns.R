test_that("compmeansC returns matrix primary result with accessible aux tables", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)
  
  out <- quiet_run(
    compmeansC(
      dv = anes$ft_rep_party,
      iv = anes$partyid3,
      z = anes$gender_s_sex,
      anova = TRUE,
      plot = FALSE,
      printC = FALSE
    )
  )
  
  expect_s3_class(out, "compmeansC_result")
  expect_true(is.matrix(out))
  expect_true(is.list(attr(out, "rcpa4_tables", exact = TRUE)))
  
  expect_s3_class(out[["anova_table"]], "data.frame")
  expect_true(is.list(out[["controlled_panel_tables"]]))
  expect_null(out[["not_a_table_key"]])
})


test_that("compmeansC keeps stable keys with NULL for optional aux tables", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)
  
  out <- quiet_run(
    compmeansC(
      dv = anes$ft_rep_party,
      iv = anes$partyid3,
      plot = FALSE,
      printC = FALSE
    )
  )
  
  expect_s3_class(out, "compmeansC_result")
  expect_true(is.matrix(out))
  expect_null(out[["anova_table"]])
  expect_null(out[["controlled_panel_tables"]])
})

test_that("compmeansC uses numeric factor labels rather than factor codes", {
  dv <- factor(c("10", "20", "30", "40"))
  iv <- factor(c("A", "A", "B", "B"))

  out <- quiet_run(compmeansC(dv = dv, iv = iv, plot = FALSE, printC = FALSE))

  expect_equal(as.numeric(out[c("A", "B"), "Mean"]), c(15, 35))
})


test_that("crosstabC returns matrix primary result with accessible aux tables", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)
  
  out <- quiet_run(
    crosstabC(
      dv = anes$r_favor_opps_death,
      iv = anes$partyid3,
      z = anes$gender_s_sex,
      chisq = TRUE,
      somers = TRUE,
      plot = FALSE,
      printC = FALSE
    )
  )
  
  expect_s3_class(out, "crosstabC_result")
  expect_true(is.matrix(out))
  expect_true(is.list(attr(out, "rcpa4_tables", exact = TRUE)))
  
  expect_s3_class(out[["chisq_table"]], "data.frame")
  expect_s3_class(out[["association_table"]], "data.frame")
  expect_true(is.list(out[["controlled_panel_tables"]]))
  expect_null(out[["not_a_table_key"]])
})


test_that("crosstabC keeps stable keys with NULL for optional aux tables", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)
  
  out <- quiet_run(
    crosstabC(
      dv = anes$r_favor_opps_death,
      iv = anes$partyid3,
      plot = FALSE,
      printC = FALSE
    )
  )
  
  expect_s3_class(out, "crosstabC_result")
  expect_true(is.matrix(out))
  expect_null(out[["chisq_table"]])
  expect_null(out[["association_table"]])
  expect_null(out[["controlled_panel_tables"]])
})
