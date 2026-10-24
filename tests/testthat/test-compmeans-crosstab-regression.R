test_that("compmeansC supports basic, controlled, weighted, and data-arg paths", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)
  
  tbl_basic <- quiet_run(
    compmeansC(dv = anes$ft_rep_party, iv = anes$partyid3, plot = FALSE, printC = FALSE)
  )
  tbl_weighted <- quiet_run(
    compmeansC(
      dv = anes$ft_rep_party, iv = anes$partyid3, w = anes$wt,
      plot = FALSE, printC = FALSE
    )
  )
  tbl_controlled <- quiet_run(
    compmeansC(
      dv = anes$ft_rep_party, iv = anes$partyid3, z = anes$gender_s_sex,
      plot = FALSE, printC = FALSE
    )
  )
  tbl_controlled_weighted <- quiet_run(
    compmeansC(
      dv = anes$ft_rep_party, iv = anes$partyid3, z = anes$gender_s_sex, w = anes$wt,
      plot = FALSE, printC = FALSE
    )
  )
  tbl_data_arg <- quiet_run(
    compmeansC(
      dv = ft_rep_party, iv = partyid3, z = gender_s_sex, w = wt, data = anes,
      plot = FALSE, printC = FALSE
    )
  )
  
  expect_true(is.matrix(tbl_basic))
  expect_true(is.matrix(tbl_weighted))
  expect_true(is.matrix(tbl_controlled))
  expect_true(is.matrix(tbl_controlled_weighted))
  expect_true(is.matrix(tbl_data_arg))
  expect_true(nrow(tbl_basic) >= 2)
  expect_true(ncol(tbl_controlled) >= ncol(tbl_basic))
})

test_that("compmeansC formats small weighted ANOVA P-values", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)

  dv <- c(rep(c(0, 1), 25), rep(c(10, 11), 25))
  iv <- factor(rep(c("A", "B"), each = 50))
  out <- quiet_run(
    compmeansC(dv = dv, iv = iv, w = rep(1, 100), anova = TRUE,
               plot = FALSE, printC = FALSE)
  )

  expect_equal(out[["anova_table"]][1, "Pr(>F)"], "<.01")
})

test_that("compmeansC compact mode and inferential options behave", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)
  
  tbl_full <- quiet_run(
    compmeansC(
      dv = anes$ft_rep_party, iv = anes$partyid3, z = anes$gender_s_sex,
      compact = FALSE, plot = FALSE, printC = FALSE
    )
  )
  tbl_compact <- quiet_run(
    compmeansC(
      dv = anes$ft_rep_party, iv = anes$partyid3, z = anes$gender_s_sex,
      compact = TRUE, plot = FALSE, printC = FALSE
    )
  )
  
  expect_true(is.matrix(tbl_full))
  expect_true(is.matrix(tbl_compact))
  expect_equal(dim(tbl_compact), dim(tbl_full))
  
  expect_silent(
    quiet_run(
      compmeansC(
        dv = anes$ft_rep_party, iv = anes$partyid3,
        anova = TRUE, plot = FALSE, printC = FALSE
      )
    )
  )
  expect_silent(
    quiet_run(
      compmeansC(
        dv = anes$ft_rep_party, iv = anes$partyid3, z = anes$gender_s_sex,
        anova = TRUE, plot = FALSE, printC = FALSE
      )
    )
  )
})

test_that("compmeansC supports all plot types and plot options", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)
  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file, width = 12, height = 8)
  on.exit({
    grDevices::dev.off()
    unlink(plot_file)
  }, add = TRUE)
  
  for (ptype in c(TRUE, "bar", "line", "points")) {
    expect_silent(
      quiet_run(
        compmeansC(
          dv = anes$ft_rep_party, iv = anes$partyid3,
          plot = ptype, printC = FALSE
        )
      )
    )
  }
  
  expect_silent(
    quiet_run(
      compmeansC(
        dv = anes$ft_rep_party, iv = anes$partyid3, z = anes$gender_s_sex,
        plot = "line", plot.ci = TRUE,
        main = "Controlled Means", xlab = "Group", ylab = "Turnout", ylim = c(0, 100),
        legend.title = "South", z.palette = "LightGrays", printC = FALSE
      )
    )
  )
  expect_silent(
    quiet_run(
      compmeansC(
        dv = anes$ft_rep_party, iv = anes$partyid3, z = anes$gender_s_sex,
        plot = "bar",
        main = "Controlled Means Bar", xlab = "Control", ylab = "Turnout",
        legend.title = "Region", z.palette = "LightGrays", printC = FALSE
      )
    )
  )
})

test_that("crosstabC supports basic, controlled, weighted, and data-arg paths", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)
  
  tbl_basic <- quiet_run(
    crosstabC(dv = anes$r_favor_opps_death, iv = anes$partyid3, plot = FALSE, printC = FALSE)
  )
  tbl_weighted <- quiet_run(
    crosstabC(
      dv = anes$r_favor_opps_death, iv = anes$partyid3, w = anes$wt,
      plot = FALSE, printC = FALSE
    )
  )
  tbl_controlled <- quiet_run(
    crosstabC(
      dv = anes$r_favor_opps_death, iv = anes$partyid3, z = anes$gender_s_sex,
      plot = FALSE, printC = FALSE
    )
  )
  tbl_controlled_weighted <- quiet_run(
    crosstabC(
      dv = anes$r_favor_opps_death, iv = anes$partyid3, z = anes$gender_s_sex, w = anes$wt,
      plot = FALSE, printC = FALSE
    )
  )
  tbl_data_arg <- quiet_run(
    crosstabC(
      dv = r_favor_opps_death, iv = partyid3, z = gender_s_sex, w = wt, data = anes,
      plot = FALSE, printC = FALSE
    )
  )
  
  expect_true(is.matrix(tbl_basic))
  expect_true(is.matrix(tbl_weighted))
  expect_true(is.matrix(tbl_controlled))
  expect_true(is.matrix(tbl_controlled_weighted))
  expect_true(is.matrix(tbl_data_arg))
  expect_true(nrow(tbl_basic) >= 2)
})

test_that("crosstabC compact mode and inferential options behave", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)
  
  tbl_full <- quiet_run(
    crosstabC(
      dv = anes$r_favor_opps_death, iv = anes$partyid3,
      compact = FALSE, plot = FALSE, printC = FALSE
    )
  )
  tbl_compact <- quiet_run(
    crosstabC(
      dv = anes$r_favor_opps_death, iv = anes$partyid3,
      compact = TRUE, plot = FALSE, printC = FALSE
    )
  )
  
  expect_true(is.matrix(tbl_full))
  expect_true(is.matrix(tbl_compact))
  expect_lt(nrow(tbl_compact), nrow(tbl_full))
  
  expect_silent(
    quiet_run(
      crosstabC(
        dv = anes$r_favor_opps_death, iv = anes$partyid3,
        chisq = TRUE, lambda = TRUE, somers = TRUE, cramers = TRUE,
        plot = FALSE, printC = FALSE
      )
    )
  )
  expect_silent(
    quiet_run(
      crosstabC(
        dv = anes$r_favor_opps_death, iv = anes$partyid3, z = anes$gender_s_sex,
        chisq = TRUE, lambda = TRUE, somers = TRUE, cramers = TRUE,
        plot = FALSE, printC = FALSE
      )
    )
  )
})

test_that("crosstabC supports all plot types and plot options", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)
  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file, width = 12, height = 8)
  on.exit({
    grDevices::dev.off()
    unlink(plot_file)
  }, add = TRUE)
  
  for (ptype in c(TRUE, "bar", "line")) {
    expect_silent(
      quiet_run(
        crosstabC(
          dv = anes$r_favor_opps_death, iv = anes$partyid3,
          plot = ptype, printC = FALSE
        )
      )
    )
  }
  
  expect_silent(
    quiet_run(
      crosstabC(
        dv = anes$r_favor_opps_death, iv = anes$partyid3, z = anes$gender_s_sex,
        plot = "bar",
        main = "Controlled Crosstab", xlab = "Religiosity", ylab = "Percent",
        legend.title = "Region", z.palette = "LightGrays", printC = FALSE
      )
    )
  )
  expect_silent(
    quiet_run(
      crosstabC(
        dv = anes$r_favor_opps_death, iv = anes$partyid3, z = anes$gender_s_sex,
        plot = "line",
        main = "Controlled Crosstab Line", xlab = "Region", ylab = "Percent",
        legend.title = "Religiosity", z.palette = "LightGrays", printC = FALSE
      )
    )
  )
  expect_silent(
    quiet_run(
      crosstabC(
        dv = anes$r_favor_opps_death, iv = anes$partyid3, z = anes$gender_s_sex,
        plot = "mosaic",
        main = "Controlled Mosaic", xlab = "Predictor", ylab = "Response",
        z.palette = "LightGrays", printC = FALSE
      )
    )
  )
})
