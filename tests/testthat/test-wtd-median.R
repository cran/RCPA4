test_that("wtd_median uses numeric factor labels rather than factor codes", {
  x <- factor(c("10", "20"))

  expect_equal(wtd_median(x), 15)
})

test_that("wtd_median returns an observed ordered-factor category", {
  x <- ordered(
    c("Low", "High", "High"),
    levels = c("Low", "Medium", "High")
  )

  expect_equal(wtd_median(x), "High")
})

test_that("wtd_median rejects unordered nonnumeric factors", {
  x <- factor(c("Low", "High"))

  expect_error(wtd_median(x), "must be numeric, logical, or an ordered factor")
})
