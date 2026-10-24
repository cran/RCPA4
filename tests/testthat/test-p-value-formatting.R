test_that("format_p_value respects requested precision with a two-digit minimum", {
  expect_equal(
    format_p_value(c(0, 0.004, 0.01, 0.016, NA_real_), digits = 2),
    c("<.01", "<.01", ".01", ".02", "")
  )
  expect_equal(
    format_p_value(c(0.004, 0.0004), digits = 3),
    c(".004", "<.001")
  )
  expect_equal(
    format_p_value(c(0.004, 0.016), digits = 0),
    c("<.01", ".02")
  )
})

test_that("pchisqC result printing does not quote formatted values", {
  result <- structure(
    c("12.345", "2", "<.001"),
    names = c("Chi-Squared", "    DF", "    P-Value"),
    class = "pchisqC_result"
  )
  output <- capture.output(print(result))

  expect_true(any(grepl("<.001", output, fixed = TRUE)))
  expect_false(any(grepl('"', output, fixed = TRUE)))
})
