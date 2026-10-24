test_that("transformC assigns label attributes without labelled class", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE, RCPA4.pause_seconds = 0)
  on.exit(options(old), add = TRUE)

  centered <- quiet_run(transformC("center", unionized, data = states, confirm = FALSE))
  expect_type(centered, "double")
  expect_false(inherits(centered, "labelled"))
  expect_true(nzchar(attr(centered, "label", exact = TRUE)))

  dummy <- quiet_run(transformC("dummy", right_to_work, data = states, response = "Yes", confirm = FALSE))
  expect_type(dummy, "double")
  expect_false(inherits(dummy, "labelled"))
  expect_true(nzchar(attr(dummy, "label", exact = TRUE)))

  grouped <- quiet_run(transformC("cut", unionized, data = states, cutpoints = c(5, 10, 15), confirm = FALSE))
  expect_s3_class(grouped, "ordered")
  expect_false(inherits(grouped, "labelled"))
  expect_true(nzchar(attr(grouped, "label", exact = TRUE)))
})
