test_that("ANES partyid3 is retained and correctly recoded", {
  expect_true("partyid3" %in% names(anes))
  expect_s3_class(anes$partyid3, "ordered")
  expect_identical(
    levels(anes$partyid3),
    c("1. Democrat", "2. Independent", "3. Republican")
  )

  expected <- rep(NA_integer_, nrow(anes))
  expected[as.integer(anes$party_id) %in% 1:2] <- 1L
  expected[as.integer(anes$party_id) %in% 3:5] <- 2L
  expected[as.integer(anes$party_id) %in% 6:7] <- 3L

  expect_identical(as.integer(anes$partyid3), expected)
  expect_identical(
    as.vector(table(anes$partyid3)),
    c(1930L, 1810L, 1743L)
  )
  expect_equal(sum(is.na(anes$partyid3)), 38)
})
