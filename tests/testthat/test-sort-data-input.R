test_that("sortC resolves bare character columns in package datasets", {
  expect_silent(direct <- sortC(states$state, states$volunteer_hrs_pc,
                               limit = 10, confirm = FALSE, table = FALSE))
  expect_silent(masked <- sortC(state, volunteer_hrs_pc, data = states,
                               limit = 10, confirm = FALSE, table = FALSE))
  expect_identical(masked, direct)

  direct <- sortC(world$country, world$vdem_4cat, thenby = world$gini_index,
                  descending = c(FALSE, TRUE), confirm = FALSE, table = FALSE)
  masked <- sortC(country, vdem_4cat, thenby = gini_index, data = world,
                  descending = c(FALSE, TRUE), confirm = FALSE, table = FALSE)
  expect_identical(masked, direct)
})

test_that("bare columns are values even when they resemble selectors", {
  d <- data.frame(id = c("by", "id"), by = c(1, 2), tie = c(2, 1))
  for (mask in list(d, as.list(d), list2env(d))) {
    result <- sortC(id, by, thenby = tie, data = mask, table = FALSE)
    expect_identical(as.character(result$id), c("id", "by"))
    expect_identical(result$by, c(2, 1))
    expect_identical(result$tie, c(1, 2))
  }
})

test_that("explicit selectors and caller-held selectors still work", {
  d <- data.frame(id = c("A", "B"), by = c(1, 2))
  selector <- "id"
  index <- 2L
  expect_identical(eval_varlist_expr(quote("id"), data = d)$names, "id")
  expect_identical(eval_varlist_expr(quote(selector), data = d)$names, "id")
  expect_identical(eval_varlist_expr(quote(2L), data = d)$names, "by")
  expect_identical(eval_varlist_expr(quote(index), data = d)$names, "by")
  expect_error(eval_varlist_expr(quote("missing"), data = d),
               "Unknown variable")
})
