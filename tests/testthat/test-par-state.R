test_that("plotting functions restore key par settings", {
  old_par <- graphics::par(c("mar", "mfrow", "xpd", "ask"))
  
  quiet_run(freqC(states$region, plot = TRUE, printC = FALSE))
  
  new_par <- graphics::par(c("mar", "mfrow", "xpd", "ask"))
  expect_equal(new_par, old_par)
})
