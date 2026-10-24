side_legend_data <- function() {
  data.frame(y = seq_len(32),
             dv = factor(rep(c("Yes", "No"), 16)),
             iv = factor(rep(c("A", "B"), each = 16)),
             z = factor(rep(c("Low", "High"), each = 8, times = 2)))
}

test_that("side legends preserve unequal widths and spanning layout panels", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE)
  on.exit(options(old), add = TRUE)
  grDevices::pdf(file = NULL, width = 14, height = 9)
  on.exit(grDevices::dev.off(), add = TRUE)
  d <- side_legend_data()
  calls <- list(
    function() compmeansC(y, iv, z = z, data = d, table = FALSE, plot = "line"),
    function() compmeansC(y, iv, z = z, data = d, table = FALSE, plot = "bar"),
    function() crosstabC(dv, iv, z = z, data = d, table = FALSE,
                         plot = "bar", plot.response = "Yes"),
    function() crosstabC(dv, iv, data = d, table = FALSE,
                         plot = "line", plot.response = "all")
  )
  for (draw in calls) {
    graphics::layout(matrix(c(1, 2), nrow = 1), widths = c(2, 1))
    before <- graphics::par(c("mar", "xpd", "cex", "mex", "mfrow"))
    expect_silent(draw())
    expect_equal(graphics::par("fig"), c(0, 2/3, 0, 1))
    expect_equal(graphics::par(c("mar", "xpd", "cex", "mex", "mfrow")), before)
    graphics::plot.new()
    expect_equal(graphics::par("fig"), c(2/3, 1, 0, 1))

    graphics::layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE),
                     widths = c(2, 1), heights = c(2, 1))
    expect_silent(draw())
    expect_equal(graphics::par("fig"), c(0, 1, 1/3, 1))
    graphics::plot.new()
    expect_equal(graphics::par("fig"), c(0, 2/3, 0, 1/3))
    graphics::plot.new()
    expect_equal(graphics::par("fig"), c(2/3, 1, 0, 1/3))
  }
})

test_that("side-legend errors restore margins without replacing the layout", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE)
  on.exit(options(old), add = TRUE)
  grDevices::pdf(file = NULL, width = 12, height = 8)
  on.exit(grDevices::dev.off(), add = TRUE)
  testthat::local_mocked_bindings(
    draw_margin_legend = function(...) stop("Simulated legend failure"),
    .package = "RCPA4"
  )
  d <- side_legend_data()
  for (draw in list(
    function() compmeansC(y, iv, z = z, data = d, table = FALSE, plot = "line"),
    function() crosstabC(dv, iv, z = z, data = d, table = FALSE,
                         plot = "bar", plot.response = "Yes")
  )) {
    graphics::layout(matrix(c(1, 2), nrow = 1), widths = c(2, 1))
    before <- graphics::par(c("mar", "xpd"))
    expect_error(draw(), "Simulated legend failure")
    expect_equal(graphics::par(c("mar", "xpd")), before)
    graphics::plot.new()
    expect_equal(graphics::par("fig"), c(2/3, 1, 0, 1))
  }
})
