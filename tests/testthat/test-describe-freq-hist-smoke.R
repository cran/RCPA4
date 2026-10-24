test_that("describeC returns a descriptive statistics table", {
  tbl <- quiet_run(describeC(states$vep20_turnout, digits = 2, printC = FALSE))
  
  expect_s3_class(tbl, "data.frame")
  expect_true(ncol(tbl) >= 1)
  expect_true("Observed values" %in% rownames(tbl))
  expect_true(is.numeric(tbl["Range", 1]))
  expect_true(is.character(tbl["Class", 1]))
})

test_that("describeC does not convert numeric-looking factors to numeric", {
  character_scores <- c("10", "20", "10")
  factor_scores <- factor(character_scores)

  expect_message(
    character_table <- describeC(character_scores, printC = FALSE),
    "numeric-looking factor levels"
  )
  expect_false("Mean" %in% rownames(character_table))
  expect_equal(character_table["Class", 1], "factor")

  expect_message(
    factor_table <- describeC(factor_scores, printC = FALSE),
    "numeric-looking factor levels"
  )
  expect_false("Mean" %in% rownames(factor_table))
  expect_equal(factor_table["Class", 1], "factor")
})

test_that("freqC returns frequency table for categorical variable", {
  tbl <- quiet_run(freqC(states$region, plot = FALSE, printC = FALSE))
  
  expect_s3_class(tbl, "data.frame")
  expect_true("Frequency" %in% colnames(tbl))
  expect_true(nrow(tbl) >= 2)
})

test_that("freqC digits argument affects display, not returned numeric values", {
  x <- factor(c("A", "A", "B", "B", "B"))
  w <- c(1.2, 0.8, 1.1, 1.1, 0.9)
  
  tbl1 <- quiet_run(freqC(x, w = w, digits = 1, plot = FALSE, printC = FALSE))
  tbl4 <- quiet_run(freqC(x, w = w, digits = 4, plot = FALSE, printC = FALSE))
  
  expect_s3_class(tbl1, "data.frame")
  expect_true(is.numeric(tbl1$Frequency))
  expect_true(is.numeric(tbl1$Percent))
  expect_true(is.numeric(tbl4$Frequency))
  expect_true(is.numeric(tbl4$Percent))
  expect_equal(tbl1$Frequency, tbl4$Frequency)
  expect_equal(tbl1$Percent, tbl4$Percent)
})

test_that("freqC accepts digits=0 without mutating returned numeric values", {
  x <- factor(c("A", "A", "B", "B", "B"))
  w <- c(1.2, 0.8, 1.1, 1.1, 0.9)
  
  expect_no_error(
    tbl0 <- quiet_run(freqC(x, w = w, digits = 0, plot = FALSE, printC = FALSE))
  )
  expect_true(is.numeric(tbl0$Frequency))
  expect_true(is.numeric(tbl0$Percent))
})

test_that("histC returns binned frequency table for numeric variable", {
  tbl <- quiet_run(histC(states$vep20_turnout, plot = FALSE, printC = FALSE))
  
  expect_s3_class(tbl, "data.frame")
  expect_equal(colnames(tbl), c("Interval", "Frequency", "Percent", "Cumulative"))
  expect_true(nrow(tbl) >= 2)
})

test_that("freqC uses Cumulative column name for ordered variables", {
  x <- factor(c("Low", "Low", "Med", "High"), levels = c("Low", "Med", "High"), ordered = TRUE)
  tbl <- quiet_run(freqC(x, plot = FALSE, printC = FALSE))
  
  expect_s3_class(tbl, "data.frame")
  expect_true("Cumulative" %in% colnames(tbl))
})

test_that("freqC includes cumulative percents for numeric variables with few categories", {
  x <- c(1, 1, 2, 2, 3, 3, 3)
  tbl <- quiet_run(freqC(x, plot = FALSE, printC = FALSE))
  
  expect_s3_class(tbl, "data.frame")
  expect_true("Cumulative" %in% colnames(tbl))
})

test_that("freqC includes cumulative percents for numeric variables with many categories", {
  x <- c(seq_len(30), 30, 29, 28)
  tbl <- quiet_run(freqC(x, plot = FALSE, printC = FALSE))
  
  expect_s3_class(tbl, "data.frame")
  expect_true("Cumulative" %in% colnames(tbl))
})

test_that("freqC omits cumulative percents for binary x values", {
  x_factor_binary <- factor(c("No", "Yes", "No", "Yes"))
  x_ordered_binary <- factor(c("Low", "High", "Low", "High"),
                             levels = c("Low", "High"), ordered = TRUE)
  x_numeric_binary <- c(0, 1, 0, 1, 1)
  
  tbl_factor <- quiet_run(freqC(x_factor_binary, plot = FALSE, printC = FALSE))
  tbl_ordered <- quiet_run(freqC(x_ordered_binary, plot = FALSE, printC = FALSE))
  tbl_numeric <- quiet_run(freqC(x_numeric_binary, plot = FALSE, printC = FALSE))
  
  expect_false("Cumulative" %in% colnames(tbl_factor))
  expect_false("Cumulative" %in% colnames(tbl_ordered))
  expect_false("Cumulative" %in% colnames(tbl_numeric))
})
