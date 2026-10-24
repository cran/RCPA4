test_that("helpC finds exported functions outside the package directory", {
  original_dir <- getwd()
  on.exit(setwd(original_dir), add = TRUE)
  setwd(tempdir())

  output <- capture.output(helpC(1))

  expect_true(any(grepl("CImean", output, fixed = TRUE)))
  expect_false(any(grepl("Colors", output, fixed = TRUE)))
  expect_false(any(grepl("lineType", output, fixed = TRUE)))
  expect_false(any(grepl("plotChar", output, fixed = TRUE)))
  expect_false(any(grepl("pchisqC", output, fixed = TRUE)))
  expect_false(any(grepl("random.names", output, fixed = TRUE)))
  expect_false(any(grepl("random_names", output, fixed = TRUE)))
  expect_true(any(grepl("Show help file for RCPA4 function", output, fixed = TRUE)))
})

test_that("helpC shows workspace cleanup commands without removing objects", {
  before_names <- base::ls(envir = .GlobalEnv, all.names = TRUE)
  before_values <- base::mget(before_names, envir = .GlobalEnv, inherits = FALSE)
  local_helpC <- helpC
  environment(local_helpC) <- list2env(list(
    ls = function(envir, all.names) {
      expect_identical(envir, .GlobalEnv)
      expect_false(all.names)
      c("beta", "alpha")
    },
    rm = function(...) stop("helpC must not remove objects")
  ), parent = environment(helpC))
  output <- testthat::with_mocked_bindings(
    capture.output(local_helpC(7)),
    ask = function(...) "",
    .package = "RCPA4"
  )

  expect_true(any(grepl("1. alpha", output, fixed = TRUE)))
  expect_true(any(grepl("2. beta", output, fixed = TRUE)))
  expect_true(any(grepl("rm(object_name)", output, fixed = TRUE)))
  expect_true(any(grepl("rm(obj1_name, obj2_name)", output, fixed = TRUE)))
  expect_true(any(grepl("rm(list = ls())", output, fixed = TRUE)))
  expect_true(any(grepl("No objects have been removed", output, fixed = TRUE)))
  expect_true(any(grepl("7. Clean working environment", output, fixed = TRUE)))
  expect_false(any(grepl("Enter object number to remove", output, fixed = TRUE)))
  expect_identical(base::ls(envir = .GlobalEnv, all.names = TRUE), before_names)
  expect_identical(base::mget(before_names, envir = .GlobalEnv, inherits = FALSE), before_values)
})

test_that("helpC shows cleanup guidance for an empty workspace", {
  local_helpC <- helpC
  environment(local_helpC) <- list2env(list(
    ls = function(...) character(),
    rm = function(...) stop("helpC must not remove objects")
  ), parent = environment(helpC))
  output <- testthat::with_mocked_bindings(
    capture.output(local_helpC(7)),
    ask = function(...) "",
    .package = "RCPA4"
  )

  expect_true(any(grepl("no non-hidden objects found", output, fixed = TRUE)))
  expect_true(any(grepl("rm(obj1_name, obj2_name)", output, fixed = TRUE)))
  expect_true(any(grepl("rm(list = ls())", output, fixed = TRUE)))
})

test_that("viewing helpC settings preserves unset options and shows defaults", {
  old <- options(RCPA4.verbose = NULL, RCPA4.slow = NULL,
                 RCPA4.pause_seconds = NULL)
  on.exit(options(old), add = TRUE)
  before <- options()
  output <- testthat::with_mocked_bindings(
    capture.output(helpC(6)),
    ask = function(...) "",
    .package = "RCPA4"
  )

  expect_identical(options(), before)
  expect_true(any(grepl("  verbose: TRUE", output, fixed = TRUE)))
  expect_true(any(grepl("  slow: TRUE", output, fixed = TRUE)))
  expect_true(any(grepl("  pause_seconds: 2", output, fixed = TRUE)))
})

test_that("viewing helpC settings preserves existing option values", {
  old <- options(RCPA4.verbose = FALSE, RCPA4.slow = FALSE,
                 RCPA4.pause_seconds = 3L)
  on.exit(options(old), add = TRUE)
  before <- options()
  output <- testthat::with_mocked_bindings(
    capture.output(helpC(6)),
    ask = function(...) "",
    .package = "RCPA4"
  )

  expect_identical(options(), before)
  expect_true(any(grepl("  verbose: FALSE", output, fixed = TRUE)))
  expect_true(any(grepl("  slow: FALSE", output, fixed = TRUE)))
  expect_true(any(grepl("  pause_seconds: 3", output, fixed = TRUE)))
})

test_that("helpC applies settings after an explicit menu choice", {
  old <- options(RCPA4.verbose = NULL, RCPA4.slow = NULL,
                 RCPA4.pause_seconds = NULL)
  on.exit(options(old), add = TRUE)
  responses <- c("1", "", "")
  output <- testthat::with_mocked_bindings(
    capture.output(helpC(6)),
    ask = function(...) {
      answer <- responses[1]
      responses <<- responses[-1]
      answer
    },
    .package = "RCPA4"
  )

  expect_identical(getOption("RCPA4.verbose"), FALSE)
  expect_true(any(grepl("verbose is now FALSE", output, fixed = TRUE)))
})

test_that("helpC lists chapter resources", {
  output <- capture.output(helpC(9))

  expect_true(any(grepl("0. Getting Started with R", output, fixed = TRUE)))
  expect_true(any(grepl("1. Using R for Data Analysis", output, fixed = TRUE)))
  expect_true(any(grepl("15. Doing Your Own Political Analysis", output, fixed = TRUE)))
  expect_true(any(grepl("99. Advanced Topics", output, fixed = TRUE)))
})

test_that("helpC reports when no graphics device needs resetting", {
  grDevices::graphics.off()
  output <- capture.output(helpC(8))

  expect_true(any(grepl("No graphics device is open", output, fixed = TRUE)))
  expect_true(any(grepl("default graphical settings", output, fixed = TRUE)))
})

test_that("helpC resets the active graphics device after confirmation", {
  grDevices::graphics.off()
  plot_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(plot_file)
  on.exit({
    grDevices::graphics.off()
    unlink(plot_file)
  }, add = TRUE)

  responses <- c("1", "")
  output <- testthat::with_mocked_bindings(
    capture.output(helpC(8)),
    ask = function(...) {
      answer <- responses[1]
      responses <<- responses[-1]
      answer
    },
    .package = "RCPA4"
  )

  expect_true(any(grepl("Closed the active graphics device", output, fixed = TRUE)))
  expect_null(grDevices::dev.list())
})

test_that("helpC lists data-source options", {
  output <- capture.output(helpC(5))

  expect_true(any(grepl("1. Installed packages with datasets", output, fixed = TRUE)))
  expect_true(any(grepl("2. Journal Dataverse archives", output, fixed = TRUE)))
  expect_true(any(grepl("3. Web sites with topical collections", output, fixed = TRUE)))
  expect_true(any(grepl("4. Browse by subfield", output, fixed = TRUE)))
})

test_that("helpC provides dataset loading commands without loading data", {
  before_names <- base::ls(envir = .GlobalEnv, all.names = TRUE)
  before_values <- base::mget(before_names, envir = .GlobalEnv, inherits = FALSE)
  dataset_index <- data.frame(
    Package = c("datasets", "datasets"),
    Item = c("beaver1 (beavers)", "iris"),
    Title = c("Beaver data", "Iris data")
  )
  local_helpC <- helpC
  environment(local_helpC) <- list2env(list(
    get0 = function(...) function(...) character()
  ), parent = environment(helpC))
  responses <- c("1", "1", "1", "", "2", "", "", "", "", "")
  prompts <- character()
  output <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      capture.output(local_helpC(5)),
      ask = function(msg, ...) {
        prompts <<- c(prompts, msg)
        answer <- responses[1]
        responses <<- responses[-1]
        answer
      },
      .package = "RCPA4"
    ),
    data = function(...) {
      args <- list(...)
      if (!is.null(args$list) || !is.null(args$envir)) stop("helpC must not load datasets")
      list(results = dataset_index)
    },
    .package = "utils"
  )

  expect_true(any(grepl('data("beavers", package = "datasets")', output, fixed = TRUE)))
  expect_true(any(grepl('data("iris", package = "datasets")', output, fixed = TRUE)))
  expect_equal(sum(grepl("No dataset has been loaded by helpC", output, fixed = TRUE)), 2L)
  expect_equal(sum(prompts == "Press <Enter> to go back: "), 2L)
  expect_false(any(grepl("Would you like to load", output, fixed = TRUE)))
  expect_identical(base::ls(envir = .GlobalEnv, all.names = TRUE), before_names)
  expect_identical(base::mget(before_names, envir = .GlobalEnv, inherits = FALSE), before_values)
})

test_that("helpC lists data subfields alphabetically", {
  responses <- c("4", "", "", "")
  output <- testthat::with_mocked_bindings(
    capture.output(helpC(5)),
    ask = function(...) {
      answer <- responses[1]
      responses <<- responses[-1]
      answer
    },
    .package = "RCPA4"
  )

  expect_true(any(grepl("Explore data by subfield:", output, fixed = TRUE)))
  expect_true(any(grepl("1. African Politics", output, fixed = TRUE)))
  expect_true(any(grepl("8. Law, Courts, and Justice", output, fixed = TRUE)))
  expect_true(any(grepl("15. Teaching and Learning", output, fixed = TRUE)))
  expect_true(any(grepl("16. Voting and Elections", output, fixed = TRUE)))
})

test_that("helpC lists RCPA4 datasets for exploration", {
  output <- capture.output(helpC(2))

  expect_true(any(grepl("Explore RCPA4 datasets and variables", output, fixed = TRUE)))
  expect_true(any(grepl("1. anes", output, fixed = TRUE)))
  expect_true(any(grepl("2. debate", output, fixed = TRUE)))
  expect_true(any(grepl("3. gss", output, fixed = TRUE)))
  expect_true(any(grepl("4. states", output, fixed = TRUE)))
  expect_true(any(grepl("5. world", output, fixed = TRUE)))
})

test_that("helpC respects the configured help viewer", {
  responses <- c("1", "")
  help_args <- NULL
  had_global_help <- exists("help", envir = .GlobalEnv, inherits = FALSE)
  if (had_global_help) old_global_help <- get("help", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (had_global_help) assign("help", old_global_help, envir = .GlobalEnv)
    else rm("help", envir = .GlobalEnv)
  }, add = TRUE)

  assign(
    "help",
    function(...) {
      help_args <<- list(...)
      "mock help topic"
    },
    envir = .GlobalEnv
  )

  testthat::with_mocked_bindings(
    capture.output(helpC(1)),
    ask = function(...) {
      answer <- responses[1]
      responses <<- responses[-1]
      answer
    },
    .package = "RCPA4"
  )

  expect_identical(help_args$package, "RCPA4")
  expect_null(help_args$help_type)
})

test_that("helpC explores Debate variables and ANES categories", {
  debate_responses <- c("2", "1", "1", "", "", "", "", "")
  debate_output <- testthat::with_mocked_bindings(
    capture.output(helpC(2)),
    ask = function(...) {
      answer <- debate_responses[1]
      debate_responses <<- debate_responses[-1]
      answer
    },
    .package = "RCPA4"
  )

  expect_true(any(grepl("Explore variables in the debate dataset by category:", debate_output, fixed = TRUE)))
  expect_true(any(grepl("Experiment Administration", debate_output, fixed = TRUE)))
  expect_true(any(grepl("1. obs", debate_output, fixed = TRUE)))
  expect_true(any(grepl("Variable details:", debate_output, fixed = TRUE)))
  expect_true(any(grepl("Name:         obs", debate_output, fixed = TRUE)))
  expect_true(any(grepl("Label:        Unique identification number for each subject", debate_output, fixed = TRUE)))
  expect_true(any(grepl("Class:        integer", debate_output, fixed = TRUE)))
  expect_true(any(grepl("NA handling:  Missing-value information is not available.", debate_output, fixed = TRUE)))

  anes_responses <- c("1", "", "", "")
  anes_output <- testthat::with_mocked_bindings(
    capture.output(helpC(2)),
    ask = function(...) {
      answer <- anes_responses[1]
      anes_responses <<- anes_responses[-1]
      answer
    },
    .package = "RCPA4"
  )

  expect_true(any(grepl("Explore variables in the anes dataset by category:", anes_output, fixed = TRUE)))
  expect_true(any(grepl("Abortion Policy", anes_output, fixed = TRUE)))
})

test_that("helpC runs documented examples from source-package Rd files", {
  responses <- c("7", "")
  output <- testthat::with_mocked_bindings(
    capture.output(helpC(3)),
    ask = function(...) {
      answer <- responses[1]
      responses <<- responses[-1]
      answer
    },
    .package = "RCPA4"
  )

  expect_true(any(grepl("Descriptive Statistics", output, fixed = TRUE)))
  expect_false(any(grepl("Could not run example", output, fixed = TRUE)))
})
