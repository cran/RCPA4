args <- commandArgs(trailingOnly = TRUE)

run_examples <- function(root, script, result_path) {
  setup_start <- proc.time()[["elapsed"]]
  pkgload::load_all(root, quiet = TRUE)
  options(RCPA4.verbose = NULL, RCPA4.slow = NULL, RCPA4.pause_seconds = NULL)
  working_dir <- tempfile("example-timing-")
  dir.create(working_dir)
  previous_dir <- setwd(working_dir)
  on.exit({
    grDevices::graphics.off()
    setwd(previous_dir)
    unlink(working_dir, recursive = TRUE)
  }, add = TRUE)
  options(device = function(...) grDevices::pdf(file = "example-%03d.pdf", ...))
  grDevices::pdf(file = "example-%03d.pdf")
  setup_seconds <- proc.time()[["elapsed"]] - setup_start
  warnings <- character()
  error <- NULL
  start <- proc.time()[["elapsed"]]
  tryCatch(
    withCallingHandlers(
      sys.source(script, envir = new.env(parent = .GlobalEnv)),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) error <<- conditionMessage(e)
  )
  elapsed <- proc.time()[["elapsed"]] - start
  saveRDS(list(elapsed = elapsed, setup_seconds = setup_seconds,
               warnings = unique(warnings), error = error), result_path)
}

run_examples(args[[1]], args[[2]], args[[3]])
