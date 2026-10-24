quiet_run <- function(expr) {
  out <- suppressWarnings(
    suppressMessages(
      utils::capture.output(
        result <- eval.parent(substitute(expr))
      )
    )
  )
  invisible(out)
  result
}
