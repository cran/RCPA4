args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(if (length(args)) args[[1]] else ".", winslash = "/")
output_dir <- if (length(args) > 1L) args[[2]] else file.path(root, "tests", "example-timing")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
worker <- file.path(root, "tests", "example-timing", "worker.R")
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
timeout_seconds <- 90
if (!requireNamespace("processx", quietly = TRUE)) stop("Install processx to run the timing inventory.")

has_tag <- function(node, tag) {
  identical(attr(node, "Rd_tag"), tag) ||
    (is.list(node) && any(vapply(node, has_tag, logical(1), tag = tag)))
}
featured <- c("boxplotC", "CImean", "CIprop", "compmeansC", "correlateC", "crosstabC",
              "describeC", "freqC", "getC", "helpC", "histC", "logregC", "printC",
              "regC", "sampdistC", "sortC", "testmeansC", "testpropsC", "transformC", "welcomeC")
temporary_dir <- tempfile("timing-inventory-")
dir.create(temporary_dir)

run_one <- function(topic, script, run) {
  result_path <- file.path(temporary_dir, paste0(topic, "-", run, ".rds"))
  execution <- tryCatch(
    processx::run(rscript, c("--vanilla", worker, root, script, result_path),
                 timeout = timeout_seconds, error_on_status = FALSE),
    error = identity
  )
  if (file.exists(result_path)) {
    result <- readRDS(result_path)
    return(data.frame(topic = topic, run = run,
                      elapsed_seconds = round(result$elapsed, 3),
                      setup_seconds = round(result$setup_seconds, 3),
                      status = if (is.null(result$error)) "passed" else "error",
                      warnings = paste(result$warnings, collapse = " | "),
                      error = if (is.null(result$error)) "" else result$error))
  }
  timed_out <- inherits(execution, "system_command_timeout_error")
  detail <- if (inherits(execution, "error")) conditionMessage(execution) else execution$stderr
  data.frame(topic = topic, run = run, elapsed_seconds = NA_real_, setup_seconds = NA_real_,
             status = if (timed_out) "timeout" else "setup error", warnings = "", error = detail)
}

inventory <- list()
for (rd_path in list.files(file.path(root, "man"), pattern = "[.]Rd$", full.names = TRUE)) {
  rd <- tools::parse_Rd(rd_path)
  if (!has_tag(rd, "\\examples")) next
  topic <- sub("[.]Rd$", "", basename(rd_path))
  script <- file.path(temporary_dir, paste0(topic, ".R"))
  tools::Rd2ex(rd, out = script, commentDontrun = TRUE, commentDonttest = FALSE)
  expressions <- as.list(parse(script))
  setup_or_guard <- function(expr) {
    is.call(expr) && (as.character(expr[[1]]) %in% c("library", "require") ||
      (identical(expr[[1]], as.name("if")) && identical(expr[[2]], quote(interactive()))))
  }
  interactive_only <- length(expressions) > 0L &&
    all(vapply(expressions, setup_or_guard, logical(1))) &&
    any(vapply(expressions, function(expr) is.call(expr) &&
      identical(expr[[1]], as.name("if")), logical(1)))
  runs <- run_one(topic, script, 1L)
  if (runs$status == "passed" && !interactive_only &&
      runs$elapsed_seconds >= 3 && runs$elapsed_seconds <= 7) {
    runs <- rbind(runs, run_one(topic, script, 2L), run_one(topic, script, 3L))
  }
  runs$group <- if (topic %in% featured) "featured" else "helper/other"
  runs$has_donttest <- has_tag(rd, "\\donttest")
  runs$interactive_only <- interactive_only
  inventory[[topic]] <- runs
  current <- do.call(rbind, inventory)
  utils::write.csv(current, file.path(output_dir, "example-timings.csv"), row.names = FALSE)
  cat(sprintf("%-24s %s  %s s\n", topic, runs$status[[1]], runs$elapsed_seconds[[1]]))
  flush.console()
}

all_runs <- do.call(rbind, inventory)
summary_rows <- lapply(split(all_runs, all_runs$topic), function(runs) {
  elapsed <- runs$elapsed_seconds[!is.na(runs$elapsed_seconds)]
  worst <- if (length(elapsed)) max(elapsed) else NA_real_
  status <- if (any(runs$status != "passed")) paste(unique(runs$status[runs$status != "passed"]), collapse = ", ") else
    if (all(runs$interactive_only)) "interactive-only (skipped)" else
    if (worst < 3) "safely under" else if (worst <= 7) "borderline" else "slow"
  data.frame(topic = runs$topic[[1]], group = runs$group[[1]], runs = nrow(runs),
             median_seconds = if (length(elapsed)) round(stats::median(elapsed), 3) else NA_real_,
             slowest_seconds = worst, status = status, has_donttest = runs$has_donttest[[1]],
             warnings = paste(unique(runs$warnings[nzchar(runs$warnings)]), collapse = " | "),
             error = paste(unique(runs$error[nzchar(runs$error)]), collapse = " | "))
})
summary <- do.call(rbind, summary_rows)
summary <- summary[order(summary$status, summary$slowest_seconds, summary$topic, na.last = TRUE), ]
utils::write.csv(summary, file.path(output_dir, "example-timing-summary.csv"), row.names = FALSE)
lines <- c("# Example Timing Inventory", "",
           paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
           paste("R:", R.version.string), "",
           "Complete generated Rd example blocks, including donttest, run in separate R processes.",
           "Default RCPA4 options and pauses are retained. Interactive guards are not entered.",
           "Each process uses a temporary working directory and PDF graphics device.",
           "Timing excludes process startup, pkgload setup, and device setup; library calls inside examples are timed.",
           "This measures the current source package via pkgload, not an installed-package CRAN check.",
           "Borderline pages (3-7 seconds on the first run) are repeated twice. Other pages run once.",
           paste("The per-process timeout, including setup, is", timeout_seconds, "seconds."),
           "Classification uses the slowest measured run: under 3s safely under, 3-7s borderline, over 7s slow.",
           "These margins are inventory heuristics, not CRAN rules. Errors are not recommendations to unwrap.", "",
           "| Topic | Group | Runs | Median (s) | Slowest (s) | Status | donttest |",
           "|---|---|---:|---:|---:|---|---|")
for (i in seq_len(nrow(summary))) {
  row <- summary[i, ]
  lines <- c(lines, sprintf("| %s | %s | %d | %s | %s | %s | %s |", row$topic,
                           row$group, row$runs, row$median_seconds, row$slowest_seconds,
                           row$status, row$has_donttest))
}
issues <- summary[nzchar(summary$warnings) | nzchar(summary$error), ]
if (nrow(issues)) {
  lines <- c(lines, "", "## Warnings And Errors", "")
  for (i in seq_len(nrow(issues))) {
    lines <- c(lines, paste0("- `", issues$topic[i], "`: ",
                            paste(c(issues$warnings[i], issues$error[i])[nzchar(c(issues$warnings[i], issues$error[i]))], collapse = " | ")))
  }
}
writeLines(lines, file.path(output_dir, "example-timing-report.md"))
unlink(temporary_dir, recursive = TRUE)
cat("\nSaved timing inventory to", output_dir, "\n")
print(table(summary$status))
