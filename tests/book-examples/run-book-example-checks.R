args <- commandArgs(trailingOnly = TRUE)

`%||%` <- function(x, y) if (is.null(x)) y else x

parse_args <- function(x) {
  out <- list()
  if (length(x) == 0L) return(out)
  for (arg in x) {
    if (!grepl("^--[^=]+=.*$", arg)) next
    key <- sub("^--([^=]+)=.*$", "\\1", arg)
    val <- sub("^--[^=]+=(.*)$", "\\1", arg)
    out[[key]] <- val
  }
  out
}

opts <- parse_args(args)

source(file.path("tests", "book-examples", "harness.R"))

chapters_csv <- opts$chapters %||% Sys.getenv("RCPA4_BOOK_CHAPTERS", "")
chapters <- if (nzchar(chapters_csv)) trimws(strsplit(chapters_csv, ",", fixed = TRUE)[[1]]) else NULL

max_blocks_txt <- opts$max_blocks %||% Sys.getenv("RCPA4_BOOK_MAX_BLOCKS", "")
max_blocks <- if (nzchar(max_blocks_txt)) as.integer(max_blocks_txt) else NULL

fail_txt <- tolower(opts$fail_on_mismatch %||% Sys.getenv("RCPA4_BOOK_FAIL_ON_MISMATCH", "true"))
fail_on_mismatch <- fail_txt %in% c("1", "true", "yes", "y")

report_csv <- opts$report %||% Sys.getenv(
  "RCPA4_BOOK_REPORT",
  file.path("tests", "book-examples", "book-example-report.csv")
)

book_dir <- opts$book_dir %||% Sys.getenv(
  "RCPA4_BOOK_DIR",
  file.path("..", "EPUB", "OEBPS", "text-prebuild")
)

run <- run_book_examples(
  chapters = chapters,
  book_dir = book_dir,
  max_blocks_per_chapter = max_blocks,
  fail_on_mismatch = fail_on_mismatch,
  verbose = TRUE
)

write_book_example_report(run, report_csv = report_csv)

cat("\nSummary:\n")
print(run$summary)
cat("\nWrote report to:", report_csv, "\n")
