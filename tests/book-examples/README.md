# Book Example Test Harness

This harness validates textbook R examples in `EPUB/OEBPS/text-prebuild/*.xhtml`.

What it checks:
- Extracts each `<pre class="sourceCode r">...</pre>` code block.
- Executes examples sequentially within each chapter (shared environment per chapter).
- If a paired `<div class="r-console-output">` exists, checks that expected markers are present in runtime output:
  - heading text (when present)
  - every `Table: ...` caption line

Result statuses:
- `ok`: code ran and expected markers were found.
- `mismatch`: code ran but one or more expected markers were missing.
- `error`: code raised an error.
- `skipped`: chunk matched an interactive skip pattern (e.g., `open3d`, `View`).

## Run From Package Root

```r
Rscript tests/book-examples/run-book-example-checks.R
```

Optional arguments:
- `--chapters=ch02.xhtml,ch12.xhtml`
- `--max_blocks=10`
- `--fail_on_mismatch=true`
- `--book_dir=../EPUB/OEBPS/text-prebuild`
- `--report=tests/book-examples/book-example-report.csv`

Environment-variable equivalents:
- `RCPA4_BOOK_CHAPTERS`
- `RCPA4_BOOK_MAX_BLOCKS`
- `RCPA4_BOOK_FAIL_ON_MISMATCH`
- `RCPA4_BOOK_DIR`
- `RCPA4_BOOK_REPORT`

Output:
- CSV report at `tests/book-examples/book-example-report.csv` (default).
