book_example_decode_entities <- function(x) {
  if (length(x) == 0L) return(x)
  
  decode_map <- c(
    "&lt;" = "<",
    "&gt;" = ">",
    "&amp;" = "&",
    "&quot;" = "\"",
    "&#39;" = "'",
    "&nbsp;" = " "
  )
  for (key in names(decode_map)) {
    x <- gsub(key, decode_map[[key]], x, fixed = TRUE)
  }
  
  decode_numeric_entities_one <- function(s) {
    m <- gregexpr("&#x[0-9A-Fa-f]+;|&#[0-9]+;", s, perl = TRUE)[[1]]
    if (identical(m, -1L)) return(s)
    
    hits <- regmatches(s, list(m))[[1]]
    repl <- vapply(hits, function(hit) {
      if (startsWith(hit, "&#x")) {
        code <- suppressWarnings(strtoi(sub("^&#x|;$", "", hit), base = 16L))
      } else {
        code <- suppressWarnings(as.integer(sub("^&#|;$", "", hit)))
      }
      if (is.na(code)) return(hit)
      intToUtf8(code)
    }, character(1))
    
    regmatches(s, list(m)) <- list(repl)
    s
  }
  
  vapply(x, decode_numeric_entities_one, character(1), USE.NAMES = FALSE)
}


book_example_strip_tags <- function(x) {
  gsub("<[^>]+>", "", x, perl = TRUE)
}


book_example_trim_empty_ends <- function(lines) {
  if (length(lines) == 0L) return(lines)
  while (length(lines) > 0L && !nzchar(trimws(lines[1]))) lines <- lines[-1]
  while (length(lines) > 0L && !nzchar(trimws(lines[length(lines)]))) lines <- lines[-length(lines)]
  lines
}


book_example_filter_expected_markers <- function(markers) {
  if (length(markers) == 0L) return(markers)
  markers <- markers[nzchar(trimws(markers))]
  # Prompt lines (e.g., "> x <- 1") are console input, not expected output.
  markers <- markers[!grepl("^[>+]\\s*", markers, perl = TRUE)]
  markers
}


book_example_clean_lines <- function(lines, keep_blank = FALSE) {
  if (length(lines) == 0L) return(character(0))
  out <- book_example_decode_entities(book_example_strip_tags(lines))
  out <- gsub("\r", "", out, fixed = TRUE)
  out <- gsub("[[:space:]]+$", "", out, perl = TRUE)
  if (!isTRUE(keep_blank)) out <- out[nzchar(trimws(out))]
  out
}


extract_book_examples <- function(xhtml_file) {
  lines <- readLines(xhtml_file, warn = FALSE, encoding = "UTF-8")
  n <- length(lines)
  blocks <- list()
  block_index <- 0L
  i <- 1L
  
  start_tag <- "<pre class=\"sourceCode r\"><code class=\"sourceCode r\">"
  end_tag <- "</code></pre>"
  
  while (i <= n) {
    if (!grepl(start_tag, lines[i], fixed = TRUE)) {
      i <- i + 1L
      next
    }
    
    block_index <- block_index + 1L
    start_line <- i
    code_raw <- character(0)
    
    first <- sub(paste0("^.*", start_tag), "", lines[i], perl = TRUE)
    if (grepl(end_tag, first, fixed = TRUE)) {
      code_raw <- c(code_raw, sub(paste0(end_tag, ".*$"), "", first, perl = TRUE))
      end_line <- i
    } else {
      code_raw <- c(code_raw, first)
      j <- i + 1L
      repeat {
        if (j > n) {
          end_line <- n
          break
        }
        cur <- lines[j]
        if (grepl(end_tag, cur, fixed = TRUE)) {
          code_raw <- c(code_raw, sub(paste0(end_tag, ".*$"), "", cur, perl = TRUE))
          end_line <- j
          break
        }
        code_raw <- c(code_raw, cur)
        j <- j + 1L
      }
    }
    
    i <- end_line + 1L
    
    # Look for paired console-output block immediately after the code block.
    j <- i
    while (j <= n && !nzchar(trimws(lines[j]))) j <- j + 1L
    
    expected_raw <- character(0)
    if (j <= n && grepl("<div class=\"r-console-output\"", lines[j], fixed = TRUE)) {
      repeat {
        expected_raw <- c(expected_raw, lines[j])
        if (grepl("</div>", lines[j], fixed = TRUE) || j >= n) break
        j <- j + 1L
      }
      i <- j + 1L
    }
    
    code_lines <- book_example_trim_empty_ends(book_example_clean_lines(code_raw, keep_blank = TRUE))
    code <- paste(code_lines, collapse = "\n")
    
    expected_lines <- book_example_trim_empty_ends(book_example_clean_lines(expected_raw, keep_blank = TRUE))
    expected_tables <- expected_lines[grepl("^Table:\\s*", expected_lines, perl = TRUE)]
    
    heading <- NA_character_
    if (length(expected_lines) > 0L) {
      first_table <- which(grepl("^Table:\\s*", expected_lines, perl = TRUE))[1]
      if (!is.na(first_table) && first_table > 1L) {
        pre_table <- expected_lines[1:(first_table - 1L)]
      } else if (is.na(first_table)) {
        pre_table <- expected_lines
      } else {
        pre_table <- character(0)
      }
      pre_table <- pre_table[nzchar(trimws(pre_table))]
      if (length(pre_table) > 0L) heading <- pre_table[1]
    }
    
    if (length(expected_tables) > 0L) {
      expected_markers <- unique(expected_tables)
    } else if (!is.na(heading) && nzchar(trimws(heading))) {
      expected_markers <- heading
    } else {
      expected_markers <- character(0)
    }
    expected_markers <- book_example_filter_expected_markers(expected_markers)
    
    blocks[[length(blocks) + 1L]] <- list(
      chapter = basename(xhtml_file),
      chunk = block_index,
      start_line = start_line,
      end_line = end_line,
      code = code,
      expected_heading = heading,
      expected_tables = expected_tables,
      expected_markers = expected_markers
    )
  }
  
  blocks
}


book_example_default_skip_patterns <- function() {
  data.frame(
    pattern = c(
      "\\brgl\\b",
      "open3d\\(",
      "View\\(",
      "identify\\(",
      "locator\\(",
      "\\bwidgetFactory\\(",
      "\\bgetC\\(",
      "\\bfile\\.choose\\(",
      "\\binstall\\.packages\\("
    ),
    reason = c(
      "rgl graphics are interactive and not stable in automated runs",
      "open3d requires interactive graphics device",
      "View() is interactive",
      "identify() is interactive",
      "locator() is interactive",
      "widgetFactory() is interactive by design",
      "getC() depends on local/interactive file access",
      "file.choose() requires interactive file picker",
      "install.packages() depends on network/package repositories"
    ),
    stringsAsFactors = FALSE
  )
}


book_example_should_skip <- function(code, skip_patterns = NULL) {
  if (is.null(skip_patterns) || nrow(skip_patterns) == 0L) return(NULL)
  hit <- vapply(skip_patterns$pattern, function(p) grepl(p, code, perl = TRUE), logical(1))
  if (!any(hit)) return(NULL)
  idx <- which(hit)[1]
  list(
    pattern = skip_patterns$pattern[idx],
    reason = skip_patterns$reason[idx]
  )
}


book_example_force_transform_confirm <- function(expr) {
  rewrite <- function(node) {
    if (!is.call(node)) return(node)
    
    parts <- as.list(node)
    if (length(parts) > 1L) {
      for (i in 2:length(parts)) parts[[i]] <- rewrite(parts[[i]])
    }
    
    fn <- parts[[1]]
    is_transform <- FALSE
    if (is.symbol(fn) && identical(as.character(fn), "transformC")) {
      is_transform <- TRUE
    } else if (is.call(fn) &&
               length(fn) == 3L &&
               is.symbol(fn[[1]]) &&
               as.character(fn[[1]]) %in% c("::", ":::") &&
               is.symbol(fn[[3]]) &&
               identical(as.character(fn[[3]]), "transformC")) {
      is_transform <- TRUE
    }
    
    if (is_transform) {
      arg_names <- names(parts)
      if (is.null(arg_names)) arg_names <- rep("", length(parts))
      if (!("confirm" %in% arg_names[-1])) {
        parts <- c(parts, list(FALSE))
        arg_names <- c(arg_names, "confirm")
        names(parts) <- arg_names
      }
    }
    
    as.call(parts)
  }
  
  rewrite(expr)
}


book_example_eval_code <- function(code, env) {
  exprs <- parse(text = code, keep.source = FALSE)
  for (k in seq_along(exprs)) {
    expr <- book_example_force_transform_confirm(exprs[[k]])
    res <- withVisible(eval(expr, envir = env))
    if (isTRUE(res$visible)) print(res$value)
  }
  invisible(TRUE)
}


book_example_run_one <- function(block, env, skip_patterns = NULL) {
  skip <- book_example_should_skip(block$code, skip_patterns = skip_patterns)
  if (!is.null(skip)) {
    return(list(
      status = "skipped",
      error = NA_character_,
      missing_markers = character(0),
      output = character(0),
      skip_reason = skip$reason
    ))
  }
  
  messages <- character(0)
  warnings <- character(0)
  error_text <- NA_character_
  
  tmp_pdf <- tempfile("book_example_device_", fileext = ".pdf")
  grDevices::pdf(tmp_pdf)
  on.exit({
    while (grDevices::dev.cur() > 1L) grDevices::dev.off()
    if (file.exists(tmp_pdf)) unlink(tmp_pdf)
  }, add = TRUE)
  
  output <- tryCatch(
    utils::capture.output(
      withCallingHandlers(
        book_example_eval_code(block$code, env = env),
        message = function(m) {
          messages <<- c(messages, conditionMessage(m))
          invokeRestart("muffleMessage")
        },
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
    ),
    error = function(e) {
      error_text <<- conditionMessage(e)
      character(0)
    }
  )
  
  full_output <- c(
    output,
    if (length(messages) > 0L) paste0("MESSAGE: ", messages) else character(0),
    if (length(warnings) > 0L) paste0("WARNING: ", warnings) else character(0)
  )
  clean_output <- book_example_clean_lines(full_output, keep_blank = TRUE)
  output_text <- paste(clean_output, collapse = "\n")
  
  if (!is.na(error_text)) {
    return(list(
      status = "error",
      error = error_text,
      missing_markers = block$expected_markers,
      output = clean_output,
      skip_reason = NA_character_
    ))
  }
  
  missing_markers <- character(0)
  if (length(block$expected_markers) > 0L) {
    marker_hit <- vapply(block$expected_markers, function(marker) {
      grepl(marker, output_text, fixed = TRUE)
    }, logical(1))
    missing_markers <- block$expected_markers[!marker_hit]
  }
  
  status <- if (length(missing_markers) > 0L) "mismatch" else "ok"
  list(
    status = status,
    error = NA_character_,
    missing_markers = missing_markers,
    output = clean_output,
    skip_reason = NA_character_
  )
}


book_example_ensure_rcpa4_loaded <- function(pkg_dir = ".") {
  if ("package:RCPA4" %in% search()) return(invisible(TRUE))
  
  # Prefer local source package when running in the repo, so tests
  # reflect current data/functions (not an older installed build).
  if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists(file.path(pkg_dir, "DESCRIPTION"))) {
    desc_pkg <- tryCatch(
      unname(as.character(read.dcf(file.path(pkg_dir, "DESCRIPTION"), fields = "Package")[1, "Package"])),
      error = function(e) NA_character_
    )
    if (identical(desc_pkg, "RCPA4")) {
      pkgload::load_all(path = pkg_dir, export_all = FALSE, quiet = TRUE)
      return(invisible(TRUE))
    }
  }
  
  if (requireNamespace("RCPA4", quietly = TRUE)) {
    suppressPackageStartupMessages(library(RCPA4))
    return(invisible(TRUE))
  }
  
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(path = pkg_dir, export_all = FALSE, quiet = TRUE)
    return(invisible(TRUE))
  }
  
  stop("Could not load RCPA4. Install RCPA4 or install pkgload for load_all().")
}


run_book_examples <- function(chapters = NULL,
                              book_dir = file.path("..", "EPUB", "OEBPS", "text-prebuild"),
                              max_blocks_per_chapter = NULL,
                              skip_patterns = book_example_default_skip_patterns(),
                              fail_on_mismatch = FALSE,
                              verbose = TRUE) {
  book_dir <- normalizePath(book_dir, winslash = "/", mustWork = TRUE)
  
  if (is.null(chapters)) {
    files <- list.files(book_dir, pattern = "^ch[0-9]{2}\\.xhtml$", full.names = TRUE)
  } else {
    files <- file.path(book_dir, chapters)
    missing_files <- files[!file.exists(files)]
    if (length(missing_files) > 0L) {
      stop("Missing chapter files: ", paste(basename(missing_files), collapse = ", "))
    }
  }
  files <- sort(files)
  
  book_example_ensure_rcpa4_loaded(pkg_dir = ".")
  
  old_opts <- options(
    RCPA4.verbose = FALSE,
    RCPA4.slow = FALSE,
    RCPA4.pause_seconds = 0,
    width = 80
  )
  on.exit(options(old_opts), add = TRUE)
  
  results <- list()
  total <- 0L
  
  for (f in files) {
    blocks <- extract_book_examples(f)
    if (!is.null(max_blocks_per_chapter)) {
      blocks <- blocks[seq_len(min(length(blocks), as.integer(max_blocks_per_chapter)))]
    }
    
    if (isTRUE(verbose)) {
      cat("Running", basename(f), "with", length(blocks), "example(s)\n")
    }
    
    env <- new.env(parent = globalenv())
    
    for (b in blocks) {
      total <- total + 1L
      run <- book_example_run_one(b, env = env, skip_patterns = skip_patterns)
      results[[length(results) + 1L]] <- data.frame(
        chapter = b$chapter,
        chunk = b$chunk,
        start_line = b$start_line,
        status = run$status,
        expected_markers = length(b$expected_markers),
        missing_markers = paste(run$missing_markers, collapse = " | "),
        error = ifelse(is.na(run$error), "", run$error),
        skip_reason = ifelse(is.na(run$skip_reason), "", run$skip_reason),
        code_preview = substr(strsplit(b$code, "\n", fixed = TRUE)[[1]][1], 1, 120),
        stringsAsFactors = FALSE
      )
      
      if (isTRUE(verbose) && run$status %in% c("mismatch", "error")) {
        cat("  -", b$chapter, "#", b$chunk, ":", run$status, "\n", sep = "")
      }
    }
  }
  
  out <- do.call(rbind, results)
  rownames(out) <- NULL
  
  summary <- as.data.frame(table(out$status), stringsAsFactors = FALSE)
  names(summary) <- c("status", "n")
  
  if (isTRUE(fail_on_mismatch) && any(out$status %in% c("mismatch", "error"))) {
    stop("Book example checks failed: found mismatches/errors.")
  }
  
  list(
    summary = summary,
    results = out,
    total = total
  )
}


write_book_example_report <- function(run_result,
                                      report_csv = file.path("tests", "book-examples", "book-example-report.csv")) {
  utils::write.csv(run_result$results, file = report_csv, row.names = FALSE)
  invisible(report_csv)
}
