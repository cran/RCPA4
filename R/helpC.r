#' Interactive helper for RCPA4 documentation, examples, resources, plotting references, and settings
#'
#' @description
#' Opens an interactive menu with these top-level options:
#'
#' 1. **Show help file for RCPA4 function.** Select a core RCPA4 function and display its help file.
#' 2. **Explore RCPA4 datasets and variables.** Select an RCPA4 dataset, display its help file, and browse its variables.
#' 3. **Show an example.** Select a core RCPA4 function and run its documented example.
#' 4. **Help with plots.** Open color, line-type, and plotting-character references, or check graphics support.
#' 5. **Find data.** Browse datasets in installed packages or open online sources of political data.
#' 6. **Change RCPA4 package settings.** Adjust `RCPA4.verbose`, `RCPA4.slow`, and `RCPA4.pause_seconds`.
#' 7. **Clean working environment.** List workspace objects and show commands
#'    users can run themselves to remove one, several, or all non-hidden objects.
#' 8. **Reset graphical parameters.** Close the active graphics device so later plots use default settings.
#' 9. **Browse chapter resources.** Open the supplemental web resources for Getting Started, Chapters 1-15, or Advanced Topics.
#'
#' @param option (Optional) A top-level menu choice from `1` to `9` to run first.
#'   For example, `helpC(5)` opens the "Find data" submenu directly.
#' @return Invisibly returns `NULL`.
#' @template console-output
#' @section Interactive menu output:
#' The settings menu at `helpC(6)` displays effective session preferences,
#' using package defaults for unset options. Viewing and exiting the menu does
#' not change options. Preferences change only when you select a settings action.
#' The `helpC()` menu and prompts remain visible when `RCPA4.verbose = FALSE`.
#' Result-table controls are documented separately in each analysis function's
#' help page.
#' @section Workspace cleanup:
#' Option 7 lists non-hidden objects in the global environment and displays
#' `rm(object_name)`, `rm(obj1_name, obj2_name)`, and `rm(list = ls())` as commands
#' to run at the R console. Replace the placeholder names with your object names.
#' `rm(list = ls())` removes all non-hidden objects in the current environment.
#' The menu itself does not remove or modify any workspace objects.
#' @section Loading installed datasets:
#' In option 5, the installed-package dataset browser displays dataset help
#' and a `utils::data()` command to run at the R console. The browser does not
#' load the selected dataset or modify workspace objects. For entries such as
#' `beaver1 (beavers)`, the command uses the dataset name in parentheses.
#' @examples
#' if(interactive()){
#' helpC()
#' helpC(5)
#' }
#' @md
#' @export
helpC <- function(option = NULL) {
  package_name <- "RCPA4"
  
  parse_exports_from_namespace <- function(path) {
    if (!file.exists(path)) return(character(0))
    lines <- readLines(path, warn = FALSE)
    exports <- sub("^export\\(([^)]+)\\).*$", "\\1", grep("^export\\(", lines, value = TRUE))
    exports <- trimws(exports)
    exports[nzchar(exports)]
  }
  
  parse_defined_functions <- function(path) {
    if (!file.exists(path)) return(character(0))
    lines <- readLines(path, warn = FALSE)
    defs <- grep("^\\s*[A-Za-z][A-Za-z0-9._]*\\s*(<-|=)\\s*function\\s*\\(", lines, value = TRUE)
    fn <- sub("^\\s*([A-Za-z][A-Za-z0-9._]*)\\s*(<-|=)\\s*function\\s*\\(.*$", "\\1", defs)
    sort(unique(fn[nzchar(fn)]))
  }

  find_package_root <- function() {
    namespace_path <- tryCatch(
      getNamespaceInfo(asNamespace(package_name), "path"),
      error = function(e) NA_character_
    )
    installed_path <- tryCatch(
      find.package(package_name),
      error = function(e) NA_character_
    )
    candidates <- unique(c(namespace_path, installed_path, getwd()))
    candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
    has_namespace <- file.exists(file.path(candidates, "NAMESPACE"))
    if (!any(has_namespace)) return(NULL)
    candidates[which(has_namespace)[1]]
  }
  
  get_exported_functions <- function() {
    student_functions <- c(
      "boxplotC", "CImean", "CIprop", "compmeansC", "correlateC", "crosstabC",
      "describeC", "freqC", "getC", "helpC", "histC", "logregC", "printC",
      "regC", "sampdistC", "sortC", "testmeansC", "testpropsC", "transformC",
      "welcomeC"
    )
    helper_exports_fallback <- c(
      "CramersV", "fit_svyglm", "inverse_logit", "lambda", "logregR2", "orci",
      "tablesomersDC", "wtd_cor", "wtd_hist", "wtd_kurtosis", "wtd_mean",
      "wtd_median", "wtd_mode", "wtd_quantile", "wtd_sd", "wtd_skewness", "wtd_var"
    )
    
    exported <- tryCatch(getNamespaceExports(package_name), error = function(e) character(0))
    
    if (length(exported) == 0L) {
      attached_name <- paste0("package:", package_name)
      if (attached_name %in% search()) exported <- ls(attached_name, all.names = FALSE)
    }
    
    package_root <- find_package_root()

    # Development fallback: use the package's NAMESPACE, independent of getwd().
    if (length(exported) == 0L) {
      namespace_path <- if (is.null(package_root)) "NAMESPACE" else file.path(package_root, "NAMESPACE")
      exported <- parse_exports_from_namespace(namespace_path)
    }
    
    helper_path <- if (is.null(package_root)) "R/helper.functions.r" else file.path(package_root, "R", "helper.functions.r")
    if (file.exists(helper_path)) {
      helper_funs <- parse_defined_functions(helper_path)
      exported <- setdiff(exported, helper_funs)
    } else {
      exported <- setdiff(exported, helper_exports_fallback)
    }
    
    student_functions[student_functions %in% exported]
  }

  find_rd_file <- function(topic) {
    package_root <- find_package_root()
    rd_path <- if (is.null(package_root)) NA_character_ else file.path(package_root, "man", paste0(topic, ".Rd"))
    if (!is.na(rd_path) && file.exists(rd_path)) rd_path else NULL
  }

  display_help_topic <- function(topic, package) {
    # Honor pkgload's help shim when the package was loaded with load_all().
    help_fun <- get0(
      "help",
      envir = .GlobalEnv,
      mode = "function",
      inherits = TRUE
    )
    if (is.null(help_fun)) help_fun <- utils::help

    help_obj <- do.call(
      help_fun,
      list(topic = topic, package = package)
    )
    if (length(help_obj) == 0L) return(invisible(FALSE))
    print(help_obj)
    invisible(TRUE)
  }

  show_function_help <- function(topic) {
    if (isTRUE(display_help_topic(topic, package_name))) return(invisible(TRUE))

    rd_path <- find_rd_file(topic)
    if (!is.null(rd_path)) {
      tryCatch(
        tools::Rd2txt(tools::parse_Rd(rd_path)),
        error = function(e) cat("Could not render help for", topic, ":", conditionMessage(e), "\n")
      )
      return(invisible(TRUE))
    }

    cat("No help file found for", topic, "in", package_name, ".\n")
    invisible(FALSE)
  }

  run_function_examples <- function(topic) {
    help_obj <- do.call(utils::help, list(topic = topic, package = package_name))
    if (length(help_obj) > 0L) {
      do.call(utils::example, list(topic = topic, package = package_name, ask = FALSE))
      return(invisible(TRUE))
    }

    rd_path <- find_rd_file(topic)
    if (is.null(rd_path)) {
      cat("No documented examples found for", topic, "in", package_name, ".\n")
      return(invisible(FALSE))
    }

    example_path <- tempfile(fileext = ".R")
    on.exit(unlink(example_path), add = TRUE)
    tryCatch({
      tools::Rd2ex(tools::parse_Rd(rd_path), out = example_path)
      example_lines <- readLines(example_path, warn = FALSE)
      executable <- trimws(example_lines)
      executable <- executable[nzchar(executable) & !startsWith(executable, "#")]
      if (length(executable) == 0L) {
        cat("No runnable examples found for", topic, ".\n")
        return(invisible(FALSE))
      }
      sys.source(example_path, envir = new.env(parent = .GlobalEnv))
      invisible(TRUE)
    }, error = function(e) {
      cat("Could not run example for", topic, ":", conditionMessage(e), "\n")
      invisible(FALSE)
    })
  }
  
  choose_number <- function(n, prompt) {
    repeat {
      ans <- ask(prompt)
      if (length(ans) == 0L) return(NULL)
      ans <- trimws(ans[1])
      if (!nzchar(ans)) return(NULL)
      idx <- suppressWarnings(as.integer(ans))
      if (!is.na(idx) && idx >= 1L && idx <= n) return(idx)
      cat(wrap_console_text(paste("Invalid choice. Please enter a number from 1 to", n, "or press <Enter> to go back.")), "\n", sep = "")
    }
  }
  
  normalize_top_choice <- function(x) {
    if (is.null(x)) return(NULL)
    if (length(x) != 1L) stop("option must be a single value from 1 to 9.")
    
    if (is.character(x)) {
      x <- trimws(x)
      if (!nzchar(x)) return(NULL)
      idx <- suppressWarnings(as.integer(x))
    } else {
      idx <- suppressWarnings(as.integer(x))
    }
    
    if (is.na(idx) || idx < 1L || idx > 9L) stop("option must be an integer from 1 to 9.")
    idx
  }
  
  print_numbered_menu <- function(items, titles = NULL, compact_threshold = 15L) {
    n <- length(items)
    if (n == 0L) return(invisible(FALSE))
    
    has_titles <- !is.null(titles) && length(titles) == n
    use_compact <- n >= compact_threshold
    
    if (has_titles && !use_compact) {
      for (i in seq_len(n)) {
        title <- titles[i]
        if (is.na(title)) title <- ""
        title <- trimws(title)
        if (nzchar(title)) cat(sprintf("  %d. %s - %s\n", i, items[i], title))
        else cat(sprintf("  %d. %s\n", i, items[i]))
      }
      return(invisible(FALSE))
    }
    
    labels <- sprintf("%d. %s", seq_len(n), items)
    console_width <- max(20L, getOption("width", 80L))
    cell_width <- max(10L, max(nchar(labels, type = "width"), na.rm = TRUE) + 3L)
    ncol <- max(1L, console_width %/% cell_width)
    nrow <- ceiling(n / ncol)
    idx_matrix <- matrix(NA_integer_, nrow = nrow, ncol = ncol)
    idx_matrix[seq_len(n)] <- seq_len(n)
    
    for (r in seq_len(nrow)) {
      row_parts <- character(0)
      for (c in seq_len(ncol)) {
        idx <- idx_matrix[r, c]
        if (is.na(idx)) next
        part <- labels[idx]
        if (c < ncol) part <- format(part, width = cell_width, justify = "left")
        row_parts <- c(row_parts, part)
      }
      cat("  ", paste0(row_parts, collapse = ""), "\n", sep = "")
    }
    
    if (has_titles && use_compact) {
      cat("  (Compact view: dataset titles hidden. Select a number for details.)\n")
    }
    
    invisible(use_compact)
  }
  
  choose_function <- function(funs, prompt = "Enter function number, or press <Enter> to go back: ") {
    if (length(funs) == 0L) {
      cat("No exported functions were found.\n")
      return(NULL)
    }
    cat("\nSelect a function:\n")
    print_numbered_menu(funs)
    idx <- choose_number(length(funs), prompt)
    if (is.null(idx)) return(NULL)
    funs[idx]
  }

  choose_dataset <- function(datasets) {
    cat("\nExplore RCPA4 datasets and variables:\n")
    print_numbered_menu(datasets)
    idx <- choose_number(length(datasets), "Enter dataset number, or press <Enter> to go back: ")
    if (is.null(idx)) return(NULL)
    datasets[idx]
  }

  get_dataset_metadata <- function(dataset_name) {
    metadata_file <- paste0(dataset_name, "_metadata.csv")
    package_root <- find_package_root()
    candidates <- c(
      system.file("extdata", "metadata", metadata_file, package = package_name),
      if (!is.null(package_root)) file.path(package_root, "inst", "extdata", "metadata", metadata_file),
      if (!is.null(package_root)) file.path(package_root, "extdata", "metadata", metadata_file)
    )
    candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
    if (length(candidates) == 0L) return(NULL)

    metadata <- tryCatch(
      utils::read.csv(candidates[1], stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    if (is.null(metadata) || !all(c("var", "label", "category") %in% names(metadata))) return(NULL)
    metadata
  }

  metadata_text <- function(metadata_row, field, fallback = "Not available") {
    value <- metadata_row[[field]][1]
    if (is.na(value) || !nzchar(trimws(as.character(value)))) return(fallback)
    as.character(value)
  }

  show_variable_details <- function(metadata_row) {
    variable_name <- metadata_text(metadata_row, "var")
    label <- metadata_text(metadata_row, "label")
    class <- metadata_text(metadata_row, "class")
    levels <- metadata_text(metadata_row, "levels", fallback = "Not applicable")
    notes <- metadata_text(metadata_row, "notes", fallback = "None")
    n <- suppressWarnings(as.numeric(metadata_row$n[1]))
    missing <- suppressWarnings(as.numeric(metadata_row$missing[1]))

    missing_handling <- if (is.finite(n) && is.finite(missing)) {
      if (missing == 0) {
        "No missing values are recorded."
      } else {
        paste0(format(missing, trim = TRUE, scientific = FALSE), " of ",
               format(n, trim = TRUE, scientific = FALSE),
               " observations are missing and are represented as NA in R.")
      }
    } else {
      "Missing-value information is not available."
    }

    detail_labels <- c("Name:", "Label:", "Class:", "Levels:", "NA handling:", "Notes:")
    detail_values <- c(variable_name, label, class, levels, missing_handling, notes)
    label_width <- max(nchar(detail_labels, type = "width"))
    value_width <- max(20L, getOption("width", 80L) - label_width - 5L)

    cat("\nVariable details:\n")
    for (i in seq_along(detail_labels)) {
      value_lines <- strwrap(gsub("\n", " ", detail_values[i], fixed = TRUE), width = value_width)
      if (length(value_lines) == 0L) value_lines <- ""
      cat("  ", sprintf("%-*s  %s\n", label_width, detail_labels[i], value_lines[1]), sep = "")
      if (length(value_lines) > 1L) {
        for (line in value_lines[-1L]) {
          cat("  ", sprintf("%-*s  %s\n", label_width, "", line), sep = "")
        }
      }
    }
  }

  run_dataset_variable_explorer <- function(dataset_name) {
    metadata <- get_dataset_metadata(dataset_name)
    if (is.null(metadata)) {
      cat("Variable metadata for", dataset_name, "is not available in this RCPA4 installation.\n")
      return(invisible(NULL))
    }

    categories <- sort(unique(metadata$category[!is.na(metadata$category) & nzchar(metadata$category)]))
    if (length(categories) == 0L) {
      cat("No variable categories are available for", dataset_name, ".\n")
      return(invisible(NULL))
    }

    repeat {
      cat("\nExplore variables in the ", dataset_name, " dataset by category:\n", sep = "")
      print_numbered_menu(categories)
      category_index <- choose_number(
        length(categories),
        "Enter category number, or press <Enter> to return to the dataset menu: "
      )
      if (is.null(category_index)) break

      category <- categories[category_index]
      category_metadata <- metadata[metadata$category == category, , drop = FALSE]
      repeat {
        cat("\nSee details for variables in ", category, ":\n", sep = "")
        print_numbered_menu(category_metadata$var)
        variable_index <- choose_number(
          nrow(category_metadata),
          "Enter variable number, or press <Enter> to return to the category list: "
        )
        if (is.null(variable_index)) break

        show_variable_details(category_metadata[variable_index, , drop = FALSE])
        ask("Press <Enter> to return to the variable list: ")
      }
    }

    invisible(NULL)
  }
  
  run_plot_reference <- function(fun_name) {
    fun_obj <- tryCatch(getExportedValue(package_name, fun_name), error = function(e) NULL)
    if (is.null(fun_obj) || !is.function(fun_obj)) {
      cat("Could not locate", fun_name, "in", package_name, ".\n")
      return(invisible(NULL))
    }
    fun_obj()
    invisible(NULL)
  }
  
  report_graphics_support <- function() {
    cat("\nGraphics support in this R session:\n")
    cat("  R version:", as.character(getRversion()), "\n")
    cat("  Platform:", R.version$platform, "\n")
    
    cur_dev <- grDevices::dev.cur()
    cur_dev_name <- names(cur_dev)
    if (length(cur_dev_name) == 0L || !nzchar(cur_dev_name[1])) cur_dev_name <- "unknown"
    cat("  Active graphics device:", cur_dev_name[1], "(", as.integer(cur_dev[1]), ")\n")
    
    default_dev_opt <- getOption("device")
    default_dev_name <- if (is.character(default_dev_opt)) {
      default_dev_opt[1]
    } else if (is.function(default_dev_opt)) {
      "<function>"
    } else {
      as.character(default_dev_opt)[1]
    }
    if (!nzchar(default_dev_name)) default_dev_name <- "<unspecified>"
    cat("  Default graphics device option:", default_dev_name, "\n")
    
    cap <- capabilities()
    cap_names <- intersect(c("cairo", "png", "jpeg", "tiff", "X11", "aqua"), names(cap))
    if (length(cap_names) > 0L) {
      cat("\nCapabilities:\n")
      for (nm in cap_names) {
        cat(" ", sprintf("%-6s", paste0(nm, ":")), isTRUE(cap[[nm]]), "\n")
      }
    }
    
    test_png_type <- function(type_name) {
      tf <- tempfile(fileext = ".png")
      ok <- tryCatch({
        grDevices::png(filename = tf, width = 100, height = 100, type = type_name)
        grDevices::dev.off()
        TRUE
      }, error = function(e) FALSE)
      if (file.exists(tf)) unlink(tf)
      ok
    }
    
    png_types <- c("windows", "cairo", "cairo-png")
    png_ok <- vapply(png_types, test_png_type, logical(1))
    available_png_types <- png_types[png_ok]
    
    cat("\nPNG device types available now:", if (length(available_png_types) > 0L) paste(available_png_types, collapse = ", ") else "none", "\n")
    
    preferred_type <- if ("cairo" %in% available_png_types) {
      "cairo"
    } else if ("windows" %in% available_png_types) {
      "windows"
    } else if ("cairo-png" %in% available_png_types) {
      "cairo-png"
    } else {
      NA_character_
    }
    
    if (!is.na(preferred_type)) {
      antialias_values <- c("default", "none", "gray", "subpixel", "cleartype")
      test_antialias <- function(aa) {
        tf <- tempfile(fileext = ".png")
        ok <- tryCatch({
          grDevices::png(filename = tf, width = 100, height = 100, type = preferred_type, antialias = aa)
          grDevices::dev.off()
          TRUE
        }, error = function(e) FALSE)
        if (file.exists(tf)) unlink(tf)
        ok
      }
      aa_ok <- vapply(antialias_values, test_antialias, logical(1))
      aa_available <- antialias_values[aa_ok]
      cat("Preferred png type for this session:", preferred_type, "\n")
      cat("Valid antialias values for", preferred_type, ":", paste(aa_available, collapse = ", "), "\n")
    }
    
    cat("Cairo support available:", isTRUE(capabilities("cairo")), "\n")
    cat("\n")
    invisible(NULL)
  }
  
  run_plot_help_menu <- function() {
    repeat {
      cat("\nPlot help options:\n")
      cat("  1. Colors\n")
      cat("  2. lineType\n")
      cat("  3. plotChar\n")
      cat("  4. Check graphics support\n")
      choice <- choose_number(4L, "Enter plot option number, or press <Enter> to go back: ")
      if (is.null(choice)) break
      
      if (choice == 1L) run_plot_reference("Colors")
      if (choice == 2L) run_plot_reference("lineType")
      if (choice == 3L) run_plot_reference("plotChar")
      if (choice == 4L) report_graphics_support()
    }
    invisible(NULL)
  }
  
  get_available_datasets <- function() {
    out <- tryCatch(
      utils::data(package = .packages(all.available = TRUE)),
      error = function(e) NULL
    )
    if (is.null(out) || is.null(out$results) || nrow(out$results) == 0L) return(NULL)
    as.data.frame(out$results, stringsAsFactors = FALSE)
  }
  
  run_installed_data_menu <- function() {
    dataset_index <- get_available_datasets()
    if (is.null(dataset_index)) {
      cat("No datasets were found in installed packages.\n")
      return(invisible(NULL))
    }
    
    packages_with_data <- sort(unique(dataset_index$Package))
    if (length(packages_with_data) == 0L) {
      cat("No datasets were found in installed packages.\n")
      return(invisible(NULL))
    }
    
    repeat {
      cat("\nThese installed packages have datasets, pick one.\n")
      print_numbered_menu(packages_with_data)
      
      idx <- choose_number(length(packages_with_data), "Enter package number, or press <Enter> to go back: ")
      if (is.null(idx)) break
      
      pkg <- packages_with_data[idx]
      pkg_rows <- dataset_index[dataset_index$Package == pkg, , drop = FALSE]
      pkg_rows <- pkg_rows[!duplicated(pkg_rows$Item), , drop = FALSE]
      pkg_rows <- pkg_rows[order(pkg_rows$Item), , drop = FALSE]

      if (nrow(pkg_rows) == 0L) {
        cat("No datasets were found for package", pkg, ".\n")
        next
      }

      repeat {
        cat("\nThe ", pkg, " package contains the following datasets:\n", sep = "")
        print_numbered_menu(pkg_rows$Item, titles = pkg_rows$Title)

        data_idx <- choose_number(nrow(pkg_rows), paste0("Enter dataset number from ", pkg, " for details, or press <Enter> to go back: "))
        if (is.null(data_idx)) break

        dataset_name <- pkg_rows$Item[data_idx]
        if (!isTRUE(display_help_topic(dataset_name, pkg))) {
          cat("No help file found for", dataset_name, "in", pkg, ".\n")
        }

        load_name <- dataset_name
        if (grepl(" \\([^()]+\\)$", dataset_name)) {
          load_name <- sub("^.* \\(([^()]+)\\)$", "\\1", dataset_name)
        }
        cat("\nTo load this dataset, run this command at the R console:\n")
        cat("  data(", encodeString(load_name, quote = '"'),
            ", package = ", encodeString(pkg, quote = '"'), ")\n", sep = "")
        cat("No dataset has been loaded by helpC.\n")
        ask("Press <Enter> to go back: ")
      }
    }
    
    invisible(NULL)
  }

  run_subfield_data_menu <- function() {
    subfield_pages <- c(
      "African Politics" = "https://www.poliscidata.com/pages/africanPolitics.php",
      "American Politics" = "https://www.poliscidata.com/pages/americanPolitics.php",
      "Asian Politics" = "https://www.poliscidata.com/pages/asianPolitics.php",
      "Comparative Politics" = "https://www.poliscidata.com/pages/comparativePolitics.php",
      "European Politics" = "https://www.poliscidata.com/pages/europeanPolitics.php",
      "International Relations" = "https://www.poliscidata.com/pages/internationalRelations.php",
      "Latin American Politics" = "https://www.poliscidata.com/pages/latinAmericanPolitics.php",
      "Law, Courts, and Justice" = "https://www.poliscidata.com/pages/lawCourtsJustice.php",
      "Methodology" = "https://www.poliscidata.com/pages/methodology.php",
      "Migration and Citizenship" = "https://www.poliscidata.com/pages/migrationCitizenship.php",
      "Political Behavior" = "https://www.poliscidata.com/pages/politicalBehavior.php",
      "Political Theory" = "https://www.poliscidata.com/pages/politicalTheory.php",
      "Public Administration" = "https://www.poliscidata.com/pages/publicAdministration.php",
      "Public Policy" = "https://www.poliscidata.com/pages/publicPolicy.php",
      "Teaching and Learning" = "https://www.poliscidata.com/pages/teachingAndLearning.php",
      "Voting and Elections" = "https://www.poliscidata.com/pages/votingAndElections.php"
    )

    repeat {
      cat("\nExplore data by subfield:\n")
      print_numbered_menu(names(subfield_pages), compact_threshold = 16L)

      choice <- choose_number(
        length(subfield_pages),
        "Enter subfield number, or press <Enter> to go back: "
      )
      if (is.null(choice)) break

      subfield <- names(subfield_pages)[choice]
      opened <- tryCatch({
        utils::browseURL(subfield_pages[[choice]])
        TRUE
      }, error = function(e) {
        cat("Could not open", subfield, ":", conditionMessage(e), "\n")
        FALSE
      })
      if (opened) cat("Opened", subfield, "resources in your web browser.\n")
    }

    invisible(NULL)
  }

  run_find_data_menu <- function() {
    open_data_url <- function(url, label) {
      opened <- tryCatch({
        utils::browseURL(url)
        TRUE
      }, error = function(e) {
        cat("Could not open", label, ":", conditionMessage(e), "\n")
        FALSE
      })
      if (opened) cat("Opened", label, "in your web browser.\n")
    }

    repeat {
      cat("\nFind data options:\n")
      cat("  1. Installed packages with datasets\n")
      cat("  2. Journal Dataverse archives\n")
      cat("  3. Web sites with topical collections\n")
      cat("  4. Browse by subfield\n")

      choice <- choose_number(4L, "Enter data option number, or press <Enter> to go back: ")
      if (is.null(choice)) break

      if (choice == 1L) {
        run_installed_data_menu()
      } else if (choice == 2L) {
        open_data_url(
          "https://www.poliscidata.com/pages/journalReplicationData.php",
          "Journal Dataverse archives"
        )
      } else if (choice == 3L) {
        open_data_url(
          "https://www.poliscidata.com/pages/statisticalDataCollections.php",
          "web sites with topical collections"
        )
      } else if (choice == 4L) {
        run_subfield_data_menu()
      }
    }

    invisible(NULL)
  }
  
  get_current_settings <- function() {
    defaults <- rcpa4_default_options()
    verbose <- getOption("RCPA4.verbose", defaults$verbose)
    slow <- getOption("RCPA4.slow", defaults$slow)
    pause_seconds <- getOption("RCPA4.pause_seconds", defaults$pause_seconds)
    
    if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) verbose <- defaults$verbose
    if (!is.logical(slow) || length(slow) != 1L || is.na(slow)) slow <- defaults$slow
    if (!is.numeric(pause_seconds) || length(pause_seconds) != 1L || is.na(pause_seconds)) pause_seconds <- defaults$pause_seconds
    pause_seconds <- max(0, as.numeric(pause_seconds))
    
    list(
      verbose = isTRUE(verbose),
      slow = isTRUE(slow),
      pause_seconds = pause_seconds
    )
  }
  
  apply_settings <- function(current) {
    options(
      RCPA4.verbose = isTRUE(current$verbose),
      RCPA4.slow = isTRUE(current$slow),
      RCPA4.pause_seconds = max(0, as.numeric(current$pause_seconds))
    )
  }
  
  print_settings_status <- function(current) {
    cat("\nCurrent RCPA4 settings:\n")
    cat("  verbose:", current$verbose, "\n")
    cat("  slow:", current$slow, "\n")
    cat("  pause_seconds:", current$pause_seconds, "\n")
    
    cat("\nOptions:\n")
    if (isTRUE(current$verbose)) cat("  1. Turn verbose off\n")
    else cat("  1. Turn verbose on\n")
    if (isTRUE(current$slow)) cat("  2. Turn slow mode off\n")
    else cat("  2. Turn slow mode on\n")
    cat("  3. Add one second of pause\n")
    cat("  4. Decrease one second of pause\n")
    cat("  5. Restore default settings\n")
  }
  
  run_settings_menu <- function() {
    defaults <- rcpa4_default_options()
    current <- get_current_settings()
    
    repeat {
      print_settings_status(current)
      raw_choice <- ask("\nEnter 1-5, or press <Enter> to go back: ")
      if (length(raw_choice) == 0L) break
      choice <- trimws(raw_choice[1])
      if (!nzchar(choice)) break
      
      if (choice == "1") {
        current$verbose <- !isTRUE(current$verbose)
        cat("verbose is now", current$verbose, "\n")
      } else if (choice == "2") {
        current$slow <- !isTRUE(current$slow)
        cat("slow is now", current$slow, "\n")
      } else if (choice == "3") {
        current$pause_seconds <- max(0, as.numeric(current$pause_seconds) + 1)
        cat("pause_seconds is now", current$pause_seconds, "\n")
      } else if (choice == "4") {
        current$pause_seconds <- max(0, as.numeric(current$pause_seconds) - 1)
        cat("pause_seconds is now", current$pause_seconds, "\n")
      } else if (choice == "5") {
        current <- defaults
        cat("Settings restored to defaults.\n")
      } else {
        cat("Invalid choice. Please enter 1-5, or press <Enter> to go back.\n")
        next
      }
      
      apply_settings(current)
    }
    
    invisible(NULL)
  }
  
  run_clean_environment_menu <- function() {
    global_objects <- ls(envir = .GlobalEnv, all.names = FALSE)
    global_objects <- sort(unique(global_objects))

    if (length(global_objects) == 0L) {
      cat("\nYour global environment is already clean (no non-hidden objects found).\n")
    } else {
      cat("\nObjects in your global environment:\n")
      print_numbered_menu(global_objects, compact_threshold = 1000L)
    }

    cat("\nTo remove objects, run these commands at the R console:\n")
    cat("  One object:              rm(object_name)\n")
    cat("  Multiple objects:        rm(obj1_name, obj2_name)\n")
    cat("  All non-hidden objects:  rm(list = ls())\n")
    cat("Replace the placeholder names with your object names.\n")
    cat("No objects have been removed by helpC.\n")
    ask("Press <Enter> to go back: ")

    invisible(NULL)
  }

  run_graphics_reset_menu <- function() {
    devices <- grDevices::dev.list()
    if (is.null(devices) || length(devices) == 0L) {
      cat("\nNo graphics device is open. The next plot will use default graphical settings.\n")
      return(invisible(NULL))
    }

    active <- grDevices::dev.cur()
    active_number <- as.integer(active[1])
    active_name <- names(active)[1]
    if (!nzchar(active_name)) active_name <- "graphics device"

    confirm <- choose_number(
      1L,
      "This closes the active graphics device and any current plot. Enter 1 to proceed, or press <Enter> to exit: "
    )
    if (is.null(confirm)) return(invisible(NULL))

    tryCatch(
      grDevices::dev.off(which = active_number),
      error = function(e) stop("Could not close the active graphics device: ", conditionMessage(e), call. = FALSE)
    )
    cat("Closed the active graphics device. Later plots will use default graphical settings.\n")

    invisible(NULL)
  }

  run_chapter_resources_menu <- function() {
    chapter_titles <- c(
      "0" = "Getting Started with R",
      "1" = "Using R for Data Analysis",
      "2" = "Descriptive Statistics",
      "3" = "Creating and Transforming Variables",
      "4" = "Making Comparisons",
      "5" = "Graphing Relationships and Describing Patterns",
      "6" = "Random Assignment and Sampling",
      "7" = "Making Controlled Comparisons",
      "8" = "Foundations of Statistical Inference",
      "9" = "Hypothesis Tests with One or Two Samples",
      "10" = "Chi-Square Test and Analysis of Variance",
      "11" = "Correlation and Bivariate Regression",
      "12" = "Multiple Regression",
      "13" = "Analyzing Regression Residuals",
      "14" = "Logistic Regression",
      "15" = "Doing Your Own Political Analysis",
      "99" = "Advanced Topics"
    )

    repeat {
      cat("\nBrowse chapter resources:\n")
      for (chapter in names(chapter_titles)) {
        cat(" ", chapter, ". ", chapter_titles[[chapter]], "\n", sep = "")
      }

      choice <- ask("Enter chapter number, or press <Enter> to go back: ")
      if (length(choice) == 0L || !nzchar(trimws(choice[1]))) break
      choice <- trimws(choice[1])
      if (!(choice %in% names(chapter_titles))) {
        cat("Invalid chapter number. Enter 0 through 15, or 99.\n")
        next
      }

      chapter_id <- if (identical(choice, "0")) "I" else choice
      url <- paste0("https://www.poliscidata.com/pages/rDemosResources.php?chapter=", chapter_id)
      opened <- tryCatch({
        utils::browseURL(url)
        TRUE
      }, error = function(e) {
        cat("Could not open chapter resources:", conditionMessage(e), "\n")
        FALSE
      })
      if (opened) cat("Opened resources for ", chapter_titles[[choice]], ".\n", sep = "")
    }

    invisible(NULL)
  }
  
  exported_funs <- get_exported_functions()
  rcpa4_datasets <- c("anes", "debate", "gss", "states", "world")
  preselected_choice <- normalize_top_choice(option)
  
  cat("\nWelcome to RCPA4 helpC.\n")
  cat(wrap_console_text("I can help you find RCPA4 function and dataset documentation, run examples, browse chapter resources, open plot reference tools, check graphics support, find datasets in installed packages, change package settings, show workspace cleanup commands, and reset graphics."), "\n", sep = "")
  
  repeat {
    if (is.null(preselected_choice)) {
       cat("\nHow can I help?\n")
       cat("  1. Show help file for RCPA4 function\n")
       cat("  2. Explore RCPA4 datasets and variables\n")
       cat("  3. Show an example\n")
       cat("  4. Help with plots\n")
       cat("  5. Find data\n")
       cat("  6. Change RCPA4 package settings\n")
       cat("  7. Clean working environment\n")
       cat("  8. Reset graphical parameters\n")
       cat("  9. Browse chapter resources\n")
       
       choice <- choose_number(9L, "Enter 1-9, or press <Enter> to exit: ")
      if (is.null(choice)) break
    } else {
      choice <- preselected_choice
      preselected_choice <- NULL
    }
    
    if (choice == 1L) {
      fun_name <- choose_function(exported_funs)
      if (!is.null(fun_name)) {
        show_function_help(fun_name)
      }
      next
    }
    
    if (choice == 2L) {
      repeat {
        dataset_name <- choose_dataset(rcpa4_datasets)
        if (is.null(dataset_name)) break
        show_function_help(dataset_name)
        run_dataset_variable_explorer(dataset_name)
      }
      next
    }

    if (choice == 3L) {
      fun_name <- choose_function(exported_funs)
      if (!is.null(fun_name)) {
        run_function_examples(fun_name)
      }
      next
    }
    
    if (choice == 4L) {
      run_plot_help_menu()
      next
    }
    
    if (choice == 5L) {
      run_find_data_menu()
      next
    }
    
    if (choice == 6L) {
      run_settings_menu()
      next
    }
    
    if (choice == 7L) {
      run_clean_environment_menu()
      next
    }

    if (choice == 8L) {
      run_graphics_reset_menu()
      next
    }

    if (choice == 9L) {
      run_chapter_resources_menu()
      next
    }
  }
  
  invisible(NULL)
}
