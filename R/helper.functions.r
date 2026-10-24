##' @name helper.functions
##' @noRd
##' @title Functions that help user-facing functions but not called directly.


add_block <- function(blocks, obj) {
  blocks[[length(blocks) + 1]] <- obj
  return(blocks)
}

rcpa4_default_options <- function() {
  list(
    verbose = TRUE,
    slow = TRUE,
    pause_seconds = 2
  )
}

rcpa4_get_option <- function(name) {
  defaults <- rcpa4_default_options()
  getOption(paste0("RCPA4.", name), defaults[[name]])
}

wrap_console_text <- function(text,
                              width = NULL,
                              indent = 0,
                              exdent = 0,
                              prefix = "",
                              initial = "") {
  text <- paste(as.character(text), collapse = "")
  
  if (is.null(width)) width <- getOption("width", 80)
  if (!is.numeric(width) || length(width) != 1L || is.na(width) || !is.finite(width)) {
    width <- 80
  }
  width <- as.integer(width)
  if (width < 10L) width <- 10L
  
  parts <- strsplit(text, "\n", fixed = TRUE)[[1]]
  wrapped <- vapply(parts, function(part) {
    if (!nzchar(part)) return("")
    paste(
      strwrap(
        part,
        width = width,
        indent = indent,
        exdent = exdent,
        prefix = prefix,
        initial = initial
      ),
      collapse = "\n"
    )
  }, character(1))
  
  paste(wrapped, collapse = "\n")
}

format_p_value <- function(p, digits = 3) {
  if (length(digits) != 1L || is.na(digits) || !is.finite(digits) || digits < 0) {
    stop("`digits` must be a single non-negative number.", call. = FALSE)
  }

  p_digits <- max(2L, as.integer(digits))
  cutoff <- 10^(-p_digits)
  out <- rep("", length(p))
  keep <- !is.na(p) & is.finite(p)
  small <- keep & p < cutoff
  regular <- keep & !small

  cutoff_text <- sub("^0\\.", ".", format(cutoff, scientific = FALSE, trim = TRUE, nsmall = p_digits))
  out[small] <- paste0("<", cutoff_text)
  out[regular] <- sub(
    "^0\\.",
    ".",
    format(round(p[regular], p_digits), scientific = FALSE, trim = TRUE, nsmall = p_digits)
  )
  out
}


weighted_crosstab <- function(dep,
                              indep,
                              w = NULL,
                              dep_levels = NULL,
                              indep_levels = NULL,
                              dnn = c("Dependent", "Independent")) {
  if (length(dep) != length(indep)) {
    stop("`dep` and `indep` must have the same length.", call. = FALSE)
  }
  
  if (is.null(w)) {
    w <- rep(1, length(dep))
  }
  if (length(w) != length(dep)) {
    stop("`w` must have the same length as `dep` and `indep`.", call. = FALSE)
  }
  
  keep <- stats::complete.cases(dep, indep, w)
  dep <- dep[keep]
  indep <- indep[keep]
  w <- as.numeric(w[keep])
  
  if (is.null(dep_levels)) {
    dep_levels <- if (is.factor(dep)) levels(dep) else sort(unique(dep))
  }
  if (is.null(indep_levels)) {
    indep_levels <- if (is.factor(indep)) levels(indep) else sort(unique(indep))
  }
  
  dep_fac <- factor(dep, levels = dep_levels, ordered = is.ordered(dep))
  indep_fac <- factor(indep, levels = indep_levels, ordered = is.ordered(indep))
  
  tab <- stats::xtabs(w ~ dep_fac + indep_fac, drop.unused.levels = FALSE)
  if (length(dnn) == 2L) names(dimnames(tab)) <- dnn
  
  col_totals <- colSums(tab)
  prop_col <- matrix(NA_real_, nrow = nrow(tab), ncol = ncol(tab),
                     dimnames = dimnames(tab))
  nonzero_cols <- col_totals > 0
  if (any(nonzero_cols)) {
    prop_col[, nonzero_cols] <- sweep(tab[, nonzero_cols, drop = FALSE], 2, col_totals[nonzero_cols], "/")
  }
  
  cs <- setNames(as.numeric(col_totals), colnames(tab))
  
  list(
    tab = tab,
    prop.col = prop_col,
    cs = cs,
    total.n = sum(tab)
  )
}


weighted_freq_table <- function(x, w = NULL, include_total = TRUE, include_cum = NULL) {
  if (is.null(w)) {
    w <- rep(1, length(x))
  }
  if (length(w) != length(x)) {
    stop("`w` must have the same length as `x`.", call. = FALSE)
  }
  
  keep <- stats::complete.cases(x, w)
  x <- x[keep]
  w <- as.numeric(w[keep])
  
  if (is.null(include_cum)) include_cum <- is.ordered(x)
  
  x_levels <- if (is.factor(x)) levels(x) else sort(unique(x))
  x_fac <- factor(x, levels = x_levels, ordered = is.ordered(x))
  
  counts_tab <- stats::xtabs(w ~ x_fac, drop.unused.levels = FALSE)
  counts <- as.numeric(counts_tab)
  names(counts) <- levels(x_fac)
  
  total_n <- sum(counts)
  percent <- if (total_n > 0) (counts / total_n) * 100 else rep(NA_real_, length(counts))
  
  out <- data.frame(
    Frequency = counts,
    Percent = percent,
    row.names = names(counts),
    check.names = FALSE
  )
  
  if (isTRUE(include_cum)) {
    out$`Cumulative` <- cumsum(percent)
  }
  
  if (isTRUE(include_total)) {
    total_vals <- rep(NA_real_, ncol(out))
    names(total_vals) <- names(out)
    total_vals["Frequency"] <- total_n
    total_vals["Percent"] <- if (total_n > 0) 100 else NA_real_
    if ("Cumulative" %in% names(out)) total_vals["Cumulative"] <- NA_real_
    out <- rbind(out, as.data.frame(as.list(total_vals), row.names = "Total", check.names = FALSE))
  }
  
  out
}


weighted_cor_pair <- function(x, y, w) {
  keep <- stats::complete.cases(x, y, w)
  x <- as.numeric(x[keep])
  y <- as.numeric(y[keep])
  w <- as.numeric(w[keep])
  
  if (length(w) == 0L) {
    return(list(correlation = NA_real_, std.err = NA_real_, t.value = NA_real_,
                p.value = NA_real_, n = 0L, n.eff = NA_real_))
  }
  
  if (any(!is.finite(w))) {
    stop("Weights must be finite.", call. = FALSE)
  }
  if (any(w < 0)) {
    stop("Weights must be non-negative.", call. = FALSE)
  }
  
  w_sum <- sum(w)
  if (!is.finite(w_sum) || w_sum <= 0) {
    return(list(correlation = NA_real_, std.err = NA_real_, t.value = NA_real_,
                p.value = NA_real_, n = length(w), n.eff = NA_real_))
  }
  
  x_center <- x - sum(w * x) / w_sum
  y_center <- y - sum(w * y) / w_sum
  
  var_x <- sum(w * x_center^2)
  var_y <- sum(w * y_center^2)
  
  if (var_x <= 0 || var_y <= 0) {
    correlation <- NA_real_
  } else {
    correlation <- sum(w * x_center * y_center) / sqrt(var_x * var_y)
    correlation <- max(min(correlation, 1), -1)
  }
  
  w_sq_sum <- sum(w^2)
  n_eff <- if (w_sq_sum > 0) (w_sum^2) / w_sq_sum else NA_real_
  
  if (!is.finite(correlation) || !is.finite(n_eff) || n_eff <= 2) {
    std.err <- NA_real_
    t.value <- NA_real_
    p.value <- NA_real_
  } else if (abs(correlation) >= 1) {
    std.err <- 0
    t.value <- if (correlation >= 0) Inf else -Inf
    p.value <- 0
  } else {
    std.err <- sqrt((1 - correlation^2) / (n_eff - 2))
    if (!is.finite(std.err) || std.err <= 0) {
      t.value <- if (correlation >= 0) Inf else -Inf
      p.value <- 0
    } else {
      t.value <- correlation / std.err
      p.value <- 2 * stats::pt(-abs(t.value), df = n_eff - 2)
    }
  }
  
  list(
    correlation = correlation,
    std.err = std.err,
    t.value = t.value,
    p.value = p.value,
    n = length(w),
    n.eff = n_eff
  )
}


weighted_cor <- function(x, y = NULL, weight = NULL, ...) {
  if (is.null(y)) {
    if (is.data.frame(x)) x <- as.matrix(x)
    if (!is.matrix(x)) stop("For multivariate input, `x` must be a matrix or data frame.", call. = FALSE)
    if (!is.numeric(x)) stop("All variables in `x` must be numeric.", call. = FALSE)
    
    n <- nrow(x)
    p <- ncol(x)
    if (is.null(weight)) weight <- rep(1, n)
    if (!is.numeric(weight) || length(weight) != n) {
      stop("Weights must be numeric and have same length as variables.", call. = FALSE)
    }
    
    correlation <- matrix(NA_real_, nrow = p, ncol = p)
    std.err <- matrix(NA_real_, nrow = p, ncol = p)
    t.value <- matrix(NA_real_, nrow = p, ncol = p)
    p.value <- matrix(NA_real_, nrow = p, ncol = p)
    n.eff <- matrix(NA_real_, nrow = p, ncol = p)
    
    for (i in seq_len(p)) {
      for (j in i:p) {
        stats_ij <- weighted_cor_pair(x[, i], x[, j], weight)
        correlation[i, j] <- stats_ij$correlation
        correlation[j, i] <- stats_ij$correlation
        std.err[i, j] <- stats_ij$std.err
        std.err[j, i] <- stats_ij$std.err
        t.value[i, j] <- stats_ij$t.value
        t.value[j, i] <- stats_ij$t.value
        p.value[i, j] <- stats_ij$p.value
        p.value[j, i] <- stats_ij$p.value
        n.eff[i, j] <- stats_ij$n.eff
        n.eff[j, i] <- stats_ij$n.eff
      }
    }
    
    if (!is.null(colnames(x))) {
      rownames(correlation) <- colnames(correlation) <- colnames(x)
      rownames(std.err) <- colnames(std.err) <- colnames(x)
      rownames(t.value) <- colnames(t.value) <- colnames(x)
      rownames(p.value) <- colnames(p.value) <- colnames(x)
      rownames(n.eff) <- colnames(n.eff) <- colnames(x)
    }
    
    return(list(
      correlation = correlation,
      std.err = std.err,
      t.value = t.value,
      p.value = p.value,
      n.eff = n.eff
    ))
  }
  
  if (is.data.frame(x) || is.matrix(x)) x <- x[, 1]
  if (is.data.frame(y) || is.matrix(y)) y <- y[, 1]
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("`x` and `y` must be numeric.", call. = FALSE)
  }
  if (length(x) != length(y)) {
    stop("`x` and `y` must have the same length.", call. = FALSE)
  }
  
  if (is.null(weight)) weight <- rep(1, length(x))
  if (!is.numeric(weight) || length(weight) != length(x)) {
    stop("Weights must be numeric and have same length as variables.", call. = FALSE)
  }
  
  stats_xy <- weighted_cor_pair(x, y, weight)
  out <- matrix(
    c(stats_xy$correlation, stats_xy$std.err, stats_xy$t.value, stats_xy$p.value),
    nrow = 1
  )
  colnames(out) <- c("correlation", "std.err", "t.value", "p.value")
  out
}


wtd_hist_internal <- function(x, weight = NULL, breaks = "Sturges", plot = TRUE, ...) {
  if (!is.numeric(x)) stop("`x` must be numeric.", call. = FALSE)
  if (is.null(weight)) weight <- rep(1, length(x))
  if (!is.numeric(weight) || length(weight) != length(x)) {
    stop("`weight` must be numeric and have same length as `x`.", call. = FALSE)
  }
  
  keep <- stats::complete.cases(x, weight)
  x <- as.numeric(x[keep])
  weight <- as.numeric(weight[keep])
  
  if (length(x) == 0L) stop("No finite, non-missing observations available.", call. = FALSE)
  if (any(!is.finite(weight))) stop("Weights must be finite.", call. = FALSE)
  if (any(weight < 0)) stop("Weights must be non-negative.", call. = FALSE)
  if (sum(weight) <= 0) stop("Weights must sum to a positive value.", call. = FALSE)
  if (is.null(breaks)) breaks <- "Sturges"
  
  dots <- list(...)
  dot_names <- names(dots)
  has_dot <- function(nm) !is.null(dot_names) && (nm %in% dot_names)
  right <- if (has_dot("right")) as.logical(dots$right)[1] else TRUE
  include.lowest <- if (has_dot("include.lowest")) as.logical(dots$include.lowest)[1] else TRUE
  fuzz <- if (has_dot("fuzz")) as.numeric(dots$fuzz)[1] else 1e-07
  
  # Compute breaks the same way hist() does, then apply weighted bin counts.
  base_hist <- graphics::hist(
    x = x,
    breaks = breaks,
    plot = FALSE,
    right = right,
    include.lowest = include.lowest,
    fuzz = fuzz
  )
  
  nbins <- length(base_hist$breaks) - 1L
  widths <- diff(base_hist$breaks)
  n_breaks <- length(base_hist$breaks)
  diddle <- fuzz * if (n_breaks > 5L) {
    stats::median(widths)
  } else if (n_breaks <= 3L) {
    diff(range(x))
  } else {
    min(widths[widths > 0])
  }
  fuzz_vec <- if (right) {
    c(if (include.lowest) -diddle else diddle, rep.int(diddle, n_breaks - 1L))
  } else {
    c(rep.int(-diddle, n_breaks - 1L), if (include.lowest) diddle else -diddle)
  }
  fuzzy_breaks <- base_hist$breaks + fuzz_vec
  bins <- .bincode(x, breaks = fuzzy_breaks, right = right, include.lowest = include.lowest)

  counts <- numeric(nbins)
  if (any(bins > 0L)) {
    sums <- tapply(weight[bins > 0L], bins[bins > 0L], sum)
    counts[as.integer(names(sums))] <- as.numeric(sums)
  }
  
  total_w <- sum(counts)
  density <- if (total_w > 0) counts / (total_w * widths) else rep(NA_real_, nbins)
  
  out <- list(
    breaks = base_hist$breaks,
    counts = counts,
    density = density,
    mids = base_hist$mids,
    xname = base_hist$xname,
    equidist = base_hist$equidist
  )
  class(out) <- "histogram"
  
  if (isTRUE(plot)) {
    plot_args <- dots
    plot_args$right <- NULL
    plot_args$include.lowest <- NULL
    plot_args$fuzz <- NULL
    do.call(graphics::plot, c(list(x = out), plot_args))
  }
  
  out
}



## Internal input validation helpers -----------------------------------------
#' Assert numeric values are between 0 and 1 (inclusive)
#'
#' @param ... Values to validate.
#' @keywords internal
#' @noRd
assert_between_0_1 <- function(...) {
  exprs <- as.list(substitute(list(...)))[-1L]
  vals <- list(...)
  labels <- vapply(exprs, deparse1, character(1))
  
  if (length(vals) == 0L) {
    stop("No arguments supplied to assert_between_0_1().", call. = FALSE)
  }
  
  for (i in seq_along(vals)) {
    x <- vals[[i]]
    nm <- labels[[i]]
    
    if (!is.numeric(x) || any(is.na(x)) || any(!is.finite(x))) {
      stop(nm, " must be numeric with finite, non-missing value(s).", call. = FALSE)
    }
    if (any(x < 0) || any(x > 1)) {
      stop(nm, " must be between 0 and 1 (inclusive).", call. = FALSE)
    }
  }
  
  invisible(TRUE)
}

#' Assert numeric values are strictly positive
#'
#' @param ... Values to validate.
#' @keywords internal
#' @noRd
assert_positive_numeric <- function(...) {
  exprs <- as.list(substitute(list(...)))[-1L]
  vals <- list(...)
  labels <- vapply(exprs, deparse1, character(1))
  
  if (length(vals) == 0L) {
    stop("No arguments supplied to assert_positive_numeric().", call. = FALSE)
  }
  
  for (i in seq_along(vals)) {
    x <- vals[[i]]
    nm <- labels[[i]]
    
    if (!is.numeric(x) || any(is.na(x)) || any(!is.finite(x)) || any(x <= 0)) {
      stop(nm, " must be numeric with finite, non-missing value(s) > 0.", call. = FALSE)
    }
  }
  
  invisible(TRUE)
}

#' Assert scalar positive integers
#'
#' @param ... Values to validate.
#' @keywords internal
#' @noRd
assert_positive_integer <- function(...) {
  exprs <- as.list(substitute(list(...)))[-1L]
  vals <- list(...)
  labels <- vapply(exprs, deparse1, character(1))
  
  if (length(vals) == 0L) {
    stop("No arguments supplied to assert_positive_integer().", call. = FALSE)
  }
  
  for (i in seq_along(vals)) {
    x <- vals[[i]]
    nm <- labels[[i]]
    
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x <= 0 || x %% 1 != 0) {
      stop(nm, " must be a single positive integer.", call. = FALSE)
    }
  }
  
  invisible(TRUE)
}

#' Assert scalar non-negative integers
#'
#' @param ... Values to validate.
#' @keywords internal
#' @noRd
assert_nonnegative_integer <- function(...) {
  exprs <- as.list(substitute(list(...)))[-1L]
  vals <- list(...)
  labels <- vapply(exprs, deparse1, character(1))
  
  if (length(vals) == 0L) {
    stop("No arguments supplied to assert_nonnegative_integer().", call. = FALSE)
  }
  
  for (i in seq_along(vals)) {
    x <- vals[[i]]
    nm <- labels[[i]]
    
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) || x < 0 || x %% 1 != 0) {
      stop(nm, " must be a single non-negative integer.", call. = FALSE)
    }
  }
  
  invisible(TRUE)
}


#' Assert required arguments are supplied
#'
#' @param ... Arguments to check.
#' @keywords internal
#' @noRd
assert_required <- function(...) {
  exprs <- as.list(substitute(list(...)))[-1L]
  labels <- vapply(exprs, deparse1, character(1))
  target_env <- parent.frame()

  is_missing <- vapply(exprs, function(sym) {
    if (!is.symbol(sym)) return(FALSE)
    eval(bquote(missing(.(sym))), envir = target_env)
  }, logical(1))

  miss <- labels[is_missing]
  if (length(miss)) {
    stop(
      "Ooops, you're missing required argument(s): ",
      paste(miss, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Assert variables are numeric (allows NA values)
#'
#' @param ... Values to validate.
#' @keywords internal
#' @noRd
assert_numeric <- function(...) {
  exprs <- as.list(substitute(list(...)))[-1L]
  vals <- list(...)
  labels <- vapply(exprs, deparse1, character(1))
  
  if (length(vals) == 0L) {
    stop("No arguments supplied to assert_numeric().", call. = FALSE)
  }
  
  for (i in seq_along(vals)) {
    x <- vals[[i]]
    nm <- labels[[i]]
    
    if (length(x) == 0L) {
      stop(nm, " has no values.", call. = FALSE)
    }
    
    is_num_like <- is.numeric(x) || is.logical(x) || Hmisc::all.is.numeric(x, extras = c(NA))
    if (!is_num_like) {
      stop(nm, " must be numeric.", call. = FALSE)
    }
  }
  
  invisible(TRUE)
}

#' Suggest whether variable looks categorical by number of unique values
#'
#' @param ... Values to evaluate.
#' @param max_unique Maximum distinct non-missing values before warning.
#' @keywords internal
#' @noRd
suggest_categorical <- function(..., max_unique = 12) {
  exprs <- as.list(substitute(list(...)))[-1L]
  vals <- list(...)
  labels <- vapply(exprs, deparse1, character(1))
  
  if (!is.numeric(max_unique) || length(max_unique) != 1L || is.na(max_unique) || max_unique < 1 || max_unique %% 1 != 0) {
    stop("max_unique must be a single positive integer.", call. = FALSE)
  }
  
  if (length(vals) == 0L) {
    stop("No arguments supplied to suggest_categorical().", call. = FALSE)
  }
  
  for (i in seq_along(vals)) {
    x <- vals[[i]]
    nm <- labels[[i]]
    n_unique <- length(unique(stats::na.omit(x)))
    
    if (n_unique > max_unique) {
      warning(
        "Suggestion: ", nm, " has ", n_unique, " unique values. ",
        "Consider a method for more continuous variables.",
        call. = FALSE
      )
    }
  }
  
  invisible(TRUE)
}




#' Check value and generate custom message about problems
#' @param msg Prompt to show user
#' @param con Connect (default: standard console input)
#' @noRd
#' @keywords internal
ask <- function(msg = "Press <Enter> to continue: ", con = stdin()) 
{
  cat(msg)
  ans <- readLines(con = con, n = 1)
  if (length(ans) == 0L) return("")
  ans[[1]]
}


wrap_axis_labels <- function(labels, width) {
  labels <- as.character(labels)
  width <- max(1L, as.integer(width))
  vapply(labels, function(lbl) {
    parts <- strwrap(lbl, width = width)
    if (length(parts) == 0L) return("")
    paste(parts, collapse = "\n")
  }, character(1))
}


axis_label_layout_cfg <- function(labels,
                                  n_slots = length(labels),
                                  mar_left = 4.1,
                                  mar_top = 3.6,
                                  mar_right = 1.1,
                                  long_label_message = NULL,
                                  allow_wrap = TRUE,
                                  allow_angled45 = TRUE,
                                  allow_vertical = TRUE,
                                  cex_candidates = c(0.9, 0.85, 0.8, 0.75),
                                  wrap_width_candidates = c(30, 24, 20, 16, 12),
                                  fit_fraction = 0.9) {
  labels <- as.character(labels)
  if (length(labels) == 0L) {
    return(list(
      labels = character(0),
      cex = 0.9,
      las = 1,
      mar = c(4.1, mar_left, mar_top, mar_right),
      mtext = 2.5,
      method = "horizontal",
      notify = FALSE,
      message = NULL
    ))
  }
  
  slot_count <- as.integer(n_slots)
  if (!is.finite(slot_count) || slot_count <= 0L) slot_count <- length(labels)
  slot_count <- max(1L, slot_count)
  
  pin <- graphics::par("pin")
  slot_width <- suppressWarnings(as.numeric(pin[1]) / slot_count)
  if (!is.finite(slot_width) || slot_width <= 0) slot_width <- Inf
  
  label_metrics <- function(lbls, cex) {
    split_lines <- strsplit(lbls, "\n", fixed = TRUE)
    widths <- vapply(split_lines, function(parts) {
      if (length(parts) == 0L) return(0)
      suppressWarnings(max(graphics::strwidth(parts, units = "inches", cex = cex), na.rm = TRUE))
    }, numeric(1))
    heights <- vapply(split_lines, function(parts) {
      if (length(parts) == 0L) return(0)
      suppressWarnings(sum(graphics::strheight(parts, units = "inches", cex = cex), na.rm = TRUE))
    }, numeric(1))
    list(
      widths = widths,
      heights = heights,
      max_width = suppressWarnings(max(widths, na.rm = TRUE)),
      max_height = suppressWarnings(max(heights, na.rm = TRUE))
    )
  }
  
  max_lines <- function(lbls) {
    split_lines <- strsplit(lbls, "\n", fixed = TRUE)
    max(vapply(split_lines, length, integer(1)))
  }
  
  fits_slots <- function(lbls, cex) {
    metrics <- label_metrics(lbls, cex)
    is.finite(metrics$max_width) && (metrics$max_width <= slot_width * fit_fraction)
  }
  
  fits_angled45 <- function(lbls, cex) {
    metrics <- label_metrics(lbls, cex)
    angle <- pi / 4
    projected <- metrics$widths * cos(angle) + metrics$heights * sin(angle)
    max_projected <- suppressWarnings(max(projected, na.rm = TRUE))
    is.finite(max_projected) && (max_projected <= slot_width * fit_fraction)
  }
  
  cfg <- NULL
  
  for (cex in cex_candidates) {
    if (fits_slots(labels, cex)) {
      cfg <- list(labels = labels, cex = cex, las = 1, method = "horizontal")
      break
    }
  }
  
  if (is.null(cfg) && isTRUE(allow_wrap)) {
    for (cex in cex_candidates) {
      for (wrap_width in wrap_width_candidates) {
        wrapped <- wrap_axis_labels(labels, width = wrap_width)
        if (fits_slots(wrapped, cex)) {
          cfg <- list(labels = wrapped, cex = cex, las = 1, method = "wrapped")
          break
        }
      }
      if (!is.null(cfg)) break
    }
  }
  
  if (is.null(cfg) && isTRUE(allow_angled45)) {
    for (cex in cex_candidates) {
      if (fits_angled45(labels, cex)) {
        cfg <- list(labels = labels, cex = cex, las = 1, method = "angled45")
        break
      }
    }
  }
  
  if (is.null(cfg) && isTRUE(allow_vertical)) {
    cex <- tail(cex_candidates, 1)
    cfg <- list(labels = labels, cex = cex, las = 2, method = "vertical")
  }
  
  if (is.null(cfg)) {
    cfg <- list(labels = labels, cex = tail(cex_candidates, 1), las = 1, method = "horizontal")
  }
  
  line_height_in <- suppressWarnings(as.numeric(graphics::par("csi"))[1])
  if (!is.finite(line_height_in) || line_height_in <= 0) line_height_in <- 0.2
  
  if (identical(cfg$method, "horizontal")) {
    mar_bottom <- 4.1
    mtext_lines <- 2.5
  } else if (identical(cfg$method, "wrapped")) {
    n_lines <- max_lines(cfg$labels)
    mar_bottom <- 4.1 + 0.9 * (n_lines - 1)
    mtext_lines <- max(2.5, mar_bottom - 1.2)
  } else if (identical(cfg$method, "angled45")) {
    metrics <- label_metrics(cfg$labels, cfg$cex)
    angle <- pi / 4
    projected_height <- metrics$widths * sin(angle) + metrics$heights * cos(angle)
    max_projected_height <- suppressWarnings(max(projected_height, na.rm = TRUE))
    mar_bottom <- 3.8 + (max_projected_height / line_height_in)
    mtext_lines <- max(2.5, mar_bottom - 1.1)
  } else {
    max_chars <- max(nchar(labels), na.rm = TRUE)
    mar_bottom <- 4.1 + sqrt(max_chars + 1)
    mtext_lines <- max(2.5, mar_bottom - 1.5)
  }
  
  mar_bottom <- min(max(mar_bottom, 4.1), 12)
  notify <- !identical(cfg$method, "horizontal") && !is.null(long_label_message) && nzchar(long_label_message)
  
  list(
    labels = cfg$labels,
    cex = cfg$cex,
    las = cfg$las,
    mar = c(mar_bottom, mar_left, mar_top, mar_right),
    mtext = mtext_lines,
    method = cfg$method,
    notify = notify,
    message = if (isTRUE(notify)) long_label_message else NULL
  )
}


draw_x_axis_labels <- function(at, cfg, side = 1, draw_axis = TRUE, draw_ticks = TRUE, ...) {
  if (!identical(side, 1)) {
    graphics::axis(side = side, at = at, labels = cfg$labels, cex.axis = cfg$cex, las = cfg$las, ...)
    return(invisible(NULL))
  }
  
  if (identical(cfg$method, "angled45")) {
    if (isTRUE(draw_axis)) {
      axis_args <- c(
        list(side = side, at = at, labels = FALSE),
        if (!isTRUE(draw_ticks)) list(tck = 0, lwd.ticks = 0) else list(),
        list(...)
      )
      do.call(graphics::axis, axis_args)
    }
    old_xpd <- graphics::par("xpd")
    on.exit(graphics::par(xpd = old_xpd), add = TRUE)
    graphics::par(xpd = NA)
    usr <- graphics::par("usr")
    pin <- graphics::par("pin")
    y_per_in <- suppressWarnings(diff(usr[3:4]) / as.numeric(pin[2]))
    if (!is.finite(y_per_in) || y_per_in <= 0) y_per_in <- 1
    line_h_user <- graphics::par("csi") * cfg$cex * y_per_in
    y_top <- usr[3] - if (isTRUE(draw_axis)) 0.35 * line_h_user else 0.2 * line_h_user
    graphics::text(
      x = at,
      y = y_top,
      labels = cfg$labels,
      srt = 45,
      adj = c(1, 1),
      cex = cfg$cex
    )
  } else if (identical(cfg$method, "wrapped")) {
    if (isTRUE(draw_axis)) {
      axis_args <- c(
        list(side = side, at = at, labels = FALSE),
        if (!isTRUE(draw_ticks)) list(tck = 0, lwd.ticks = 0) else list(),
        list(...)
      )
      do.call(graphics::axis, axis_args)
    }
    old_xpd <- graphics::par("xpd")
    on.exit(graphics::par(xpd = old_xpd), add = TRUE)
    graphics::par(xpd = NA)
    
    usr <- graphics::par("usr")
    pin <- graphics::par("pin")
    y_per_in <- suppressWarnings(diff(usr[3:4]) / as.numeric(pin[2]))
    if (!is.finite(y_per_in) || y_per_in <= 0) y_per_in <- 1
    line_h_user <- graphics::par("csi") * cfg$cex * y_per_in
    y_top <- usr[3] - if (isTRUE(draw_axis)) 0.55 * line_h_user else 0.35 * line_h_user
    
    graphics::text(
      x = at,
      y = y_top,
      labels = cfg$labels,
      adj = c(0.5, 1),
      cex = cfg$cex
    )
  } else {
    graphics::axis(side = side, at = at, labels = cfg$labels, cex.axis = cfg$cex, las = cfg$las, ...)
  }
  invisible(NULL)
}


capture_varname <- function(expr) {
  deparse(substitute(expr))
}

friendly_expr_label <- function(expr) {
  if (is.symbol(expr)) {
    return(as.character(expr))
  }
  
  if (is.call(expr)) {
    fn <- as.character(expr[[1]])
    
    # data$var -> var
    if (fn == "$" && length(expr) == 3L) {
      rhs <- expr[[3]]
      if (is.symbol(rhs)) return(as.character(rhs))
      if (is.character(rhs) && length(rhs) == 1L) return(rhs)
    }
    
    # data[["var"]] -> var
    if (fn == "[[" && length(expr) == 3L) {
      rhs <- expr[[3]]
      if (is.character(rhs) && length(rhs) == 1L) return(rhs)
      if (is.symbol(rhs)) return(as.character(rhs))
    }
    
    # data[, "var"] -> var
    if (fn == "[" && length(expr) >= 3L) {
      rhs <- expr[[3]]
      if (is.character(rhs) && length(rhs) == 1L) return(rhs)
    }
  }
  
  paste(deparse(expr), collapse = "")
}



#' Calculates Cramer's V
#' @param chi A number equal to the Chi-Squared statistic
#' @param r A number equal to the number of rows
#' @param c A number equal to the number of columns
#' @param n A number equal to the sample size
#' @return The Cramer's V statistic, a number between 0 and 1.
#' @description Calculates Cramer's V, a measure of association to gauge the strength of the relationship between two nominal-level variables.  
#'   A score of 0 indicates no relationship; a score of 1 indicates a perfect relationship.
#' @examples 
#'   library(RCPA4)
#'   
#'   CramersV(84.18, 2, 2, 1315)
#'   CramersV(chi=84.18, r=2, c=2, n=1315)
#' @export
#' @keywords internal
#'
CramersV = function(chi, r, c, n)
{
  r1 = r-1
  c1 = c-1
  minr1c1 = min(r1, c1)
  denom = n * minr1c1
  V = sqrt(chi/denom)
  return(V)
}



eval_data_arg <- function(data_expr, env = parent.frame()) {
  if (is.null(data_expr)) return(NULL)
  
  data_val <- eval(data_expr, envir = env)
  
  if (is.null(data_val)) return(NULL)
  
  if (is.matrix(data_val)) data_val <- as.data.frame(data_val)
  
  # Allow data.frame, list (named columns), or environment
  if (is.data.frame(data_val) || is.list(data_val) || is.environment(data_val)) {
    return(data_val)
  }
  
  stop("`data` must evaluate to a data.frame, list, matrix, or environment.")
}


eval_in_data_expr <- function(expr, data = NULL, env = parent.frame()) {
  if (is.null(data)) {
    return(eval(expr, envir = env))
  }
  
  if (is.matrix(data)) data <- as.data.frame(data)
  
  # If data is already an environment, use it directly as the evaluation environment
  if (is.environment(data)) {
    return(eval(expr, envir = data, enclos = env))
  }
  
  # Otherwise data is list/data.frame mask (like base::with)
  eval(expr, envir = data, enclos = env)
}


eval_varlist_expr <- function(x_expr, data = NULL, env = parent.frame(), strings_as_factors = TRUE) {
  
  # ---- CASE A: user typed list(...) or c(...) with expressions ----
  if (is.call(x_expr) && length(x_expr) > 1) {
    head <- x_expr[[1]]
    if (is.symbol(head) && as.character(head) %in% c("list", "c")) {
      
      exprs <- as.list(x_expr[-1])
      
      # Preserve names if provided: list(a=var1, b=var2)
      supplied_names <- names(exprs)
      has_supplied <- !is.null(supplied_names) && any(nzchar(supplied_names))
      
      # Build labels:
      # - use supplied names when present/nonempty
      # - otherwise use deparse(expression)
      labels <- vapply(seq_along(exprs), function(i) {
        nm <- if (has_supplied) supplied_names[i] else ""
        if (!is.null(nm) && nzchar(nm)) nm else friendly_expr_label(exprs[[i]])
      }, character(1))
      
      vals <- lapply(exprs, function(e) eval_in_data_expr(e, data = data, env = env))
      
      df <- as.data.frame(vals, stringsAsFactors = strings_as_factors)
      colnames(df) <- labels
      
      return(list(df = df, names = labels, exprs = exprs))
    }
  }
  
  # ---- Otherwise evaluate x_expr as a single object ----
  x_val <- eval_in_data_expr(x_expr, data = data, env = env)
  # A bare column name denotes its values, not a vector of column selectors.
  is_data_column <- !is.null(data) && is.symbol(x_expr) &&
    if (is.environment(data)) {
      exists(as.character(x_expr), envir = data, inherits = FALSE)
    } else {
      as.character(x_expr) %in% names(data)
    }
  
  # ---- CASE B: x is already a data.frame / matrix: use its columns ----
  if (is.matrix(x_val)) x_val <- as.data.frame(x_val)
  
  if (is.data.frame(x_val)) {
    nm <- colnames(x_val)
    if (is.null(nm) || !all(nzchar(nm))) nm <- paste0("V", seq_len(ncol(x_val)))
    colnames(x_val) <- nm
    return(list(df = x_val, names = nm, exprs = NULL))
  }
  
  # ---- CASE C: x evaluates to a character selector, with data provided ----
  if (!is.null(data) && !is_data_column && is.character(x_val)) {
    if (is.environment(data)) {
      # if data is env, select objects by name and build df
      missing_vars <- setdiff(x_val, ls(envir = data, all.names = TRUE))
      if (length(missing_vars)) stop("Unknown variable(s) in `x`: ", paste(missing_vars, collapse = ", "))
      vals <- mget(x_val, envir = data, inherits = TRUE)
      df <- as.data.frame(vals, stringsAsFactors = strings_as_factors)
      colnames(df) <- x_val
      return(list(df = df, names = x_val, exprs = NULL))
    } else {
      # list/data.frame mask
      missing_vars <- setdiff(x_val, names(data))
      if (length(missing_vars)) stop("Unknown variable(s) in `x`: ", paste(missing_vars, collapse = ", "))
      df <- as.data.frame(data[x_val], stringsAsFactors = strings_as_factors)
      colnames(df) <- x_val
      return(list(df = df, names = x_val, exprs = NULL))
    }
  }
  
  # ---- CASE D: x evaluates to numeric selector, with data provided ----
  if (!is.null(data) && !is_data_column && is.numeric(x_val)) {
    
    # Only treat as column indices if it *looks like* indices:
    # - no NA
    # - integer-valued
    # - length is plausible for columns (<= ncol)
    # - all within [1, ncol]
    if (!is.environment(data)) {
      p <- length(data)  # number of columns in data.frame/list
      
      looks_like_indices <-
        length(x_val) >= 1 &&
        length(x_val) <= p &&
        all(!is.na(x_val)) &&
        all(is.finite(x_val)) &&
        all(x_val == as.integer(x_val)) &&
        all(x_val >= 1L & x_val <= p)
      
      if (looks_like_indices) {
        idx <- as.integer(x_val)
        nm <- names(data)[idx]
        if (is.null(nm) || !all(nzchar(nm))) nm <- paste0("V", idx)
        df <- as.data.frame(data[idx], stringsAsFactors = strings_as_factors)
        colnames(df) <- nm
        return(list(df = df, names = nm, exprs = NULL))
      }
    }
  }
  
  # ---- CASE E: evaluated to list-of-columns (your current case) ----
  if (is.list(x_val) && !is.data.frame(x_val)) {
    nm <- names(x_val)
    if (is.null(nm) || !all(nzchar(nm))) nm <- paste0("V", seq_along(x_val))
    df <- as.data.frame(x_val, stringsAsFactors = strings_as_factors)
    colnames(df) <- nm
    return(list(df = df, names = nm, exprs = NULL))
  }
  
  # ---- CASE F: single variable/expression ----
  label <- friendly_expr_label(x_expr)
  df <- as.data.frame(list(x_val), stringsAsFactors = strings_as_factors)
  colnames(df) <- label
  return(list(df = df, names = label, exprs = list(x_expr)))
}



#' Calculates model fit statistics for a svyglm linear regression model
#' @param svyglm An object of type svyglm. This object is the output of the svyglm function as well as regC function with w argument.
#' @param digits Number of digits to report after decimal place, optional (default = 3).
#' @return Returns a numeric vector of R-Squared and Adjusted R-sSquared statistics.
#' @description Model fit statistics for a svyglm weighted linear regression model (not for weighted logistic regression).
#' @details With weighted logistic regression, McFadden's R2 is the same as the R-squared from this function, and adjusted R-squared is generally not used.
#' @examples 
#'    library(RCPA4)
#'    
#'    \donttest{
#'    ft.police.model <- regC(ft_police ~ r_self_idntfd_race, w=wt, data=anes)
#'    fit_svyglm(ft.police.model)
#'    }
#' @export
#' @importFrom methods is
#' @keywords internal
fit_svyglm <- function(svyglm, digits=3)
{
  assert_required(svyglm)
  if(methods::is(svyglm, "svyglm")!=TRUE) message("Warning: Not a svyglm object.")
  
  r2 = (svyglm$null.deviance - svyglm$deviance) / svyglm$null.deviance 
  adjust = svyglm$df.null / svyglm$df.residual
  value  = 1 - ((1-r2) * adjust)
  results = c(R2=round(r2, digits), adjR2=round(value, digits))
  names(results) = c("R-Squared","     Adjusted R-Squared")
  return(results)
}

#' Get and return user's operating system type ("mac", "windows", or "linux")
#' @noRd
#' @keywords internal
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "mac"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "mac"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

#' Get type to use for PNG graphics based on user's cairo support
#' @noRd
#' @keywords internal
getPNGtype <- function()
{
  if((suppressWarnings(capabilities()["cairo"]))==TRUE) return("cairo")
  else return(NULL)
}




#' Converts linear predictor to probability 
#' @param logged_odds A numeric value, or vector of numeric values.
#' @return Returns predicted probability corresponding the the logged odds value, a numeric value between 0 and 1.
#' @description Calculates predicted probability for a given logged odds value. This function calculates predicted probability for a given logged odds value, often useful for plotting or reporting predicted probabilities.
#' @examples 
#'    library(RCPA4)
#'    
#'    inverse_logit(0)
#'    inverse_logit(-5:5)
#' @export
#' @keywords internal
inverse_logit <- function(logged_odds)
{
  # check if user supplied a numeric
  assert_numeric(logged_odds)
  probability = exp(logged_odds) / (1 + exp(logged_odds))
  return(probability)
}



#' Calculates Lambda, a measure of association for a cross-tabulation. 
#' @param tab The conditional frequencies of a cross-tabulation. The dependent variable should correspond to rows and the independent variable, to columns.
#' @param digits Number of decimal places of Lambda reported (defaults to 3).
#' @param detailed Do you want detailed report on how lambda value calculated? (default is FALSE).
#' @return Returns the value of Lambda.
#' @description Given a table of conditional frequencies, the lambda function calculates and returns Lambda, a measure of association between two categorical variables (factors). Lambda is equal to the proportion of errors reduced by knowledge of the independent variable.
#' @examples 
#'   library(RCPA4)
#'   
#'   conditional.frequencies <- table(dv=states$lgbtq_equality_5cat, iv=states$religiosity_3cat)
#'   lambda(tab=conditional.frequencies, detailed=TRUE)
#' @export
#' @keywords internal
#'
lambda = function(tab, digits=3, detailed=FALSE)
{
  assert_required(tab)
  tab.name <- deparse(substitute(tab))
  if(!is.table(tab)) tab <- as.table(tab)
  
  # print(tab)
  if((nrow(tab) <= 1)) stop(wrap_console_text(paste(gettext("There's a problem:", domain = "R-descr"), tab.name, gettext("doesn't appear to be a table.", domain = "R-descr"))))
  if(sum(tab[-nrow(tab), -ncol(tab)]) == tab[nrow(tab), ncol(tab)]) message(wrap_console_text(paste("There may be a problem:", tab.name, "appears to include margin totals; it should only contain conditional distributions. Try omitting last row and last column.")))
  # return(list(sum(tab[-nrow(tab), -ncol(tab)]), tab[nrow(tab), ncol(tab)]))
  n.total <- sum(tab)
  row.sums <- rowSums(tab)
  naive.errors <- n.total - max(row.sums)
  n.columns <- ncol(tab)
  prediction.errors <- 0
  for(i in 1:n.columns)
  {
    prediction.errors <- prediction.errors + sum(tab[,i]) - max(tab[,i])
    # print(prediction.errors)
  }
  lambda = (naive.errors - prediction.errors) / naive.errors

  lambda.df    <- data.frame(naive.errors=naive.errors, prediction.errors=prediction.errors, lambda=lambda)
  colnames(lambda.df) <- c("Naive Errors", "Prediction Errors", "Lambda")
  rownames(lambda.df) <- NULL
  # names(lambda) = c("Lambda")
  # cat("Hello from lambda function!\n")
  if(detailed==FALSE) 
  {
    lambda <- lambda.df[ , 3]
    names(lambda) <- "Lambda"
    if (is.null(digits)) return(lambda)
    return(round(lambda, digits))
  }
  # names(naive.errors) = c("  Naive Errors")
  # names(prediction.errors) = c("  Model Errors")
  if(detailed==TRUE) {
    if (is.null(digits)) return(lambda.df)
    return(round(lambda.df, digits))
  }
}



#' Logistic regression model fit statistics
#' @param model An estimated logistic regression model (a glm or svyglm object)
#' @param digits Number of digits to be displayed after decimal points
#' @return Returns list of statistics about model (a "LogRegR2" class object)
#' @description Logistic regression model statistics, reported by logregC with fit.stats=TRUE argument, can be used for both weighted and unweighted logregC models.
#' @examples 
#'    library(RCPA4)
#'    
#'    \donttest{
#'    logit.model <- logregC(states$battleground2020 ~ states$vep20_turnout)
#'    logregR2(logit.model)
#'    }
#' @export
#' @importFrom methods is
#' @importFrom stats pchisq
#' @keywords internal
#'
logregR2 = function(model, digits=3)
{
  assert_required(model)  
  # check if user supplied a glm
  if(!(methods::is(model, "glm") || methods::is(model, "svyglm"))) {
    message("Warning: Model is not a glm/svyglm object.")
  }
  if(!is.null(model$model)) n <- nrow(model$model)
  else if(!is.null(model$y)) n <- length(model$y)
  else if(!is.null(model$df.null)) n <- model$df.null + 1
  else n <- NA_real_
  
  chi2_raw <- as.numeric(model$null.deviance - model$deviance)
  Df   <- as.numeric(model$df.null - model$df.residual)
  p_raw <- if(is.finite(chi2_raw) && is.finite(Df) && Df > 0) stats::pchisq(chi2_raw, Df, lower.tail = FALSE) else NA_real_
  Chi2 <- round(chi2_raw, digits)
  p <- format_p_value(p_raw, digits = digits)
  cox_raw <- if(is.finite(chi2_raw) && is.finite(n) && n > 0) 1 - exp(-chi2_raw/n) else NA_real_
  Cox  <- round(cox_raw, digits)
  denom_nag <- if(is.finite(n)) (1 - exp(-model$null.deviance/n)) else NA_real_
  Nag  <- round(cox_raw/denom_nag, digits)
  RL2  <- round(chi2_raw/model$null.deviance, digits)
  fit.stats.df    <- data.frame(Statistic= c("Chi2", "DF (for Chi2)", "p-Value of Chi2", "McFadden's R2", "Cox & Snell Index", "Nagelkerke Index"), 
                                Value=c(Chi2, Df, p, RL2, Cox, Nag),
                                stringsAsFactors = FALSE)
  # colnames(fit.stats.df) 
  # class(x) <- "LogRegR2"
  return(fit.stats.df)
}


missing_args <- function(...) {
  exprs <- as.list(substitute(list(...)))[-1L]
  labels <- vapply(exprs, deparse1, character(1))
  target_env <- parent.frame(2L)
  
  is_missing <- vapply(exprs, FUN = function(sym) {
    if (!is.symbol(sym)) return(FALSE)
    eval(bquote(missing(.(sym))), envir = target_env)
  }, logical(1))
  
  labels[is_missing]
}



#' Translate logistic regression coefficients into odds ratios with confidence interval
#' @param model An estimated logistic regression model
#' @param digits Number of digits after decimal to display
#' @return Returns odds rations and confidence intervals in columns (a matrix class object)
#' @description Generates odds-ratios based on logistic regression model coefficients, reported by logregC with orci=TRUE argument
#' @examples 
#'    library(RCPA4)
#'    
#'    \donttest{
#'    logit.model <- logregC(states$battleground2020 ~ states$vep20_turnout)
#'    orci(logit.model)
#'    }
#' @export
#' @keywords internal
#' @importFrom methods is
#' @importFrom stats coef
#'
orci = function(model, digits=3)
{
  assert_required(model)  
  # check if user supplied a glm
  if(methods::is(model, "glm")!=TRUE) message("Warning: Model is not a glm object.")
  
  coef_vals <- stats::coef(model)
  ci_vals <- tryCatch(
    stats::confint.default(model), # Wald CI (fast; avoids profiling message)
    error = function(e) suppressMessages(suppressWarnings(stats::confint(model)))
  )
  results <- exp(cbind(OddsRatio = coef_vals, ci_vals))
  percent.change <- (exp(stats::coef(model)) -1) * 100
  
  results <- data.frame(results, percent.change)
  colnames(results) = c("Odds Ratio", "OR CI 2.5%", "OR CI 97.5%", "% Change in Odds")
  return(round(results[-1, ], digits))
}


plot_block <- function(draw, filename_base="plot", width=6, height=4.5) {
  stopifnot(is.function(draw))
  structure(list(draw=draw, filename_base=filename_base,
                 width=width, height=height),
            class="plotC")
}

legend_panel_width_inches <- function(labels,
                                      title = NULL,
                                      cex = 0.9,
                                      style = c("fill", "line", "point"),
                                      min_width = 0.4,
                                      max_width = 1.8) {
  style <- match.arg(style)
  labels <- as.character(labels)
  labels <- labels[nzchar(labels)]
  title_text <- if (is.null(title)) character(0) else as.character(title)[1]
  if (length(title_text) == 0L || !nzchar(title_text)) title_text <- character(0)
  legend_text <- c(labels, title_text)
  
  if (length(legend_text) == 0L) return(0)
  
  text_width <- suppressWarnings(max(
    graphics::strwidth(legend_text, units = "inches", cex = cex),
    na.rm = TRUE
  ))
  if (!is.finite(text_width) || text_width < 0) text_width <- 0
  
  em_width <- suppressWarnings(graphics::strwidth("M", units = "inches", cex = cex))
  if (!is.finite(em_width) || em_width <= 0) em_width <- 0.08
  
  key_width <- switch(
    style,
    line = max(0.22, em_width * 2.8),
    point = max(0.16, em_width * 1.4),
    fill = max(0.18, em_width * 2.1)
  )
  
  est_width <- text_width + key_width + (em_width * 1.4)
  est_width <- max(min_width, min(max_width, est_width))
  as.numeric(est_width)
}


draw_margin_legend <- function(legend, title = NULL, cex = 0.9, ...) {
  # Figure clipping keeps the legend out of neighboring panels in a layout.
  x <- graphics::grconvertX(graphics::par("plt")[2] +
                             0.08 / graphics::par("fin")[1],
                           from = "nfc", to = "user")
  y <- graphics::grconvertY(mean(graphics::par("plt")[3:4]),
                           from = "nfc", to = "user")
  args <- list(legend = legend, title = title, cex = cex, ...)
  available <- graphics::grconvertX(0.98, from = "nfc", to = "user") - x
  measure <- function() {
    do.call(graphics::legend, c(list(x = x, y = y, plot = FALSE), args))$rect$w
  }
  width <- measure()
  if (width > available && available > 0) {
    wrap_width <- max(nchar(as.character(legend)), nchar(as.character(title)), 1L)
    while (width > available && wrap_width > 4L) {
      wrap_width <- wrap_width - 1L
      args$legend <- vapply(as.character(legend), function(label) {
        paste(strwrap(label, width = wrap_width), collapse = "\n")
      }, character(1))
      if (!is.null(title)) args$title <- paste(strwrap(title, width = wrap_width), collapse = "\n")
      line_counts <- lengths(strsplit(args$legend, "\n", fixed = TRUE))
      args$y.intersp <- 1 + 0.5 * (max(line_counts) - 1)
      width <- measure()
    }
    if (width > available) args$cex <- cex * available / width
  }
  height <- do.call(graphics::legend,
                    c(list(x = x, y = y, plot = FALSE), args))$rect$h
  available_height <- diff(graphics::grconvertY(graphics::par("plt")[3:4],
                                               from = "nfc", to = "user")) * 0.95
  if (height > available_height && available_height > 0) {
    args$cex <- args$cex * available_height / height
  }
  do.call(graphics::legend,
          c(list(x = x, y = y, xjust = 0, yjust = 0.5, xpd = TRUE), args))
}

print_blocksC <- function(blocks, file = NULL) {
  tables_appended <- FALSE
  images_appended <- NULL
  outputfile <- NULL
  location <- NULL
  
  for (b in blocks) {
    result <- if (is.null(file)) {
      printC(b)
    } else {
      printC(b, file = file)
    }
    if (isTRUE(result$tables_appended)) tables_appended <- TRUE
    if (!is.null(result$images_appended) && length(result$images_appended) > 0) {
      images_appended <- unique(c(images_appended, result$images_appended))
    }
    if (is.null(outputfile) && !is.null(result$outputfile)) outputfile <- result$outputfile
    if (is.null(location) && !is.null(result$location)) location <- result$location
  }
  
  if (isTRUE(tables_appended) && !is.null(outputfile)) {
    output.label <- basename(outputfile)
    output.path <- normalizePath(outputfile, winslash = "/", mustWork = FALSE)
    if (file.exists(output.path)) {
      message(paste("Table(s) appended to", output.label))
    } else {
      warning(paste("Tables were reported as appended, but output file was not found:", output.path))
    }
  }
  
  if (!is.null(images_appended) && length(images_appended) > 0L) {
    message(paste("Image file(s) created:", paste(images_appended, collapse = ", ")))
  }
  
  has_output <- isTRUE(tables_appended) || (!is.null(images_appended) && length(images_appended) > 0L)
  if (has_output && !is.null(outputfile)) {
    output.path <- normalizePath(outputfile, winslash = "/", mustWork = FALSE)
    if (file.exists(output.path)) message(paste("Location:", location))
    else warning(paste("Output path reported, but file is missing:", output.path))
  } else if (has_output && !is.null(location)) {
    message(paste("Location:", location))
  }
  
  invisible(NULL)
}

#' Print caption heading to .html file to organize page
#' @param text String of text to be printed in caption
#' @noRd
#' @keywords internal
printCaption <- function(text)
{
  text <- trimws(paste(as.character(text), collapse = " "))
  if(!grepl("^\\s*Table\\s*:", text)) text <- paste("Table:", text)
  newcaption <- paste("<h4 class=\"rcpa4-caption\">" , text, "</h4>", sep="")
  return(newcaption)
}



#' Prints string to console quickly
#' @param linetoprint Text to be printed to console
#' @return No value returned
#' @description Prints string to console quickly, but still scolls for visual effect
#' @noRd
#' @keywords internal
#' @importFrom utils flush.console
#' 
quickConsolePrint = function(linetoprint)
{
  line = strsplit(linetoprint, split="")
  linetoprint = line[[1]]
  for(i in 1:length(linetoprint))
  {
    Sys.sleep(.006)
    cat(linetoprint[i])
    utils::flush.console()
  }
}


render_heading <- function(blocks, text, width=NULL, marker="=", printC=FALSE, pause=TRUE) {
  if (!isTRUE(rcpa4_get_option("verbose"))) return(blocks)
  text <- paste(as.character(text), collapse = " ")
  
  if (is.null(width)) width <- getOption("width", 80)
  if (!is.numeric(width) || length(width) != 1L || is.na(width) || !is.finite(width)) {
    width <- 80
  }
  width <- as.integer(width)
  if (width < 10L) width <- 10L

  border    <- paste(rep(marker, width), collapse = "")
  inner_width <- max(1L, width - 2L)
  lines <- strwrap(
    text,
    width = inner_width,
    simplify = TRUE,
    exdent = 0,
    prefix = "",
    initial = ""
  )
  if (length(lines) == 0L) lines <- ""
  
  center_line <- function(line, width) {
    line_n <- nchar(line, type = "width")
    leadchar <- floor((width - line_n) / 2)
    if (leadchar < 0) leadchar <- 0
    paste0(paste(rep(" ", leadchar), collapse = ""), line)
  }
  centered_lines <- vapply(lines, center_line, character(1), width = width)
  
  heading <- c(
    paste(border, "\n", sep=""),
    paste(centered_lines, "\n", sep=""),
    paste(border, "\n", sep="")
  )
  class(heading) <- "banner.heading"
  
  # console side-effect (always)
  cat(heading, sep="")
  
  # built-in delay (optional)
  if (isTRUE(pause)) slightpause()
  
  # printC: only add to blocks when requested
  if (isTRUE(printC)) blocks <- add_block(blocks, heading)
  
  blocks
}

render_table <- function(blocks, x, caption = NULL, digits = 3, printC = FALSE, footer = NULL, col_groups = NULL, html_colnames = NULL, align = NULL, html_table_class = NULL, console = TRUE, footer_console = NULL, footer_html = NULL) {
  if (is.null(blocks)) blocks <- list()
  
  normalize_footer <- function(ft) {
    if (is.null(ft)) return(NULL)
    ft <- paste(as.character(ft), collapse = "\n")
    ft <- gsub("\r\n?", "\n", ft)
    if (!nzchar(trimws(ft))) return(NULL)
    ft
  }
  
  html_escape <- function(text) {
    text <- gsub("&", "&amp;", text, fixed = TRUE)
    text <- gsub("<", "&lt;", text, fixed = TRUE)
    text <- gsub(">", "&gt;", text, fixed = TRUE)
    text <- gsub("\"", "&quot;", text, fixed = TRUE)
    text <- gsub("'", "&#39;", text, fixed = TRUE)
    text
  }
  
  append_html_footer <- function(tbl_html, ft_text, colspan) {
    if (is.null(ft_text)) return(tbl_html)
    colspan <- max(1L, as.integer(colspan))
    ft_html <- html_escape(ft_text)
    ft_html <- gsub("\n", "<br />", ft_html, fixed = TRUE)
    tfoot <- paste0(
      "<tfoot><tr><td colspan=\"", colspan, "\" class=\"rcpa4-table-footer\">",
      ft_html,
      "</td></tr></tfoot>"
    )
    sub("</table>", paste0(tfoot, "</table>"), tbl_html, fixed = TRUE)
  }
  
  append_html_col_groups <- function(tbl_html, groups) {
    if (is.null(groups) || !is.list(groups)) return(tbl_html)
    labels <- groups$labels
    spans <- groups$spans
    if (is.null(labels) || is.null(spans)) return(tbl_html)
    labels <- as.character(labels)
    spans <- suppressWarnings(as.integer(spans))
    keep <- !is.na(spans) & spans > 0L & nzchar(labels)
    labels <- labels[keep]
    spans <- spans[keep]
    if (length(labels) == 0L || length(spans) == 0L || length(labels) != length(spans)) return(tbl_html)
    
    thead_match <- regexpr("<thead>[\\s\\S]*?</thead>", tbl_html, perl = TRUE)
    if (thead_match[1] < 0L) return(tbl_html)
    thead_text <- regmatches(tbl_html, thead_match)[1]
    
    first_tr_match <- regexpr("<tr>[\\s\\S]*?</tr>", thead_text, perl = TRUE)
    if (first_tr_match[1] < 0L) return(tbl_html)
    first_tr <- regmatches(thead_text, first_tr_match)[1]
    
    th_count <- lengths(regmatches(first_tr, gregexpr("<th\\b", first_tr, perl = TRUE)))
    if (!is.finite(th_count) || th_count <= 0L) return(tbl_html)
    
    span_total <- sum(spans)
    lead_span <- th_count - span_total
    if (lead_span < 0L) return(tbl_html)
    
    lead_cell <- if (lead_span > 0L) paste0("<th colspan=\"", lead_span, "\" class=\"rcpa4-colgroup rcpa4-colgroup-blank\"></th>") else ""
    group_cells <- paste(
      vapply(seq_along(labels), function(i) {
        group_class <- if (spans[i] > 1L) "rcpa4-colgroup rcpa4-colgroup-multi" else "rcpa4-colgroup rcpa4-colgroup-single"
        paste0(
          "<th colspan=\"", spans[i], "\" class=\"", group_class, "\">",
          html_escape(labels[i]),
          "</th>"
        )
      }, character(1)),
      collapse = ""
    )
    group_row <- paste0("<tr>", lead_cell, group_cells, "</tr>")
    
    sub("<thead>", paste0("<thead>\n", group_row), tbl_html, fixed = TRUE)
  }
  
  print_console_footer <- function(ft_text) {
    if (is.null(ft_text)) return(invisible(NULL))
    cat(wrap_console_text(ft_text), "\n", sep = "")
    invisible(NULL)
  }
  
  footer_text <- normalize_footer(footer)
  footer_console_text <- if (is.null(footer_console)) footer_text else normalize_footer(footer_console)
  footer_html_text <- if (is.null(footer_html)) footer_text else normalize_footer(footer_html)
  
  normalize_html_colnames <- function(cols, n_cols) {
    if (is.null(cols)) return(NULL)
    cols <- as.character(cols)
    if (length(cols) != n_cols) return(NULL)
    cols
  }
  
  normalize_align <- function(spec, n_cols) {
    if (n_cols <= 0L) return("r")
    if (is.null(spec)) return(rep("r", n_cols))
    spec <- as.character(spec)
    if (length(spec) == 1L) return(rep(spec, n_cols))
    if (length(spec) == n_cols) return(spec)
    rep(spec[1], n_cols)
  }
  
  append_html_table_class <- function(tbl_html, class_name) {
    if (is.null(class_name)) return(tbl_html)
    class_name <- trimws(as.character(class_name)[1])
    if (!nzchar(class_name)) return(tbl_html)
    sub("<table>", paste0("<table class=\"", html_escape(class_name), "\">"), tbl_html, fixed = TRUE)
  }
  
  print_console_caption <- function(cap) {
    if (is.null(cap)) return(invisible(NULL))
    cap <- as.character(cap)
    if (length(cap) == 0L || !nzchar(cap[1])) return(invisible(NULL))
    cap_line <- cap[1]
    if (!grepl("^\\s*Table\\s*:", cap_line)) {
      cap_line <- paste("Table:", cap_line)
    }
    cat("\n", wrap_console_text(cap_line), sep = "")
  }
  
  normalize_html_caption <- function(cap) {
    if (is.null(cap)) return(NULL)
    cap <- as.character(cap)
    if (length(cap) == 0L || !nzchar(cap[1])) return(NULL)
    cap_line <- trimws(cap[1])
    if (exists("printCaption", mode = "function")) {
      return(printCaption(cap_line))
    }
    if (!grepl("^\\s*Table\\s*:", cap_line)) cap_line <- paste("Table:", cap_line)
    cap_line
  }
  
  print_console_kable <- function(tbl, align_spec) {
    lines <- utils::capture.output(
      print(knitr::kable(
        tbl, format = "simple", caption = NULL, align = align_spec, escape = TRUE
      ))
    )
    if (length(lines) >= 2L) {
      cn <- colnames(tbl)
      if (!is.null(cn) && length(cn) >= 1L && !nzchar(cn[length(cn)])) {
        non_empty <- which(nzchar(trimws(lines)))
        if (length(non_empty) >= 2L) {
          sep_idx <- non_empty[2]
          lines[sep_idx] <- sub("\\s{2,}-+\\s*$", "", lines[sep_idx])
        }
      }
    }
    cat(paste(lines, collapse = "\n"), "\n", sep = "")
    invisible(NULL)
  }
  
  # Helper: format a data.frame's cells as character (preserve colnames)
  fmt_df <- function(df) {
    rn <- rownames(df)
    cn <- colnames(df)
    out <- as.data.frame(lapply(df, function(col) {
      format(col, drop0trailing = FALSE, nsmall = digits, digits = digits)
    }), check.names = FALSE, stringsAsFactors = FALSE)
    rownames(out) <- rn
    colnames(out) <- cn
    out
  }
  
  # ----------------------------
  # CASE 1: matrix (KEEP as matrix)
  # ----------------------------
  if (is.matrix(x)) {
    rn <- rownames(x)
    cn <- colnames(x)
    
    x_fmt <- format(x, drop0trailing = FALSE, nsmall = digits, digits = digits)
    dimnames(x_fmt) <- list(rn, cn)  # matrices allow duplicate rownames
    
    align_spec <- normalize_align(align, ncol(x_fmt))
    
    # Print caption ourselves so long captions wrap cleanly in console.
    if (isTRUE(console)) {
      print_console_caption(caption)
      print_console_kable(x_fmt, align_spec)
      print_console_footer(footer_console_text)
    }
    
    if (isTRUE(printC)) {
      html_caption <- normalize_html_caption(caption)
      x_html <- x_fmt
      html_cols <- normalize_html_colnames(html_colnames, ncol(x_html))
      if (!is.null(html_cols)) colnames(x_html) <- html_cols
      html_tbl <- knitr::kable(
        x_html, format = "html", caption = html_caption, align = align_spec, escape = TRUE
      )
      html_tbl <- append_html_col_groups(as.character(html_tbl), col_groups)
      html_tbl <- append_html_table_class(as.character(html_tbl), html_table_class)
      html_tbl <- append_html_footer(as.character(html_tbl), footer_html_text, ncol(x_fmt))
      class(html_tbl) <- "knitr_kable"
      blocks <- add_block(blocks, html_tbl)
    }
    
    return(blocks)
  }
  
  # ----------------------------
  # CASE 2: data.frame (or coerce to df)
  # ----------------------------
  if (!is.data.frame(x)) {
    x <- data.frame(x, check.names = FALSE, stringsAsFactors = FALSE)
  }
  
  rn <- rownames(x)
  
  # If rownames are duplicated, move them into a first column for display
  if (!is.null(rn) && anyDuplicated(rn)) {
    x <- cbind(.row = rn, x)
    rownames(x) <- NULL
    colnames(x)[1] <- ""  # cleaner display
  }
  
  x_fmt <- fmt_df(x)
  
  align_spec <- normalize_align(align, ncol(x_fmt))
  
  # Print caption ourselves so long captions wrap cleanly in console.
  if (isTRUE(console)) {
    print_console_caption(caption)
    print_console_kable(x_fmt, align_spec)
    print_console_footer(footer_console_text)
  }
  
  if (isTRUE(printC)) {
    html_caption <- normalize_html_caption(caption)
    x_html <- x_fmt
    html_cols <- normalize_html_colnames(html_colnames, ncol(x_html))
    if (!is.null(html_cols)) colnames(x_html) <- html_cols
    html_tbl <- knitr::kable(
      x_html, format = "html", caption = html_caption, align = align_spec, escape = TRUE
    )
    html_tbl <- append_html_col_groups(as.character(html_tbl), col_groups)
    html_tbl <- append_html_table_class(as.character(html_tbl), html_table_class)
    html_tbl <- append_html_footer(as.character(html_tbl), footer_html_text, ncol(x_fmt))
    class(html_tbl) <- "knitr_kable"
    blocks <- add_block(blocks, html_tbl)
  }
  
  blocks
}

resolve_weights <- function(w_expr, data, env, n) {
  
  if (is.null(w_expr)) {
    return(list(
      w = rep(1, n),
      name = NULL,
      weighted = FALSE,
      sum = n
    ))
  }
  
  w_val <- eval_in_data_expr(w_expr, data = data, env = env)
  
  if (!is.numeric(w_val))
    stop("Weights must be numeric.")
  
  if (length(w_val) != n)
    stop("Weights must have same length as variables.")
  
  list(
    w = w_val,
    name = friendly_expr_label(w_expr),
    weighted = TRUE,
    sum = sum(w_val, na.rm = TRUE)
  )
}

setup_data_context <- function(data_expr = quote(NULL), env = parent.frame(), data_missing = FALSE) {
  supplied <- !isTRUE(data_missing) && !is.null(data_expr) && !identical(data_expr, quote(NULL))
  data_val <- NULL
  data_name <- NULL
  
  if (supplied) {
    data_val <- eval_data_arg(data_expr, env = env)
    data_name <- paste(deparse(data_expr), collapse = "")
  }
  
  list(
    value = data_val,
    expr = data_expr,
    name = data_name,
    supplied = supplied
  )
}

apply_complete_cases <- function(vars, on = names(vars)) {
  if (!is.list(vars) || length(vars) == 0) stop("`vars` must be a non-empty list.")
  if (is.null(names(vars)) || any(!nzchar(names(vars)))) stop("`vars` must be a named list.")
  if (is.null(on)) on <- names(vars)
  if (!all(on %in% names(vars))) {
    bad <- setdiff(on, names(vars))
    stop("Unknown variable(s) in `on`: ", paste(bad, collapse = ", "))
  }
  
  ref_n <- length(vars[[on[1]]])
  if (!all(vapply(vars[on], function(v) length(v) == ref_n, logical(1)))) {
    stop("All variables in `on` must have the same length.")
  }
  
  complete_mask <- stats::complete.cases(as.data.frame(vars[on], stringsAsFactors = FALSE))
  
  cleaned <- vars
  for (nm in names(cleaned)) {
    if (length(cleaned[[nm]]) == ref_n) {
      cleaned[[nm]] <- cleaned[[nm]][complete_mask]
    }
  }
  
  list(
    vars = cleaned,
    mask = complete_mask,
    n_total = ref_n,
    n_complete = sum(complete_mask),
    n_dropped = sum(!complete_mask),
    on = on
  )
}

label_or_fallback <- function(x, fallback) {
  lbl <- attr(x, "label")
  if (is.null(lbl) || length(lbl) == 0) return(fallback)
  lbl <- as.character(lbl)[1]
  if (!nzchar(lbl)) fallback else lbl
}

setup_vars_input <- function(var_exprs,
                             w_expr = NULL,
                             data_expr = quote(NULL),
                             env = parent.frame(),
                             data_missing = FALSE,
                             drop_complete = FALSE,
                             complete_on = NULL,
                             single_vars = names(var_exprs),
                             ref_var = NULL,
                             strings_as_factors = TRUE) {
  if (!is.list(var_exprs) || length(var_exprs) == 0) stop("`var_exprs` must be a non-empty named list.")
  if (is.null(names(var_exprs)) || any(!nzchar(names(var_exprs)))) stop("`var_exprs` must be a named list.")
  if ("w" %in% names(var_exprs)) stop("`var_exprs` cannot include a variable named `w`; use `w_expr` instead.")
  if (is.null(single_vars)) single_vars <- character(0)
  if (!all(single_vars %in% names(var_exprs))) {
    bad <- setdiff(single_vars, names(var_exprs))
    stop("Unknown variable(s) in `single_vars`: ", paste(bad, collapse = ", "))
  }
  if (is.null(ref_var)) ref_var <- names(var_exprs)[1]
  if (!(ref_var %in% names(var_exprs))) stop("`ref_var` must be one of names(var_exprs).")
  
  data_ctx <- setup_data_context(data_expr = data_expr, env = env, data_missing = data_missing)
  
  vars <- list()
  meta_vars <- list()
  
  for (nm in names(var_exprs)) {
    expr <- var_exprs[[nm]]
    info <- eval_varlist_expr(
      expr,
      data = data_ctx$value,
      env = env,
      strings_as_factors = strings_as_factors
    )
    
    if (nm %in% single_vars && ncol(info$df) != 1) {
      stop("Expected a single variable in `", nm, "`.")
    }
    
    if (nm %in% single_vars) {
      v <- info$df[[1]]
      v_name <- info$names[1]
    } else {
      v <- info$df
      v_name <- paste(info$names, collapse = ", ")
    }
    
    vars[[nm]] <- v
    expr_text <- paste(deparse(expr), collapse = "")
    v_label <- label_or_fallback(v, v_name)
    v_levels <- if (is.factor(v) || is.ordered(v)) levels(v) else NULL
    
    meta_vars[[nm]] <- list(
      name = v_name,
      label = v_label,
      expr = expr_text,
      display = v_label,
      levels = v_levels
    )
  }
  
  ref_n <- if (is.data.frame(vars[[ref_var]])) nrow(vars[[ref_var]]) else length(vars[[ref_var]])
  var_lengths <- vapply(vars, function(v) if (is.data.frame(v)) nrow(v) else length(v), integer(1))
  if (!all(var_lengths == ref_n)) stop("All resolved variables must have the same length.")
  
  w_info <- resolve_weights(w_expr, data = data_ctx$value, env = env, n = ref_n)
  w <- w_info$w
  w_name <- w_info$name
  w_expr_text <- if (is.null(w_expr)) NULL else paste(deparse(w_expr), collapse = "")
  w_label <- if (is.null(w_name)) NULL else label_or_fallback(w, w_name)
  w_display <- if (is.null(w_label)) w_name else w_label
  
  vars_with_w <- c(vars, list(w = w))
  complete_info <- NULL
  if (isTRUE(drop_complete)) {
    if (is.null(complete_on)) complete_on <- c(names(var_exprs), "w")
    complete_info <- apply_complete_cases(vars = vars_with_w, on = complete_on)
    vars_with_w <- complete_info$vars
  }
  
  n_out <- if (is.data.frame(vars_with_w[[ref_var]])) nrow(vars_with_w[[ref_var]]) else length(vars_with_w[[ref_var]])
  
  meta_out <- c(meta_vars, list(
    w = list(
      name = w_name,
      label = w_label,
      expr = w_expr_text,
      display = w_display
    ),
    weighted = w_info$weighted,
    n = n_out,
    w_sum = sum(vars_with_w$w, na.rm = TRUE),
    complete = complete_info
  ))
  
  list(
    data = data_ctx,
    vars = vars_with_w,
    meta = meta_out,
    mask = if (is.null(complete_info)) NULL else complete_info$mask
  )
}

#' Pauses R briefly for user to see scrolling Console output 
#' @description Slight pauses help user see Console output before it scrolls by. 
#' @param second Number of seconds to pause (default: 2)
#' @noRd
#' @keywords internal
slightpause = function(seconds=2)
{
  if (!isTRUE(rcpa4_get_option("slow"))) return(invisible(NULL))
  if (missing(seconds)) seconds <- rcpa4_get_option("pause_seconds")
  if (!is.numeric(seconds) || length(seconds) != 1L || is.na(seconds)) {
    seconds <- rcpa4_get_option("pause_seconds")
  }
  seconds <- max(0, as.numeric(seconds))
  Sys.sleep(seconds)
}


#' Prints string to console slowly
#' @param linetoprint Text to be printed to console
#' @param slow Time, in milliseconds, to wait between characters printed to console (default is .05).
#' @return No value returned
#' @description Prints string to console slowly to make text more readable to the user. Used by welcome and widgetFactory functions.
#' @noRd
#' @keywords internal
#' @importFrom utils flush.console
#'
slowConsolePrint = function(linetoprint, slow=.05)
{
  line = strsplit(linetoprint, split="")
  linetoprint = line[[1]]
  for(i in 1:length(linetoprint))
  {
    Sys.sleep(slow)
    cat(linetoprint[i])
    utils::flush.console()
  }
}




#' Calculates the Somers' d measure of association for a cross-tabulation. 
#'
#' @param tab The conditional frequencies of a cross-tabulation. The dependent and independent variable in cross-tabulation should be be ordinal-level (ordered factors).
#' @param dep	which dimension stands for the dependent variable (1 = ROWS, 2 = COLS), default is 1.
#' @param digits Number of decimal places reported in result (defaults to 3).
#' @return Returns the value of Somers' D.
#' @description Given a table of conditional frequencies, tablesomersDC calculates and returns Somers' D, a measure of association between two ordinal-level variables.
#' @examples 
#'   library(RCPA4)
#'   
#'   conditional.frequencies <- table(dv=states$lgbtq_equality_5cat, iv=states$religiosity_3cat)
#'   tablesomersDC(tab=conditional.frequencies)
#' @export
#' @keywords internal
tablesomersDC = function(tab, dep=1, digits=3) 
{ 
  assert_required(tab)  
  tab.name <- deparse(substitute(tab))
  if(!is.table(tab)) tab <- as.table(tab)
  if((nrow(tab) <= 1) || (ncol(tab) <= 1)) stop(wrap_console_text(paste(gettext("There's a problem:", domain = "R-descr"), tab.name, gettext("doesn't appear to be a table.", domain = "R-descr"))))
  if(sum(tab[-nrow(tab), -ncol(tab)]) == tab[nrow(tab), ncol(tab)]) message(wrap_console_text(paste("There may be a problem:", tab.name, "appears to include margin totals; it should only contain conditional distributions. Try omitting last row and last column.")))
  
  compADPQ = function(tab)
  {
    nr = nrow(tab)
    nc = ncol(tab)
    Aij = matrix(0, nrow=nr, ncol=nc)
    Dij = matrix(0, nrow=nr, ncol=nc)
    for (i in 1:nr){
      for (j in 1:nc){
        
        Aij[i,j] = sum(tab[1:i,1:j]) + sum(tab[i:nr,j:nc]) - sum(tab[i,]) - sum(tab[,j])
        Dij[i,j] = sum(tab[i:nr,1:j]) + sum(tab[1:i,j:nc]) - sum(tab[i,]) - sum(tab[,j])
      }
    }
    P = sum(tab*Aij)
    Q = sum(tab*Dij)
    return(list(Aij=Aij, Dij=Dij, P=P, Q=Q))
  }
  
  # Statistic 
  if (dep==1) tab=t(tab) 
  tmp = compADPQ(tab) 
  P   = tmp$P 
  Q   = tmp$Q 
  n   = sum(tab) 
  w   = n^2 - sum(apply(tab,1,sum)^2) 
  somers = (P-Q)/w 
  out = c(somers)
  if (!is.null(digits)) out <- round(out, digits)
  names(out) = c("Somers' dyx")
  # cat("     Somers'dyx:\n")
  return(out) 
}


#' Calculates Correlation Coefficient Between Two Numeric Variables
#' Uses RCPA4's internal weighted correlation helper.
#' @param x1 A variable (must be numeric), should be in dataset$var form unless dataset specified in optional data argument.
#' @param x2 Another variable, different than x1 (must be numeric), should be in dataset$var form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param digits (Optional) Number of decimal places reported in result. Default is NULL (no rounding).
#' @param stats (Optional) Do you want inferential statistics for correlation coefficient? (default: FALSE)
#' @param ... (Optional) Additional arguments passed to the internal weighted correlation helper.
#' @return Returns the coefficient of correlation between x1 and x2 variables, a numeric value.
#' @description Given two numeric variables, wtd.cor reports correlation coefficient, works with sampling weights.
#' @examples 
#'    library(RCPA4)
#'    
#'    wtd_cor(x1=anes$ft_rep_party, x2=anes$ft_trump_pre, w=anes$wt)
#'    wtd_cor(x1=ft_dem_party, x2=ft_harris_pre, w=wt, data=anes)
#' @export
#' @keywords internal
wtd_cor <- function(x1, x2, w=NULL, data=NULL, digits=NULL, stats=FALSE, ...) 
{ 
  assert_required(x1, x2)  
  ctx <- setup_vars_input(
    var_exprs = list(x1 = substitute(x1), x2 = substitute(x2)),
    w_expr = if (missing(w)) NULL else substitute(w),
    data_expr = substitute(data),
    env = parent.frame(),
    data_missing = missing(data),
    drop_complete = FALSE,
    single_vars = c("x1", "x2"),
    ref_var = "x1"
  )
  x1 <- ctx$vars$x1
  x2 <- ctx$vars$x2
  w <- ctx$vars$w
  
  # Validate selections
  assert_numeric(x1, x2)

  result <- weighted_cor(x=x1, y=x2, weight=w, ...) 
  # return(attributes(result))
  if(stats==FALSE) {
    out <- result[,"correlation"]
    if (!is.null(digits)) out <- round(out, digits)
    return(out)
  }
  if(stats==TRUE) {
    out <- result
    if (!is.null(digits)) out <- round(out, digits)
    return(out)
  }
}




#' Creates a Histogram Showing Distribution of Variable Values With Option for Weights
#' Uses RCPA4's internal wtd histogram helper.
#' @param x A variable (must be numeric), should be in dataset$var form unless dataset specified in optional data argument.
#' @param w Sampling weights of variable (optional), must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param breaks (Optional) Specifies the breakpoints between bins of the histogram.
#' @param suggest (Optional) If TRUE, suggest bar chart when x has very few distinct values.
#' @param main (Optional) Main title for histogram; defaults to "Histogram of x".
#' @param xlab (Optional) X-axis label for histogram; defaults to "x Values".
#' @param ... (Optional) Additional arguments passed to the internal wtd histogram helper.
#' @return Generate a histogram plot.
#' @description Takes in variable and generates histogram plot to show the distribution of its values, works with sampling weights.
#' Bin counts are computed with optional weights.
#' @examples 
#'    library(RCPA4)
#'    
#'    wtd_hist(x=anes$r_age_election_day, w=anes$wt)
#' @export
#' @keywords internal
#' @importFrom Hmisc all.is.numeric
wtd_hist <- function(x, w=NULL, data=NULL, breaks=NULL, suggest=TRUE, main=NULL, xlab=NULL, ...) 
{ 
  assert_required(x)  
  ctx <- setup_vars_input(
    var_exprs = list(x = substitute(x)),
    w_expr = if (missing(w)) NULL else substitute(w),
    data_expr = substitute(data),
    env = parent.frame(),
    data_missing = missing(data),
    drop_complete = FALSE,
    single_vars = "x",
    ref_var = "x"
  )
  x <- ctx$vars$x
  w <- ctx$vars$w
  x.name <- ctx$meta$x$name
  
  assert_numeric(x)
  if(isTRUE(suggest) && length(unique(x)) <= 10) 
  { msg <- paste(gettext("Suggestion:", domain = "R-descr"), x.name, 
                 gettext("has only", domain = "R-descr"), length(unique(x)), 
                 gettext("unique values. Maybe you want to use a bar chart?", domain = "R-descr") )
    message(msg)
  }
  
  if (is.null(main)) main <- paste("Histogram of", x.name)
  if (is.null(xlab)) xlab <- paste(x.name, "Values")
  
  wtd_hist_internal(x=x, weight=w, breaks=breaks, main=main, xlab=xlab, ...) 
}



#' Generates Kurtosis Statistic with Option to Weight Observations
#' @param x Variable (a numeric vector)
#' @param w (Optional) Weights variable
#' @param data (Optional) Dataset, can be used to abbreviate x to variable name
#' @param digits (Optional) Number of digits after decimal point. Default is NULL (no rounding).
#' @return Returns the kurtosis of the variable, a numeric value.
#' @examples 
#'    library(RCPA4)
#'    
#'    wtd_kurtosis(x=anes$r_age_election_day, w=anes$wt)
#'    wtd_kurtosis(x=anes$r_age_election_day)
#' @export
#' @keywords internal
#' @importFrom stats weighted.mean
wtd_kurtosis <- function(x, w=NULL, data=NULL, digits=NULL) {
  
  assert_required(x)  
  ctx <- setup_vars_input(
    var_exprs = list(x = substitute(x)),
    w_expr = if (missing(w)) NULL else substitute(w),
    data_expr = substitute(data),
    env = parent.frame(),
    data_missing = missing(data),
    drop_complete = TRUE,
    complete_on = c("x", "w"),
    single_vars = "x",
    ref_var = "x"
  )
  x <- ctx$vars$x
  w <- ctx$vars$w
  
  n <- length(x)
  w.n <- sum(w) # equals n if w=1
  x.mean <- stats::weighted.mean(x, w)
  # w.sd  <- sqrt(Hmisc::wtd.var(x=x, weights=w)) # weighted 
  # z.dev <- (x - x.mean) / w.sd
  kurtosis_value <- w.n * sum(w*(x - x.mean)^4)/(sum((x - x.mean)^2)^2) # same as moments::kurtosis
  class(kurtosis_value) <- "numeric"
  if (!is.null(digits)) kurtosis_value <- round(kurtosis_value, digits)
  return(kurtosis_value)
}



#' Calculates Mean of Variable With Option for Weights
#' @param x A variable (must be numeric), should be in dataset$var form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param digits (Optional) Number of decimal places reported in result. Default is NULL (no rounding).
#' @param ... (Optional) Additional arguments passed to Hmisc::wtd.mean function.
#' @return Returns the mean of the variable, a numeric value.
#' @description Takes in variable and calculates its mean, works with sampling weights.
#' Makes use of the wtd.mean function, part of the Hmisc package. See \code{\link[Hmisc]{wtd.stats}} documentation for details.
#' @examples 
#'    library(RCPA4)
#'    
#'    wtd_mean(anes$ft_liberals, w=anes$wt)
#'    wtd_mean(anes$ft_liberals)
#'    
#'    wtd_mean(anes$r_age_election_day, w=anes$wt)
#'    wtd_mean(anes$r_age_election_day)
#' @export
#' @keywords internal
#' @importFrom Hmisc wtd.mean
wtd_mean <- function(x, w=NULL, data, digits=NULL, ...) 
{
  assert_required(x)  
  ctx <- setup_vars_input(
    var_exprs = list(x = substitute(x)),
    w_expr = substitute(w),
    data_expr = substitute(data),
    env = parent.frame(),
    data_missing = missing(data),
    drop_complete = FALSE,
    single_vars = "x",
    ref_var = "x"
  )
  x <- ctx$vars$x
  w <- ctx$vars$w
  
  # Validate selections
  assert_numeric(x)

  result = Hmisc::wtd.mean(x=x, weights=w, ...)
  if (!is.null(digits)) result <- round(result, digits)
  return(result)
}



#' Find Median of Variable with Option to Weight Observations
#' @param x A variable
#' @param w (Optional) Sampling weights of variable
#' @param data (Optional) Name of dataset that contains x (and w) variable
#' @param ... (Optional) Additional arguments passed to `Hmisc::wtd.quantile`.
#' @return Returns the median value of `x`. For numeric input, returns a numeric value; for factor input, returns the corresponding level label.
#' @description Takes in a numeric variable or ordered factor and finds its median, with optional sampling weights. Numeric-looking factor labels are interpreted as their displayed numeric values. Ordered factors return the first category whose cumulative weight reaches one-half of the total.
#' @examples 
#'    library(RCPA4)
#'    
#'    wtd_median(x=anes$ft_biden_pre, w=anes$wt)
#'    wtd_median(x=anes$ft_biden_pre)
#' @export
#' @keywords internal
#' @importFrom Hmisc wtd.quantile
wtd_median <- function(x, w=NULL, data=NULL, ...)
{
  assert_required(x)  
  ctx <- setup_vars_input(
    var_exprs = list(x = substitute(x)),
    w_expr = substitute(w),
    data_expr = substitute(data),
    env = parent.frame(),
    data_missing = missing(data),
    drop_complete = FALSE,
    single_vars = "x",
    ref_var = "x"
  )
  x <- ctx$vars$x
  w <- ctx$vars$w
  
  # Numeric-looking factor labels represent values, not factor storage codes.
  if (is.factor(x) && !is.ordered(x) && Hmisc::all.is.numeric(x, extras = c(NA))) {
    x <- as.numeric(as.character(x))
  }

  if (is.numeric(x) || is.logical(x)) {
    return(as.numeric(Hmisc::wtd.quantile(as.numeric(x), weights = w, probs = .5)))
  }

  if (!is.ordered(x)) {
    stop("x must be numeric, logical, or an ordered factor with meaningful level order.", call. = FALSE)
  }

  keep <- stats::complete.cases(x, w)
  if (!any(keep)) {
    stop("x and weights have no complete observations.", call. = FALSE)
  }

  x_codes <- as.integer(x[keep])
  w_keep <- w[keep]
  if (any(!is.finite(w_keep)) || any(w_keep < 0) || sum(w_keep) <= 0) {
    stop("Weights must be finite, non-negative, and have a positive sum.", call. = FALSE)
  }

  level_weights <- numeric(length(levels(x)))
  observed_weights <- tapply(w_keep, x_codes, sum)
  level_weights[as.integer(names(observed_weights))] <- observed_weights
  median_index <- which(cumsum(level_weights) >= sum(level_weights) / 2)[1]
  levels(x)[median_index]
}



#' Find Mode of Variable with Option to Weight Observations
#' @param x A variable
#' @param w (Optional) Sampling weights of variable
#' @param data (Optional) A dataset
#' @param ... (Optional) Reserved for backward compatibility.
#' @return Returns the modal value(s) of the variable, if mode exists
#' @description Takes in variable and finds mode, works with sampling weights.
#' @examples 
#'    library(RCPA4)
#'    
#'    wtd_mode(x=anes$feel_angry, w=anes$wt)
#'    wtd_mode(x=anes$feel_angry)
#'    
#'    wtd_mode(x=anes$discrim_us_men, w=anes$wt)
#'    wtd_mode(x=anes$discrim_us_men)
#' @export
#' @keywords internal
#' @importFrom Hmisc all.is.numeric
wtd_mode <- function(x, w=NULL, data=NULL, ...)
{
  assert_required(x)  
  
  ctx <- setup_vars_input(
    var_exprs = list(x = substitute(x)),
    w_expr = substitute(w),
    data_expr = substitute(data),
    env = parent.frame(),
    data_missing = missing(data),
    drop_complete = TRUE,
    complete_on = c("x", "w"),
    single_vars = "x",
    ref_var = "x"
  )
  x <- ctx$vars$x
  w <- ctx$vars$w
  
  if(length(x) == length(unique(x))) {
    message("Every value of ", paste(deparse(substitute(x)), collapse=" "),
            " is unique. There is no modal value.")
    return("None")
  }

  result <- weighted_freq_table(x = x, w = w, include_total = FALSE, include_cum = FALSE)
  maxfreq <- max(result$Frequency)
  mode <- row.names(result)[result$Frequency == maxfreq]
  # what if two or more values have same frequency? okay
  if(Hmisc::all.is.numeric(mode)) mode = as.numeric(mode)
  return(mode)
}



#' Finds the Quantile Values of Variable With Option for Weights
#' @param x A variable (must be numeric), should be in dataset$var form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param q (Optional) The quantiles you want calculated as a number or vector of numbers. Input as percentages. For median only, q=50. For quartiles, q=c(25, 50, 75).
#' @param digits (Optional) Number of decimal places reported in result. Default is NULL (no rounding).
#' @param ... (Optional) Additional arguments passed to Hmisc::wtd.quantile function.
#' @return Returns quantile values of a variable, a vector of numeric values.
#' @description Takes in variable and calculates desired quantile values, works with sampling weights. Makes use of the wtd.quantile function from the Hmisc package. See \code{\link[Hmisc]{wtd.stats}} documentation for details.
#' @examples 
#'    library(RCPA4)
#'    
#'    wtd_quantile(x=anes$ft_police, q=c(25, 50, 75), w=anes$wt)
#'    wtd_quantile(x=anes$ft_police, q=c(25, 50, 75))
#' @export
#' @keywords internal
#' @importFrom Hmisc wtd.quantile
wtd_quantile <- function(x, w=NULL, data, q=c(5,10,25,50,75,90,95), digits=NULL, ...) 
{
  assert_required(x)  

  ctx <- setup_vars_input(
    var_exprs = list(x = substitute(x)),
    w_expr = substitute(w),
    data_expr = substitute(data),
    env = parent.frame(),
    data_missing = missing(data),
    drop_complete = FALSE,
    single_vars = "x",
    ref_var = "x"
  )
  x <- ctx$vars$x
  w <- ctx$vars$w
  
  assert_numeric(x)

  if(all(q <= 1) & all(q >= 0)) {
    q <- q * 100
  }
  result = Hmisc::wtd.quantile(x=x, weights=w, probs=q/100, ...)
  if (!is.null(digits)) result <- round(result, digits)
  return(result)
}



#' Calculates Standard Deviation of Variable With Option for Weights
#' @param x A variable (must be numeric), should be in dataset$var form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param digits (Optional) Number of decimal places reported in result. Default is NULL (no rounding).
#' @param ... (Optional) Additional arguments passed to Hmisc::wtd.var function.
#' @return Returns the standard deviation of the variable, a numeric value.
#' @description Takes in variable and calculates standard deviation, works with sampling weights. Makes use of the wtd.var function, part of the Hmisc package.
#' @examples 
#'    library(RCPA4)
#'    
#'    wtd_sd(x=anes$ft_liberals, w=anes$wt)
#'    wtd_sd(x=anes$ft_liberals)
#' @export
#' @keywords internal
#' @importFrom Hmisc wtd.var
wtd_sd <- function(x, w=NULL, data, digits=NULL, ...) 
{
  assert_required(x)  

  ctx <- setup_vars_input(
    var_exprs = list(x = substitute(x)),
    w_expr = substitute(w),
    data_expr = substitute(data),
    env = parent.frame(),
    data_missing = missing(data),
    drop_complete = FALSE,
    single_vars = "x",
    ref_var = "x"
  )
  x <- ctx$vars$x
  w <- ctx$vars$w
  
  assert_numeric(x)

  result = sqrt(Hmisc::wtd.var(x=x, weights=w, ...))
  if (!is.null(digits)) result <- round(result, digits)
  return(result)
}



#' Calculates Skewness of Numeric Variable with Option to Weight Observations
#' @param x Variable (must be a numeric vector)
#' @param w (Optional) Weights variable 
#' @param data (Optional) Dataset
#' @param digits (Optional) Number of digits after decimal point. Default is NULL (no rounding).
#' @return Returns the skewness of the variable, a numeric value.
#' @examples 
#'    library(RCPA4)
#'    
#'    wtd_skewness(x=anes$ft_liberals, w=anes$wt)
#'    wtd_skewness(x=anes$ft_liberals)
#' @export
#' @keywords internal
#' @importFrom Hmisc wtd.var
#' @importFrom stats weighted.mean
wtd_skewness <- function(x, w=NULL, data=NULL, digits=NULL) 
  {
  assert_required(x)  

  ctx <- setup_vars_input(
    var_exprs = list(x = substitute(x)),
    w_expr = substitute(w),
    data_expr = substitute(data),
    env = parent.frame(),
    data_missing = missing(data),
    drop_complete = TRUE,
    complete_on = c("x", "w"),
    single_vars = "x",
    ref_var = "x"
  )
  x <- ctx$vars$x
  w <- ctx$vars$w
  
  n <- length(x)
  w.n <- sum(w) # equals n if w=1
  x.mean <- stats::weighted.mean(x, w)
  w.sd  <- sqrt(Hmisc::wtd.var(x=x, weights=w)) # weighted 
  z.dev <- (x - x.mean) / w.sd
  
  skewness_value <- (sum(w*(x - x.mean)^3)/w.n)/(sum((x - x.mean)^2)/n)^(3/2)
  class(skewness_value) <- "numeric"
  if (!is.null(digits)) skewness_value <- round(skewness_value, digits)
  return(skewness_value)
}



#' Calculates Variance of Variable With Option for Weights
#' @param x A variable (must be numeric), should be in dataset$var form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param digits (Optional) Number of decimal places reported in result. Default is NULL (no rounding).
#' @param ... (Optional) Additional arguments passed to Hmisc::wtd.var function.
#' @return Returns the variance of the variable, a numeric value.
#' @description Takes in variable and calculates its variance, works with sampling weights. Makes use of the wtd.var function from the Hmisc package. See \code{\link[Hmisc]{wtd.stats}} documentation for details.
#' @examples 
#'    library(RCPA4)
#'    
#'    wtd_var(x=anes$ft_liberals, w=anes$wt)
#'    wtd_var(x=anes$ft_liberals)
#' @export
#' @keywords internal
#' @importFrom Hmisc wtd.var
wtd_var <- function(x, w=NULL, data=NULL, digits=NULL, ...) 
{
  assert_required(x)  

  ctx <- setup_vars_input(
    var_exprs = list(x = substitute(x)),
    w_expr = substitute(w),
    data_expr = substitute(data),
    env = parent.frame(),
    data_missing = missing(data),
    drop_complete = FALSE,
    single_vars = "x",
    ref_var = "x"
  )
  x <- ctx$vars$x
  w <- ctx$vars$w
  
  assert_numeric(x)

  result <- Hmisc::wtd.var(x=x, weights=w, ...)
  if (!is.null(digits)) result <- round(result, digits)
  return(result)
}


# Internal helper: attach standardized auxiliary tables + metadata to return object.
rcpa4_attach_tables <- function(out, tables, fn_name, call_obj = NULL) {
  if (is.null(tables) || !is.list(tables)) {
    stop("`tables` must be a list.")
  }
  attr(out, "rcpa4_tables") <- tables
  attr(out, "rcpa4_meta") <- list(fn = fn_name, call = call_obj)
  out
}

# Internal helper: resolve auxiliary table by key from rcpa4_tables attribute.
rcpa4_lookup_aux_table <- function(x, key) {
  if (!is.character(key) || length(key) != 1L || is.na(key)) {
    return(list(found = FALSE, value = NULL))
  }
  tabs <- attr(x, "rcpa4_tables", exact = TRUE)
  if (is.null(tabs) || !is.list(tabs)) {
    return(list(found = FALSE, value = NULL))
  }
  tab_names <- names(tabs)
  if (is.null(tab_names) || !(key %in% tab_names)) {
    return(list(found = FALSE, value = NULL))
  }
  list(found = TRUE, value = tabs[[key]])
}

# Internal helper: shared [[ dispatcher for augmented model objects.
rcpa4_result_bracket2 <- function(x, i, class_name, ...) {
  base_x <- x
  class(base_x) <- setdiff(class(base_x), class_name)
  
  if (is.character(i) && length(i) == 1L && !is.na(i)) {
    native_names <- names(base_x)
    if (!is.null(native_names) && (i %in% native_names)) {
      return(base::`[[`(base_x, i, ...))
    }
    aux <- rcpa4_lookup_aux_table(x, i)
    if (isTRUE(aux$found)) {
      return(aux$value)
    }
    return(NULL)
  }
  
  base::`[[`(base_x, i, ...)
}

#' @keywords internal
#' @noRd
#' @export
`[[.regC_result` <- function(x, i, ...) {
  rcpa4_result_bracket2(x = x, i = i, class_name = "regC_result", ...)
}

#' @keywords internal
#' @noRd
#' @export
`[[.logregC_result` <- function(x, i, ...) {
  rcpa4_result_bracket2(x = x, i = i, class_name = "logregC_result", ...)
}

#' @keywords internal
#' @noRd
#' @export
`[[.compmeansC_result` <- function(x, i, ...) {
  rcpa4_result_bracket2(x = x, i = i, class_name = "compmeansC_result", ...)
}

#' @keywords internal
#' @noRd
#' @export
`[[.crosstabC_result` <- function(x, i, ...) {
  rcpa4_result_bracket2(x = x, i = i, class_name = "crosstabC_result", ...)
}

#' @keywords internal
#' @noRd
#' @export
`[[.sampdistC_result` <- function(x, i, ...) {
  rcpa4_result_bracket2(x = x, i = i, class_name = "sampdistC_result", ...)
}
