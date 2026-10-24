#' Reports the frequency distribution of dataset variable with table and bar chart
#'
#' @description Generates a frequency distribution table and bar chart to describe the distribution of variable values. Supports optional case weights.
#' Includes cumulative percents for ordered factors and for numeric variables
#' (treating numeric values as ordered categories), except when `x` has only
#' two values.
#' @param x The variable to be analyzed. If dataset not specified with data argument, should be a vector in form dataset$var.
#' @param w (Optional) Sample weights (must be numeric if used), If dataset not specified with data argument, should be in form dataset$weighvar
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param digits (Optional) Number of digits to display after decimal point (default=2).
#' @param rowlabs (Optional) Vector specifying custom text for labeling table rows and chart bars. The rowlabs vector must correspond to x variable's levels (if the variable has five levels, rowlabs must be length 5). Useful when default value labels are too long. 
#' @param table (Optional) Do you want the frequency distribution table reported? (default: TRUE)
#' @param printC (Optional) Do you want to print frequency distribution table and bar chart (if plot is used) to .html file in working directory? (default: FALSE)
#' @param plot (Optional) Do you want a bar chart? (default set to TRUE)
#' @param main (Optional) Main title of bar chart.
#' @param xlab (Optional) The x-axis label of bar chart.
#' @param ylab (Optional) The y-axis label of bar chart.
#' @param bar.col (Optional) The name of color to use for bars. Default is "gray80". 
#' @param ylim (Optional) y-axis range for bar chart. If omitted, upper bound is set
#'   to the next multiple of 10 above the largest percent in table rows (excluding
#'   the Total row).
#' @param ... (Optional) Additional arguments passed to \code{\link[graphics]{barplot}} when `plot=TRUE`.
#' @return Invisibly returns a frequency distribution table (data frame) with
#' numeric frequencies and percentages. The `digits` argument controls display
#' formatting, not stored numeric values.
#' @export
#' @examples 
#'    library(RCPA4)
#'    
#'    # unordered factors
#'    freqC(x=region, data=world)
#'    
#'    # ordered factors
#'    freqC(x=threat_china, w=wt, data=anes)
#' @template console-output
#' @template result-tables
#' @section Table and plot requirements:
#' At least one of `table` or `plot` must be enabled. When using `table = FALSE`,
#' keep a valid plot enabled.
#' @section RCPA4 Package Tutorial Videos:
#' * [Make Frequency Distribution Tables and Bar Charts with RCPA4 Package's freqC Function](https://www.youtube.com/watch?v=NwFB_JLgAko) 15:33  
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapter 2. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Sage Publications, 2020), pp. 39-55. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Descriptive Statistics](https://www.poliscidata.com/pages/rDemosResources.php?chapter=2), compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom graphics par
#' @importFrom stats na.omit
#' @importFrom grDevices png dev.off
#' @md

freqC <- function(x, w=NULL, data=NULL, digits=2, rowlabs=NULL, printC=FALSE, plot=TRUE, 
                  main=NULL, xlab=NULL, ylab=NULL, bar.col="gray80", ylim=NULL, table=TRUE, ...)
{
  assert_required(x)
  if((table == FALSE) && (plot == FALSE)) stop("At least one of table or plot must be TRUE.")
  
  if(plot!=FALSE)
  {
    old.par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old.par), add=TRUE)
  }
  
  assert_nonnegative_integer(digits)
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
  x.name <- ctx$meta$x$name
  x.label <- ctx$meta$x$label
  w.name <- ctx$meta$w$name
  weighted <- ctx$meta$weighted
  
  if(!missing(rowlabs))
  {
    if(!is.null(levels(x))) x.values <- levels(x) else x.values <- unique(stats::na.omit(x))
    if(length(rowlabs) != length(x.values)) stop(
      wrap_console_text(
        paste(
          "Oops. The x variable's labels aren't the right length for this x variable's values. There are",
          length(rowlabs),
          "labels for",
          length(x.values),
          "different values. To see how to use the rowlabs argument, try help(freqC)."
        )
      )
    )
  }
  
  # ---- NEW: collect printC blocks ----------------------------------------
  blocks <- list()
  # ------------------------------------------------------------------------
  
  blocks <- render_heading(blocks, "Describing Distribution of Values", marker="=", printC=printC)
  
  caption <- paste("Frequency Distribution of", x.name)
  if(!is.null(x.label) && !identical(x.label, x.name)) caption <- paste(caption, " (", x.label, ")", sep="")
  if(weighted==TRUE) caption <- paste(caption, ", Weighted by ", w.name, sep="")
  
  if(("ordered" %in% class(x)) & ("factor" %in% class(x))) class(x) <- c("ordered", "factor")
  x_n_values <- if (is.factor(x)) nlevels(x) else length(unique(stats::na.omit(x)))
  include_cum_percents <- (is.ordered(x) || is.numeric(x)) && (x_n_values > 2L)
  
  obj1 <- weighted_freq_table(
    x = x,
    w = w,
    include_total = TRUE,
    include_cum = include_cum_percents
  )
  obj1 <- data.frame(obj1, check.names = FALSE)
  
  if(!missing(rowlabs)) row.names(obj1) <- c(rowlabs, "Total")
  else rowlabs <- row.names(obj1)[-nrow(obj1)]
  
  obj1.display <- obj1
  if(all(obj1.display[, "Frequency"]%%1==0)) n.drop.decimals <- TRUE else n.drop.decimals <- FALSE
  display_digits <- max(1L, as.integer(digits))
  obj1.display[, "Frequency"] <- format(
    obj1.display[, "Frequency"],
    drop0trailing = n.drop.decimals,
    nsmall = digits,
    digits = display_digits
  )
  obj1.display.table <- obj1.display
  numeric_cols <- vapply(obj1.display.table, is.numeric, logical(1))
  if (any(numeric_cols)) {
    obj1.display.table[numeric_cols] <- lapply(
      obj1.display.table[numeric_cols],
      function(col) {
        format(col, drop0trailing = FALSE, nsmall = digits, digits = display_digits)
      }
    )
  }
  
  if(table!=FALSE)
  {
    # Console table output, HTML table output (for printC)
    blocks <- render_table(
      blocks,
      x = obj1.display.table,
      caption = caption,
      digits = display_digits,
      printC = printC
    )
  }
  
  
  if(length(unique(x)) > 15)
  {
    tryhistmessage <- paste("Suggestion:", x.name, "has a lot of unique values. Try making a histogram with histC function for clearer description of its distribution.")
    message(wrap_console_text(tryhistmessage))
  }
  
  if(plot==TRUE)
  {
    plot_vals <- as.numeric(obj1[-nrow(obj1), "Percent"])
    if(missing(ylim)) {
      max_percent <- suppressWarnings(max(plot_vals, na.rm=TRUE))
      if(!is.finite(max_percent) || max_percent <= 0) ylim <- c(0, 10)
      else ylim <- c(0, ceiling(max_percent / 10) * 10)
    }
    
    if(missing(main)) main <- caption
    if(missing(ylab)) ylab <- "Percentage"
    if(missing(xlab)) xlab <- paste(x.name, "values")
    main <- strwrap(main, width=50)
    
    draw_freq_plot <- function(emit_label_message = TRUE) {
      restoremar <- graphics::par("mar")
      on.exit(graphics::par(mar = restoremar), add = TRUE)
      
      axis_cfg <- axis_label_layout_cfg(
        labels = rowlabs,
        n_slots = length(rowlabs),
        mar_left = 4.1,
        mar_top = 3.6,
        mar_right = 1.1,
        long_label_message = "Note: rowlabs adjusted to better fit barplot"
      )
      graphics::par(mar = axis_cfg$mar)
      if (isTRUE(emit_label_message) && isTRUE(axis_cfg$notify)) {
        message(wrap_console_text(axis_cfg$message))
      }
      
      if (axis_cfg$method %in% c("angled45", "wrapped")) {
        mids <- graphics::barplot(
          height = plot_vals,
          ylab = ylab,
          main = main,
          col = bar.col,
          names.arg = FALSE,
          axisnames = FALSE,
          ylim = ylim,
          ...
        )
        draw_x_axis_labels(at = mids, cfg = axis_cfg, side = 1, draw_axis = FALSE)
      } else {
        graphics::barplot(
          height = plot_vals,
          cex.names = axis_cfg$cex,
          las = axis_cfg$las,
          ylab = ylab,
          main = main,
          col = bar.col,
          names.arg = axis_cfg$labels,
          ylim = ylim,
          ...
        )
      }
      graphics::mtext(text = xlab, side = 1, line = axis_cfg$mtext)
    }
    
    # show plot to user
    draw_freq_plot(emit_label_message = TRUE)
    
    # export plot (printC handles file creation)
    if(isTRUE(printC)) {
      draw_freq_plot_export <- function() draw_freq_plot(emit_label_message = FALSE)
      blocks <- add_block(
        blocks,
        plot_block(draw = draw_freq_plot_export,
                   filename_base = "freqC.plot",
                   width = 6,
                   height = 4.5)
      )
    }
  }
  
  if(isTRUE(printC)) {
    blocks <- add_block(blocks, match.call(expand.dots = FALSE))
    print_blocksC(blocks)
  }
  
  invisible(obj1)
}

