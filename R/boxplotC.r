#' Generates box plots to compare interval-level dependent variable's distribution across categories of independent variable. 
#'
#' @description Generates box plots for visual comparison of interval-level dependent variable's distribution across categories of independent variable. Includes option for weighting observations, modifying colors, variable widths. Box plot can be used to compare values of interval-level dependent variable by categories of an independent variable (a factor). If `iv` is omitted, the function creates a single-variable box plot.
#' @param dv Dependent variable, should be in dataset$var form unless dataset specified in optional data argument.
#' @param iv (Optional) Independent variable, should be in dataset$var form unless dataset specified in optional data argument. If omitted or set to `NULL`, boxplotC will create a single-variable box plot of `dv`.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains dv, iv (and w) variables (optional).
#' @param main (Optional) Supply custom main label for plot; default uses names of dv and iv (or just dv if `iv` is omitted).
#' @param xlab (Optional) Supply custom x-axis label for plot; default uses name of iv when provided, and blank label when `iv` is omitted.
#' @param ylab (Optional) Supply y-axis label for plot; default uses name of dv.
#' @param box.col (Optional) The name of color to use for box fills. Default is "gray80". Border color follows `bxp` defaults unless overridden via `...` (e.g., `boxcol=` or `pars=list(boxcol=...)`). 
#' @param varwidth (Optional) Do you want the widths of boxes to be proportional to number of observations in each group? Default is TRUE; set `varwidth=FALSE` for equal-width boxes.
#' @param ivlabs (Optional) A vector of labels for the iv values that are box lablels. 
#' @param printC (Optional) Do you want to print box plot to .html file in working directory? (Default: FALSE)
#' @param ... Additional arguments passed to \code{\link[graphics]{bxp}}. You can supply `range` (or `coef`) to set whisker length multiplier (default: 1.5). Styling args accepted by `bxp` (e.g., `boxcol`, `boxfill`, `outpch`) can be supplied either in `pars=list(...)` or directly in `...`.
#' @return Invisibly returns the `bxp_input` object used to draw the plot
#' (class `"boxplot"`), including computed boxplot statistics by group.
#' @examples 
#'   library(RCPA4)
#'    
#'   # basic usage with variables as vectors
#'   boxplotC(dv=anes$ft_rep_party, iv=anes$partyid3)
#' 
#'   # single-variable box plot
#'   boxplotC(dv=anes$ft_rep_party)
#' @export
#' @template console-output
#' @section RCPA4 Package Tutorial Videos:
#' * [Making Box Plots with RCPA4's boxplotC Function](https://youtu.be/40bQJucHbY4) 15:12, shows you how to use the RCPA4 Package's boxplotC function to create box plots that show the relationship between an interval-level dependent variables and a categorical independent variable. 
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapter 5. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Sage Publications, 2020), pp. 53-55. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Graphing Relationships and Describing Patterns](https://www.poliscidata.com/pages/rDemosResources.php?chapter=5), compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com), find datasets for your own research and resources to help with political analysis. 
#' @details Numeric-looking factor or character labels, such as `"10"` and `"20"`, are interpreted as their displayed numeric values for the dependent variable.
#' @importFrom graphics par mtext
#' @importFrom grDevices png dev.off
#' @importFrom stats na.omit
#' @importFrom utils modifyList tail
#' @md
#' 
boxplotC <- function(dv, iv=NULL, w=NULL, data=NULL, main=NULL, xlab=NULL, ylab=NULL, box.col="gray80", 
                     varwidth=TRUE, ivlabs=NULL, printC=FALSE, ...) 
{ 
  assert_required(dv)
  
  old.par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old.par), add=TRUE)
  
  single_box <- missing(iv) || identical(substitute(iv), quote(NULL))
  
  if (single_box) {
    ctx <- setup_vars_input(
      var_exprs = list(dv = substitute(dv)),
      w_expr = if (missing(w)) NULL else substitute(w),
      data_expr = substitute(data),
      env = parent.frame(),
      data_missing = missing(data),
      drop_complete = TRUE,
      complete_on = c("dv", "w"),
      single_vars = "dv",
      ref_var = "dv"
    )
    dv <- ctx$vars$dv
    w <- ctx$vars$w
    dv.name <- ctx$meta$dv$name
    iv.name <- NULL
    w.name <- ctx$meta$w$name
    weighted <- ctx$meta$weighted
    iv <- factor(rep("All cases", length(dv)), levels = "All cases")
  } else {
    ctx <- setup_vars_input(
      var_exprs = list(dv = substitute(dv), iv = substitute(iv)),
      w_expr = if (missing(w)) NULL else substitute(w),
      data_expr = substitute(data),
      env = parent.frame(),
      data_missing = missing(data),
      drop_complete = TRUE,
      complete_on = c("dv", "iv", "w"),
      single_vars = c("dv", "iv"),
      ref_var = "dv"
    )
    dv <- ctx$vars$dv
    iv <- ctx$vars$iv
    w <- ctx$vars$w
    dv.name <- ctx$meta$dv$name
    iv.name <- ctx$meta$iv$name
    w.name <- ctx$meta$w$name
    weighted <- ctx$meta$weighted
  }
  
  # Numeric-looking labels represent values, rather than factor storage codes.
  if (!is.numeric(dv) && Hmisc::all.is.numeric(dv, extras = c(NA))) {
    dv <- as.numeric(as.character(dv))
  }

  # validate selections
  assert_numeric(dv)
  if (!single_box) suggest_categorical(iv)
  
  # ------------------------------------------------------------------------
  
  blocks <- list()
  # ---- Heading (ALWAYS printed to console) ----
  blocks <- render_heading(blocks, "Making Comparisons with Box Plots", marker="=", printC=printC)
  
  # Title/labels
  if(!missing(main)) {
    plotmain <- main
  } else {
    if (single_box) plotmain <- paste("Boxplot of", dv.name)
    else plotmain <- paste("Boxplot of", dv.name, "by", iv.name)
    if(weighted==TRUE) plotmain <- paste(plotmain, ", weighted by ", w.name, sep="")
  }
  
  if(missing(ylab)) ylab <- dv.name
  if(missing(xlab)) {
    if (single_box) xlab <- ""
    else xlab <- iv.name
  }

  plotmain <- strwrap(plotmain, width=50)
  
  if (is.factor(iv) || is.ordered(iv)) iv <- droplevels(iv)
  else iv <- factor(iv, levels = unique(stats::na.omit(iv)))
  
  if(!missing(ivlabs)) 
  {
    if(length(ivlabs) != length(levels(iv))) stop(
      wrap_console_text(
        paste(
          "Oops. The iv labels aren't the right length for this iv's values. There are",
          length(ivlabs),
          "labels for",
          length(levels(iv)),
          "different values. To see how to use the ivlabs argument, try help(boxplotC)."
        )
      )
    )
    levels(iv) <- ivlabs
  }
  
  group_labels <- as.character(levels(iv))
  
  dots <- list(...)
  whisker_coef <- if (!is.null(dots$range)) dots$range else if (!is.null(dots$coef)) dots$coef else 1.5
  if(!is.numeric(whisker_coef) || length(whisker_coef) != 1L || is.na(whisker_coef) || whisker_coef < 0) {
    stop("`range` (or `coef`) must be a single non-negative number.")
  }
  dots$range <- NULL
  dots$coef <- NULL
  
  # bxp styling arguments are controlled via `pars`; allow both pars=... and
  # direct ... aliases (e.g., boxcol=) for convenience.
  bxp_par_names <- c(
    "boxwex", "staplewex", "outwex",
    "boxlty", "boxlwd", "boxcol", "boxfill",
    "medlty", "medlwd", "medcol",
    "whisklty", "whisklwd", "whiskcol",
    "staplelty", "staplelwd", "staplecol",
    "outpch", "outcex", "outlty", "outlwd", "outcol"
  )
  user_pars <- dots$pars
  if (is.null(user_pars)) user_pars <- list()
  if (!is.list(user_pars)) stop("`pars` must be a list when supplied.")
  
  named_dots <- names(dots)
  if (is.null(named_dots)) named_dots <- rep("", length(dots))
  for (nm in intersect(named_dots[nzchar(named_dots)], bxp_par_names)) {
    user_pars[[nm]] <- dots[[nm]]
    dots[[nm]] <- NULL
  }
  dots$pars <- NULL
  
  plot_pars <- modifyList(list(boxfill = box.col), user_pars)
  
  compute_weighted_box_stats <- function(x, w, coef = 1.5) {
    if (!is.numeric(x) || length(x) == 0L) stop("There are no valid dv values to plot.")
    if (!is.numeric(w) || length(w) != length(x)) stop("Weights must be numeric and match dv length.")
    if (any(!is.finite(w))) stop("Weights must be finite.")
    if (any(w < 0)) stop("Weights must be non-negative.")
    if (sum(w) <= 0) stop("Weights must have a positive sum.")
    
    q <- as.numeric(Hmisc::wtd.quantile(x = x, weights = w, probs = c(0.25, 0.50, 0.75)))
    q1 <- q[1]
    med <- q[2]
    q3 <- q[3]
    iqr <- q3 - q1
    
    lower_fence <- q1 - coef * iqr
    upper_fence <- q3 + coef * iqr
    inlier <- x >= lower_fence & x <= upper_fence
    
    if (!any(inlier)) inlier <- rep(TRUE, length(x))
    
    whisker_low <- min(x[inlier])
    whisker_high <- max(x[inlier])
    outliers <- x[!inlier]
    
    n_eff <- sum(w)
    conf_delta <- 1.58 * iqr / sqrt(max(1, n_eff))
    
    list(
      stats = c(whisker_low, q1, med, q3, whisker_high),
      conf = c(med - conf_delta, med + conf_delta),
      n = as.numeric(n_eff),
      out = outliers
    )
  }
  
  group_index <- split(seq_along(dv), iv)
  n_groups <- length(group_index)
  stats_mat <- matrix(NA_real_, nrow = 5, ncol = n_groups)
  conf_mat <- matrix(NA_real_, nrow = 2, ncol = n_groups)
  n_vec <- numeric(n_groups)
  out_vals <- numeric(0)
  out_group <- integer(0)
  
  for (i in seq_len(n_groups)) {
    idx <- group_index[[i]]
    x_g <- dv[idx]
    w_g <- w[idx]
    
    grp <- compute_weighted_box_stats(x = x_g, w = w_g, coef = whisker_coef)
    stats_mat[, i] <- grp$stats
    conf_mat[, i] <- grp$conf
    n_vec[i] <- grp$n
    
    if (length(grp$out) > 0L) {
      out_vals <- c(out_vals, grp$out)
      out_group <- c(out_group, rep(i, length(grp$out)))
    }
  }
  
  bxp_input <- list(
    stats = stats_mat,
    n = n_vec,
    conf = conf_mat,
    out = out_vals,
    group = out_group,
    names = group_labels
  )
  class(bxp_input) <- "boxplot"
  
  # ---- Single draw function ----
  draw_boxplot <- function(emit_label_message = TRUE) {
    
    restore.par.mar <- graphics::par("mar")
    on.exit(graphics::par(mar = restore.par.mar), add = TRUE)
    
    axis_cfg <- axis_label_layout_cfg(
      labels = group_labels,
      n_slots = length(group_labels),
      mar_left = 4.1,
      mar_top = 3.6,
      mar_right = 1.1,
      long_label_message = "One or more text labels for boxes is long. Consider using ivlabs arguments to shorten them."
    )
    graphics::par(mar = axis_cfg$mar)
    if (isTRUE(emit_label_message) && isTRUE(axis_cfg$notify)) {
      message(wrap_console_text(axis_cfg$message))
    }
    use_custom_x_axis <- axis_cfg$method %in% c("wrapped", "angled45")
    
    bxp_args <- c(
      list(
        z = bxp_input,
        varwidth = varwidth,
        main = plotmain,
        xlab = "",
        ylab = ylab,
        pars = plot_pars,
        cex.axis = axis_cfg$cex,
        las = axis_cfg$las,
        xaxt = if (isTRUE(use_custom_x_axis)) "n" else "s"
      ),
      dots
    )
    do.call(graphics::bxp, bxp_args)
    if (isTRUE(use_custom_x_axis)) {
      draw_x_axis_labels(at = seq_along(group_labels), cfg = axis_cfg, side = 1)
    }
    
    graphics::mtext(text = xlab, side = 1, line = axis_cfg$mtext)
  }
  
  # ---- Show plot to user ----
  draw_boxplot(emit_label_message = TRUE)
  
  # ---- Export plot via printC ----
  if (isTRUE(printC)) {
    draw_boxplot_export <- function() draw_boxplot(emit_label_message = FALSE)
    
    blocks <- add_block(
      blocks,
      plot_block(
        draw = draw_boxplot_export,
        filename_base = "boxplotsC.plot",
        width = 6,
        height = 4.5
      )
    )
    
    blocks <- add_block(blocks, match.call(expand.dots = FALSE))
    
    print_blocksC(blocks)
  }
  
  invisible(bxp_input)
}

