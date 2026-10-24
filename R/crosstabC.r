#' Cross-tabulation analysis, option for weighting observations, makes controlled comparisons, generates plots, performs Chi-Square test, measures strength of association
#'
#' @description This is a workhorse function for analyzing the relationship between two variables measured at the nominal or ordinal-level (factors). Basic output is a cross-tabulation with column percentages and counts. Options include weighting observations, adding control variable for controlled cross-tabulation, several plotting options, conducting Chi-Square test of independence, and measuring strength of association.
#' @param dv Dependent variable, should be in dataset$var form unless dataset specified in optional data argument. Should be a nominal or ordinal-level variable.
#' @param iv Independent variable, should be in dataset$var form unless dataset specified in optional data argument. Should be a nominal or ordinal-level variable.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param z (Optional) Control variable, must be a facttor; should be in dataset$z form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains dv, iv (and w) variables.
#' @param digits (Optional) Number of decimal places reported in result (defaults to 2).
#' @param compact (Optional) Do you want compact display of cross-tabulation with row subtotals omitted? Default is FALSE.
#' @param dvlabs (Optional) A vector of names for the dependent variable's values (to abbreviate the cross-tabulation's row labels and dv labels on plots)
#' @param ivlabs (Optional) A vector of names for the independent variable's values (to abbreviate the cross-tabulation's column labels and iv labels on plots)
#' @param zlabs (Optional) A vector of names for the control variable's values (to abbreviate the controlled cross-tabulation's column labels and z variable labels on plots)
#' @param table (Optional) Do you want cross-tabulation and statistics tables reported? (default: TRUE)
#' @param printC (Optional) Do you want to print cross-tabulation and plot (if plot is used) to .html file in working directory? (default: FALSE)
#' @param plot (Optional) Do you want a plot of the cross-tabulation? Default is TRUE (and makes a bar plot). Other plot options: 
#' * "line" for a line plot,
#' * "mosaic" for a mosaic plot, 
#' * "bar", TRUE, or T for a bar plot (default plot)
#' * FALSE or F to suppress plot. 
#' @param plot.response (Optional) The dv value to be summarized on plot. To combine and plot multiple dv values, use transformC and create dummy variable to be plotted. Set `plot.response="all"` to plot all DV values (for uncontrolled comparisons only).
#' @param chisq (Optional) Do you want to conduct Chi-Square Test? If z argument specific, Chi-Square Test conducted on dv-iv relationship for each value of z.
#' @param lambda (Optional) Do you want Lambda reported? If z argument specified, Lambda reported for dv-iv relationship for each value of z.
#' @param somers (Optional) Do you want Somers' d reported? If z argument specific, Somers' D reported for dv-iv relationship for each value of z.
#' @param cramers (Optional) Do you want Cramer's V reported? If z argument specific, Cramer's V reported for dv-iv relationship for each value of z.
#' @param main (Optional) Main label for plot
#' @param xlab (Optional) x-axislabel for plot
#' @param ylab (Optional) y-axis label for plot
#' @param z.palette (Optional) For bar and line plots with control variables (z), the name of HCL color palette to use. Default is "LightGrays". See `grDevices::hcl.pals` for palette names and more information. Also see <https://developer.r-project.org/Blog/public/2019/04/01/hcl-based-color-palettes-in-grdevices/> to view color palettes.
#' @param legend.title (Optional) Title for legend shown if plot used with z argument.
#' @return Returns an augmented cross-tabulation matrix. Auxiliary tables are
#' available via `[[` (for example `out[["chisq_table"]]` or
#' `out[["association_table"]]`).
#' @examples  
#'   library(RCPA4)
#'   
#'   \donttest{
#'   crosstabC(dv=anes$r_favor_opps_death, iv=anes$partyid3)
#'   # with optional w, data, chisq, somers arguments
#'   crosstabC(dv=r_favor_opps_death, iv=partyid3, w=wt, data=anes, chisq=TRUE, somers=TRUE)
#'   
#'   # example with optional w, data, z, and plot="line" arguments
#'   crosstabC(dv=r_favor_opps_death, iv=partyid3, w=wt, data=anes, z=gender_s_sex, plot="line")
#'   }
#' @export
#' @template console-output
#' @template result-tables
#' @section Table and plot requirements:
#' At least one of `table` or `plot` must be enabled. When using `table = FALSE`,
#' keep a valid plot enabled.
#' @section RCPA4 Package Tutorial Videos:
#' * [Create Cross-Tabulations to Make Comparisons with RCPA4's crosstabC Function](https://www.youtube.com/watch?v=lPmpsiKZ5rY) 13:31  
#' * [Plots for Cross-Tabulations with RCPA4's crosstabC Function](https://www.youtube.com/watch?v=nwr52FIWBx4) 29:51  
#' * [Making Controlled Comparisons with RCPA4's crosstabC Function](https://www.youtube.com/watch?v=UoXwVEQpiFo) 28:34  
#' * [Conducting Chi-Square Test and Measuring Strength of Assovciation with RCPA4 Package's crosstabC Function](https://www.youtube.com/watch?v=EVTfgIBWDkE) 35:06  
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapters 4, 5, 7, 10. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Sage Publications, 2020), pp.85-97, 150-156, 215-231. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [Tutorials & Resources for Making Comparisons](https://www.poliscidata.com/pages/rDemosResources.php?chapter=4), [Graphing Relationships and Describing Patterns](https://www.poliscidata.com/pages/rDemosResources.php?chapter=5), [Making Controlled Comparisons](https://www.poliscidata.com/pages/rDemosResources.php?chapter=7), and [Chi-Square Test and Analysis of Variance](https://www.poliscidata.com/pages/rDemosResources.php?chapter=10), compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom grDevices png dev.off
#' @importFrom graphics par barplot mosaicplot points lines mtext legend axis box
#' @importFrom stats na.omit xtabs as.formula addmargins chisq.test ftable
#' @md
crosstabC <- function(dv, iv, w=NULL, z=NULL, data=NULL, digits=2, compact=FALSE, dvlabs=NULL, ivlabs=NULL, zlabs=NULL,
                      chisq=FALSE, lambda=FALSE, somers=FALSE, cramers=FALSE, printC=FALSE, 
                      plot=TRUE, plot.response=NULL, main=NULL, xlab=NULL, ylab=NULL, z.palette="LightGrays", legend.title=NULL, table=TRUE) 
{
  assert_required(dv, iv)
  assert_nonnegative_integer(digits)
  if((table == FALSE) && identical(plot, FALSE)) stop("At least one of table or plot must be TRUE.")
  
  if(!identical(plot, FALSE))
  {
    # Restoring arrangement parameters would replace an existing custom layout.
    old.par <- graphics::par(c("mar", "xpd"))
    on.exit(graphics::par(old.par), add=TRUE)
  }
  

  if (missing(z)) {
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
    z <- rep(1, length(ctx$vars$dv))
    z.name <- NULL
    controlled <- FALSE
  } else {
    ctx <- setup_vars_input(
      var_exprs = list(dv = substitute(dv), iv = substitute(iv), z = substitute(z)),
      w_expr = if (missing(w)) NULL else substitute(w),
      data_expr = substitute(data),
      env = parent.frame(),
      data_missing = missing(data),
      drop_complete = TRUE,
      complete_on = c("dv", "iv", "z", "w"),
      single_vars = c("dv", "iv", "z"),
      ref_var = "dv"
    )
    z <- ctx$vars$z
    z.name <- ctx$meta$z$name
    controlled <- TRUE
  }
  
  dv <- ctx$vars$dv
  iv <- ctx$vars$iv
  w <- ctx$vars$w
  dv.name <- ctx$meta$dv$name
  iv.name <- ctx$meta$iv$name
  w.name <- ctx$meta$w$name
  weighted <- ctx$meta$weighted

  
  if(!identical(plot, FALSE))
  {
    plot.types <- c(TRUE, "line", "bar", "mosaic")
    if(identical(plot, "lines")) plot <- "line"
    if(identical(plot, "bars")) plot <- "bar"
    if(!(plot %in% plot.types))
    {
      message(
        "There's a problem: ", plot, " isn't an available plot type.\n",
        "Here are your options:\n",
        paste0("  - ", as.character(plot.types), collapse="\n"),
        "\nTry another plot type to generate a plot."
      )
      plot <- FALSE
      slightpause()
    }
  }
  
  
  if(is.numeric(dv) && (length(unique(dv)) > 12)) stop(wrap_console_text(paste(gettext("There's a problem:", domain = "R-descr"), dv.name, gettext("is numeric variable. Try mean comparison analysis instead.", domain = "R-descr"))))
  if(is.numeric(iv) && (length(unique(iv)) > 12)) stop(wrap_console_text(paste(gettext("There's a problem:", domain = "R-descr"), iv.name, gettext("is numeric variable. Collapse values to fit a crosstab.", domain = "R-descr"))))
  
  if(!is.null(levels(dv))) dv.levels <- levels(dv) else dv.levels <- unique(stats::na.omit(dv))
  if(!is.null(levels(iv))) iv.levels <- levels(iv) else iv.levels <- unique(stats::na.omit(iv))
  if(controlled) {
    if(is.factor(z)) z.levels <- levels(droplevels(z)) else z.levels <- unique(z)
  } else {
    z.levels <- 1
  }
  
  if(!is.null(levels(dv))) response.options <- levels(dv) else response.options <- unique(stats::na.omit(dv))
  if(!missing(dvlabs)) response.options <- dvlabs
  
  # what response to plot?
  if(!identical(plot, FALSE))
  {
    if(missing(plot.response)) 
    {
      picked.plot.response.note <- TRUE
      plot.response <- response.options[1]
      slightpause()
    }
    else picked.plot.response.note <- FALSE
    
    if(!(plot.response %in% response.options) && (plot.response != "all"))
    {
      stop(
        "There's a problem: ", plot.response, " isn't one of available response values.\n",
        "Here are your options:\n",
        paste0("  - ", as.character(response.options), collapse="\n"),
        "\nPlease correct this function's plot.response argument or choose another dv.",
        call.=FALSE
      )
    }
    if(controlled && identical(plot.response, "all"))
    {
      stop("There's a problem: plot.response='all' is only available when z is omitted.")
    }
  }
  
  if(!missing(dvlabs))
  {
    if(length(dvlabs) != length(dv.levels)) stop(
      wrap_console_text(
        paste(
          "Oops. The dv labels aren't the right length for this dv's values. There are",
          length(dvlabs),
          "labels for",
          length(dv.levels),
          "different values. To see how to use the dvlabs argument, try help(crosstabC)."
        )
      )
    )
  }
  if(!missing(ivlabs))
  {
    if(length(ivlabs) != length(iv.levels)) stop(
      wrap_console_text(
        paste(
          "Oops. The iv labels aren't the right length for this iv's values. There are",
          length(ivlabs),
          "labels for",
          length(iv.levels),
          "different values. To see how to use the ivlabs argument, try help(crosstabC)."
        )
      )
    )
  }
  if(!missing(zlabs))
  {
    if(length(zlabs) != length(z.levels)) stop(
      wrap_console_text(
        paste(
          "Oops. The z variable labels aren't the right length for this z variable's values. There are",
          length(zlabs),
          "labels for",
          length(z.levels),
          "different values. To see how to use the zlabs argument, try help(crosstabC)."
        )
      )
    )
  }
  
  # ---- NEW: collect printC blocks ----------------------------------------
  blocks <- list()
  # ------------------------------------------------------------------------
  
  # Capture "missing-ness" flags ONCE (never use missing() inside draw_*())
  dvlabs_missing <- missing(dvlabs)
  ivlabs_missing <- missing(ivlabs)
  zlabs_missing <- missing(zlabs)
  main_missing <- missing(main)
  xlab_missing <- missing(xlab)
  ylab_missing <- missing(ylab)
  z_palette_missing <- missing(z.palette)
  legend_title_missing <- missing(legend.title)
  
  trim_xtab <- function(tab)
  {
    tab <- as.matrix(tab)
    tab[rowSums(tab) > 0, colSums(tab) > 0, drop=FALSE]
  }
  
  safe_chisq <- function(tab)
  {
    tab.trim <- trim_xtab(tab)
    if(nrow(tab.trim) < 2 || ncol(tab.trim) < 2 || sum(tab.trim) <= 0) {
      return(list(ok=FALSE, output="Could not compute Chi-Square Test: table must have at least 2 non-empty rows and columns.",
                  statistic=NA_real_, df=NA_real_, p.value=NA_real_, small_n=FALSE, table=tab.trim))
    }
    chisq.out <- suppressWarnings(stats::chisq.test(tab.trim, correct=TRUE))
    list(
      ok=TRUE,
      output=utils::capture.output(chisq.out),
      statistic=as.numeric(chisq.out$statistic),
      df=as.numeric(chisq.out$parameter),
      p.value=as.numeric(chisq.out$p.value),
      small_n=any(chisq.out$expected < 5),
      table=tab.trim
    )
  }
  
  build_slice_xtab <- function(idx, dv, iv, w, dv.levels, iv.levels)
  {
    dv.slice <- factor(dv[idx], levels=dv.levels)
    iv.slice <- factor(iv[idx], levels=iv.levels)
    stats::xtabs(w[idx] ~ dv.slice + iv.slice)
  }
  
  build_display_table <- function(counts.table, dv.values, digits)
  {
    n.r <- nrow(counts.table)
    out <- matrix(data=NA, nrow=2*n.r, ncol=ncol(counts.table))
    colnames(out) <- colnames(counts.table)
    rownames(out) <- rep("", 2*n.r)
    coltotals <- counts.table[n.r, ]
    for(i in seq_len(n.r))
    {
      out[(2*i)-1, ] <- round(counts.table[i, ] / coltotals * 100, digits)
      out[2*i, ] <- counts.table[i, ]
      if(i <= length(dv.values)) this.dv <- dv.values[i] else this.dv <- "Totals"
      rownames(out)[(2*i)-1] <- paste("%", this.dv)
      rownames(out)[2*i] <- "Count"
    }
    rownames(out)[(2*n.r)-1] <- "% Totals"
    out
  }
  
  estimate_console_table_width <- function(x)
  {
    if(!is.matrix(x)) x <- as.matrix(x)
    if(length(x) == 0L) return(0L)
    
    x.formatted <- format(x, trim=TRUE, justify="right")
    x.colnames <- colnames(x.formatted)
    if(is.null(x.colnames)) x.colnames <- rep("", ncol(x.formatted))
    
    col.widths <- vapply(seq_len(ncol(x.formatted)), function(j) {
      max(
        nchar(x.colnames[j], type="width"),
        nchar(x.formatted[, j], type="width"),
        na.rm=TRUE
      )
    }, numeric(1))
    
    row.labels <- rownames(x.formatted)
    row.width <- if(is.null(row.labels)) 0L else max(nchar(row.labels, type="width"), na.rm=TRUE)
    
    as.integer(row.width + sum(col.widths) + (ncol(x.formatted) + 1L) * 3L)
  }
  
  build_controlled_panel_display <- function(slice.xtab, dv.values, iv.values, digits, compact)
  {
    panel.counts <- stats::addmargins(slice.xtab)
    rownames(panel.counts) <- c(dv.values, "Totals")
    colnames(panel.counts) <- c(iv.values, "Totals")
    
    panel.display <- build_display_table(
      counts.table=panel.counts,
      dv.values=dv.values,
      digits=digits
    )
    
    if(isTRUE(compact))
    {
      panel.rows <- c(seq(1, nrow(panel.display)-2, by=2), nrow(panel.display))
      rownames(panel.display)[nrow(panel.display)] <- "Count"
      panel.display <- panel.display[panel.rows, , drop=FALSE]
    }
    
    panel.display
  }
  
  build_uncontrolled_counts <- function(xtab, dv.values, iv.values)
  {
    counts <- stats::addmargins(xtab)
    rownames(counts) <- c(dv.values, "Totals")
    colnames(counts) <- c(iv.values, "Totals")
    counts
  }
  
  build_controlled_counts <- function(slice.xtabs, z.values.disp, iv.values, dv.values)
  {
    combined <- do.call(cbind, slice.xtabs)
    combined <- stats::addmargins(combined)
    colnames(combined) <- c(rep(iv.values, length(slice.xtabs)), "| Totals")
    for(i in seq_along(slice.xtabs))
    {
      this.col <- (length(iv.values) * (i - 1)) + 1
      colnames(combined)[this.col] <- paste(z.values.disp[i], ": ", colnames(combined)[this.col], sep="")
    }
    rownames(combined) <- c(dv.values, "Totals")
    combined
  }
  
  dv.values <- if(!dvlabs_missing) as.character(dvlabs) else as.character(dv.levels)
  iv.values <- if(!ivlabs_missing) as.character(ivlabs) else as.character(iv.levels)
  if(controlled) {
    z.values.disp <- if(!zlabs_missing) as.character(zlabs) else as.character(z.levels)
    slice.index <- split(seq_along(z), factor(z, levels=z.levels))
  } else {
    z.values.disp <- "All"
    slice.index <- list(All=seq_along(z))
  }
  slice.xtabs <- lapply(slice.index, function(idx) {
    build_slice_xtab(idx=idx, dv=dv, iv=iv, w=w, dv.levels=dv.levels, iv.levels=iv.levels)
  })
  
  small.n.warning <- "Chi-Square Test results may be incorrect. At least one expected frequency is less than 5."
  
  add_statement_block <- function(blocks, lines, printC)
  {
    if(isTRUE(printC))
    {
      class(lines) <- "statement"
      blocks <- add_block(blocks, lines)
    }
    blocks
  }
  
  
  plot.table <- NULL
  plot.xtabs <- NULL
  plot.iv.values <- NULL
  plot.z.values.disp <- NULL
  plot.dv.values <- NULL
  
  display.heading <- if(controlled) "Controlled Cross-Tabulation Analysis" else "Cross-Tabulation Analysis"
  blocks <- render_heading(blocks, display.heading, marker="=", printC=printC)
  
  if(controlled) heading <- paste("Cross-Tabulation of ", dv.name, " and ", iv.name, ", controlling for ", z.name, sep="")
  else heading <- paste("Cross-Tabulation of ", dv.name, " and ", iv.name, sep="")
  if(weighted) heading <- paste(heading, ", weighted by ", w.name, sep="")
  
  if(controlled) {
    counts.table <- build_controlled_counts(
      slice.xtabs=slice.xtabs,
      z.values.disp=z.values.disp,
      iv.values=iv.values,
      dv.values=dv.values
    )
  } else {
    counts.table <- build_uncontrolled_counts(
      xtab=slice.xtabs[[1]],
      dv.values=dv.values,
      iv.values=iv.values
    )
  }
  
  full.crosstab <- build_display_table(
    counts.table=counts.table,
    dv.values=dv.values,
    digits=digits
  )
  
  if(isTRUE(compact))
  {
    display.rows <- c(seq(1, nrow(full.crosstab)-2, by=2), nrow(full.crosstab))
    rownames(full.crosstab)[nrow(full.crosstab)] <- "Count"
  }
  else display.rows <- 1:nrow(full.crosstab)
  
  shown.crosstab <- full.crosstab[display.rows, , drop=FALSE]
  controlled.panel.tables <- NULL
  controlled_col_groups <- NULL
  html_colnames_controlled <- NULL
  if (controlled) {
    group.labels <- as.character(z.values.disp)
    group.spans <- rep(length(iv.values), length(group.labels))
    if (sum(group.spans) == ncol(shown.crosstab) - 1L) {
      group.labels <- c(group.labels, "All")
      group.spans <- c(group.spans, 1L)
    }
    if (sum(group.spans) == ncol(shown.crosstab)) {
      controlled_col_groups <- list(labels = group.labels, spans = group.spans)
      html_colnames_controlled <- c(rep(iv.values, length(z.values.disp)), "Totals")
      if (length(html_colnames_controlled) != ncol(shown.crosstab)) html_colnames_controlled <- NULL
    }
    controlled.panel.tables <- lapply(
      seq_along(slice.xtabs),
      function(idx) {
        build_controlled_panel_display(
          slice.xtab = slice.xtabs[[idx]],
          dv.values = dv.values,
          iv.values = iv.values,
          digits = digits,
          compact = compact
        )
      }
    )
    names(controlled.panel.tables) <- as.character(z.values.disp)
  }
  console.width <- max(40L, as.integer(getOption("width", 80L)))
  use_controlled_panels <- controlled &&
    (estimate_console_table_width(shown.crosstab) > console.width)
  
  if(table!=FALSE)
  {
    if(isTRUE(use_controlled_panels))
    {
      for(i in seq_along(slice.xtabs))
      {
        panel.tab <- controlled.panel.tables[[i]]
        panel.caption <- paste0(heading, " (", z.name, " = ", z.values.disp[i], ")")
        blocks <- render_table(
          blocks,
          x=panel.tab,
          caption=panel.caption,
          digits=digits,
          printC=FALSE
        )
      }
      if(isTRUE(printC)) {
        invisible(utils::capture.output({
          blocks <- render_table(
            blocks,
            x=shown.crosstab,
            caption=heading,
            digits=digits,
            printC=TRUE,
            col_groups=controlled_col_groups,
            html_colnames=html_colnames_controlled
          )
        }))
      }
    }
    else
    {
      blocks <- render_table(
        blocks,
        x=shown.crosstab,
        caption=heading,
        digits=digits,
        printC=printC,
        col_groups=controlled_col_groups,
        html_colnames=html_colnames_controlled
      )
    }
  }
  
  return.crosstab <- shown.crosstab
  chisq.table <- NULL
  association.table <- NULL
  slightpause()
  
  needs_controlled_stats <- controlled && (isTRUE(chisq) || isTRUE(lambda) || isTRUE(somers) || isTRUE(cramers))
  if(needs_controlled_stats)
  {
    z.n <- length(z.values.disp)
    if(isTRUE(somers)) somersD.values <- rep(NA_real_, z.n)
    if(isTRUE(cramers)) cramersV.values <- rep(NA_real_, z.n)
    if(isTRUE(lambda)) {
      z.lambda.stat <- rep(NA_real_, z.n)
      z.lambda.naive.errors <- rep(NA_real_, z.n)
      z.lambda.model.errors <- rep(NA_real_, z.n)
    }
    if(isTRUE(chisq) || isTRUE(cramers)) z.chisq.results <- vector("list", z.n)
    
    for(i in seq_len(z.n))
    {
      z.tab <- slice.xtabs[[i]]
      z.tab.trim <- trim_xtab(z.tab)
      valid.tab <- (nrow(z.tab.trim) >= 2) && (ncol(z.tab.trim) >= 2) && (sum(z.tab.trim) > 0)
      
      if(isTRUE(somers)) {
        if(valid.tab) somersD.values[i] <- tablesomersDC(z.tab.trim, digits=NULL)
        else somersD.values[i] <- NA_real_
      }
      if(isTRUE(lambda)) {
        if(valid.tab) {
          z.lambda.detail <- lambda(z.tab.trim, digits=NULL, detailed=TRUE)
          z.lambda.stat[i] <- as.numeric(z.lambda.detail[["Lambda"]][1])
          z.lambda.naive.errors[i] <- as.numeric(z.lambda.detail[["Naive Errors"]][1])
          z.lambda.model.errors[i] <- as.numeric(z.lambda.detail[["Prediction Errors"]][1])
        } else {
          z.lambda.stat[i] <- NA_real_
          z.lambda.naive.errors[i] <- NA_real_
          z.lambda.model.errors[i] <- NA_real_
        }
      }
      if(isTRUE(chisq) || isTRUE(cramers))
      {
        z.chisq <- safe_chisq(z.tab)
        z.chisq.results[[i]] <- z.chisq
        if(isTRUE(cramers)) {
          if(isTRUE(z.chisq$ok)) cramersV.values[i] <- CramersV(z.chisq$statistic, r=nrow(z.chisq$table), c=ncol(z.chisq$table), n=sum(z.chisq$table))
          else cramersV.values[i] <- NA_real_
        }
      }
    }
  }
  
  if(isTRUE(chisq))
  {
    fmt_num <- function(x, digits = 3, nsmall = digits) {
      out <- rep("", length(x))
      keep <- !is.na(x) & is.finite(x)
      if(any(keep)) {
        out[keep] <- format(
          round(x[keep], digits),
          scientific = FALSE,
          trim = TRUE,
          nsmall = nsmall
        )
      }
      out
    }
    fmt_p <- function(x, digits = 3) {
      format_p_value(x, digits = digits)
    }
    
    chisq.test.heading <- if(controlled) "Chi-Square Tests of Independence" else "Chi-Square Test of Independence"
    chisq.null.hypo <- paste0("Null hypothesis: ", dv.name, " and ", iv.name, " are independent.")
    # blocks <- render_heading(blocks, chisq.test.heading, marker="-", printC=FALSE)
    
    if(controlled)
    {
      overall.tab <- Reduce(`+`, slice.xtabs)
      overall.chisq <- safe_chisq(overall.tab)
      chisq.results.all <- c(z.chisq.results, list(overall.chisq))
      chisq.group.names <- c(as.character(z.values.disp), "All cases")
      
      chisq.df <- data.frame(
        Statistic = c("Chi-Square", "df", "P-value"),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
      for(i in seq_along(chisq.results.all))
      {
        this.res <- chisq.results.all[[i]]
        chi.val <- if(isTRUE(this.res$ok)) fmt_num(this.res$statistic, digits = digits, nsmall = digits) else ""
        df.val <- if(isTRUE(this.res$ok)) fmt_num(this.res$df, digits = 0, nsmall = 0) else ""
        p.val <- if(isTRUE(this.res$ok)) fmt_p(this.res$p.value, digits = digits) else ""
        chisq.df[[chisq.group.names[i]]] <- c(chi.val, df.val, p.val)
      }
      
      chisq.caption <- paste("Chi-Square Tests by", z.name)
      chisq.table <- chisq.df
      if(table!=FALSE) {
        blocks <- render_table(
          blocks,
          x=chisq.df,
          caption=chisq.caption,
          digits=digits,
          printC=printC,
          footer=chisq.null.hypo
        )
      }
      if(any(vapply(chisq.results.all, function(x) isTRUE(x$small_n), logical(1)))) {
        warning(small.n.warning, call.=FALSE)
      }
    } else {
      chisq.results <- safe_chisq(slice.xtabs[[1]])
      chisq.df <- data.frame(
        Statistic = c("Chi-Square", "df", "P-value"),
        Value = c(
          fmt_num(chisq.results$statistic, digits = digits, nsmall = digits),
          fmt_num(chisq.results$df, digits = 0, nsmall = 0),
          fmt_p(chisq.results$p.value, digits = digits)
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      chisq.caption <- "Chi-Square Test Results"
      chisq.table <- chisq.df
      if(table!=FALSE) {
        blocks <- render_table(
          blocks,
          x=chisq.df,
          caption=chisq.caption,
          digits=digits,
          printC=printC,
          footer=chisq.null.hypo
        )
      }
      if(isTRUE(chisq.results$small_n)) warning(small.n.warning, call.=FALSE)
    }
    slightpause()
  }
  
  moa_requested <- isTRUE(lambda) || isTRUE(somers) || isTRUE(cramers)
  if(moa_requested)
  {
    fmt_moa_num <- function(x, digits = 3, nsmall = digits) {
      out <- rep("", length(x))
      keep <- !is.na(x) & is.finite(x)
      if(any(keep)) {
        out[keep] <- format(
          round(x[keep], digits),
          scientific = FALSE,
          trim = TRUE,
          nsmall = nsmall
        )
      }
      out
    }

    to_scalar <- function(x) {
      v <- as.numeric(x)[1]
      if(length(v) == 0 || !is.finite(v)) return(NA_real_)
      v
    }
    
    if(controlled)
    {
      overall.tab <- Reduce(`+`, slice.xtabs)
      overall.tab.trim <- trim_xtab(overall.tab)
      overall.valid <- (nrow(overall.tab.trim) >= 2) && (ncol(overall.tab.trim) >= 2) && (sum(overall.tab.trim) > 0)
      moa.group.names <- c(as.character(z.values.disp), "All cases")
      
      moa.row.names <- character(0)
      moa.row.values <- list()
      
      if(isTRUE(lambda)) {
        overall.lambda.detail <- if(overall.valid) lambda(overall.tab.trim, digits=NULL, detailed=TRUE) else NULL
        overall.lambda <- if(overall.valid) to_scalar(overall.lambda.detail[["Lambda"]][1]) else NA_real_
        overall.naive.errors <- if(overall.valid) to_scalar(overall.lambda.detail[["Naive Errors"]][1]) else NA_real_
        overall.model.errors <- if(overall.valid) to_scalar(overall.lambda.detail[["Prediction Errors"]][1]) else NA_real_
        
        moa.row.names <- c(moa.row.names, "Lambda", "Naive Errors", "Model Errors")
        moa.row.values <- c(
          moa.row.values,
          list(c(as.numeric(z.lambda.stat), overall.lambda)),
          list(c(as.numeric(z.lambda.naive.errors), overall.naive.errors)),
          list(c(as.numeric(z.lambda.model.errors), overall.model.errors))
        )
      }
      if(isTRUE(somers)) {
        overall.somers <- if(overall.valid) to_scalar(tablesomersDC(overall.tab.trim, digits=NULL)) else NA_real_
        moa.row.names <- c(moa.row.names, "Somers' d")
        moa.row.values <- c(moa.row.values, list(c(as.numeric(somersD.values), overall.somers)))
      }
      if(isTRUE(cramers)) {
        overall.chisq <- safe_chisq(overall.tab)
        overall.cramers <- if(isTRUE(overall.chisq$ok)) {
          to_scalar(CramersV(chi=overall.chisq$statistic, r=nrow(overall.chisq$table), c=ncol(overall.chisq$table), n=sum(overall.chisq$table)))
        } else {
          NA_real_
        }
        moa.row.names <- c(moa.row.names, "Cramer's V")
        moa.row.values <- c(moa.row.values, list(c(as.numeric(cramersV.values), overall.cramers)))
      }
      
      moa.df <- data.frame(
        Statistic = moa.row.names,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      if(length(moa.row.values) > 0) {
        moa.mat <- do.call(rbind, moa.row.values)
        for(j in seq_along(moa.group.names)) {
          moa.df[[moa.group.names[j]]] <- moa.mat[, j]
        }
      }
      moa.caption <- paste("Measures of Association by", z.name)
    }
    else
    {
      base.tab <- slice.xtabs[[1]]
      base.tab.trim <- trim_xtab(base.tab)
      valid.tab <- (nrow(base.tab.trim) >= 2) && (ncol(base.tab.trim) >= 2) && (sum(base.tab.trim) > 0)
      moa.names <- character(0)
      moa.values <- numeric(0)
      
      if(isTRUE(lambda)) {
        lambda.detail <- if(valid.tab) lambda(base.tab.trim, digits=NULL, detailed=TRUE) else NULL
        lambda.value <- if(valid.tab) to_scalar(lambda.detail[["Lambda"]][1]) else NA_real_
        naive.errors.value <- if(valid.tab) to_scalar(lambda.detail[["Naive Errors"]][1]) else NA_real_
        model.errors.value <- if(valid.tab) to_scalar(lambda.detail[["Prediction Errors"]][1]) else NA_real_
        moa.names <- c(moa.names, "Lambda", "Naive Errors", "Model Errors")
        moa.values <- c(moa.values, lambda.value, naive.errors.value, model.errors.value)
      }
      if(isTRUE(somers)) {
        somers.value <- if(valid.tab) to_scalar(tablesomersDC(base.tab.trim, digits=NULL)) else NA_real_
        moa.names <- c(moa.names, "Somers' d")
        moa.values <- c(moa.values, somers.value)
      }
      if(isTRUE(cramers)) {
        chisq.stat <- safe_chisq(base.tab)
        cramers.value <- if(isTRUE(chisq.stat$ok)) {
          to_scalar(CramersV(chi=chisq.stat$statistic, r=nrow(chisq.stat$table), c=ncol(chisq.stat$table), n=sum(chisq.stat$table)))
        } else {
          NA_real_
        }
        moa.names <- c(moa.names, "Cramer's V")
        moa.values <- c(moa.values, cramers.value)
      }
      
      moa.df <- data.frame(
        Statistic = moa.names,
        Value = moa.values,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      moa.caption <- "Measures of Association"
    }
    
    moa.value.cols <- setdiff(colnames(moa.df), c("Group", "Statistic"))
    moa.has.missing <- FALSE
    if(length(moa.value.cols) > 0) {
      moa.num <- as.data.frame(lapply(moa.df[, moa.value.cols, drop=FALSE], function(col) {
        suppressWarnings(as.numeric(col))
      }), stringsAsFactors = FALSE, check.names = FALSE)
      moa.mat <- as.matrix(moa.num)
      moa.has.missing <- any(!is.finite(moa.mat) | is.na(moa.mat))
      for(col in moa.value.cols) {
        moa.df[[col]] <- fmt_moa_num(moa.num[[col]], digits = digits, nsmall = digits)
      }
    }
    
    association.table <- moa.df
    # blocks <- render_heading(blocks, "Measures of Association", marker="-", printC=FALSE)
    if(table!=FALSE) {
      blocks <- render_table(blocks, x=moa.df, caption=moa.caption, digits=digits, printC=printC)
    }
    
    if(moa.has.missing)
    {
      moa.note <- "Note: NA indicates the statistic could not be computed for one or more tables."
      message(moa.note)
      blocks <- add_statement_block(blocks, moa.note, printC=printC)
    }
    
    slightpause()
  }
  
  plot.table <- full.crosstab
  plot.xtabs <- if(controlled) NULL else slice.xtabs[[1]]
  plot.iv.values <- iv.values
  plot.z.values.disp <- if(controlled) z.values.disp else NULL
  plot.dv.values <- dv.values
  
  if(!identical(plot, FALSE))
  {
    if(isTRUE(picked.plot.response.note) && plot!="mosaic")
    {
      warning_message <- paste("You did not specify a plot.response value. ",
                               "So, crosstabC will plot ", dv.name, "\'s first category: \"", response.options[1], "\". ",
                               "To plot another DV value, use the plot.response argument.\n",
                               sep="")
      warning(warning_message)
    }
    
    if(controlled) tempmain <- paste("plot of ", dv.name, " and ", iv.name, ", controlling for ", z.name, sep="")
    else tempmain <- paste("plot of ", dv.name, " and ", iv.name, sep="")
    if(weighted) tempmain <- paste(tempmain, ", weighted by ", w.name, sep="")
    
    plot.response.label <- paste("%", plot.response)
    if(!controlled && identical(plot.response, "all")) {
      plot.response.label <- c(seq(1, nrow(plot.table)-2, by=2))
    }
    
    plot_mode <- if (isTRUE(plot)) "bar" else as.character(plot)
    base_plot_width_inches <- 6
    base_plot_height_inches <- 4.5
    legend_cex <- 0.9
    legend_items <- 0L
    legend_labels <- character(0)
    legend_title_text <- NULL
    if (identical(plot_mode, "bar")) {
      if (controlled) {
        legend_items <- length(plot.iv.values)
        legend_labels <- as.character(plot.iv.values)
        legend_title_text <- if (legend_title_missing) iv.name else legend.title
      } else if (identical(plot.response, "all")) {
        legend_items <- length(plot.response.label)
        legend_labels <- as.character(rownames(plot.table)[plot.response.label])
      }
    } else if (identical(plot_mode, "line")) {
      if (controlled) {
        legend_items <- length(plot.z.values.disp)
        legend_labels <- as.character(plot.z.values.disp)
        legend_title_text <- if (legend_title_missing) z.name else legend.title
      } else if (identical(plot.response, "all")) {
        legend_items <- length(plot.response.label)
        legend_labels <- as.character(rownames(plot.table)[plot.response.label])
      }
    }
    use_side_legend_panel <- legend_items > 0L
    legend_style <- if (identical(plot_mode, "line")) "line" else "fill"
    legend_width_inches <- if (use_side_legend_panel) {
      legend_panel_width_inches(
        labels = legend_labels,
        title = legend_title_text,
        cex = legend_cex,
        style = legend_style,
        min_width = 0.42,
        max_width = 1.95
      )
    } else 0
    
    restore.par.mar <- graphics::par("mar")
    
    draw_crosstab_plot <- function(emit_label_message = TRUE)
    {
      legend_drawer <- NULL
      plot_mar_right <- 1.1
      
      resolve_bar_label_positions <- function(mids, n_labels) {
        if (!is.matrix(mids)) return(as.numeric(mids))
        if (ncol(mids) == n_labels) return(as.numeric(colMeans(mids)))
        if (nrow(mids) == n_labels) return(as.numeric(rowMeans(mids)))
        flat <- as.numeric(mids)
        if (length(flat) >= n_labels) return(flat[seq_len(n_labels)])
        return(flat)
      }
      
      draw_side_legend <- function(...) {
        draw_margin_legend(
          cex = legend_cex,
          ...
        )
      }
      
      if(identical(plot, "line")) axis.labels <- plot.iv.values
      else if(identical(plot, "mosaic")) axis.labels <- plot.iv.values
      else if(controlled) axis.labels <- plot.z.values.disp
      else axis.labels <- plot.iv.values
      if(identical(plot, "mosaic")) {
        graphics::par(mar=c(4, 4, 4, plot_mar_right))
        cfg <- list(labels = axis.labels, cex = 0.9, las = 1, mtext = 2.5)
      } else {
        line_plot <- identical(plot, "line")
        controlled_plot <- controlled
        if (isTRUE(controlled_plot)) {
          cfg <- list(
            labels = axis.labels,
            cex = 0.9,
            las = 1,
            mar = c(4.1, 4.1, 3.6, plot_mar_right),
            mtext = 2.5,
            method = "horizontal",
            notify = FALSE,
            message = NULL
          )
        } else {
          cfg <- axis_label_layout_cfg(
            labels = axis.labels,
            n_slots = length(axis.labels),
            mar_left = 4.1,
            mar_top = 3.6,
            mar_right = plot_mar_right,
            allow_angled45 = !line_plot,
            fit_fraction = if (line_plot) 0.82 else 0.9,
            long_label_message = if(controlled) {
              "Adjusting plot configuration to fit labels. Consider modifying variable levels."
            } else {
              "Adjusting plot configuration to fit labels. Consider modifying column labels with ivlabs argument."
            }
          )
        }
        if (isTRUE(use_side_legend_panel)) {
          cfg$mar[4] <- cfg$mar[4] +
            (legend_width_inches + 0.2) / (graphics::par("csi") * graphics::par("mex"))
        }
        graphics::par(mar = cfg$mar)
        if (isTRUE(emit_label_message) && isTRUE(cfg$notify)) {
          message(wrap_console_text(cfg$message))
        }
      }
      
      if(isTRUE(plot) || identical(plot, "bar"))
      {
        if(main_missing) main_local <- paste("Bar", tempmain) else main_local <- main
        main_local <- strwrap(main_local, width=50)
        
        if(controlled)
        {
          if(xlab_missing) xlab_local <- paste(z.name, "values") else xlab_local <- xlab
          if(ylab_missing) ylab_local <- paste("%", plot.response) else ylab_local <- ylab
          if(z_palette_missing) bar.colors <- grDevices::gray.colors(length(plot.iv.values), start=.1, end=.7)
          else bar.colors <- grDevices::hcl.colors(length(plot.iv.values), palette=z.palette)
          if(legend_title_missing) legend_title_local <- iv.name else legend_title_local <- legend.title
          legend_drawer <- function() {
            draw_side_legend(
              legend = plot.iv.values,
              title = legend_title_local,
              fill = bar.colors,
              bty = "n",
              ncol = 1
            )
          }
          
          plotting.matrix <- matrix(plot.table[plot.response.label, -ncol(plot.table), drop=FALSE],
                                    nrow=length(plot.iv.values), byrow=FALSE)
          if (cfg$method %in% c("angled45", "wrapped")) {
            mids <- graphics::barplot(plotting.matrix,
                                      beside=TRUE, names.arg=FALSE, axisnames=FALSE, space=c(.15, 1),
                                      axes=TRUE,
                                      ylab=ylab_local, xlab="",
                                      col=bar.colors,
                                      ylim=c(0,100), main=main_local)
            draw_x_axis_labels(
              at = resolve_bar_label_positions(mids, length(axis.labels)),
              cfg = cfg, side = 1, draw_axis = FALSE
            )
          } else {
            graphics::barplot(plotting.matrix,
                              beside=TRUE, names.arg=cfg$labels, space=c(.15, 1),
                              las=cfg$las, axes=TRUE, cex.names=cfg$cex,
                              ylab=ylab_local, xlab="",
                              col=bar.colors,
                              ylim=c(0,100), main=main_local)
          }
          graphics::mtext(text = xlab_local, side = 1, line = cfg$mtext)
        } else {
          if(xlab_missing) xlab_local <- paste(iv.name, "values") else xlab_local <- xlab
          plotting.values <- plot.table[plot.response.label, -ncol(plot.table), drop=FALSE]
          if(identical(plot.response, "all")) {
            if(ylab_missing) ylab_local <- "Percentages" else ylab_local <- ylab
            if(z_palette_missing) bar.colors <- grDevices::gray.colors(nrow(plotting.values), start=.1, end=.7)
            else bar.colors <- grDevices::hcl.colors(nrow(plotting.values), palette=z.palette)
            barplot.input <- plotting.values
          } else {
            if(ylab_missing) ylab_local <- paste("%", plot.response) else ylab_local <- ylab
            if(z_palette_missing) bar.colors <- "gray80" else bar.colors <- grDevices::hcl.colors(1, palette=z.palette)
            barplot.input <- as.numeric(plotting.values)
          }
          if(identical(plot.response, "all")) {
            legend_drawer <- function() {
              draw_side_legend(
                bty = "n",
                fill = bar.colors,
                legend = rownames(plot.table)[plot.response.label],
                ncol = 1
              )
            }
          }
          
          if (cfg$method %in% c("angled45", "wrapped")) {
            mids <- graphics::barplot(barplot.input, names.arg=FALSE, axisnames=FALSE, axes=TRUE, ylab=ylab_local,
                                      ylim=c(0,100), main=main_local, col=bar.colors,
                                      beside=TRUE)
            at <- resolve_bar_label_positions(mids, length(axis.labels))
            draw_x_axis_labels(at = at, cfg = cfg, side = 1, draw_axis = FALSE)
          } else {
            graphics::barplot(barplot.input, names.arg=cfg$labels, axes=TRUE, ylab=ylab_local,
                              ylim=c(0,100), main=main_local, col=bar.colors,
                              cex.names=cfg$cex, las=cfg$las, beside=TRUE)
          }
          graphics::mtext(text = xlab_local, side = 1, line = cfg$mtext)
        }
      }
      
      if(identical(plot, "line"))
      {
        if(main_missing) main_local <- paste("Line", tempmain) else main_local <- main
        main_local <- strwrap(main_local, width=50)
        
        if(controlled)
        {
          if(ylab_missing) ylab_local <- paste("%", plot.response) else ylab_local <- ylab
          if(xlab_missing) xlab_local <- paste(iv.name, "values") else xlab_local <- xlab
          if(z_palette_missing) line.colors <- grDevices::gray.colors(length(plot.z.values.disp), start=.1, end=.7)
          else line.colors <- grDevices::hcl.colors(length(plot.z.values.disp), palette=z.palette)
          if(legend_title_missing) legend_title_local <- z.name else legend_title_local <- legend.title
          legend_drawer <- function() {
            draw_side_legend(
              legend = plot.z.values.disp,
              lty = 1:length(plot.z.values.disp),
              lwd = 2,
              title = legend_title_local,
              col = line.colors,
              bty = "n",
              pch = seq(16, length.out = length(plot.z.values.disp)),
              ncol = 1
            )
          }
          
          plotting.matrix <- matrix(plot.table[plot.response.label, -ncol(plot.table), drop=FALSE],
                                    nrow=length(plot.z.values.disp), byrow=TRUE)
          plot(x="", y="", ylim=c(0,110), xlim=c(.5, length(plot.iv.values) + .5), axes=FALSE,
               xlab="", ylab=ylab_local, main=main_local)
          for(i in 1:length(plot.z.values.disp))
          {
            graphics::points(x=1:length(plot.iv.values), y=plotting.matrix[i, ], lty=i, col=line.colors[i], pch=15+i)
            graphics::lines(x=1:length(plot.iv.values), y=plotting.matrix[i, ], lty=i, col=line.colors[i], lwd=2)
          }
          draw_x_axis_labels(at = 1:length(plot.iv.values), cfg = cfg, side = 1, draw_ticks = FALSE)
          graphics::axis(side=2, at=seq(0,100, by=10), las=2, labels=seq(0,100, by=10))
          graphics::mtext(text = xlab_local, side = 1, line = cfg$mtext)
          graphics::box()
        } else {
          if(identical(plot.response, "all") && ylab_missing) ylab_local <- "Percentages"
          else if(ylab_missing) ylab_local <- paste("%", plot.response)
          else ylab_local <- ylab
          if(xlab_missing) xlab_local <- paste(iv.name, "values") else xlab_local <- xlab
          plotting.values <- plot.table[plot.response.label, -ncol(plot.table), drop=FALSE]
          
          if(z_palette_missing && identical(plot.response, "all")) line.color <- grDevices::gray.colors(nrow(plotting.values), start=.1, end=.7)
          else if(z_palette_missing && !identical(plot.response, "all")) line.color <- "black"
          else line.color <- grDevices::hcl.colors(max(1, nrow(plotting.values)), palette=z.palette)
          if(identical(plot.response, "all")) {
            legend_drawer <- function() {
              draw_side_legend(
                bty = "n",
                lwd = 2,
                lty = 1:nrow(plotting.values),
                col = line.color,
                legend = rownames(plot.table)[plot.response.label],
                ncol = 1
              )
            }
          }
          
          plot(x="", y="", ylim=c(0,100), xlim=c(.5, length(plot.iv.values) + .5),
               axes=FALSE, xlab="", ylab=ylab_local, main=main_local, pch=16, col=line.color[1], type="n")
          if(identical(plot.response, "all")) {
            for(i in 1:nrow(plotting.values)) graphics::lines(x=1:length(plot.iv.values), y=plotting.values[i, ], lty=i, col=line.color[i], lwd=2)
          } else {
            graphics::lines(x=1:length(plot.iv.values), y=as.numeric(plotting.values), lty=1, col=line.color[1], lwd=2)
          }
          draw_x_axis_labels(at = 1:length(plot.iv.values), cfg = cfg, side = 1, draw_ticks = FALSE)
          graphics::axis(side=2, at=seq(0,100, by=10), las=2, labels=seq(0,100, by=10))
          graphics::mtext(text = xlab_local, side = 1, line = cfg$mtext)
          graphics::box()
        }
      }
      
      if(identical(plot, "mosaic"))
      {
        if(main_missing) main_local <- paste("Mosaic", tempmain) else main_local <- main
        main_local <- strwrap(main_local, width=50)
        if(ylab_missing) ylab_local <- paste(dv.name, "values") else ylab_local <- ylab
        
        if(controlled)
        {
          if(xlab_missing) xlab_local <- paste(iv.name, "controlling for", z.name) else xlab_local <- xlab
          if(z_palette_missing) panel.colors <- grDevices::gray.colors(length(plot.dv.values), start=.1, end=.7)
          else panel.colors <- grDevices::hcl.colors(length(plot.dv.values), palette=z.palette)
          keeprows <- seq(from=1, by=2, length.out=length(plot.dv.values))
          plotting.matrix <- plot.table[keeprows, -ncol(plot.table), drop=FALSE]
          graphics::mosaicplot(t(plotting.matrix), color=panel.colors, ylab=ylab_local, main=main_local, xlab=xlab_local)
        } else {
          if(xlab_missing) xlab_local <- paste(iv.name, "values") else xlab_local <- xlab
          xtabs.plot <- plot.xtabs
          if(!ivlabs_missing) colnames(xtabs.plot) <- ivlabs
          if(!dvlabs_missing) rownames(xtabs.plot) <- dvlabs
          if(z_palette_missing) panel.colors <- grDevices::gray.colors(nrow(xtabs.plot), start=.1, end=.7)
          else panel.colors <- grDevices::hcl.colors(nrow(xtabs.plot), palette=z.palette)
          graphics::mosaicplot(t(xtabs.plot), color=panel.colors, ylab=ylab_local, main=main_local, xlab=xlab_local)
        }
      }
      
      if (is.function(legend_drawer)) {
        legend_drawer()
      }
    }
    
    draw_crosstab_plot(emit_label_message = TRUE)
    if(isTRUE(printC)) {
      draw_crosstab_plot_export <- function() draw_crosstab_plot(emit_label_message = FALSE)
      blocks <- add_block(
        blocks,
        plot_block(draw = draw_crosstab_plot_export,
                   filename_base = "crosstabC.plot",
                   width = base_plot_width_inches + legend_width_inches,
                   height = base_plot_height_inches)
      )
    }
    
    graphics::par(mar=restore.par.mar)
  }
  
  if(isTRUE(printC)) {
    blocks <- add_block(blocks, match.call(expand.dots = FALSE))
    print_blocksC(blocks)
  }
  
  return.tables <- list(
    chisq_table = chisq.table,
    association_table = association.table,
    controlled_panel_tables = controlled.panel.tables
  )
  
  out <- return.crosstab
  out <- rcpa4_attach_tables(
    out = out,
    tables = return.tables,
    fn_name = "crosstabC",
    call_obj = match.call(expand.dots = FALSE)
  )
  class(out) <- unique(c("crosstabC_result", class(out)))
  
  invisible(out)
}



