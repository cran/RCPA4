#' Mean comparison analysis function, makes controlled comparisons, generates plots, performs ANOVA
#'
#' @description Mean comparison analysis, options for weighted observations and control variable. Also supports several plotting options for basic mean comparisons and controlled mean comparisons. Can conduct single and two-factor analysis of variance (ANOVA) to test differences among multiple means.
#' @param dv Dependent variable, should be in dataset$var form unless dataset specified in optional data argument.
#' @param iv Independent variable, should be in dataset$var form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable (optional), must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param z (Optional) Control variable, must be a factor; should be in dataset$z form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains dv, iv (and w) variables (optional).
#' @param digits (Optional) The number of decimal places reported in result (defaults to 2).
#' @param compact (Optional) Do you want compact version of controlled mean comparison table with N and Std. Dev. values omitted? Default is FALSE. Compact display only available for controlled comparisons.
#' @param ivlabs (Optional) A vector of names for the independent variable's values (to abbreviate the mean comparison table's row labels and iv labels on plots)
#' @param zlabs (Optional) A vector of names for the control variable's values (to abbreviate a controlled mean comparison table's column labels and z variable's labels on plots)
#' @param table (Optional) Do you want mean comparison table(s) reported? (default: TRUE)
#' @param anova (Optional) Do you want to conduct analysis of variance (ANOVA)? Default is FALSE. 
#' @param printC (Optional) Do you want results printed to .html file in your working directory? Default is FALSE. Set to TRUE to print results.
#' @param plot (Optional) Do you want a plot of the means? Default is TRUE (makes a bar plot). Additional options:
#' * "line" to make a line plot
#' * "bar", TRUE, or T for bar plot (default plot)
#' * "points" to show means as points without connecting lines,
#' * FALSE or F to suppress plot.
#' @param main (Optional) Main label for plot, if missing, default main title generated.
#' @param xlab (Optional) x-axis label for plot, if missing, default label generated using iv name.
#' @param ylab (Optional) y-axis label for plot, if missing, default label generated using dv name.
#' @param ylim (Optional) Range of y-axis values on plot.
#' @param plot.ci (Optional) Do you want vertical 95 percent confidence intervals added to line plot of means? Default is FALSE. Only works when `plot="line"` or `plot="points"`
#' @param z.palette (Optional) For bar and line charts with control variables (z), the name of HCL color palette to use. Default is "LightGrays". See `grDevices::hcl.pals` for palette names and more information. Also see <https://developer.r-project.org/Blog/public/2019/04/01/hcl-based-color-palettes-in-grdevices/> to view color palettes.
#' @param legend.title (Optional) Customize title of legend on plot used for controlling comparisons. 
#' @return Returns an augmented mean comparison table as a matrix of values.
#' Auxiliary tables are available via `[[` (for example
#' `out[["anova_table"]]` or `out[["controlled_panel_tables"]]`).
#' @examples 
#'   library(RCPA4)
#'    
#'   compmeansC(dv=anes$ft_rep_party, iv=anes$party_id, plot=FALSE)
#'
#'   \donttest{
#'   # basic usage with a plot
#'   compmeansC(dv=anes$ft_rep_party, iv=anes$party_id, w=anes$wt, plot=TRUE)
#'   
#'   # basic usage: data argument used
#'   compmeansC(dv=infant_mortality, iv=region, data=world, plot=FALSE)
#'   
#'   # with weights and z variable
#'   compmeansC(dv=anes$ft_rep_party, iv=anes$party_id, w=anes$wt, z=anes$gender_s_sex, plot="line")
#'   compmeansC(
#'     dv=anes$ft_gay_men_lesbians, iv=anes$gender_s_sex,
#'     z=anes$partyid3, compact=TRUE, plot=TRUE
#'   )
#'   }
#' @export
#' @template console-output
#' @template result-tables
#' @section Table and plot requirements:
#' At least one of `table` or `plot` must be enabled. When using `table = FALSE`,
#' keep a valid plot enabled.
#' @section RCPA4 Package Tutorial Videos:
#' * [Make Mean Comparisons with RCPA4's compmeansC Function](https://www.youtube.com/watch?v=ZuAsP0ZJuDk) 13:20, shows you how to use the RCPA4 Package's compmeansC function to compare values of an interval-level dependent variable across categories of an independent variable.  
#' * [Plotting Means with RCPA4's compmeansC Function](https://www.youtube.com/watch?v=72ROwE9zGmg) 20:42  
#' * [Making Controlled Mean Comparisons with RCPA4's compmeansC Function](https://www.youtube.com/watch?v=sGiwNUwljLc) 25:46  
#' * [Analysis of Variance with RCPA4's compmeansC Function](https://www.youtube.com/watch?v=ssBhlAHWAa4) 8:58
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapters 4, 5, 7, 10. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Sage Publications, 2020), pp.85-97, 150-156. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Making Comparisons](https://www.poliscidata.com/pages/rDemosResources.php?chapter=4), [Graphing Relationships and Describing Patterns](https://www.poliscidata.com/pages/rDemosResources.php?chapter=5), [Making Controlled Comparisons](https://www.poliscidata.com/pages/rDemosResources.php?chapter=7), and [Chi-Square Test and Analysis of Variance](https://www.poliscidata.com/pages/rDemosResources.php?chapter=10), compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom car Anova
#' @importFrom survey svydesign svyglm
#' @importFrom graphics par points lines segments legend
#' @importFrom stats qt pf aov
#' @importFrom grDevices png dev.off
#' @md
compmeansC <- function (dv, iv, w=NULL, z=NULL, data=NULL, digits=2, compact=FALSE, ivlabs=NULL, zlabs=NULL, 
                        anova=FALSE, printC=FALSE, plot=TRUE, main=NULL, xlab=NULL, ylab=NULL, ylim=NULL, 
                        plot.ci=FALSE, z.palette="LightGrays", legend.title=NULL, table=TRUE) 
{
  assert_required(dv, iv)
  assert_nonnegative_integer(digits)
  if((table == FALSE) && (plot == FALSE)) stop("At least one of table or plot must be TRUE.")
  main_missing <- missing(main)
  xlab_missing <- missing(xlab)
  ylab_missing <- missing(ylab)
  ylim_missing <- missing(ylim)
  z_palette_missing <- missing(z.palette)
  legend_title_missing <- missing(legend.title)
  
  if(plot!=FALSE)
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
  
  # Convert numeric-looking labels, rather than factor storage codes.
  # Logical outcomes intentionally use R's conventional 0/1 representation.
  if (is.logical(dv)) {
    dv <- as.numeric(dv)
  } else if (!is.numeric(dv) && Hmisc::all.is.numeric(dv, extras = c(NA))) {
    dv <- as.numeric(as.character(dv))
  }
  assert_numeric(dv)
  suggest_categorical(iv)



  

  if(plot!=FALSE)
  {
    plot.types = c(TRUE, "line", "bar", "points")
    if(plot=="lines") plot <- "line"
    if(plot=="bars") plot <- "bar"
    if(plot=="point") plot <- "points"
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
  

  
  if(!is.null(levels(iv))) iv.levels <- levels(iv) else iv.levels <- levels(as.factor(iv))
  
  if(!missing(ivlabs))
  {
    if(length(ivlabs) != length(iv.levels)) stop(
      wrap_console_text(
        paste(
          "Oops. The iv labels aren't the right length for this iv's values. There are",
          length(ivlabs),
          "labels for",
          length(iv.levels),
          "different values. To see how to use the ivlabs argument, try help(compmeansC)."
        )
      )
    )
    iv.values <- ivlabs
  }
  else 
  {
    iv.values <- iv.levels
  }
  
  if(!missing(zlabs))
  {
    if(length(zlabs) != length(unique(stats::na.omit(z)))) stop(
      wrap_console_text(
        paste(
          "Oops. The z variable's labels aren't the right length for this z variable's values. There are",
          length(zlabs),
          "labels for",
          length(unique(stats::na.omit(z))),
          "different values. To see how to use the zlabs argument, try help(compmeansC)."
        )
      )
    )
  }
  
  # ---- Collect printC blocks (only used if printC==TRUE) ----
  blocks <- list()
  
  addplotmeans <- function(resultstable, x_positions, plot.type, plot.ci=FALSE, controlled=FALSE,
                           legend.title=NULL, palette.name=NULL, show.legend=TRUE, legend.cex=1)
  {
    n.groups <- ncol(resultstable) / 3
    legend.values <- legend.lty <- legend.pch <- NULL
    if(is.null(palette.name)) lines.colors <- grDevices::gray.colors(n.groups, start=.1, end=.7)
    else lines.colors <- grDevices::hcl.colors(n.groups, palette=palette.name)
    
    for(i in 1:n.groups)
    {
      this.column <- 3*i - 2
      pch.value <- (i + 14) %% 20
      lty.value <- i %% 6
      this.group <- sub("\\|", "", colnames(resultstable)[this.column])
      legend.values <- c(legend.values, sub("Mean", "", this.group))
      if(identical(plot.type, "line")) legend.lty <- c(legend.lty, lty.value)
      legend.pch <- c(legend.pch, pch.value)
      these.means <- resultstable[-nrow(resultstable), this.column]
      graphics::points(x=x_positions, y=these.means, pch=pch.value, col=lines.colors[i])
      if(identical(plot.type, "line")) graphics::lines(x=x_positions, y=these.means, col=lines.colors[i], lty=lty.value, lwd=2)
      se.means <- resultstable[-nrow(resultstable), this.column+2] / sqrt(resultstable[-nrow(resultstable), this.column+1])
      me.means <- se.means * stats::qt(.975, resultstable[-nrow(resultstable), this.column+1] - 1)
      if(isTRUE(plot.ci)) graphics::segments(x0=x_positions, x1=x_positions, y0=these.means-me.means, y1=these.means+me.means, col=lines.colors[i], lwd=1.5)
    }
    
    if(identical(plot.type, "points")) legend.lty <- -1
    if(isTRUE(controlled) && isTRUE(show.legend)) {
      if(is.null(legend.title)) legend.title <- z.name
      graphics::legend("topleft", legend=legend.values, title=legend.title, lty=legend.lty, col=lines.colors, pch=legend.pch, bty = "n", lwd=1.5, cex=legend.cex)
    }
  }
  
  compmeans_table <- function(dv, iv, w=NULL, digits, iv.values, iv.levels)
  {
    resultstable <- matrix(data=NA, nrow=length(iv.values)+1, ncol=3)
    rownames(resultstable) <- c(iv.values, "Total")
    iv <- factor(iv, levels=iv.levels, labels=iv.values)
    xwsum <- tapply(X=dv * w, INDEX=iv, FUN=sum)
    wsum <- tapply(X=w, INDEX=iv, FUN=sum)
    xmean <- xwsum/wsum
    wsum[is.na(wsum)] <- 0
    b <- split(data.frame(dv, w), f=iv, drop=FALSE)
    wsd <- sapply(b, function(.df) {
      if(nrow(.df) == 0) return(NA_real_)
      wtd_sd(.df$dv, .df$w, digits=NULL)
    })
    xmean[length(xmean) + 1] <- wtd_mean(dv, w, digits=NULL)
    wsum[length(wsum) + 1] <- sum(w)
    wsd[length(wsd) + 1] <- wtd_sd(dv, w, digits=NULL)
    xmean <- format(xmean, drop0trailing=F, nsmall=digits, digits=digits)
    if(all(wsum[!is.na(wsum)]%%1==0)) n.drop.decimals <- TRUE else n.drop.decimals <- FALSE
    wsum <- format(wsum, drop0trailing=n.drop.decimals, nsmall=digits, digits=digits)
    wsd <- format(wsd, drop0trailing=F, nsmall=digits, digits=digits)
    calculatedvalues <- cbind(xmean, wsum, wsd)
    if(nrow(calculatedvalues) == nrow(resultstable)) resultstable <- calculatedvalues
    else 
    {
      for(k in 1:nrow(calculatedvalues)-1) 
      {
        this.iv <- rownames(calculatedvalues)[k]
        resultstable[this.iv,] <- calculatedvalues[k, ]
      }
      resultstable["Total",] <- calculatedvalues[nrow(calculatedvalues), ]
    }
    rownames(resultstable) <- c(iv.values, "Total")
    colnames(resultstable) <- c("Mean", "N", "St. Dev.")
    resultstable.numeric <- matrix(as.numeric(resultstable), nrow=nrow(resultstable), ncol=ncol(resultstable))
    rownames(resultstable.numeric) <- rownames(resultstable)
    colnames(resultstable.numeric) <- colnames(resultstable)
    return(list(display=resultstable, numeric=resultstable.numeric))
  }
  
  build_compmeans_results <- function(dv, iv, w, z, z.values, digits, iv.values, iv.levels)
  {
    z.index <- split(seq_along(z), factor(z, levels=z.values))
    z.tables <- lapply(z.index, function(idx) compmeans_table(dv[idx], iv[idx], w[idx], digits, iv.values, iv.levels))
    list(
      display = do.call(cbind, lapply(z.tables, function(x) x$display)),
      numeric = do.call(cbind, lapply(z.tables, function(x) x$numeric))
    )
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
  
  build_controlled_panel_table <- function(resultstable, z_index, compact)
  {
    cols <- (3 * z_index - 2):(3 * z_index)
    panel <- resultstable[, cols, drop=FALSE]
    if(isTRUE(compact)) panel <- panel[, 1, drop=FALSE]
    
    if(ncol(panel) == 3) colnames(panel) <- c("Mean", "N", "St. Dev.")
    else colnames(panel) <- "Mean"
    
    panel
  }
  
  compute_anova_result <- function(dv, iv, z, w, controlled, weighted, digits, dv.name, iv.name, z.name=NULL)
  {
    format_anova_table <- function(tbl, iv.name, z.name=NULL) {
      out <- as.data.frame(tbl, stringsAsFactors = FALSE, check.names = FALSE)
      rn <- rownames(out)
      if(!is.null(rn)) {
        rn <- as.character(rn)
        rn[rn == "iv"] <- iv.name
        if(!is.null(z.name)) rn[rn == "z"] <- z.name
        rownames(out) <- rn
      }
      df.cols <- grep("df", tolower(colnames(out)), fixed = TRUE)
      if(length(df.cols) > 0) {
        for(j in df.cols) {
          x <- suppressWarnings(as.numeric(out[[j]]))
          keep <- !is.na(x) & is.finite(x)
          out[[j]] <- ""
          out[[j]][keep] <- format(round(x[keep], 0), scientific = FALSE, trim = TRUE, nsmall = 0)
        }
      }
      p.cols <- grep("pr\\(>|p-value", tolower(colnames(out)))
      if(length(p.cols) > 0) {
        for(j in p.cols) {
          displayed <- as.character(out[[j]])
          x <- suppressWarnings(as.numeric(displayed))
          keep <- !is.na(x) & is.finite(x)
          out[[j]] <- ""
          out[[j]][keep] <- format_p_value(x[keep], digits = digits)
          already.formatted <- grepl("^<", trimws(displayed))
          out[[j]][already.formatted] <- displayed[already.formatted]
        }
      }
      out
    }
    
    if(!weighted)
    {
      if(controlled)
      {
        model <- stats::aov(dv ~ iv + z)
        anova.tbl <- format_anova_table(car::Anova(model), iv.name=iv.name, z.name=z.name)
        return(list(
          heading = paste("ANOVA Table, Type II Tests (Response: ", dv.name, ")", sep=""),
          table = anova.tbl,
          prelude = NULL,
          pause = TRUE
        ))
      }
      model <- stats::aov(dv ~ iv)
      anova.tbl <- format_anova_table(round(anova(model), digits), iv.name=iv.name, z.name=NULL)
      return(list(
        heading = paste("Analysis of Variance (Response: ", dv.name, ")", sep=""),
        table = anova.tbl,
        prelude = NULL,
        pause = FALSE
      ))
    }
    
    if(controlled)
    {
      var.df <- data.frame(dv, iv, z, w)
      svy.design <- survey::svydesign(ids=~0, weights=~w, data=var.df)
      svyglm.model <- survey::svyglm(dv ~ iv + z, design=svy.design, na.action="na.omit")
      anova.tbl <- format_anova_table(car::Anova(svyglm.model, test="F", method="Wald"), iv.name=iv.name, z.name=z.name)
      return(list(
        heading = paste("ANOVA Table, Type II Tests (Response: ", dv.name, ")", sep=""),
        table = anova.tbl,
        prelude = NULL,
        pause = TRUE
      ))
    }
    
    var.df <- data.frame(dv, iv, w)
    svy.design <- survey::svydesign(ids=~0, weights=~w, data=var.df)
    svyglm.model <- survey::svyglm(dv ~ iv, design=svy.design, na.action="na.omit")
    SSE_between <- (svyglm.model$null.deviance - svyglm.model$deviance)
    num_df <- (svyglm.model$df.null - svyglm.model$df.residual)
    MSE_between <- SSE_between / num_df
    SSE_within <- (svyglm.model$deviance)
    denom_df <- (svyglm.model$df.residual)
    MSE_within <- SSE_within / denom_df
    f_stat <- MSE_between / MSE_within
    p_value <- stats::pf(f_stat, df1=num_df, df2=denom_df, lower.tail=FALSE)
    anova.table <- data.frame(c(num_df, denom_df), round(c(SSE_between, SSE_within), digits),
                              round(c(MSE_between, MSE_within), digits),
                              c(round(f_stat, digits), ""), c(format_p_value(p_value, digits = digits), ""))
    rownames(anova.table) <- c(iv.name, "Residuals")
    colnames(anova.table) <- c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
    anova.table <- format_anova_table(anova.table, iv.name=iv.name, z.name=NULL)
    list(
      heading = paste("Analysis of Variance (Response: ", dv.name, ")", sep=""),
      table = anova.table,
      prelude = NULL,
      pause = FALSE
    )
  }
  
  # ---------------------------
  # Unified comparison workflow
  # ---------------------------
  if(controlled) {
    if(is.factor(z)) z.values <- levels(droplevels(z)) else z.values <- unique(z)
  } else {
    z.values <- 1
  }
  
  resultstable.obj <- build_compmeans_results(
    dv = dv,
    iv = iv,
    w = w,
    z = if(controlled) z else rep(1, length(dv)),
    z.values = z.values,
    digits = digits,
    iv.values = iv.values,
    iv.levels = iv.levels
  )
  resultstable <- resultstable.obj$display
  resultstable.num <- resultstable.obj$numeric
  
  if(controlled) {
    for(i in 1:length(z.values))
    {
      meancol <- 1 + 3*(i-1)
      if(compact==TRUE) col.sep <- "" else col.sep <- "| "
      if(missing(zlabs)) colnames(resultstable)[meancol] <- paste(col.sep, z.values[i], " Mean", sep="")
      else colnames(resultstable)[meancol] <- paste(col.sep, zlabs[i], " Mean", sep="")
    }
    if(compact==TRUE) show.cols <- seq(1, ncol(resultstable), by=3) else show.cols <- 1:ncol(resultstable)
  } else {
    show.cols <- 1:ncol(resultstable)
  }
  
  if(!missing(ivlabs)) {
    rownames(resultstable) <- c(ivlabs, "Total")
    rownames(resultstable.num) <- c(ivlabs, "Total")
  }
  
  display.heading <- if(controlled) "Controlled Mean Comparison Analysis" else "Mean Comparison Analysis"
  blocks <- render_heading(blocks, display.heading, marker="=", printC=printC)
  
  if(controlled) heading <- paste("Comparison of ", dv.name , " Means by ", iv.name, ", Controlling for ", z.name, sep="")
  else heading <- paste("Mean Values of", dv.name , "by", iv.name)
  if(weighted==TRUE) heading <- paste(heading, ", Weighted by ", w.name, sep="")
  
  shown.resultstable <- resultstable[, show.cols, drop=FALSE]
  controlled.panel.tables <- NULL
  controlled_col_groups <- NULL
  html_colnames_controlled <- NULL
  if (controlled) {
    if(!missing(zlabs)) z.values.disp <- as.character(zlabs) else z.values.disp <- as.character(z.values)
    group.spans <- rep(if(isTRUE(compact)) 1L else 3L, length(z.values.disp))
    if (sum(group.spans) == ncol(shown.resultstable)) {
      controlled_col_groups <- list(labels = z.values.disp, spans = group.spans)
      if (isTRUE(compact)) html_colnames_controlled <- rep("Mean", length(z.values.disp))
      else html_colnames_controlled <- rep(c("Mean", "N", "St. Dev."), length(z.values.disp))
      if (length(html_colnames_controlled) != ncol(shown.resultstable)) html_colnames_controlled <- NULL
    }
    controlled.panel.tables <- lapply(
      seq_along(z.values.disp),
      function(idx) build_controlled_panel_table(
        resultstable = resultstable,
        z_index = idx,
        compact = compact
      )
    )
    names(controlled.panel.tables) <- as.character(z.values.disp)
  }
  console.width <- max(40L, as.integer(getOption("width", 80L)))
  use_controlled_panels <- controlled &&
    (estimate_console_table_width(shown.resultstable) > console.width)
  
  if(table!=FALSE) {
    if(isTRUE(use_controlled_panels)) {
      for(i in seq_along(z.values.disp)) {
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
            x=shown.resultstable,
            caption=heading,
            digits=digits,
            printC=TRUE,
            col_groups=controlled_col_groups,
            html_colnames=html_colnames_controlled
          )
        }))
      }
    } else {
      blocks <- render_table(blocks, x=shown.resultstable, caption=heading, digits=digits, printC=printC, col_groups=controlled_col_groups, html_colnames=html_colnames_controlled)
    }
  }
  
  # Compact is display-only; preserve full result matrix for backward compatibility.
  resultstable.return <- resultstable
  anova.table <- NULL
  
  if(anova==TRUE)
  {
    # blocks <- render_heading(blocks, "Analysis of Variance (ANOVA)", marker="-", printC=FALSE)
    anova.out <- compute_anova_result(
      dv=dv, iv=iv, z=z, w=w,
      controlled=controlled, weighted=weighted,
      digits=digits, dv.name=dv.name, iv.name=iv.name, z.name=z.name
    )
    if(!is.null(anova.out$prelude)) {
      message(paste(anova.out$prelude, collapse="\n\n"))
    }
    anova.table <- anova.out$table
    if(table!=FALSE) {
      blocks <- render_table(blocks, x=anova.out$table, caption=anova.out$heading, digits=digits, printC=printC)
    }
    if(isTRUE(anova.out$pause)) slightpause()
  }
  
  if(plot!=FALSE) 
  {
    plot_is_line_or_points <- plot %in% c("line", "points")
    plot_is_bar <- isTRUE(plot) || identical(plot, "bar")
    
    if(main_missing) 
    {
      if(controlled) main <- paste("Comparison of ", dv.name , " means by ", iv.name, ", controlling for ", z.name, sep="")
      else main <- paste("Plot of Means of", dv.name, "by", iv.name)
      if(weighted==TRUE) main <- paste(main, ", weighted by ", w.name, sep="")
    }
    if(ylab_missing) ylab <- paste("Means of", dv.name)
    main <- strwrap(main, width=50)
    restore.par.mar <- graphics::par("mar")
    storage.mode(resultstable.num) <- "numeric"
    
    if(controlled) {
      if(!missing(zlabs)) z.values.plot <- zlabs else z.values.plot <- z.values
    } else {
      z.values.plot <- z.values
    }
    
    if(plot_is_line_or_points) axis.labels <- rownames(resultstable)[1:(nrow(resultstable)-1)]
    else if(controlled) axis.labels <- as.character(z.values.plot)
    else axis.labels <- rownames(resultstable)[1:(nrow(resultstable)-1)]
    
    if(xlab_missing) {
      if(plot_is_line_or_points) xlab <- paste(as.character(iv.name), "values")
      else if(controlled) xlab <- paste(as.character(z.name), "values")
      else xlab <- paste(as.character(iv.name), "values")
    }
    
    legend_cex <- 0.9
    base_plot_width_inches <- 6
    base_plot_height_inches <- 4.5
    use_side_legend_panel <- FALSE
    legend_width_inches <- 0
    legend_labels <- character(0)
    legend_title_local <- NULL
    legend_line_colors <- NULL
    legend_line_lty <- NULL
    legend_line_pch <- NULL
    legend_fill <- NULL
    
    if(controlled && plot_is_line_or_points) {
      legend_labels <- as.character(z.values.plot)
      if(length(legend_labels) > 0) {
        legend_title_local <- if(legend_title_missing) z.name else legend.title
        if(z_palette_missing) legend_line_colors <- grDevices::gray.colors(length(legend_labels), start=.1, end=.7)
        else legend_line_colors <- grDevices::hcl.colors(length(legend_labels), palette=z.palette)
        if(identical(plot, "points")) legend_line_lty <- rep(-1, length(legend_labels))
        else legend_line_lty <- seq_len(length(legend_labels)) %% 6
        legend_line_pch <- (seq_len(length(legend_labels)) + 14) %% 20
        use_side_legend_panel <- TRUE
      }
    }
    
    if(controlled && plot_is_bar) {
      legend_labels <- rownames(resultstable)[1:(nrow(resultstable)-1)]
      if(length(legend_labels) > 0) {
        legend_title_local <- if(legend_title_missing) iv.name else legend.title
        if(z_palette_missing) legend_fill <- grDevices::gray.colors(length(legend_labels), start=.1, end=.7)
        else legend_fill <- grDevices::hcl.colors(length(legend_labels), palette=z.palette)
        use_side_legend_panel <- TRUE
      }
    }
    
    if(isTRUE(use_side_legend_panel)) {
      legend_style <- if(plot_is_line_or_points) "line" else "fill"
      legend_width_inches <- legend_panel_width_inches(
        labels = legend_labels,
        title = legend_title_local,
        cex = legend_cex,
        style = legend_style,
        min_width = 0.38,
        max_width = 1.45
      )
    }
    
    draw_compmeans_plot <- function(emit_label_message = TRUE) {
      legend_drawer <- NULL
      
      draw_side_legend <- function(...) {
        draw_margin_legend(
          cex = legend_cex,
          ...
        )
      }
      
      if (controlled) {
        axis_cfg <- list(
          labels = axis.labels,
          cex = 0.9,
          las = 1,
          mar = c(4.1, 4.1, 5.1, 1.1),
          mtext = 2.5,
          method = "horizontal",
          notify = FALSE,
          message = NULL
        )
      } else {
        axis_cfg <- axis_label_layout_cfg(
          labels = axis.labels,
          n_slots = length(axis.labels),
          mar_left = 4.1,
          mar_top = if(controlled) 5.1 else 3.6,
          mar_right = 1.1,
          long_label_message = if(!controlled) {
            "One or more text labels for bars is long. Consider abbreviating with ivlabs argument."
          } else {
            NULL
          }
        )
      }
      if (isTRUE(use_side_legend_panel)) {
        axis_cfg$mar[4] <- axis_cfg$mar[4] +
          (legend_width_inches + 0.2) / (graphics::par("csi") * graphics::par("mex"))
      }
      graphics::par(mar = axis_cfg$mar)
      if (isTRUE(emit_label_message) && isTRUE(axis_cfg$notify)) {
        message(wrap_console_text(axis_cfg$message))
      }
      
      n.x.values.plot <- length(axis.labels)
      x.positions <- seq_len(n.x.values.plot)
      
      if(plot_is_line_or_points)
      {
        if(ylim_missing) ylim <- range(dv, na.rm=TRUE)
        plot(x="", y="", xlim=c(.5, n.x.values.plot+.5), ylim=ylim,
             xlab="", ylab=ylab, axes=F, main=main)
        draw_x_axis_labels(at = x.positions, cfg = axis_cfg, side = 1, draw_ticks = FALSE)
        axis(side=2, las=2)
        mtext(text = xlab, side = 1, line = axis_cfg$mtext)
        box()
        
        if(controlled) {
          if(isTRUE(use_side_legend_panel)) {
            legend_drawer <- function() {
              draw_side_legend(
                legend = legend_labels,
                title = legend_title_local,
                lty = legend_line_lty,
                col = legend_line_colors,
                pch = legend_line_pch,
                bty = "n",
                lwd = 1.5,
                ncol = 1
              )
            }
          }
          addplotmeans(
            resultstable=resultstable.num,
            x_positions=x.positions,
            plot.type=plot,
            plot.ci=plot.ci,
            controlled=TRUE,
            legend.title=if(legend_title_missing) NULL else legend.title,
            palette.name=if(z_palette_missing) NULL else z.palette,
            show.legend=!isTRUE(use_side_legend_panel),
            legend.cex=legend_cex
          )
        } else {
          addplotmeans(
            resultstable=resultstable.num,
            x_positions=x.positions,
            plot.type=plot,
            plot.ci=plot.ci,
            controlled=FALSE
          )
        }
      }
      
      if(plot_is_bar)
      {
        if(ylim_missing) ylim <- range(dv, na.rm=TRUE)
        if(controlled)
        {
          if(!is.null(legend_fill)) bar.colors <- legend_fill
          else if(z_palette_missing) bar.colors <- grDevices::gray.colors(nrow(resultstable.num) - 1, start=.1, end=.7)
          else bar.colors <- grDevices::hcl.colors(nrow(resultstable.num) - 1, palette=z.palette)
          plotvalues <- resultstable.num[-nrow(resultstable.num), seq(1, ncol(resultstable.num), by=3)]
          if(isTRUE(use_side_legend_panel)) {
            legend_drawer <- function() {
              draw_side_legend(
                legend = legend_labels,
                title = legend_title_local,
                fill = bar.colors,
                bty = "n",
                ncol = 1
              )
            }
          }
          if (axis_cfg$method %in% c("angled45", "wrapped")) {
            mids <- barplot(plotvalues, names.arg=FALSE, axisnames=FALSE, beside=TRUE,
                            xlab="", ylab=ylab, axes=F, 
                            main=main, ylim=ylim,
                            col=bar.colors, xpd=F,
                            space=c(.2, 1.25))
            draw_x_axis_labels(at = colMeans(mids), cfg = axis_cfg, side = 1, draw_axis = FALSE)
          } else {
            barplot(plotvalues, names.arg=axis_cfg$labels, beside=TRUE,
                    xlab="", ylab=ylab, las=axis_cfg$las, axes=F, cex.names=axis_cfg$cex, 
                    main=main, ylim=ylim,
                    col=bar.colors, xpd=F,
                    space=c(.2, 1.25))
          }
        } else {
          if(z_palette_missing) bar.colors <- "gray80"
          else bar.colors <- grDevices::hcl.colors(1, palette=z.palette)
          if (axis_cfg$method %in% c("angled45", "wrapped")) {
            mids <- barplot(resultstable.num[-nrow(resultstable.num), 1], 
                            names.arg=FALSE, axisnames=FALSE,
                            xlab="", ylab=ylab, axes=F, 
                            main=main, space=.1, ylim=ylim, xpd=F,
                            col=bar.colors)
            draw_x_axis_labels(at = mids, cfg = axis_cfg, side = 1, draw_axis = FALSE)
          } else {
            barplot(resultstable.num[-nrow(resultstable.num), 1], 
                    names.arg=axis_cfg$labels,
                    xlab="", ylab=ylab, las=axis_cfg$las, axes=F, 
                    main=main, space=.1, ylim=ylim, xpd=F,
                    cex.names=axis_cfg$cex, col=bar.colors)
          }
        }
        axis(side=2, las=2)
        mtext(text = xlab, side = 1, line = axis_cfg$mtext)
        graphics::box(bty="l")
      }
      
      if (is.function(legend_drawer)) {
        legend_drawer()
      }
    }
    
    draw_compmeans_plot(emit_label_message = TRUE)
    
    if(isTRUE(printC)) {
      draw_compmeans_plot_export <- function() draw_compmeans_plot(emit_label_message = FALSE)
      blocks <- add_block(
        blocks,
        plot_block(draw = draw_compmeans_plot_export,
                   filename_base = "compmeansC.plot",
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
    anova_table = anova.table,
    controlled_panel_tables = controlled.panel.tables
  )
  
  out <- resultstable.return
  out <- rcpa4_attach_tables(
    out = out,
    tables = return.tables,
    fn_name = "compmeansC",
    call_obj = match.call(expand.dots = FALSE)
  )
  class(out) <- unique(c("compmeansC_result", class(out)))
  
  invisible(out)
}
