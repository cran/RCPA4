#' One and two-sample difference of means tests (t-tests) with confidence intervals.
#'
#' @description Conducts one and two-sample difference of means tests (t-tests). Options for weighting observations, known population standard deviation, equal or unequal variances, paired observations. Numeric-looking factor or character labels, such as `"10"` and `"20"`, are interpreted as their displayed numeric values.
#' @param x1 The first variable to be compared (mean of x1 will be compared to mean of x2). Must be numeric variable. Should be in the form dataset$var, unless dataset specified with data argument.
#' @param x2 The variable (or number) to which x1 is compared. Should be in the form dataset$var, unless dataset specified with data argument. You can set x2 equal to a number to conduct a one sample means test. For example, to test whether x1 could have population mean of 50, you'd set x2 = 50.
#' @param w (Optional) Weights variable (optional). Should be in the form dataset$var, unless dataset specified with data argument.
#' @param data (Optional) The dataset that contains x1, x1 and x2, or dv and iv. 
#' @param dv The dependent variable. Must be numeric variable. Should be in the form dataset$var, unless dataset specified with data argument.
#' @param iv	The independent variable. Should have two distinct values (like treatment and control). Should be in the form dataset$var, unless dataset specified with data argument.
#' @param digits (Optional) Number of digits to report after decimal place, optional (default: 2).
#' @param table (Optional) Display result tables, including the optional
#'   equal-variance test table? Defaults to `TRUE`. Set to `FALSE` to suppress
#'   console and HTML tables while still returning the computed results.
#' @param conf (Optional) Desired confidence level, as percentage (default: 95)
#' @param plot (Optional) Do you want a plot of the confidence interval of the difference of means? (default: TRUE)
#' @param var.equal (Optional) With two-sample tests, do you want to assume equal variances? (default: FALSE) 
#' @param paired (Optional) With two-sample tests, are the observations paired? (default: FALSE)
#' @param pop.sd (Optional) If the population standard deviation is known, you can specify it.
#' @param var.test (Optional) If set to TRUE, will test the assumption that two sample variance are equal using an F test. Default is FALSE. The var.test option implemented for both weighted and unweighted analysis. If you are not using sample weights, you can supplement this F test with additional tests such as `stats::bartlett.test` and `car::leveneTest`.  
#' @param printC (Optional) Do you want results printed to .html file in your working directory? Default is FALSE. Set to TRUE to print results.
#' @param main (Optional) Main title for plot of confidence interval of difference
#' @param xlab (Optional) Label for x-axis of plot of confidence interval of difference
#' @param xlim (Optional) A vector (of length 2) specifying the range of the x-axis, useful to zoom in on CI. 
#' @param ... (Optional) Additional arguments passed to `plot` function for the CI plot
#' @return Invisibly returns a structured list with stable keys:
#' `descriptive_table`, `test_stats_table`, `ci_table`, and `variance_test_table`
#' (`NULL` when optional analysis is not computed).
#' @examples 
#'    library(RCPA4)
#'    
#'    \donttest{ 
#'    # one sample test against hypothesized value
#'    testmeansC(x1=world$literacy, x2=80)
#'    
#'    # with x1 and x2 
#'    testmeansC(x1=ft_trump_post, x2=ft_vance_post, w=wt, data=anes)
#'    
#'    # with paired x1 and x2
#'    testmeansC(x1=anes$ft_trump_post, x2=anes$ft_trump_pre, w=anes$wt, paired=TRUE)
#'
#'    # with dv and iv 
#'    testmeansC(dv=anes$ft_big_business, iv=anes$gender_s_sex, w=anes$wt)
#'    }
#' @export
#' @template console-output
#' @template result-tables
#' @section Equal-variance table:
#' Use `var.test = TRUE` to compute the optional equal-variance test. Its table
#' is displayed only when `table = TRUE`. With `table = FALSE`, the test is
#' still computed and returned in `variance_test_table`, but its table is
#' omitted from console and HTML output. Use `var.test = FALSE` to skip the test.
#' @section RCPA4 Package Tutorial Videos:
#' * [Testing Means with RCPA4 Package's testmeansC Function](https://www.youtube.com/watch?v=K3sve6O77fY) 12:22   
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapter 9. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Sage Publications, 2020), pp.201-215. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Hypothesis Tests with One and Two Samples](https://www.poliscidata.com/pages/rDemosResources.php?chapter=9), Compiled by Barry C. Edwards 
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com), find datasets for your own research and resources to help with the analysis. 
#' @importFrom grDevices png dev.off
#' @importFrom stats na.omit pt pnorm pf
#' @importFrom graphics par abline points segments axis box
#' @md

testmeansC <- function(x1, x2, w=NULL, data=NULL, dv, iv, digits=2, var.equal=FALSE, paired=FALSE, pop.sd=FALSE, 
                       var.test=FALSE, printC=FALSE, conf=95, plot=TRUE, main=NULL, xlab=NULL, xlim=NULL, table=TRUE, ...)
{
  using_dv_iv <- (!missing(dv) || !missing(iv))
  using_x_pair <- (!missing(x1) || !missing(x2))
  
  if(using_dv_iv && using_x_pair) {
    stop("There's a problem: choose either dv/iv or x1/x2 for testmeansC(), but not both.")
  }
  if(using_dv_iv) {
    assert_required(dv, iv)
  } else if(using_x_pair) {
    assert_required(x1, x2)
  } else {
    stop(wrap_console_text("Oops. You need to specify a dependent variable (dv) and indepenent variable (iv), or two variables to compare (x1 and x2). Enter example(testmeansC) to see an example and help(testmeansC) to learn more about this function."))
  }
  assert_nonnegative_integer(digits)
  if((table == FALSE) && (plot == FALSE)) stop("At least one of table or plot must be TRUE.")
  

  if(plot != FALSE)
  {
    old.par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old.par), add=TRUE)
  }
  
  if (using_dv_iv)
  {
    ctx <- setup_vars_input(
      var_exprs = list(dv = substitute(dv), iv = substitute(iv)),
      w_expr = if (missing(w)) NULL else substitute(w),
      data_expr = substitute(data),
      env = parent.frame(),
      data_missing = missing(data),
      drop_complete = FALSE,
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
  if (using_x_pair)
  {
    x2_expr <- substitute(x2)
    ctx <- setup_vars_input(
      var_exprs = list(x1 = substitute(x1)),
      w_expr = if (missing(w)) NULL else substitute(w),
      data_expr = substitute(data),
      env = parent.frame(),
      data_missing = missing(data),
      drop_complete = FALSE,
      single_vars = c("x1"),
      ref_var = "x1"
    )
    x1 <- ctx$vars$x1
    w <- ctx$vars$w
    x1.name <- ctx$meta$x1$name
    x2.name <- friendly_expr_label(x2_expr)
    w.name <- ctx$meta$w$name
    weighted <- ctx$meta$weighted
    x2 <- eval_in_data_expr(x2_expr, data = ctx$data$value, env = parent.frame())
    if(length(x2) > 1 && length(x2) != length(x1)) {
      stop("There's a problem: x1 and x2 must have the same length for two-sample testmeansC().")
    }
  }
  
  coerce_numeric_labels <- function(x) {
    if (!is.numeric(x) && Hmisc::all.is.numeric(x, extras = c(NA))) {
      return(as.numeric(as.character(x)))
    }
    x
  }

  if (using_dv_iv) {
    dv <- coerce_numeric_labels(dv)
  }
  if (using_x_pair) {
    x1 <- coerce_numeric_labels(x1)
    x2 <- coerce_numeric_labels(x2)
  }

  if(using_dv_iv) 
  {
    assert_numeric(dv)
    suggest_categorical(iv, max_unique = 2)
    if(length(unique(stats::na.omit(iv))) > 2) stop(wrap_console_text(paste(gettext("There's a problem:", domain = "R-descr"), iv.name, gettext("has more than 2 unique values. Collapse values or use ANOVA instead?", domain = "R-descr"))))
  }
  
  
  if(using_x_pair) assert_numeric(x1)
  assert_positive_numeric(conf)
  if((conf > 0) & (conf < 1)) 
  {
    conf <- conf*100
    message(wrap_console_text("Please note: It looks like you entered the level for confidence interval table as a proportion. Confidence level should be percentage, like 95 or 99. Attempting to convert..."))
  }
  if(conf > 100) stop("There's a problem: confidence level for confidence interval table should not exceed 100.")
  if((conf <= 50) & (conf >= 1)) stop(wrap_console_text(paste(gettext("There's a problem: confidence level for confidence interval table should be percentage, like 95 or 99, not Type I error rate.", domain = "R-descr"))))
  
  onesampletest <- FALSE
  if(using_x_pair) 
  {
    if(length(x2) == 1) assert_numeric(x2)
    if(length(x2) == 1) onesampletest <- TRUE    
  }
  
  half_alpha <- (1 - conf/100) / 2
  
  # ---- Collect printC blocks (only used if printC==TRUE) ----
  blocks <- list()
  var.test.table <- NULL
  
  # Heading (ALWAYS printed to console)
  blocks <- render_heading(blocks, "Difference of Means Test", marker="=", printC=printC)
  
  # set-up for dv ~ iv comparison (two groups)
  if(using_dv_iv) 
  {
    if(!is.null(levels(iv))) iv.values <- levels(iv) else iv.values <- unique(iv)
    x1 <- dv[iv==iv.values[1]]
    if(!weighted) w1 <- rep(1, length(x1)) else w1 <- w[iv==iv.values[1]]
    x2 <- dv[iv==iv.values[2]]
    if(!weighted) w2 <- rep(1, length(x2)) else w2 <- w[iv==iv.values[2]]
    group1.name <- iv.values[1]
    group2.name <- iv.values[2]
    x1.name <- paste("Mean(", dv.name, "|", group1.name, ")", sep="")
    x2.name <- paste("Mean(", dv.name, "|", group2.name, ")", sep="")
    paired <- FALSE
    onesampletest <- FALSE
  }
  
  # set-up for x1 vs x2 comparison
  if(using_x_pair) 
  {
    if(!weighted) 
    {
      w <- w1 <- rep(1, length(x1))  
      w2 <- rep(1, length(x2))
    }
    else w2 <- w1 <- w
    group1.name <- x1.name
    group2.name <- x2.name
  }
  
  # handle missingness + paired/one-sample logic
  if(paired==TRUE)
  {
    k <- grep(FALSE, (is.na(x1) | is.na(x2) | is.na(w)))
    x1 <- x1[k]; x2 <- x2[k]
    w1 <- w2 <- w[k]
    var.equal <- FALSE
  }
  else if(onesampletest==TRUE)
  {
    mean2 <- x2
    sd2 <- 0
    n2 <- 0
    var.equal <- FALSE
    paired <- FALSE
  }
  else
  {
    k1 <- grep(FALSE, (is.na(x1) | is.na(w1)))
    x1 <- x1[k1]; w1 <- w1[k1]
    k2 <- grep(FALSE, (is.na(x2) | is.na(w2)))
    x2 <- x2[k2]; w2 <- w2[k2]
  }
  
  n1    <- sum(w1)
  mean1 <- wtd_mean(x1, w1, digits=NULL)
  sd1   <- wtd_sd(x1, w1, digits=NULL)
  
  if(onesampletest==FALSE)
  {
    mean2 <- wtd_mean(x2, w2, digits=NULL)
    sd2   <- wtd_sd(x2, w2, digits=NULL)
    n2    <- sum(w2)
  }
  
  if(isTRUE(onesampletest)) {
    x2.hyp.value <- format(as.numeric(x2), scientific = FALSE, trim = TRUE)
    null.hypo.statement <- paste0("Null hypo: Mean(", x1.name, ") = ", x2.hyp.value)
    alt.hypo.statement  <- paste0("Alt. hypo: Mean(", x1.name, ") \u2260 ", x2.hyp.value)
  } else if (isTRUE(using_dv_iv)) {
    null.hypo.statement <- paste0("Null hypo: ", x1.name, " = ", x2.name)
    alt.hypo.statement  <- paste0("Alt. hypo: ", x1.name, " \u2260 ", x2.name)
  } else {
    null.hypo.statement <- paste0("Null hypo: Mean(", x1.name, ") = Mean(", x2.name, ")")
    alt.hypo.statement  <- paste0("Alt. hypo: Mean(", x1.name, ") \u2260 Mean(", x2.name, ")")
  }
  
  # assumptions about variances / SE / df
  if(pop.sd!=FALSE & paired==FALSE)
  {
    assert_positive_numeric(pop.sd)
    se1 <- pop.sd / sqrt(n1)
    if(onesampletest==FALSE) se2 <- pop.sd / sqrt(n2) else se2 <- 0
    sediff <- sqrt(se1^2 + se2^2)
    df <- NA
    variances.statement <- paste("Population standard deviation assumed to be ", pop.sd, ".", sep="")
    var.equal <- FALSE
  }
  else if(paired==TRUE)
  {
    if(pop.sd!=FALSE) sediff <- pop.sd / sqrt(n1)
    differences <- x1 - x2
    diff.means <- wtd_mean(differences, w1, digits=NULL)
    if(pop.sd==FALSE) sediff <- wtd_sd(differences, w1, digits=NULL) / sqrt(n1)
    df <- n1 - 1
    if(pop.sd==FALSE) variances.statement <- paste("Population standard deviation is unknown.")
    if(pop.sd!=FALSE) variances.statement <- paste("Population standard deviation assumed to be ", pop.sd, ".", sep="")
  }
  else if(var.equal==TRUE)
  {
    variances.statement <- paste("Population standard deviation is unknown. Assuming that samples have equal variances.")
    pooled.variance <- ((n1-1)*sd1^2+(n2-1)*sd2^2) / (n1+n2-2)
    sediff <- sqrt(pooled.variance)*(sqrt((1/n1)+(1/n2)))
    df <- n1 + n2 - 2
  }
  else
  {
    se1 <- sd1 / sqrt(n1)
    if(onesampletest==FALSE) se2 <- sd2 / sqrt(n2) else se2 <- 0
    sediff <- sqrt(se1^2 + se2^2)
    df <- floor(((se1^2+se2^2)^2)/(((1/(n1-1))*(se1^4)) +((1/(n2-1))*(se2^4))))
    variances.statement <- paste("Population standard deviation is unknown. Not assuming that samples have equal variances.")
  }
  
  test.framework.footer <- paste(
    "-----",
    null.hypo.statement,
    alt.hypo.statement,
    sep="\n"
  )
  
  if(using_x_pair) {
    descr.col1 <- x1.name
    descr.col2 <- if(isTRUE(onesampletest)) "x2" else x2.name
  } else {
    descr.col1 <- group1.name
    descr.col2 <- group2.name
  }
  
  n2.display <- if(isTRUE(onesampletest)) NA_real_ else n2
  sd2.display <- if(isTRUE(onesampletest)) NA_real_ else sd2
  
  format_stat <- function(v) {
    if(is.na(v)) return("")
    format(round(v, digits), scientific = FALSE, trim = TRUE, nsmall = digits)
  }
  format_n <- function(v) {
    if(is.na(v)) return("")
    if(!weighted) return(format(round(v, 0), scientific = FALSE, trim = TRUE, nsmall = 0))
    format(round(v, digits), scientific = FALSE, trim = TRUE, nsmall = digits)
  }
  
  mean2.display <- if(isTRUE(onesampletest)) {
    format(as.numeric(mean2), scientific = FALSE, trim = TRUE)
  } else {
    format_stat(mean2)
  }
  
  meancomptable <- data.frame(
    col1 = c(format_stat(mean1), format_n(n1), format_stat(sd1)),
    col2 = c(mean2.display, format_n(n2.display), format_stat(sd2.display)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  colnames(meancomptable) <- c(descr.col1, descr.col2)
  rownames(meancomptable) <- c("Mean", "N", "St. Dev.")
  
  descr.tab.caption <- "Descriptive Statistics"
  if(table!=FALSE) {
    # Console table output, HTML table output (for printC)
    blocks <- render_table(blocks, x=meancomptable, caption=descr.tab.caption, digits=digits, printC=printC)
  }
  slightpause()
  
  # test statistics
  if(pop.sd==FALSE) t.stats.tab.caption <- "t-Test Statistics"
  if(pop.sd!=FALSE) t.stats.tab.caption <- "Z-Test Statistics"
  
  if(paired==FALSE) diff.means <- mean1 - mean2
  tstat <- diff.means/sediff
  
  if(pop.sd==FALSE) pvalue <- stats::pt(q = abs(tstat), df = df, lower.tail = FALSE) * 2
  if(pop.sd!=FALSE) pvalue <- stats::pnorm(-1*abs(tstat), 0, 1) * 2
  
  t.stats.tab <- data.frame(diff.means, sediff, tstat, df, pvalue = format_p_value(pvalue, digits = digits))
  t.stats.tab[1:4] <- lapply(t.stats.tab[1:4], function(col) round(col, digits))
  if("df" %in% names(t.stats.tab)) {
    t.stats.tab$df <- format(
      round(as.numeric(t.stats.tab$df), 0),
      scientific = FALSE,
      trim = TRUE,
      nsmall = 0
    )
  }
  names(t.stats.tab) <- c("Diff. of Means", "SE of Diff.", "t-Statistic", "DF","P-value (2-tailed)")
  if(pop.sd!=FALSE) 
  {
    names(t.stats.tab)[3] <- "Z-Statistic"
    t.stats.tab <- t.stats.tab[, -4, drop=FALSE]
  }
  
  if(table!=FALSE) {
    # Console table output, HTML table output (for printC)
    blocks <- render_table(
      blocks,
      x=t.stats.tab,
      caption=t.stats.tab.caption,
      digits=digits,
      printC=printC,
      footer=test.framework.footer
    )
  }
  slightpause()
  
  # confidence interval
  ci.caption <- paste(conf, "% Confidence Interval of Difference of Means", sep="")
  if(pop.sd==FALSE) 
  {
    lower <- diff.means - stats::qt(1-half_alpha, df=df)*sediff
    upper <- diff.means + stats::qt(1-half_alpha, df=df)*sediff
  }
  else
  {
    lower <- diff.means - stats::qnorm(1-half_alpha, 0, 1)*sediff
    upper <- diff.means + stats::qnorm(1-half_alpha, 0, 1)*sediff
  }
  ci.tab <- data.frame(lower, diff.means, upper)
  ci.tab[] <- lapply(ci.tab, function(col) if(is.numeric(col)) round(col, digits) else col)
  names(ci.tab) <- c("Lower Bound", "Point Estimate","Upper Bound")
  
  if(table!=FALSE)
  {
    # Console table output, HTML table output (for printC)
    blocks <- render_table(blocks, x=ci.tab, caption=ci.caption, digits=digits, printC=printC)
    slightpause()
  }
  
  # equal variances test
  if(var.test==TRUE)
  {
    var.test.heading <- "Equal Variances Test"
    
    if(using_dv_iv) 
    {
      x1n <- paste(dv.name, "|", group1.name, sep="")
      x2n <- paste(dv.name, "|", group2.name, sep="")
    } else {
      x1n <- x1.name
      x2n <- x2.name
    }
    
    if(onesampletest==TRUE) message("Sorry, you can't compare two variances with a one sample test.")
    else if(paired==TRUE) message("Sorry, you can't compare two variances with a paired samples test.")
    else
    {
      var.test.null.hypo <- paste("Null Hypothesis:", x1n, "and", x2n, "have equal variances.")
      var1 <- wtd_var(x1, w1, digits=NULL)
      var2 <- wtd_var(x2, w2, digits=NULL)
      f.stat <- var1/var2
      df1 <- n1 - 1
      df2 <- n2 - 1
      p_value_fstat <- min(stats::pf(f.stat, df1, df2, lower.tail=FALSE),
                           stats::pf(f.stat, df1, df2, lower.tail=TRUE)) * 2
      value_fmt <- function(v) format(round(v, digits), scientific = FALSE, trim = TRUE, nsmall = digits)
      df_fmt <- function(v) format(round(v, 0), scientific = FALSE, trim = TRUE, nsmall = 0)
      
      var.test.table <- data.frame(
        Statistic = c(
          paste0("Variance(", x1n, ")"),
          paste0("Variance(", x2n, ")"),
          "F statistic",
          "Num DF",
          "Denom DF",
          "P-value"
        ),
        Value = c(
          value_fmt(var1),
          value_fmt(var2),
          value_fmt(f.stat),
          df_fmt(df1),
          df_fmt(df2),
          format_p_value(p_value_fstat, digits = digits)
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
      var.test.footer <- var.test.null.hypo
      if(weighted==TRUE) {
        var.test.footer <- paste(var.test.footer, paste("Note: Variances and df weighted by", w.name), sep="\n")
      }
      
      if(table!=FALSE) {
        blocks <- render_table(
          blocks,
          x = var.test.table,
          caption = var.test.heading,
          digits = digits,
          printC = printC,
          footer = var.test.footer
        )
      }
    }
  }
  if(table!=FALSE) cat("\n")
  
  # CI plot (show to user + export via printC)
  if(plot==TRUE)
  {
    # set defaults once (important so exported plot matches interactive plot)
    if(missing(xlab)) xlab0 <- "Estimate of Difference of Means" else xlab0 <- xlab
    if(missing(main)) main0 <- ci.caption else main0 <- main
    if(missing(xlim)) xlim0 <- c(lower-.5*abs(diff.means-lower), upper+.5*abs(upper-diff.means)) else xlim0 <- xlim
    
    main0 <- strwrap(main0, width=50)
    
    draw_ci_plot <- function() {
      graphics::plot(x="", y="", xlim=xlim0, ylim=c(.5,1.5), axes=FALSE,
                     xlab=xlab0, ylab="", main=main0, ...)
      graphics::abline(v=0, col="gray80")
      graphics::points(x=diff.means, y=1, pch=16, cex=1.5)
      graphics::segments(x0=lower, x1=upper, y0=1, y1=1, lty=3, lwd=1.5)
      graphics::axis(side=1)
      graphics::box()
    }
    
    # show plot to user
    draw_ci_plot()
    
    # export plot via printC
    if(isTRUE(printC)) {
      blocks <- add_block(
        blocks,
        plot_block(draw = draw_ci_plot,
                   filename_base = "testmeansC.plot",
                   width = 5.4,
                   height = 2.25)
      )
    }
  }
  
  if(isTRUE(printC)) {
    blocks <- add_block(blocks, match.call(expand.dots = FALSE))
    print_blocksC(blocks)
  }
  
  out <- list(
    descriptive_table = meancomptable,
    test_stats_table = t.stats.tab,
    ci_table = ci.tab,
    variance_test_table = var.test.table
  )
  class(out) <- c("testmeansC_result", "list")
  out <- rcpa4_attach_tables(
    out = out,
    tables = list(
      descriptive_table = meancomptable,
      test_stats_table = t.stats.tab,
      ci_table = ci.tab,
      variance_test_table = var.test.table
    ),
    fn_name = "testmeansC",
    call_obj = match.call(expand.dots = FALSE)
  )
  invisible(out)
}

