#' Tests the difference between two sample proportions, or difference between sample proportion and hypothesized value, with options for weighted observations, confidence intervals  
#'
#' @description Difference of proportions test with optional sample weights. Reports P-value of two-tailed significance test. Currently limited to testing one response from one dataset. If you want to compare x1 from dataset1 and x2 from dataset2, you can create new dataframe to test as dv ~ iv where dv is vector of x1 and x2 values and iv is vector identifying the source (i.e. dataset1 and dataset2). If you want to compare different responses, such as "Yes" value for x1 and "Agree" value for x2, you will need to transform one of the variables so they have comparable response values.   
#' @param x1 A categorical variable
#' @param x2 Value or variable to compare x1 against. 
#' @param w (Optional) Weights variable.
#' @param data (Optional) Specify name of dataset (data frame) with x1 and x2 variables (or dv and iv).
#' @param dv Dependent variable
#' @param iv Independent variable, should have only two unique values. For comparison purposes, group1 will be first level of iv and group2 will be the second level of iv. To change order of groups, you can modify levels(iv). 
#' @param digits (Optional) Number of digits to report after decimal place, optional (default: 3).
#' @param response (Optional) Identify the response value you wish to compare. If not specified, the function will compare first value of the dv (or x1 variable). If you want to group multiple responses together, use transformC to make dummy variable.
#' @param table (Optional) Do you want summary tables reported? (default: TRUE)
#' @param conf (Optional) Desired confidence level, as percentage (default: 95)
#' @param plot (Optional) Do you want a plot of the confidence interval of the difference of proportions? (default: TRUE)
#' @param printC (Optional) Do you want results printed to .html file in your working directory? Default is FALSE. Set to TRUE to print results.
#' @param main (Optional) Main title for plot of confidence interval of difference
#' @param xlab (Optional) Label for x-axis of plot of confidence interval of difference
#' @param xlim (Optional) A vector (of length 2) specifying the range of the x-axis, useful to zoom in on CI. By default, `xlim=c(-1, 1)`.
#' @param ... (Optional) Additional arguments passed to plot function for the CI plot
#' @return Invisibly returns a structured list with stable keys:
#' `descriptive_table`, `z_test_table`, and `ci_table`
#' (`NULL` where optional analysis is not computed).
#' @examples 
#'    library(RCPA4)
#'    
#'    \donttest{
#'    # one sample test: x1 variable against hypothesized value (of x2)
#'    testpropsC(x1=anes$favor_opps_bckgrnd, x2=.500, w=anes$wt, response="Favor a great deal", 
#'               xlim=c(0, .2))
#'
#'    # two sample test x1 versus x2
#'    testpropsC(x1=has_sons, x2=has_daughters, w=wt, data=anes, 
#'               response="Yes", xlim=c(-.10, .10))
#'    
#'    # test of proportions dv by iv
#'    testpropsC(dv=mrtl_stts_5_ctgry, iv=gender_s_sex, w=wt, data=anes, response="Widowed", 
#'               xlim=c(-.10, 0))
#'    }
#' @export
#' @template console-output
#' @template result-tables
#' @section RCPA4 Package Tutorial Videos:
#' * [Testing Proportions with RCPA4 Package's testpropsC Function](https://www.youtube.com/watch?v=-2t2mwHeh_A) 27:33   
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapter 9. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Sage Publications, 2020), pp.201-215. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Hypothesis Tests with One and Two Samples](https://www.poliscidata.com/pages/rDemosResources.php?chapter=9), Compiled by Barry C. Edwards 
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications.  
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom grDevices png dev.off
#' @importFrom stats na.omit pnorm
#' @importFrom graphics par abline points segments axis box 
#' @md

testpropsC <- function(x1, x2, w=NULL, data=NULL, dv, iv, digits=3, response=NULL, printC=FALSE, 
                       conf=95, plot=TRUE, main=NULL, xlab=NULL, xlim=NULL, table=TRUE, ...)
{
  using_dv_iv <- (!missing(dv) || !missing(iv))
  using_x_pair <- (!missing(x1) || !missing(x2))
  
  if(using_dv_iv && using_x_pair) {
    stop("There's a problem: choose either dv/iv or x1/x2 for testpropsC(), but not both.")
  }
  if(using_dv_iv) {
    assert_required(dv, iv)
  } else if(using_x_pair) {
    assert_required(x1, x2)
  } else {
    stop(wrap_console_text("Oops. You need to specify a dependent variable (dv) and indepenent variable (iv), or two variables to compare (x1 and x2). Enter example(testpropsC) to see an example and help(testpropsC) to learn more about this function."))
  }
  
  if(plot != FALSE) {
    old.par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old.par), add=TRUE)
  }
  if((table == FALSE) && (plot == FALSE)) stop("At least one of table or plot must be TRUE.")
  
  assert_nonnegative_integer(digits)
  
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
      stop("There's a problem: x1 and x2 must have the same length for two-sample testpropsC().")
    }
  }
  
  assert_positive_numeric(conf)
  if((conf > 0) & (conf < 1)) 
  {
    conf <- conf * 100
    message(wrap_console_text("Please note: It looks like you entered the level for confidence interval table as a proportion. Confidence level should be percentage, like 95 or 99. Attempting to convert..."))
  }
  if(conf > 100) stop("There's a problem: confidence level for confidence interval table should not exceed 100.")
  if((conf <= 50) & (conf >= 1)) stop(wrap_console_text(paste(gettext("There's a problem: confidence level for confidence interval table should be percentage, like 95 or 99, not Type I error rate.", domain = "R-descr"))))
  
  if(length(w)==0) stop(paste("The weights variable,", w.name, "doesn't have values. Check spelling?"))
  
  # Determine response options
  if(using_x_pair) 
  {
    if(!is.null(levels(x1))) response.options <- levels(droplevels(x1)) else response.options <- unique(stats::na.omit(x1))
  }
  if(using_dv_iv) 
  {
    if(!is.null(levels(dv))) response.options <- levels(droplevels(dv)) else response.options <- unique(stats::na.omit(dv))
  }
  
  if(missing(response)) 
  {
    message(wrap_console_text("Note: You did not identify the response value you want to compare."))
    message(wrap_console_text(paste("Therefore, testpropC will compare the first response value: \"", response.options[1], "\"", sep="")))
    response <- response.options[1]
    message(wrap_console_text("To compare another response value, use this function's response argument.\n"))
    slightpause()
  }
  
  if(!(response %in% response.options))
  {
    stop(
      "There's a problem: ", response, " isn't one of available response values.\n",
      "Here are your options:\n",
      paste0("  - ", as.character(response.options), collapse="\n"),
      "\nPlease correct this function's response argument or choose another x1.",
      call.=FALSE
    )
  }
  
  # x2 checks (2-sample categorical vs 1-sample numeric)
  if(using_x_pair)
  {
    if(length(x2) != 1)
    {
      if(!(response %in% unique(stats::na.omit(x2)))) {
        response.options2 <- unique(stats::na.omit(x2))
        stop(
          "There's a problem: ", response, " isn't one of values of ", x2.name, ".\n",
          "Here are your options:\n",
          paste0("  - ", as.character(response.options2), collapse="\n"),
          "\nPlease correct this function's response argument or choose another x2.",
          call.=FALSE
        )
      }
    }
    else
    {
      if(!is.numeric(x2)) {
        stop(
          "There's a problem: For one sample test of proportions, x2 must be a single number.\n",
          "Please correct this function's x2 argument or choose another x2.",
          call.=FALSE
        )
      }
      assert_between_0_1(x2)
    }
  }
  
  # ---- Compute props & Ns ----
  if(using_dv_iv)
  {
    xtp.obj1 <- weighted_crosstab(
      dep = dv,
      indep = iv,
      w = w,
      dep_levels = if (is.factor(dv)) levels(droplevels(dv)) else sort(unique(stats::na.omit(dv))),
      indep_levels = if (is.factor(iv)) levels(droplevels(iv)) else sort(unique(stats::na.omit(iv)))
    )
    if (ncol(xtp.obj1$tab) != 2) {
      stop(wrap_console_text(paste("There's a problem:", iv.name, "must have exactly 2 groups for testpropsC with dv/iv arguments.")))
    }
    prop1 <- xtp.obj1$prop.col[response,1]
    n1    <- sum(xtp.obj1$tab[,1])
    prop2 <- xtp.obj1$prop.col[response,2]
    n2    <- sum(xtp.obj1$tab[,2])
    group.names <- colnames(xtp.obj1$tab)
    group1.name <- group.names[1]
    group2.name <- group.names[2]
    onesampletest <- FALSE
  }
  
  if(using_x_pair)
  {
    k1 <- grep(FALSE, (is.na(x1) | is.na(w)))
    x1 <- x1[k1]
    w1 <- w[k1]
    n1 <- sum(w1)
    prop1 <- sum(as.numeric(x1==response)*w1) / n1
    
    if(length(x2)==1) 
    {
      prop2 <- x2
      n2 <- ""
      onesampletest <- TRUE
    }
    else
    {
      k2 <- grep(FALSE, (is.na(x2) | is.na(w)))
      x2 <- x2[k2]
      w2 <- w[k2]
      n2 <- sum(w2)
      prop2 <- sum(as.numeric(x2==response)*w2) / n2
      onesampletest <- FALSE
    }
    group1.name <- x1.name
    group2.name <- x2.name
  }
  
  # ---- Collect printC blocks (only used if printC==TRUE) ----
  blocks <- list()
  
  # Heading (ALWAYS printed to console)
  blocks <- render_heading(blocks, "Difference of Proportions Test", marker="=", printC=printC)
  
  response.txt <- as.character(response)
  response.quoted <- shQuote(response.txt, type = "cmd")
  response.prop.label <- paste0("Prop(", response.quoted, ")")
  
  testvalue1 <- paste0("Prop(", response.quoted, "|", group1.name, ")")
  if(using_x_pair && length(x2)==1) {
    x2.hyp.value <- format(as.numeric(x2), scientific = FALSE, trim = TRUE)
    x2.hyp.value <- sub("^(-?)0\\.", "\\1.", x2.hyp.value)
    null.hypo.statement <- paste0("Null hypo: ", response.prop.label, " = ", x2.hyp.value)
    alt.hypo.statement  <- paste0("Alt. hypo: ", response.prop.label, " \u2260 ", x2.hyp.value)
  } else {
    testvalue2 <- paste0("Prop(", response.quoted, "|", group2.name, ")")
    null.hypo.statement <- paste0("Null hypo: ", testvalue1, " = ", testvalue2)
    alt.hypo.statement  <- paste0("Alt. hypo: ", testvalue1, " \u2260 ", testvalue2)
  }
  test.framework.console.footer <- paste(
    "-----",
    null.hypo.statement,
    alt.hypo.statement,
    sep="\n"
  )
  test.framework.html.footer <- paste(
    null.hypo.statement,
    alt.hypo.statement,
    sep="\n"
  )
  slightpause()
  
  # ---- SEs ----
  se1 <- sqrt(prop1*(1-prop1)) / sqrt(n1)
  if(using_x_pair && length(x2)==1) se2 <- 0
  else se2 <- sqrt(prop2*(1-prop2)) / sqrt(n2)
  
  if(isTRUE(onesampletest)) {
    sediff <- se1
  } else {
    pooled.prop <- (prop1*n1 + prop2*n2) / (n1 + n2)
    sediff <- sqrt(pooled.prop*(1-pooled.prop)*(1/n1 + 1/n2))
  }
  
  diff <- prop1 - prop2
  
  # ---- Descriptives ----
  descr.df.caption <- "Descriptive Statistics"
  descr.df <- data.frame(group1=c(prop1, n1), group2=c(prop2, n2))
  if(using_x_pair) {
    descr.col1 <- x1.name
    descr.col2 <- if(isTRUE(onesampletest)) "x2" else x2.name
    colnames(descr.df) <- c(descr.col1, descr.col2)
  } else {
    colnames(descr.df) <- c(group1.name, group2.name)
  }
  prop.row.label <- response.prop.label
  rownames(descr.df) <- c(prop.row.label, "N")
  if(!weighted) {
    descr.df.display <- data.frame(
      group1 = c(
        format(round(prop1, digits), scientific = FALSE, trim = TRUE, nsmall = digits),
        format(round(n1, 0), scientific = FALSE, trim = TRUE, nsmall = 0)
      ),
      group2 = c(
        format(round(prop2, digits), scientific = FALSE, trim = TRUE, nsmall = digits),
        if(identical(n2, "")) "" else format(round(n2, 0), scientific = FALSE, trim = TRUE, nsmall = 0)
      ),
      stringsAsFactors = FALSE
    )
    colnames(descr.df.display) <- colnames(descr.df)
    rownames(descr.df.display) <- rownames(descr.df)
  } else {
    descr.df.display <- descr.df
  }
  if(table!=FALSE) {
    # Console table output, HTML table output (for printC)
    blocks <- render_table(blocks, x=descr.df.display, caption=descr.df.caption, digits=digits, printC=printC)
  }
  slightpause()
  
  # ---- Z test ----
  z.stats.caption <- "Z-Test Statistics"
  half_alpha <- (1 - conf/100) / 2
  
  zstat <- diff/sediff
  pval  <- stats::pnorm(-1*abs(zstat)) * 2
  
  z.stats.df <- data.frame(diff, sediff, zstat, pval = format_p_value(pval, digits = digits))
  z.stats.df[1:3] <- lapply(z.stats.df[1:3], function(col) round(col, digits))
  colnames(z.stats.df) <- c("Diff. of Props.", "SE of Diff.", "Z-statistic", "P-value (2-tailed)")
  rownames(z.stats.df) <- NULL
  
  if(table!=FALSE) {
    # Console table output, HTML table output (for printC)
    blocks <- render_table(
      blocks,
      x=z.stats.df,
      caption=z.stats.caption,
      digits=digits,
      printC=printC,
      footer_console=test.framework.console.footer,
      footer_html=test.framework.html.footer
    )
  }
  slightpause()
  
  # ---- CI ----
  ci.caption <- paste(conf, "% Confidence Interval of Difference of Proportions", sep="")
  sediff.ci <- sqrt(se1^2 + se2^2)
  lower <- diff - stats::qnorm(1-half_alpha)*sediff.ci
  upper <- diff + stats::qnorm(1-half_alpha)*sediff.ci
  ci.df <- data.frame(lower, diff, upper)
  colnames(ci.df) <- c("Lower Bound", "Point Estimate", "Upper Bound")
  rownames(ci.df) <- NULL
  
  if(table!=FALSE)
  {
    # Console table output, HTML table output (for printC)
    blocks <- render_table(blocks, x=ci.df, caption=ci.caption, digits=digits, printC=printC)
    slightpause()
  }
  
  # ---- CI plot (show user + export via printC blocks) ----
  if(plot == TRUE)
  {
    if(missing(xlab)) xlab0 <- "Estimate of Difference of Proportions" else xlab0 <- xlab
    if(missing(main)) main0 <- ci.caption else main0 <- main
    main0 <- strwrap(main0, width=50)
    if(missing(xlim)) xlim0 <- c(-1, 1) else xlim0 <- xlim
    
    draw_ci_plot <- function() {
      mar.now <- graphics::par("mar")
      graphics::par(mar = c(mar.now[1], mar.now[4], mar.now[3], mar.now[4]))
      graphics::plot(x="", y="", xlim=xlim0, ylim=c(.5,1.5),
                     axes=FALSE, xlab=xlab0, ylab="", main=main0, ...)
      graphics::abline(v=0, col="gray80")
      graphics::points(x=diff, y=1, pch=16, cex=1)
      graphics::segments(x0=lower, x1=upper, y0=1, y1=1, lty=3, lwd=1.5)
      graphics::axis(side=1, at=seq(-1, 1, by=.1))
      graphics::box()
    }
    
    # show plot to user
    draw_ci_plot()
    
    # export plot via printC
    if(isTRUE(printC)) {
      blocks <- add_block(
        blocks,
        plot_block(draw = draw_ci_plot,
                   filename_base = "testpropsC.plot",
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
    descriptive_table = descr.df.display,
    z_test_table = z.stats.df,
    ci_table = ci.df
  )
  class(out) <- c("testpropsC_result", "list")
  out <- rcpa4_attach_tables(
    out = out,
    tables = list(
      descriptive_table = descr.df.display,
      z_test_table = z.stats.df,
      ci_table = ci.df
    ),
    fn_name = "testpropsC",
    call_obj = match.call(expand.dots = FALSE)
  )
  invisible(out)
}

