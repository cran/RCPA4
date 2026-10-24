#' Confidence interval of a dataset variable's sample mean in table and figure
#'
#' @description Reports the confidence interval of a sample mean in table and plot. Default is 95% CI but use can raise or lower confidence level. 
#' @param x A numeric variable, should be in dataset$var form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable (optional), must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable (optional).
#' @param digits (Optional) Number of decimal places reported in result (defaults to 3).
#' @param conf (Optional) A single number equal to the desired confidence level (i.e. 95, 99, 90, etc.). Default value is 95 percent confidence level.
#' @param pop.sd (Optional) A single number equal to the known population standard deviation of x. This value is rarely know, but if it is, critical values for confidence interval are based on standard normal distribution.
#' @param table (Optional) Do you want the confidence interval table reported? (default: TRUE)
#' @param printC (Optional) Do you want results printed to .html file in your working directory? Default is FALSE. Set to TRUE to print results.
#' @param plot (Optional) Do you want a plot of the confidence interval? Default is TRUE.
#' @param main (Optional) Change the main title of plot. Default title generated from conf, x, and w.
#' @param xlab (Optional) Label for x-axis of confidence interval plot.
#' @param xlim (Optional) Modify x-axis limits of confidence interval plot.
#' @param ... (Optional) additional arguments passed to `plot` function.
#' @return Returns the confidence interval as a vector of numeric values (the lower and upper bounds).
#' @examples  
#'   library(RCPA4)
#'   
#'   CImean(anes$r_age_election_day)
#'   
#'   \donttest{
#'   # using optional w, conf, and data arguments
#'   CImean(x=anes$r_age_election_day, w=anes$wt, conf=90)
#'   CImean(x=r_age_election_day, data=anes, conf=95)
#'   }
#' @export
#' @template console-output
#' @template result-tables
#' @section RCPA4 Package Tutorial Videos:
#' * [Confidence Intervals for Sample Means with RCPA4's CImean Function](https://www.youtube.com/watch?v=EmU7cQJ_uRM) 16:16 
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapter 8. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Sage Publications, 2020), pp. 184-186. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Foundations of Inference](https://www.poliscidata.com/pages/rDemosResources.php?chapter=8), Compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom Hmisc all.is.numeric
#' @importFrom grDevices png dev.off
#' @importFrom graphics par abline points segments axis box
#' @md

CImean = function(x, w=NULL, data=NULL, digits=3, conf=95, pop.sd=NULL, printC=FALSE, 
                  plot=TRUE, main=NULL, xlab=NULL, xlim=NULL, table=TRUE, ...) 
{
  assert_required(x)
  if((table == FALSE) && (plot == FALSE)) stop("At least one of table or plot must be TRUE.")
  
  if(plot!=FALSE) {
    old.par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old.par), add=TRUE)
  }
  
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
  
  # Convert numeric-looking labels, rather than factor storage codes.
  if (!is.numeric(x) && Hmisc::all.is.numeric(x, extras = c(NA))) {
    x <- as.numeric(as.character(x))
  }
  assert_numeric(x)
  assert_nonnegative_integer(digits)
  assert_positive_numeric(conf)
  if(!missing(pop.sd)) assert_positive_numeric(pop.sd)
  if(conf < 1 & conf > 0) {
    conf <- conf*100
    message(wrap_console_text("Please note: It looks like you entered confidence level as a proportion. Confidence level should be percentage, like 95 or 99. Attempting to convert..."))
  }
  if(conf >= 100) stop("There's a problem: confidence level should not exceed 100.")
  if((conf <= 50) & (conf >= 1)) stop(wrap_console_text(paste(gettext("There's a problem: confidence level should be percentage, like 95 or 99, not Type I error rate.", domain = "R-descr"))))
  
  
  half_alpha = (1 - conf/100) / 2
  mean <- wtd_mean(x, w, digits=NULL)
  sd   <- wtd_sd(x, w, digits=NULL)
  x.n  <- sum(w) # the n of sample, needs to adjust in case of weights
  
  if(!missing(pop.sd)) {
    se <- pop.sd/sqrt(x.n)
    critical_value <- stats::qnorm(1-half_alpha)
  } else {
    se <- sd/sqrt(x.n)
    critical_value <- stats::qt(1-half_alpha, df=(x.n-1))
  }
  
  lower = mean - critical_value*se
  upper = mean + critical_value*se
  
  blocks <- list()
  # ---- Heading (ALWAYS printed to console) ----
  blocks <- render_heading(blocks, "Confidence Interval of a Mean", marker="=", printC=printC)
  
  title <- paste(conf, "% CI of Mean of ", x.name, sep="")
  if(!is.null(x.label) && !identical(x.label, x.name)) title <- paste(title, " (", x.label, ")", sep="")
  if(weighted==TRUE) title <- paste(title, ", weighted by ", w.name, sep="")
  
  ci.tab <- data.frame(lower, mean, upper)
  names(ci.tab) = c("Lower Bound", "Point Estimate","Upper Bound")
  

  if(table!=FALSE) {
    # Console table output, HTML table output (for printC)
    blocks <- render_table(blocks, x=ci.tab, caption=title, digits=digits, printC=printC)
  }
  
  if(plot==TRUE)
  {
    if(missing(xlab)) xlab <- "Estimate of Population Mean"
    if(missing(main)) main <- title
    if(missing(xlim)) xlim <- c(lower-.5*abs(mean-lower), upper+.5*abs(upper-mean))
    main <- strwrap(main, width=50)
    
    # One draw function (used for screen + export)
    draw_cimean <- function() {
      plot(x="", y="", xlim=xlim, ylim=c(.5,1.5), axes=FALSE, xlab=xlab, ylab="", main=main, ...)
      graphics::abline(v=0, col="gray80")
      graphics::points(x=mean, y=1, pch=16, cex=1.5)
      graphics::segments(x0 = lower, x1 = upper, y0=1, y1=1, lty=3, lwd=1.5)
      graphics::axis(side = 1)
      graphics::box()
    }
    
    # Show plot to user
    draw_cimean()
    
    # Export plot via printC (no png here; printC will save it)
    if (isTRUE(printC)) {
      blocks <- add_block(
        blocks,
        plot_block(draw = draw_cimean,
                   filename_base = "CImean.plot",
                   width = 5.4,
                   height = 2.25)
      )
    }
  }
  
  if(isTRUE(printC)) {
    blocks <- add_block(blocks, match.call(expand.dots = FALSE))
    print_blocksC(blocks)
  }
  
  invisible(ci.tab)
}


