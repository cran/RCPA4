#' Confidence intervals of a dataset variable's sample proportions in table and figure
#'
#' @description Reports the confidence interval of sample proportions in table and plot. Default is 95% CI but use can raise or lower confidence level. 
#' @param x A nominal or ordinal variable (factor), should be in dataset$var form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param digits (Optional) Number of decimal places reported in result (defaults to 3).
#' @param conf (Optional) A single number equal to the desired confidence level (i.e. 95, 99, 90, etc.). Default value is 95 percent confidence level.
#' @param table (Optional) Do you want the confidence interval table reported? (default: TRUE)
#' @param printC (Optional) Do you want results printed to .html file in your working directory? Default is FALSE. Set to TRUE to print results.
#' @param plot (Optional) Do you want a plot of the confidence interval? Default is TRUE.
#' @param main (Optional) Change the main title of plot. Default title generated from conf, x, and w.
#' @param xlab (Optional) Label for x-axis of confidence interval plot.
#' @param xlim (Optional) Modify x-axis limits of confidence interval plot.
#' @param ... (Optional) Additional arguments passed to `plot` function.
#' @return Returns a data frame that gives the lower bound, point estimate, and upper bounds of each value of x variable.
#' @examples  
#'   library(RCPA4)
#' 
#'   CIprop(anes$life_exprnc_know_smn)
#'   
#'   \donttest{
#'   # using optional w and data arguments
#'   CIprop(x=anes$life_exprnc_know_smn, w=anes$wt)
#'   CIprop(x=life_exprnc_know_smn, w=wt, data=anes)
#'   }
#' @export
#' @template console-output
#' @template result-tables
#' @section RCPA4 Package Tutorial Videos:
#' * [Confidence Intervals for Sample Proportions with RCPA4's CIprop Function](https://www.youtube.com/watch?v=uRukzm66BdI) 18:29 
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapter 8. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Sage Publications, 2020), pp. 184-186. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Foundations of Inference](https://www.poliscidata.com/pages/rDemosResources.php?chapter=8), Compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications.  
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom graphics par points segments axis box
#' @importFrom grDevices png dev.off
#' @md

CIprop = function(x, w=NULL, data=NULL, digits=3, conf=95, printC=FALSE, plot=TRUE, 
                  main=NULL, xlab=NULL, xlim=NULL, table=TRUE, ...) 
{
  assert_required(x)
  if((table == FALSE) && (plot == FALSE)) stop("At least one of table or plot must be TRUE.")
  
  if(plot!=FALSE)
  {
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
  
  
  
  assert_nonnegative_integer(digits)
  assert_positive_numeric(conf)
  if((conf > 0) & (conf < 1)) 
  {
    conf <- conf*100
    message(wrap_console_text("Please note: It looks like you entered confidence level as a proportion. Confidence level should be percentage, like 95 or 99. Attempting to convert..."))
  }
  if(conf > 100) stop("There's a problem: confidence level should not exceed 100.")
  if((conf <= 50) & (conf >= 1)) stop(wrap_console_text(paste(gettext("There's a problem: confidence level should be percentage, like 95 or 99, not Type I error rate.", domain = "R-descr"))))
  
  
  
  if(!is.null(levels(x))) x.values <- trimws(levels(x), which="both")
  else x.values <- trimws(sort(unique(x)), which="both")
  
  n.x.values <- length(x.values)
  x.n  <- sum(w)
  props <- rep(NA, n.x.values)
  se.prop <- rep(NA, n.x.values)
  out1 <- NULL
  
  for(i in 1:n.x.values)
  {
    thisvalue = x.values[i]
    j <- (x==thisvalue)
    n.thisvalue <- sum(w[j])
    props[i] <- n.thisvalue / x.n
    se.prop[i] <- sqrt(props[i]*(1-props[i])/x.n)
  }
  
  half_alpha = (1 - conf/100) / 2
  critical_value <- stats::qnorm(1-half_alpha)
  
  lowers = props - critical_value*se.prop
  uppers = props + critical_value*se.prop
  
  blocks <- list()
  # ---- Heading (ALWAYS printed to console) ----
  blocks <- render_heading(blocks, "Confidence Intervals of Proportions", marker="=", printC=printC)
  
  title <- paste(conf, "% CI of values of ", x.name, sep="")
  if(!is.null(x.label) && !identical(x.label, x.name)) title <- paste(title, " (", x.label, ")", sep="")
  if(weighted==TRUE) title <- paste(title, ", weighted by ", w.name, sep="")
  
  ci.tab = round(data.frame(lowers, props, uppers), digits)
  colnames(ci.tab) = c("Lower Bound", "Point Estimate","Upper Bound")
  rownames(ci.tab) <- x.values
  

  if(table!=FALSE) {
    # Console table output, HTML table output (for printC)
    blocks <- render_table(blocks, x=ci.tab, caption=title, digits=digits, printC=printC)
  }
  
    
  if(plot==TRUE)
  {
    if(missing(xlab)) xlab <- "Estimates of Population Proportions"
    if(missing(main)) main <- title
    main <- strwrap(main, width=50)
    if(missing(xlim)) xlim <- c(0,1)
    
    draw_ciprop <- function() {
      restore.par.mar <- graphics::par("mar")
      graphics::par(mar=c(4.6, 4.1 + sqrt(max(nchar(x.values))), 4.1, 3.1))
      y_pos <- n.x.values:1
      
      plot(x="", y="", xlim=xlim, ylim=c(.5, n.x.values+.5), 
           axes=FALSE, xlab=xlab, ylab="", main=main, ...)
      
      graphics::points(x=props, y=y_pos, pch=16, cex=1)
      graphics::segments(x0 = lowers, x1 = uppers, y0=y_pos, y1=y_pos, lty=3, lwd=1.5)
      graphics::axis(side = 1, at = seq(0,1,by=.1))
      graphics::axis(side = 2, at = y_pos, labels = x.values, las=2, hadj=1)
      graphics::box()
      
      graphics::par(mar=restore.par.mar)
    }
    
    # Show plot to user (no ask)
    draw_ciprop()
    
    # Export plot via printC (no png here; printC will save it)
    if(isTRUE(printC)) {
      blocks <- add_block(
        blocks,
        plot_block(draw = draw_ciprop,
                   filename_base = "CIprop.plot",
                   width = 5.4,
                   height = 1.5*(1.2 + length(x.values)*.3))
      )
    }
  }
  
  if(isTRUE(printC)) {
    blocks <- add_block(blocks, match.call(expand.dots = FALSE))
    print_blocksC(blocks)
  }
  
  invisible(ci.tab)
}

