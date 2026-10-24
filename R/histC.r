#' Creates histogram to show distribution of interval (numeric) variable's values
#'
#' @description Generates frequency distribution table of binned values and a histogram to describe distribution of variable values.
#' @param x The variable to be analyzed. If dataset not specified with data argument, should be a vector in form dataset$var.
#' @param w (Optional) Sample weights (must be numeric if used), If dataset not specified with data argument, should be in form dataset$weighvar
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param breaks (Optional) Specify how to break the x variable into bins. Options include the number of breaks, a vector specifying the breakpoints, or the name of an algorithm that generates breakpoints. Default value is "Sturges" (other algorithms are "Scott" and "FD").
#' @param digits (Optional) Number of digits to display after decimal points (default is 2).
#' @param table (Optional) Do you want the binned frequency table reported? (default: TRUE)
#' @param printC (Optional) Do you want the histogram and binned frequencies table printed to working directory? (default: FALSE)
#' @param plot (Optional) Do you want the histogram graphic? (default: TRUE)
#' @param main (Optional) Customize main title for histogram.
#' @param xlab (Optional) Custom label for x-axis of histogram. 
#' @param ylab (Optional) Custom label for y-axis of histogram.
#' @param bar.col (Optional) Color for histogram bars; default is "gray80".
#' @param ... (Optional) Additional arguments passed to the internal wtd histogram helper.
#' @return A frequency distribution table of binned x variable values.
#' @export
#' @examples 
#'    library(RCPA4)
#'    
#'    histC(x=states$covid_cases_per1000)
#'    
#' @template console-output
#' @template result-tables
#' @section Table and plot requirements:
#' At least one of `table` or `plot` must be enabled. When using `table = FALSE`,
#' keep a valid plot enabled.
#' @section RCPA4 Package Tutorial Videos:
#' * [Generate Histograms with RCPA4 PAckages's histC Function](https://www.youtube.com/watch?v=uQEHe8vAORI)   
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapter 2. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Sage Publications, 2020), pp. 39-55. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Descriptive Statistics](https://www.poliscidata.com/pages/rDemosResources.php?chapter=2), Compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom graphics par box
#' @importFrom grDevices png dev.off
#' @md
histC <- function(x, w=NULL, data=NULL, breaks="Sturges", digits=2, printC=FALSE, plot=TRUE, 
                  main=NULL, xlab=NULL, ylab="Frequency", bar.col="gray80", table=TRUE, ...)
{
  assert_required(x)
  assert_nonnegative_integer(digits)
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
  
  # Validate selections
  assert_numeric(x)
  if(length(unique(x)) <= 10) message(wrap_console_text(paste("Suggestion:", x.name, "has only", length(unique(x)), "unique values. Maybe you want to use a bar chart?")))
  

  
  # here, printing binned freq table before plotting the histogram
  results <- wtd_hist(x=x, w=w, breaks=breaks, plot=FALSE, suggest=FALSE, ...) 
  
  # collect printC blocks
  blocks <- list()
  
  # Heading (ALWAYS printed to console)
  heading <- if(plot!=FALSE && table!=FALSE) "Describing Distribution with Frequency Table & Histogram"
  else if(plot!=FALSE) "Describing Distribution with Histogram"
  else "Describing Distribution with Frequency Table"
  blocks <- render_heading(blocks, heading, marker="=", printC=printC)
  
  from.values  <- c(results$breaks[-length(results$breaks)], results$breaks[1])
  to.values    <- c(results$breaks[-1], results$breaks[length(results$breaks)])
  freq         <- round(c(results$counts, sum(results$counts)), digits)
  percents     <- round(c(results$counts/sum(results$counts)*100, 100), digits)
  
  cum.percents <- rep(NA, length(results$breaks))
  for(i in 1:length(results$counts)) cum.percents[i] <- round(sum(percents[1:i]), digits)
  
  brackets <- c("[", rep("(", length(results$counts)-1))
  interval.names <- paste(brackets, from.values, " - ", to.values, "]", sep="")
  interval.names[length(percents)] <- "Total"
  
  freq.table <- data.frame(interval.names, freq, percents, cum.percents)
  colnames(freq.table) <- c("Interval", "Frequency", "Percent", "Cumulative")
  if(all(freq.table[, "Frequency"]%%1==0)) n.drop.decimals <- TRUE else n.drop.decimals <- FALSE
  freq.table[, "Frequency"] <- format(freq.table[, "Frequency"], drop0trailing=n.drop.decimals,
                                      nsmall=digits, digits=digits)
  
  caption <- paste("Distribution of Binned Values of", x.name)
  if(!is.null(x.label) && !identical(x.label, x.name)) caption <- paste(caption, " (", x.label, ")", sep="")
  if(weighted==TRUE) caption <- paste(caption, ", Weighted by ", w.name, sep="")
  
  if(table!=FALSE)
  {
    # Console table output, HTML table output (for printC)
    blocks <- render_table(blocks, x=freq.table, caption=caption, digits=digits, printC=printC)
  }
  
  # Plot: just draw if plot=TRUE; export via plot block if printC=TRUE
  if(plot==TRUE)
  {
    if(missing(main)) main <- caption
    main <- strwrap(main, width=50)
    if(missing(xlab)) xlab <- paste(x.name, "values")

    draw_hist <- function() {
      graphics::par(new=FALSE)
      wtd_hist(x=x, w=w, breaks=breaks, plot=TRUE, suggest=FALSE,
               main=main, xlab=xlab, ylab=ylab, col=bar.col, ...)
      graphics::box(bty = "l")
    }
    
    # show plot to user (no ask)
    draw_hist()
    
    # export plot (no png here; printC will save it)
    if (isTRUE(printC)) {
      blocks <- add_block(
        blocks,
        plot_block(draw = draw_hist,
                   filename_base = "histC.plot",
                   width = 6,
                   height = 4.5)
      )
    }
  }
  
  if(isTRUE(printC)) {
    blocks <- add_block(blocks, match.call(expand.dots = FALSE))
    print_blocksC(blocks)
  }
  
  invisible(freq.table)
}

