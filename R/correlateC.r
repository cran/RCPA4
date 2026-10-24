#' Correlation analysis for two or more numeric variables, with options for scatterplots, weighted observations, and inferential statistics.
#'
#' @description Given two or more numeric variables, `correlateC` reports Pearson's r, along with inferential statistics (if requested), works with sampling weights. If more than two x variables are supplied, the function calculates Pearson's r values using pairwise complete observations (as opposed to limiting analysis to observations complete on all variables).
#' @param x A list of variables for correlation analysis, variables must be numeric. Should be entered as list(dataset$var1, dataset$var2, dataset$var3 ... ) form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable (optional).
#' @param digits (Optional) Number of decimal places reported in result (defaults to 3).
#' @param stats (Optional) Do you want the inferential statistics (standard errors, t-statistics, and P-values)? Default is FALSE. Set to TRUE for inferential statistics.
#' @param printC (Optional) Do you want results printed to .html file in your working directory? Default is FALSE. Set to TRUE to print results.
#' @param plot (Optional) Do you want scatterplot(s)? Default is FALSE.
#' @param table (Optional) Do you want correlation table(s) reported? (default: TRUE)
#' @param jitter (Optional) Do you want scatterplot pointed jittered? By default, points jittered when there are more than 500 observations, but you can set this arguments to TRUE/FALSE to override the default.
#' @param ... (Optional) Additional arguments passed to the internal weighted correlation helper.
#' @details Correlations are computed internally with optional weights and pairwise-complete handling for multivariate input.
#' @return Returns the coefficients of correlation among x variables; if `stats=TRUE`, inferential statistics returned in tables as well.
#' @examples 
#'    library(RCPA4)
#'    
#'    correlateC(x=list(prochoice_percent, stateleg_women), data=states, plot=FALSE)
#'    
#'    \donttest{
#'    # with weighted observations and inferential statistics
#'    correlateC(x=list(anes$ft_rep_party, anes$ft_trump_pre, anes$ft_dem_party, anes$ft_biden_pre), 
#'               w=anes$wt, stats=TRUE)
#'    }
#' @export
#' @template console-output
#' @template result-tables
#' @section Table and plot requirements:
#' At least one of `table` or `plot` must be enabled. When using `table = FALSE`,
#' keep a valid plot enabled.
#' @section RCPA4 Package Tutorial Videos:
#' * [Correlation Analysis with RCPA4's correlateC Function](https://www.youtube.com/watch?v=sYBywL4t1fY) 9:01  
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapter 11. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Sage Publications, 2020), pp. 240-244. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Correlation and Bivariate Regression](https://www.poliscidata.com/pages/rDemosResources.php?chapter=11), compiled by Barry C. Edwards.
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom graphics par pairs
#' @importFrom grDevices png dev.off
#' @md
correlateC <- function(x, w=NULL, data=NULL, digits=3, stats=FALSE, printC=FALSE, 
                       plot=FALSE, jitter=FALSE, table=TRUE, ...) 
{ 
  # TODO: remove any remaining legacy warning-option patterns and prefer suppressWarnings().
  assert_required(x)
  assert_nonnegative_integer(digits)
  if((table == FALSE) && (plot == FALSE)) stop("At least one of table or plot must be TRUE.")
  
  if(plot!=FALSE)
  {
    old.par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old.par), add=TRUE)
  }
  
  w_expr <- if (missing(w)) NULL else substitute(w)


  ctx <- setup_vars_input(
    var_exprs = list(x = substitute(x)),
    w_expr = w_expr,
    data_expr = substitute(data),
    env = parent.frame(),
    data_missing = missing(data),
    drop_complete = FALSE,
    single_vars = character(0),
    ref_var = "x"
  )
  variable.set <- ctx$vars$x
  variable.names <- colnames(variable.set)
  w.name <- ctx$meta$w$name
  
  if (ncol(variable.set) < 2) {
    stop(wrap_console_text("There's a problem: Your list of variables must contain two or more variables for correlation analysis. To see how to use this function, try example(correlateC) or help(correlateC)."))
  }
  
  # convert to NULL for unweighted path (keeps existing behavior)
  w <- if (ctx$meta$weighted) ctx$vars$w else NULL
  weighted <- ctx$meta$weighted
  # ------------------------------------------------------------------------
  
  # ---- NEW: collect printC blocks ----------------------------------------
  blocks <- list()
  # ------------------------------------------------------------------------
  
  # Heading (ALWAYS printed to console)
  blocks <- render_heading(blocks, "Correlation Analysis", marker="=", printC=printC)
  
  
  # -------------------------
  # Bivariate correlation
  # -------------------------
  if(ncol(variable.set) == 2)
  {
    result.raw <- weighted_cor(x=variable.set[, 1], y=variable.set[, 2], weight=w, ...)
    result <- data.frame(round(result.raw, digits))
    result$p.value <- format_p_value(result.raw[, "p.value"], digits = digits)
    rownames(result) <- NULL
    
    caption <- paste("Correlation between", variable.names[1], "and", variable.names[2])
    if(weighted==TRUE) caption <- paste(caption, ", weighted by ", w.name, sep="")
    
    if(stats==TRUE)
    {      
      return.result <- result
      # Console table output, HTML table output (for printC)
      if(table!=FALSE) blocks <- render_table(blocks, x=result, caption=caption, digits=digits, printC=printC)
    }
    
    if(stats==FALSE) 
    {
      coef.only <- as.numeric(result[, 1])
      names(coef.only) <- c("Pearson's r:")
      return.result <- coef.only
      
      coef.table <- data.frame(
        Statistic = "Pearson's r",
        Value = unname(coef.only),
        row.names = NULL,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      if(table!=FALSE) blocks <- render_table(blocks, x=coef.table, caption=caption, digits=digits, printC=printC)
    }
    
    
    # bivariate scatterplot
    if(isTRUE(plot))
    {
      # Decide jitter behavior once (no missing() used here)
      do_jitter <- isTRUE(jitter)
      if(nrow(variable.set) > 500 && !isTRUE(jitter))
      {
        do_jitter <- TRUE
        message(wrap_console_text("Note: Because there are many points to plot, correlateC is jittering them. Set jitter=FALSE to prevent this."))
      }
      
      draw_scatter <- function() {
        vs <- variable.set
        
        if(nrow(vs) > 500) 
        {
          marker.cex <- .8
          marker.col <- "#00000020"
        }
        else
        {
          marker.cex <- 1
          marker.col <- "#000000"
        }
        
        if(do_jitter) {
          for(j in 1:ncol(vs)) vs[, j] <- jitter(vs[, j], amount=0)
        }
        
        main <- paste("Scatterplot of", variable.names[1], "and", variable.names[2])
        main <- strwrap(main, width=50)
        
        plot(x=vs[,1], y=vs[,2],
             xlab=variable.names[1], ylab=variable.names[2],
             cex=marker.cex, col=marker.col, main=main)
      }
      
      # show to user
      draw_scatter()
      
      # export via printC
      if(isTRUE(printC)) {
        blocks <- add_block(
          blocks,
          plot_block(draw = draw_scatter,
                     filename_base = "correlateC.plot",
                     width = sqrt(6 * 4.5),
                     height = sqrt(6 * 4.5))
        )
      }
    }
  }
  
  # -------------------------
  # Multivariate correlation
  # -------------------------
  if(ncol(variable.set) > 2)
  {
    result <- suppressWarnings(weighted_cor(x=variable.set, weight=w, ...))
    
    correlation <- round(result[["correlation"]], digits)
    variable.names.string <- paste(variable.names, collapse = ", ")
    caption <- paste("Correlation among", variable.names.string)
    if(weighted==TRUE) caption <- paste(caption, ", weighted by ", w.name, sep="")
    
    # Console table output, HTML table output (for printC)
    if(table!=FALSE) blocks <- render_table(blocks, x=correlation, caption=caption, digits=digits, printC=printC)
    
    slightpause()
    
    if(stats==FALSE) 
    {
      return.result <- correlation
    }
    
    if(stats==TRUE) 
    {
      restore.options.scipen <- options(scipen = 999)
      on.exit(options(restore.options.scipen), add = TRUE)
      
      standard.errors <- round(result[["std.err"]], digits)
      se.caption <- "Standard Errors of Correlation Coefficients"
      for(i in 1:ncol(standard.errors)) standard.errors[i, i] <- NA

      # Console table output, HTML table output (for printC)
      if(table!=FALSE) blocks <- render_table(blocks, x=standard.errors, caption=se.caption, digits=digits, printC=printC)
      
      return.result <- result # not sure if this should be standard.errors
      slightpause()
      
      t.stats <- round(result[["t.value"]], digits)
      for(i in 1:ncol(t.stats)) t.stats[i, i] <- NA
      t.stats.caption <- "t-Statistics of Correlation Coefficients"
      
      # Console table output, HTML table output (for printC)
      if(table!=FALSE) blocks <- render_table(blocks, x=t.stats, caption=t.stats.caption, digits=digits, printC=printC)
      slightpause()
            
      p.raw <- result[["p.value"]]
      p.values <- matrix(
        format_p_value(as.vector(p.raw), digits = digits),
        nrow = nrow(p.raw),
        dimnames = dimnames(p.raw)
      )
      for(i in 1:ncol(p.values)) p.values[i, i] <- NA_character_
      p.values.caption <- "p-Values of the t-Statistics"

      # Console table output, HTML table output (for printC)
      if(table!=FALSE) blocks <- render_table(blocks, x=p.values, caption=p.values.caption, digits=digits, printC=printC)
      
      slightpause()
      options(restore.options.scipen)
    }
    
    if(isTRUE(plot))
    {
      do_jitter <- isTRUE(jitter)
      if(nrow(variable.set) > 500 && !isTRUE(jitter))
      {
        do_jitter <- TRUE
        message(wrap_console_text("Note: Because there are many points to plot, correlateC is jittering them. Set jitter=FALSE to prevent this."))
      }
      
      draw_pairs <- function() {
        vs <- variable.set
        
        if(nrow(vs) > 500) 
        {
          marker.cex <- .8
          marker.col <- "#00000020"
        }
        else
        {
          marker.cex <- 1
          marker.col <- "#000000"
        }
        
        if(do_jitter) {
          for(j in 1:ncol(vs)) vs[, j] <- jitter(vs[, j], amount=0)
        }
        
        graphics::pairs(x=vs, labels=variable.names, cex=marker.cex, cex.labels=1.6, col=marker.col)
      }
      
      # show to user
      draw_pairs()
      
      # export via printC
      if(isTRUE(printC)) {
        blocks <- add_block(
          blocks,
          plot_block(draw = draw_pairs,
                     filename_base = "correlateC.plot",
                     width = 6,
                     height = 6)
        )
      }
    }
  }
  
  if(isTRUE(printC)) {
    blocks <- add_block(blocks, match.call(expand.dots = FALSE))
    print_blocksC(blocks)
  }
  
  invisible(return.result)
}
