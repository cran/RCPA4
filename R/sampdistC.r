#' Plots probability and cumulative density functions (PDFs and CDFs) of sample statistics
#'
#' @description Visualize expected sampling distributions for sample statistics. You can plot the probability and cumulative density functions for statistics based on either the normal distribution or a t-distribution. 
#' The `sampdistC` function also generates the confidence interval (default 95%) for a sample statistic which is useful for obtaining the CI of a summary statistic (when you're not estimating it from the dataset yourself).
#' @param stat A numeric statistic, the point estimate of a parameter based on a sample of observations, like a sample mean or a sample proportion.
#' @param se The standard error of the statistic, must be a positive number.
#' @param t.df (Optional) If critical values for sampling distribution should be based on t-distribution (generally true when statistic is a mean), set t.df to the number of degrees of freedom (typically n-1).
#' @param plot.cdf (Optional) Do you want to plot the cumulative density function? Default = FALSE (for probability density function).
#' @param ci (Optional) Specify desired confidence level for confidence interval as a percentage. Set ci=FALSE to suppress CI table (default: 95)
#' @param digits (Optional) Number of digits after decimal to display in CI table (default: 3)
#' @param printC (Optional) Do you want to sampling distribution plot to .html file in working directory? (default: FALSE)
#' @param table (Optional) Do you want the confidence interval table reported when `ci` is used? (default: TRUE)
#' @return Invisibly returns a data frame (`sampdistC_result`) containing the CI table
#' when `ci` is requested. If `ci=FALSE`, returns an empty CI-shaped data frame and
#' stores `ci_table=NULL` in auxiliary metadata.
#' @examples  
#'   library(RCPA4)
#' 
#'   sampdistC(stat=10, se=1)
#'   
#'   \donttest{
#'   # based on normal distributions
#'   sampdistC(stat=10, se=1, plot.cdf=TRUE)
#'   
#'   # based on t-distribution with 15 degrees of freedom
#'   sampdistC(stat=8, se=2, t.df=15)
#'   sampdistC(stat=8, se=2, t.df=15, plot.cdf=TRUE)
#'   }
#' @export
#' @template console-output
#' @template result-tables
#' @section RCPA4 Package Tutorial Videos:
#' * [Viewing Sampling Distributions with RCPA4's sampdistC Function](https://www.youtube.com/watch?v=Tz5wlmo1Jzk) 21:51   
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapter 8. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Sage Publications, 2020), Chapter 6. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Foundations of Inference](https://www.poliscidata.com/pages/rDemosResources.php?chapter=8), Compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications.
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom graphics par mtext polygon segments text arrows abline axis legend box
#' @importFrom grDevices png dev.off
#' @importFrom stats qnorm qt pnorm dnorm dt pt
#' @md
sampdistC <- function(stat, se, t.df=NULL, plot.cdf=FALSE, ci=95, digits=3, printC=FALSE, table=TRUE) 
{
  assert_required(stat, se)
  assert_numeric(stat)
  assert_positive_numeric(se)
  assert_nonnegative_integer(digits)
  if(!missing(t.df)) assert_positive_numeric(t.df)
  
  old.par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old.par), add=TRUE)

  
  if(ci!=FALSE)
  {
    assert_positive_numeric(ci)
    if((ci > 0) & (ci < 1))
    {
      ci <- ci * 100
      message(wrap_console_text("Please note: It looks like you entered confidence level as a proportion. Confidence level should be percentage, like 95 or 99. Attempting to convert..."))
    }
    if(ci > 100) stop("There's a problem: confidence level should not exceed 100.")
    if((ci <= 50) & (ci >= 1)) stop(wrap_console_text("There's a problem: confidence level should be percentage, like 95 or 99, not Type I error rate."))
  }
  
  # ---- NEW: collect printC blocks ----------------------------------------
  blocks <- list()
  # ------------------------------------------------------------------------
  
  # Heading (ALWAYS printed to console)
  blocks <- render_heading(blocks, "Expected Distribution of Sample Statistics", marker="=", printC=printC)
  
  # precompute "missingness" OUTSIDE any draw() closure
  t_df_missing <- missing(t.df)
  
  # CI level helper
  if(ci!=FALSE) half.alpha <- (100-ci)/200 else half.alpha <- .025 
  
  # compute bounds once (used for table + plot ranges)
  if(t_df_missing)
  {
    lower <- stat + stats::qnorm(.025)*se
    upper <- stat + stats::qnorm(.975)*se
    lower99 <- stat + stats::qnorm(.005)*se
    upper99 <- stat + stats::qnorm(.995)*se
    lower.otherlevel <- stat + stats::qnorm(half.alpha)*se
    upper.otherlevel <- stat + stats::qnorm(1-half.alpha)*se
  }
  else 
  {
    lower <- stat + stats::qt(.025, df=t.df)*se
    upper <- stat + stats::qt(.975, df=t.df)*se
    lower99 <- stat + stats::qt(.005, df=t.df)*se
    upper99 <- stat + stats::qt(.995, df=t.df)*se
    lower.otherlevel <- stat + stats::qt(half.alpha, df=t.df)*se
    upper.otherlevel <- stat + stats::qt(1-half.alpha, df=t.df)*se
  }
  
  ci.tab <- NULL
  if(ci != FALSE)
  {
    title <- paste(ci, "% Confidence Interval of a Sample Statistic", sep = "")
    if(ci==95) ci.tab <- data.frame(lower, stat, upper)
    else if(ci==99) ci.tab <- data.frame(lower99, stat, upper99)
    else ci.tab <- data.frame(lower.otherlevel, stat, upper.otherlevel)
    names(ci.tab) <- c("Lower Bound", "Point Estimate", "Upper Bound")
    if(table != FALSE) {
      # Console table output, HTML table output (for printC)
      blocks <- render_table(blocks, x=ci.tab, caption=title, digits=digits, printC=printC)
      slightpause()
    }
  }
  
  # plot prep
  plot.over.x <- seq(lower-2*(stat-lower), upper+2*(upper-stat), by=(upper-lower)/100)
  
  # draw function (used for console + export)
  draw_sampdist_plot <- function() {
    # densities
    if(t_df_missing) 
    {
      if(isTRUE(plot.cdf)) densities.y <- stats::pnorm(q=plot.over.x, mean=stat, sd=se, lower.tail=TRUE)
      if(!isTRUE(plot.cdf)) densities.y <- stats::dnorm(x=plot.over.x, mean=stat, sd=se)
    }
    else 
    {
      plot.over.t.stats <- (plot.over.x - stat) / se
      if(!isTRUE(plot.cdf)) densities.y <- stats::dt(x=plot.over.t.stats, df=t.df)
      if(isTRUE(plot.cdf))  densities.y <- stats::pt(q=plot.over.t.stats, df=t.df, lower.tail=TRUE)
    }
    
    graphics::par(mar=c(8,4,3,1))
    
    plot(x="", y="",
         xlim=c(lower-1.25*(stat-lower), upper+1.25*(upper-stat)),
         ylim=c(0, max(densities.y)),
         axes=FALSE, xlab="", ylab="", main="")
    
    if(t_df_missing) dist.title <- paste("Normal Distribution of Sample Statistics (stat=", stat, ", se=", se, ")", sep="")
    else dist.title <- paste("t-Distribution of Sample Statistics (stat=", stat, ", se=", se, ", df=", t.df, ")", sep="")
    graphics::mtext(text = dist.title, side = 3, line=1.25, font=2, cex=1.1)
    
    # PDF-style plot (density)
    if(!isTRUE(plot.cdf))
    {
      graphics::polygon(x=c(min(plot.over.x), plot.over.x, max(plot.over.x)),
                        y=c(0, densities.y, 0), col="gray90")
      
      # +/- 1 SE
      if(t_df_missing) 
      {
        density.minus1se <- stats::dnorm((stat-se), mean=stat, sd=se)
        density.plus1se  <- stats::dnorm((stat+se), mean=stat, sd=se)
      }
      else 
      {
        density.minus1se <- stats::dt(-1, df=t.df)
        density.plus1se  <- stats::dt(+1, df=t.df)
      }
      graphics::segments(x0=stat-se, x1=stat-se, y0=0, y1=max(densities.y), lty=3, col="gray50")
      graphics::segments(x0=stat+se, x1=stat+se, y0=0, y1=max(densities.y), lty=3, col="gray50")
      graphics::polygon(x=c(plot.over.x[plot.over.x<=(stat-se)], (stat-se), (stat-se)),
                        y=c(densities.y[plot.over.x<=(stat-se)], density.minus1se, 0), col="pink")
      graphics::polygon(x=c(plot.over.x[plot.over.x>=(stat+se)], (stat+se), (stat+se)),
                        y=c(densities.y[plot.over.x>=(stat+se)], 0, density.plus1se), col="pink")
      
      # 5% cutoffs (already have lower/upper)
      if(t_df_missing) 
      {
        density.lower <- stats::dnorm(lower, mean=stat, sd=se)
        density.upper <- stats::dnorm(upper, mean=stat, sd=se)
      }
      else
      {
        density.lower <- stats::dt(stats::qt(.025, df=t.df), df=t.df)
        density.upper <- stats::dt(stats::qt(.975, df=t.df), df=t.df)
      }
      graphics::segments(x0=lower, x1=lower, y0=0, y1=max(densities.y), lty=3, col="gray50")
      graphics::segments(x0=upper, x1=upper, y0=0, y1=max(densities.y), lty=3, col="gray50")
      graphics::polygon(x=c(plot.over.x[plot.over.x<=lower], lower, lower),
                        y=c(densities.y[plot.over.x<=lower], density.lower, 0), col="red")
      graphics::polygon(x=c(plot.over.x[plot.over.x>=upper], upper, upper),
                        y=c(densities.y[plot.over.x>=upper], 0, density.upper), col="red")
      
      graphics::text(x=lower-.1*(stat-lower), y=.2*max(densities.y), labels="2.5% of area", pos=2, cex=.8)
      graphics::text(x=upper+.1*(upper-stat), y=.2*max(densities.y), labels="2.5% of area", pos=4, cex=.8)
      
      # 1% cutoffs (already have lower99/upper99)
      if(t_df_missing) 
      {
        density.lower99 <- stats::dnorm(lower99, mean=stat, sd=se)
        density.upper99 <- stats::dnorm(upper99, mean=stat, sd=se)
      }
      else
      {
        density.lower99 <- stats::dt(stats::qt(.005, df=t.df), df=t.df)
        density.upper99 <- stats::dt(stats::qt(.995, df=t.df), df=t.df)
      }
      graphics::segments(x0=lower99, x1=lower99, y0=0, y1=max(densities.y), lty=3, col="gray50")
      graphics::segments(x0=upper99, x1=upper99, y0=0, y1=max(densities.y), lty=3, col="gray50")
      graphics::polygon(x=c(plot.over.x[plot.over.x<=lower99], lower99, lower99),
                        y=c(densities.y[plot.over.x<=lower99], density.lower99, 0), col="darkred")
      graphics::polygon(x=c(plot.over.x[plot.over.x>=upper99], upper99, upper99),
                        y=c(densities.y[plot.over.x>=upper99], 0, density.upper99), col="darkred")
      
      graphics::text(x=lower99-.1*(stat-lower), y=.05*max(densities.y), labels="0.5% of area", pos=2, cex=.6)
      graphics::text(x=upper99+.1*(upper-stat), y=.05*max(densities.y), labels="  0.5% of area", pos=4, cex=.6)
      
      # SE areas
      if(t_df_missing) outside.1se.area <- "16"
      else {
        outside.1se.area <- 100 * stats::pt(1, lower.tail=FALSE, df=t.df)
        outside.1se.area <- round(outside.1se.area, 1)
      }
      graphics::text(x=stat-1.4*se, y=.65*max(densities.y), labels=paste(outside.1se.area, "% of area", sep=""), pos=2, cex=.9)
      graphics::text(x=stat+1.4*se, y=.65*max(densities.y), labels=paste(outside.1se.area, "% of area", sep=""), pos=4, cex=.9)
      
      if(t_df_missing) middle.area.percent <- "68"
      else {
        middle.area.percent <- 100 - 2*outside.1se.area
        middle.area.percent <- round(middle.area.percent, 1)
      }
      graphics::text(x=stat, y=.15*max(densities.y), labels=paste(middle.area.percent, "% of\narea", sep=""), pos=3)
      
      graphics::arrows(x0=lower99, x1=lower99-1.25*se, y0=.09*max(densities.y), y1=.09*max(densities.y), angle=15, lend=1, length=.1)
      graphics::arrows(x0=upper99, x1=upper99+1.25*se, y0=.09*max(densities.y), y1=.09*max(densities.y), angle=15, lend=1, length=.1)
      graphics::arrows(x0=lower, x1=lower-2*se, y0=.25*max(densities.y), y1=.25*max(densities.y), angle=15, lend=1, length=.1)
      graphics::arrows(x0=upper, x1=upper+2*se, y0=.25*max(densities.y), y1=.25*max(densities.y), angle=15, lend=1, length=.1)
      graphics::arrows(x0=stat-se, x1=stat-se-3*se, y0=.7*max(densities.y), y1=.7*max(densities.y), angle=15, lend=1, length=.1)
      graphics::arrows(x0=stat+se, x1=stat+se+3*se, y0=.7*max(densities.y), y1=.7*max(densities.y), angle=15, lend=1, length=.1)
      
      graphics::mtext(text="Probability Density", side=2, line=1.5)
    }
    
    # CDF-style plot
    if(isTRUE(plot.cdf))
    {
      graphics::abline(h=c(0.025, 0.975), lty=3, col="red")
      graphics::abline(h=c(0.005, 0.995), lty=3, col="darkred")
      
      if(t_df_missing) 
      {
        cd.minus1se <- stats::pnorm((stat-se), mean=stat, sd=se, lower.tail=TRUE)
        cd.plus1se  <- stats::pnorm((stat+se), mean=stat, sd=se, lower.tail=TRUE)
      }
      else 
      {
        cd.minus1se <- stats::pt(-1, df=t.df, lower.tail=TRUE)
        cd.plus1se  <- stats::pt(+1, df=t.df, lower.tail=TRUE)
      }
      graphics::abline(h=c(cd.minus1se, cd.plus1se), lty=3, col="pink")
      
      graphics::polygon(x=c(min(plot.over.x), plot.over.x, max(plot.over.x)),
                        y=c(0, densities.y, 0), col="gray90", border="gray40")
      
      if(t_df_missing) 
      {
        cd.lower   <- stats::pnorm(lower,   mean=stat, sd=se, lower.tail=TRUE)
        cd.upper   <- stats::pnorm(upper,   mean=stat, sd=se, lower.tail=TRUE)
        cd.lower99 <- stats::pnorm(lower99, mean=stat, sd=se, lower.tail=TRUE)
        cd.upper99 <- stats::pnorm(upper99, mean=stat, sd=se, lower.tail=TRUE)
      }
      else
      {
        cd.lower   <- stats::pt(stats::qt(.025, df=t.df), df=t.df, lower.tail=TRUE)
        cd.upper   <- stats::pt(stats::qt(.975, df=t.df), df=t.df, lower.tail=TRUE)
        cd.lower99 <- stats::pt(stats::qt(.005, df=t.df), df=t.df, lower.tail=TRUE)
        cd.upper99 <- stats::pt(stats::qt(.995, df=t.df), df=t.df, lower.tail=TRUE)
      }
      graphics::segments(x0=lower, x1=lower, y0=0, y1=cd.lower, lty=3, col="red")
      graphics::segments(x0=upper, x1=upper, y0=0, y1=cd.upper, lty=3, col="red")
      graphics::segments(x0=lower99, x1=lower99, y0=0, y1=cd.lower99, lty=3, col="darkred")
      graphics::segments(x0=upper99, x1=upper99, y0=0, y1=cd.upper99, lty=3, col="darkred")
      graphics::segments(x0=stat-se, x1=stat-se, y0=0, y1=cd.minus1se, lty=3, col="pink")
      graphics::segments(x0=stat+se, x1=stat+se, y0=0, y1=cd.plus1se, lty=3, col="pink")
      
      graphics::mtext(text="Cumulative Density", side=2, line=3)
      graphics::axis(side=2, line=0, las=2, at=seq(0,1,by=.1), labels=seq(0,1,by=.1))
      graphics::legend(x=lower-1.25*(stat-lower), y=.7,
                       legend=c("+/- 1 SE", "5% levels", "1% levels"),
                       lty=3, col=c("pink","red","darkred"), cex=.8, bty="n")
    }
    
    # common axes
    graphics::axis(side=1, line=0)
    graphics::axis(side=1,
                   at=c(stat-4*se, stat-3*se, stat-2*se, stat-se, stat, stat+se, stat+2*se, stat+3*se, stat+4*se),
                   labels=c(-4,-3,-2,-1,0,1,2,3,4),
                   line=4.5)
    graphics::mtext(text="Expected Sample Statistics (Original Scale)", side=1, line=2.25)
    xlab <- if(t_df_missing) "Standardized Statistics (Z)" else "Standardized Statistics (t)"
    graphics::mtext(text=xlab, side=1, line=6.5)
    graphics::box()
  }
  
  # show plot to user
  draw_sampdist_plot()
  
  # export plot for printC
  if(isTRUE(printC)) {
    blocks <- add_block(
      blocks,
      plot_block(draw = draw_sampdist_plot,
                 filename_base = "sampdistC.plot",
                 width = 6,
                 height = 4.5)
    )
  }
  
  if(isTRUE(printC)) {
    blocks <- add_block(blocks, match.call(expand.dots = FALSE))
    print_blocksC(blocks)
  }
  
  if(is.null(ci.tab)) {
    out <- data.frame(
      "Lower Bound" = numeric(0),
      "Point Estimate" = numeric(0),
      "Upper Bound" = numeric(0),
      check.names = FALSE
    )
  } else {
    out <- ci.tab
  }
  class(out) <- c("sampdistC_result", class(out))
  out <- rcpa4_attach_tables(
    out = out,
    tables = list(ci_table = ci.tab),
    fn_name = "sampdistC",
    call_obj = match.call(expand.dots = FALSE)
  )
  invisible(out)
}



