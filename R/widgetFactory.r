#' An interactive game to practice using R functions. 
#'
#' @description A fun, interactive game to practice using R functions. Players must execute functions to make widgets per strict specifications to win the game. The Widget Factory needs your help!
#' @return No value returned
#' @examples  
#' library(RCPA4)
#'  
#' # Play the Widget Factory game
#' if (interactive()) {
#' widgetFactory()
#' }
#' @export
#' @section RCPA4 Package Tutorial Videos:
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook Reference:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapter 1. 
#' @section Online Resource:
#' * [Tutorials & Resources for Using R for Data Analysis](https://www.poliscidata.com/pages/rDemosResources.php?chapter=1), compiled by Barry C. Edwards 
#' @importFrom graphics par points abline
#' @importFrom beepr beep
#' @md

widgetFactory <- function()
{
  message("The Widget Factory needs your help!\n")
  slowConsolePrint("Widgets come in different quantities, sizes, colors, and shapes. ")

  # intro slide of widget catalog
  old.par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old.par), add=TRUE)
  graphics::par(ask=FALSE, mar=c(0,0,1,0), xpd=TRUE)
  
  plot(x="", y="", xlim=c(0,4), ylim=c(0,4), axes=F, xlab="", ylab="", main="Sample Widget Shapes, Sizes, and Colors", cex.main=1)
  # small widgets
  graphics::points(x=c(.25, .75, 1.25, 1.75)+.5, y=rep(3.5, 4), pch=c(15, 16, 17,18), col="orange", cex=2)
  # medium widgets
  graphics::points(x=c(.25, 1, 1.75, 2.5), y=rep(2.5, 4), pch=c(15, 16, 17,18), col="red", cex=8)
  # large widgets
  graphics::points(x=c(.5,1.9,3.3), y=rep(1,3), pch=c(15, 16, 17), col=c("black", "blue", "green"), cex=20)
  graphics::points(x=3.3, y=3.25, pch=18, col="purple", cex=20)
  Sys.sleep(.5)
  slowConsolePrint("Make the right widgets and you'll earn points.\n\n")
  slowConsolePrint("The makeWidget function has four arguments and can make all the widgets you'll need to win the game:\n")
  slowConsolePrint("  q       can = 1, 2, or 3\n")
  slowConsolePrint("  size    can = \"small\", \"medium\", or \"large\"\n")
  slowConsolePrint("  col     can = \"red\", \"blue\", \"orange\", \"green\", \"purple\", or \"black\"\n")
  slowConsolePrint("  shape   can = \"square\", \"circle\", \"triangle\", or \"diamond\"\n\n")
  slowConsolePrint("Customers are waiting for their widgets, so ready, set ....... let's go!\n\n")
  beepr::beep(sound="treasure")
  gameon <- TRUE
  currentscore <- 0
  level <- 1
  start_time <- NULL
  
  
  findWidget <- function(shape, size, q, col, yaxis=.8)
  {
    if(size=="small") cex <- 2
    else if(size=="medium") cex <- 8
    else if(size=="large") cex <- 20
    else warning(paste("Can't find size \"", size, "\"!", sep=""))
    
    if(q==1) xlocations = c(2)
    else if(q==2) xlocations = c(1.25,2.75)
    else if(q==3) xlocations = c(.65,2,3.35)
    else warning(paste("Can't find quantity \"", q, "\"!", sep=""))
    
    if(shape=="square") pch <- 15
    else if(shape=="circle") pch <- 16
    else if(shape=="triangle") pch <- 17
    else if(shape=="diamond") pch <- 18
    else if(shape=="star") pch <- "*"
    else if(shape=="minus") pch <- "-"
    else if(shape=="plus") pch <- "+"
    else if(shape=="box") pch <- 7
    else warning(paste("Can't find shape \"", shape, "\"!", sep=""))
    
    if(col!="red" && col!="blue" && col!="orange" && col!="green" && col!="purple" && col!="black") warning(wrap_console_text(paste("Can't find color \"", col, "\"!", sep="")))
    graphics::points(x=xlocations, y=rep(yaxis, length(xlocations)), pch=pch, cex=cex, col=col)
    # list(q, size, col, shape, yaxis)
    point.value = 3
    if(shape=="star" | shape=="bar" | shape=="plus" | shape=="box") point.value <- point.value + q
    return(list(q=q, size=size, col=col, shape=shape, point.value=point.value))
  }
  
  
  while(gameon==TRUE)
  {
    # make a random widget and plot it
    color <- sample(x=c("red", "blue", "orange", "green", "purple", "black"), size = 1)
    size  <- sample(x=c("small", "medium", "large"), size=1)
    widget.shapes <- c("square", "circle", "triangle", "diamond")
    if(level==3) widget.shapes <- c(widget.shapes, "star", "minus", "plus", "box")
    shape <- sample(x=widget.shapes, size=1)
    quantity <- sample(1:3, size=1)

    
    if(currentscore < 10)
    {
      prompt.user <- "Use the makeWidget function to make the widget(s) you see in the plot. Make the right widget(s) to earn points!\n"
    }
    else if (currentscore > 50)
    {
      if(level<3) 
      {
        beepr::beep(sound="fanfare")
        slowConsolePrint("Amazing news..........................")
        message("The Widget Factory is doing so well with you producing widgets, it's introducing four new shapes!")
        
        # slide for new widget catalog
        plot(x="", y="", xlim=c(0,4), ylim=c(0,4), axes=F, xlab="", ylab="", main="New Widget Shapes", cex.main=1)
        # small widgets
        graphics::points(x=c(.75, 1.25, 1.75), y=rep(3.5, 3), pch=c("*", "-", "+"), col="orange", cex=2)
        graphics::points(x=2.25, y=3.5, pch=7, col="orange", cex=2)
        # medium widgets
        graphics::points(x=c(.25, 1, 1.75), y=rep(2.5, 3), pch=c("*", "-", "+"), col="red", cex=8)
        graphics::points(x=2.5, y=2.5, pch=7, col="red", cex=8)
        # large widgets
        graphics::points(x=c(.25,2,3.75), y=rep(1,3), pch=c("*", "-", "+"), col=c("black", "blue", "green"), cex=20)
        graphics::points(x=3.75, y=3.25, pch=7, col="purple", cex=20)

        slowConsolePrint("\nIn addition to the four classic shapes,\n")
        slowConsolePrint("  shape   can now = \"star\", \"box\", \"plus\", or \"minus\"\n\n")
        slowConsolePrint("When you produce widgets with these new shapes, you'll earn extra points!\n\n")
      }
      prompt.user <- "Produce the right widget(s) quickly and accurately to win the game!\n"
      level <- 3
    }
    else
    {
      if(level==1) 
        {
          beepr::beep(sound="fanfare")
          slowConsolePrint("Exciting news..........................")
          message(wrap_console_text("Your widget-making skills have earned you a promotion!! You can now use the findWidget function to find widget(s) instead of making them."))
          slowConsolePrint("\nThe findWidget function has the same arguments as the makeWidget function, but they're in a different order. Don't rely on positional matching; use the argument keywords. It costs less to find widgets than to make them, so finding the right widget(s) will score you more points!\n\n")
        }
      prompt.user <- "Use the findWidget function to find the right widget(s) and score more points!\n"
      level <- 2
    }
    
    
    
    plot(x="", y="", xlim=c(0,4), ylim=c(0,4), axes=F, xlab="", ylab="", 
         main=paste("Current score:", currentscore), cex.main=.8)
    graphics::abline(h=2, col="black")  
    model.widget <- makeWidget(q=quantity, size=size, col=color, shape=shape, yaxis=2.9)
    start_time <- Sys.time()
    
    
    # print(as.expression(user.command))
    user.command <- ask(prompt.user)
    # return(list(class(user.command), user.command, length(user.command)))
    user.widget <- eval(str2expression(user.command))

    # return(list(class(user.widget), user.widget, length(user.widget)))
    
    cat("\nThe Widget Factory is evaluating the widget(s) ")
    slowConsolePrint("you made..........................\n")
    
    # this should end game if no input without throwing error
    if(is.null(user.widget))
    {
      gameon <- FALSE
      gameover.message <- paste("\nSorry, you didn't make the right widget(s). Final score: ", currentscore, ".\n", sep="")
      message(gameover.message)
      return("Please try again!")
    }
    
    # print(model.widget)
    # print(user.widget)
    cat("Same quantity? ")
    q.match <- (model.widget$q == user.widget$q)
    if(q.match) 
      {
      slowConsolePrint("Yes!\n") 
      # beepr::beep(sound="coin")
      }
    else slowConsolePrint(paste("No. Model shows ", model.widget$q, " widget(s). You made ", user.widget$q, " widget(s).\n", sep=""))
    cat("Same size?     ")
    size.match <- (model.widget$size == user.widget$size)
    if(size.match) 
    {
      slowConsolePrint("Yes!\n") 
      # beepr::beep(sound="coin")
    }
    else slowConsolePrint(paste("No. Model size is ", model.widget$size, ", not ", user.widget$size, ".\n", sep=""))
    cat("Same color?    ")
    col.match <- (model.widget$col == user.widget$col)
    if(col.match) 
    {
      slowConsolePrint("Yes!\n") 
      # beepr::beep(sound="coin")
    }
    else slowConsolePrint(paste("No. Model color is ", model.widget$col, ", not ", user.widget$col, ".\n", sep=""))
    cat("Same shape?    ")
    shape.match <- (model.widget$shape == user.widget$shape)
    if(shape.match) 
    {
      slowConsolePrint("Yes!\n") 
      # beepr::beep(sound="coin")
    }
    else slowConsolePrint(paste("No. Model shape is ", model.widget$shape, ", not ", user.widget$shape, ".\n", sep=""))
    
    
    if(q.match && size.match && col.match && shape.match)
    {
      complete_time <- Sys.time()
      time_to_complete <- round(complete_time - start_time)
      message("\nCongratulations! You made the right widget(s). Keep it up.\n") 
      beepr::beep(sound="coin", Sys.sleep(.2))
      
      if(time_to_complete < 30)
      {
        message("Bonus point for speed!\n")
        beepr::beep(sound="coin", Sys.sleep(.2))
        currentscore <- currentscore + 1
      }
      if(shape=="star" | shape=="bar" | shape=="plus" | shape=="box")
      {
        message("Bonus points for producing new shapes!\n")
        beepr::beep(sound="coin", Sys.sleep(.2))
      }
      # cat("That took: ", round(complete_time - start_time), "seconds \n")
      currentscore <- currentscore + user.widget$point.value
    }
    else
    {
      message(wrap_console_text(paste("\nSorry, you didn't make the right widget(s). Final score: ", currentscore, ". Please try again!\n", sep="")))
      gameon <- FALSE
    }
    
    
    if(currentscore > 100)
    {
      slowConsolePrint("Now for a special announcement from the Widget Factory......................\n")
      beepr::beep(sound="mario")
      quickConsolePrint("                           !     !     !\n")
      quickConsolePrint("(          (    *         |V|   |V|   |V|        )   *   )       (\n")
      quickConsolePrint(" )   *      )             | |   | |   | |        (       (   *    )\n")
      quickConsolePrint("(          (           (*******************)    *       *    )    *\n")
      quickConsolePrint("(     (    (           (    *         *    )               )    (\n")
      quickConsolePrint(" )   * )    )          (   \\|/       \\|/   )         *    (      )\n")
      quickConsolePrint("(     (     *          (<<<<<<<<<*>>>>>>>>>)               )    (\n")
      quickConsolePrint(" )     )        ((*******************************))       (  *   )\n")
      quickConsolePrint("(     (   *     ((          PARTY TIME!!!!       ))      * )    (\n")
      quickConsolePrint(" ) *   )        ((   *    *   *    *    *    *   ))   *   (      )\n")
      quickConsolePrint("(     (         ((  \\|/  \\|/ \\|/  \\|/  \\|/  \\|/  ))        )    (\n")
      quickConsolePrint("*)     )        ((^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^))       (      )\n")
      quickConsolePrint("(     (   (**********************************************) )  * (\n\n")
      message("Amazing job. You've filled all the orders for widgets and won the game!\n")
      slowConsolePrint("You've learned to execute R functions quickly and accurately. Apply these skills in your political analysis with R. \n\n")
      gameon <- FALSE
    }
    
  } # end of while loop for game
}


#' @importFrom graphics points
makeWidget <- function(q, size, col, shape, yaxis=.8)
{
  if(size=="small") cex <- 2
  else if(size=="medium") cex <- 8
  else if(size=="large") cex <- 20
  else warning("Can't make that size!")
  
  if(q==1) xlocations = c(2)
  else if(q==2) xlocations = c(1.25,2.75)
  else if(q==3) xlocations = c(.65,2,3.35)
  else warning("Can't make that quantity!")
  
  if(shape=="square") pch <- 15
  else if(shape=="circle") pch <- 16
  else if(shape=="triangle") pch <- 17
  else if(shape=="diamond") pch <- 18
  else if(shape=="star") pch <- "*"
  else if(shape=="minus") pch <- "-"
  else if(shape=="plus") pch <- "+"
  else if(shape=="box") pch <- 7
  else warning("Can't make that shape!")
  
  if(col!="red" && col!="blue" && col!="orange" && col!="green" && col!="purple" && col!="black") warning(wrap_console_text("Can't make that color!"))
  
  graphics::points(x=xlocations, y=rep(yaxis, length(xlocations)), pch=pch, cex=cex, col=col)
  # list(q, size, col, shape, yaxis)
  return(list(q=q, size=size, col=col, shape=shape, point.value=1))
}



