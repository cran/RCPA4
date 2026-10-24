#' @name graph.info.functions
#' @title Reference information on graphics parameters, like colors, line types, plotting characters
#' @description Students can use these functions as quick references on some of R's plotting options.
#' @export


#' @rdname graph.info.functions
#' @return No value returned
#' @description The **Colors** function produces plot of colors available in R. Colored cells labels with numeric value that corresponds to that color. No values supplied to function.
#' @examples 
#'   Colors()
#' @export
#' @keywords internal
#' @importFrom graphics title rect text
#' @importFrom grDevices col2rgb colors
#' @md
Colors = function()
{
  old.par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old.par), add=TRUE)
  
  colCount <- 25 # number per row
  rowCount <- 27
  plot( c(1,colCount), c(0,rowCount), type="n", ylab="", xlab="",
                  axes=FALSE, ylim=c(rowCount,0))
  graphics::title("R colors")
  
  SetTextContrastColor = function(color)
  {
    ifelse( mean(grDevices::col2rgb(color)) > 127, "black", "white")
  }
  
  for (j in 0:(rowCount-1))
  {
    base <- j*colCount
    remaining <- length(grDevices::colors()) - base
    RowSize <- ifelse(remaining < colCount, remaining, colCount)
    thisColorRect = grDevices::colors()[base + (1:RowSize)]
    
    thisColorText = rep(NA, length(thisColorRect))
    for(i in 1:length(thisColorRect))
    {
      thisColorText[i] = SetTextContrastColor(thisColorRect[i])
    }
    graphics::rect((1:RowSize)-0.5, j-0.5, (1:RowSize)+0.5, j+0.5,
                   border="black",
                   col=thisColorRect)
    graphics::text((1:RowSize), j, paste(base + (1:RowSize)), cex=0.6,
                   col=thisColorText)  # 
  }
}



#' @rdname graph.info.functions
#' @return No value returned
#' @description The **lineType** function displays line types for R plots. No values supplied to function.
#' @examples 
#'   lineType()
#' @export
#' @keywords internal
#' @importFrom graphics points text
#' @md
lineType = function()
{
  old.par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old.par), add=TRUE)
  
  # Set up the plotting area
  plot(NA, xlim=c(0,1), ylim=c(6.5, -0.5),
                 xaxt="n", yaxt="n",
                 xlab=NA, ylab=NA )
  # Draw the lines
  for (i in 0:6)
  {
    graphics::points(c(0.25,1), c(i,i), lty=i, lwd=2, type="l")
  }
  # Add labels
  graphics::text(0, 0, "0. 'blank'"   ,  adj=c(0,.5))
  graphics::text(0, 1, "1. 'solid'"   ,  adj=c(0,.5))
  graphics::text(0, 2, "2. 'dashed'"  ,  adj=c(0,.5))
  graphics::text(0, 3, "3. 'dotted'"  ,  adj=c(0,.5))
  graphics::text(0, 4, "4. 'dotdash'" ,  adj=c(0,.5))
  graphics::text(0, 5, "5. 'longdash'",  adj=c(0,.5))
  graphics::text(0, 6, "6. 'twodash'" ,  adj=c(0,.5))
}



#' @rdname graph.info.functions
#' @return No return
#' @description The **plotChar** function displays plotting characters available in R. No values supplied to function.
#' @examples 
#'   plotChar()
#' @export
#' @keywords internal
#' @importFrom graphics points text
#' @md
plotChar = function()
{
  old.par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old.par), add=TRUE)
  
  Pex  <- 3           ## good for .Device=="postscript", "x11", "quartz"
  ipch <- 0:35
  np   <- length( ipch )
  k    <- floor( sqrt( np ) )
  dd   <- c( -1, 1 ) / 2
  ix   <- ipch %/% k
  iy   <- 3 + ( k - 1 ) - ipch %% k
  rx   <- dd + range( ix )
  ry   <- dd + range( iy )
  pch  <- as.list( ipch )
  
  ##  Values of 0 through 25 specify standard R symbols.
  ##  Points can also be drawn as text characters; add 10 characters to the
  ##  list of symbols to be plotted.
  
  pch[ 26 + 1:10 ] <-
    as.list( c( "*", ".", "o", "O", "0", "+", "-", "|", "%", "#" ) )
  
  ##  Plot without drawing any points or lines (type = "n").
  ##  This draws the title and sets up the coordinates of the plot.
  
  plot(
    x    = rx,
    y    = ry,
    type = "n",
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = paste( "Plotting Characters in R" ) )
  
  ##  Draw horizontal and vertical dotted grid lines.
  ##  abline( v = ix, h = iy, col = "lightgray", lty = "dotted" )
  
  ##  Iterate through the points, drawing the specified pch (given by an
  ##  integer from 0 through 25 or by a character ('*' through '#').
  
  for ( i in 1:np )
  {
    ##  Extract each plot character from the list using "[[", which
    ##  gets a list component ("[" would return a list).
    
    pc <- pch[[ i ]]
    
    ##  Call points() to draw the plot character.
    ##  Draw red symbols with a yellow interior (filled interior is possible
    ##  only for symbols 21 through 25).
    ##  Expand the size of the plot character.
    
    graphics::points(
      x   = ix[ i ],
      y   = iy[ i ],
      pch = pc,
      col = "black",
      bg  = "gray50",
      cex = Pex )
    
    ##  Label the points with the symbol numbers or characters.
    
    graphics::text(
      x      = ix[ i ] - 0.4,
      y      = iy[ i ],
      labels = pc,
      col    = "brown",
      cex    = 1.2 )
  }
}
