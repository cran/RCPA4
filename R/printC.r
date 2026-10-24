# Internal CSS helper for printC HTML output.

printC_css <- function() {
  paste(
    c(
      "<style>",
      "body { margin-left: 5%; margin-right: 5%; font-family: verdana; }",
      "h2 { font-size: 22px; }",
      "h3 { font-size: 16px; }",
      "h4 { font-size: 14px; line-height: 1.1; }",
      "p { font-size: 12px; }",
      "tr:nth-child(even) { background: #F7F7F7; }",
      ".rcpa4-caption { color: #404040; margin: 4px 0 8px 0; font-weight: 700; text-align: center; }",
      ".rcpa4-table-wrap { border: 1px solid black; border-radius: 4px; padding: 0 6px 0 6px; display: inline-block; margin: 10px; }",
      ".rcpa4-table-wrap table { border-spacing: 0px; border-collapse: collapse; display: inline-table; width: auto; min-width: 0; max-width: 100%; table-layout: auto; }",
      ".rcpa4-table-wrap table caption { caption-side: top; text-align: center; padding: 2px 0 6px 0; }",
      ".rcpa4-table-wrap table caption .rcpa4-caption { margin: 2px 0 6px 0; }",
      ".rcpa4-table-wrap th { font-size: 14px; background-color: #EFEFEF; border-bottom: 1.5px dotted black; padding: 4px; }",
      ".rcpa4-table-wrap th.rcpa4-colgroup { text-align: center; background-color: #E7E7E7; border-bottom: 1px solid #B0B0B0; }",
      ".rcpa4-table-wrap thead tr:first-child th.rcpa4-colgroup { border-bottom: none; }",
      ".rcpa4-table-wrap thead tr:first-child th.rcpa4-colgroup-multi { border-bottom: 1.5px dotted black; }",
      ".rcpa4-table-wrap thead tr:first-child th.rcpa4-colgroup-single { border-bottom: none; }",
      ".rcpa4-table-wrap thead tr:first-child th.rcpa4-colgroup-blank { border-bottom: none; }",
      ".rcpa4-table-wrap td { font-size: 14px; padding: 2px; }",
      ".rcpa4-table-wrap table.rcpa4-signif-table thead th:last-child { text-align: left !important; }",
      ".rcpa4-table-wrap table.rcpa4-signif-table tbody td:last-child { text-align: left !important; background-color: #FFFFFF !important; }",
      ".rcpa4-table-wrap tfoot td { font-size: 12px; text-align: left; padding: 6px 4px; border-top: 1px solid #A0A0A0; background-color: #FAFAFA; }",
      ".rcpa4-table-footer { font-style: italic; white-space: pre-line; }",
      ".rcpa4-statement { background-color: #F5F5F5; margin: 10px; padding: 0 10px 0 10px; display: inline-block; border: 1px solid black; border-radius: 4px; }",
      ".rcpa4-statement pre { font-size: 14px; font-family: verdana; }",
      ".rcpa4-output-box { background-color: white; margin: 10px; padding: 0 10px 0 10px; display: inline-block; border: 1px solid gray; border-radius: 4px; }",
      ".rcpa4-output-box pre { font-size: 14px; }",
      ".rcpa4-banner { border-radius: 5px; width: 100%; text-align: center; margin: 2px; }",
      ".rcpa4-banner-content { background-color: white; min-height: 280px; position: relative; width: 100%; margin: 0px; }",
      ".rcpa4-table-wrap { max-width: 100%; overflow-x: auto; vertical-align: top; }",
      ".rcpa4-image-float { text-align: center; width: 300px; max-width: 100%; margin: 10px 0 10px auto; }",
      ".rcpa4-image-thumb { border: 0; width: 300px; }",
      ".rcpa4-image-center { text-align: center; width: 100%; }",
      ".rcpa4-image-full { border: 0; }",
      ".rcpa4-notes { font-size: 12px; }",
      ".rcpa4-banner-content:has(.rcpa4-image-float) {",
      "  display: grid;",
      "  grid-template-columns: minmax(0, 1fr) 320px;",
      "  column-gap: 12px;",
      "  align-items: start;",
      "}",
      ".rcpa4-banner-content:has(.rcpa4-image-float) > .rcpa4-table-wrap,",
      ".rcpa4-banner-content:has(.rcpa4-image-float) > .rcpa4-statement,",
      ".rcpa4-banner-content:has(.rcpa4-image-float) > .rcpa4-output-box,",
      ".rcpa4-banner-content:has(.rcpa4-image-float) > .rcpa4-notes {",
      "  grid-column: 1;",
      "  justify-self: start;",
      "}",
      ".rcpa4-banner-content:has(.rcpa4-image-float) > .rcpa4-table-wrap { width: fit-content; max-width: 100%; }",
      ".rcpa4-banner-content:has(.rcpa4-image-float) > .rcpa4-image-float {",
      "  grid-column: 2;",
      "  grid-row: 1 / span 100;",
      "  align-self: start;",
      "  margin-top: 10px;",
      "}",
      "@media (max-width: 1100px) {",
      "  .rcpa4-banner-content:has(.rcpa4-image-float) { grid-template-columns: minmax(0, 1fr); }",
      "  .rcpa4-banner-content:has(.rcpa4-image-float) > .rcpa4-image-float {",
      "    grid-column: 1;",
      "    grid-row: auto;",
      "    margin: 10px auto;",
      "  }",
      "  .rcpa4-banner-content:has(.rcpa4-image-float) > .rcpa4-table-wrap { width: auto; max-width: 100%; }",
      "}",
      "</style>"
    ),
    collapse = "\n"
  )
}

printC_write_header <- function(outputfile) {
  cat("<head>\n", printC_css(), "\n</head>\n\n", file = outputfile, append = TRUE, sep = "")
}

printC_ensure_css <- function(outputfile) {
  if (!file.exists(outputfile)) {
    printC_write_header(outputfile)
    return(invisible(NULL))
  }
  
  lines <- tryCatch(readLines(outputfile, warn = FALSE), error = function(e) character(0))
  has_modern_css <- any(grepl(".rcpa4-table-wrap { border: 1px solid black;", lines, fixed = TRUE)) &&
    any(grepl(".rcpa4-banner { border-radius: 5px;", lines, fixed = TRUE))
  
  if (!has_modern_css) {
    cat("\n", printC_css(), "\n", file = outputfile, append = TRUE, sep = "")
  }
  
  invisible(NULL)
}

#' Prints table of results to a .html file in local working directory
#'
#' @description Prints table or summary of results to a .html file in local
#' working directory. Converting Console format tables to .html tables helps
#' users quickly create publication- and presentation-ready tables. The .html
#' file's name is displayed as Console message.
#' Current date added to Table.Output.html file name to keep output organized.
#' You can print output directly from Companion functions using `printC=TRUE`
#' argument (where available).
#' @param objx A table or data frame. The table must be html-ready, not all
#' Console output is organized in tables. If objx is not a html-ready table,
#' `printC` will write it as preformatted text to the .html file in the working
#' directory.
#' @param file (Optional) The path/file name for .html output. If not
#' specified, function will output to .html file in your working directory.
#' @return Invisibly returns append metadata used by internal block printers.
#' The formatted objx is outputted to a .html file in working directory.
#' @examples
#'    library(RCPA4)
#'
#'    example.table <- freqC(x=world$vdem_4cat, plot=FALSE)
#'    # running printC will generate a .html file in your working directory
#' \donttest{
#'    printC(example.table, file=tempfile(fileext = ".html"))
#' }
#' @export
#' @section RCPA4 Package Tutorial Videos:
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more.
#' @section Textbook Reference:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapter 1.
#' @importFrom knitr kables
#' @importFrom utils capture.output
#' @md
printC <- function(objx, file=NULL)
{ 
  assert_required(objx)
  if(missing(file)) 
  {
    outputfile <- paste("Table.Output.", format(Sys.Date(), "%b%d%y"), ".html", sep="")
  }          
  else 
  {
    outputfile <- file
  }
  outputfile.label <- basename(outputfile)
  output.dir <- dirname(normalizePath(outputfile, winslash = "/", mustWork = FALSE))
  tables_appended <- FALSE
  images_appended <- NULL
  
  add_images <- function(curr, new_images) {
    if(is.null(new_images) || length(new_images) == 0) return(curr)
    unique(c(curr, new_images))
  }
  
  # ensure output file has current CSS rules used by class-based print blocks
  printC_ensure_css(outputfile)
  
  # ----------------------------------------------------------------------
  # NEW: plot block support (used by revised histC)
  # ----------------------------------------------------------------------
  if(isa(objx, "plotC"))
  {
    if(is.null(objx$draw) || !is.function(objx$draw)) {
      stop("plotC object must contain a draw function in objx$draw.")
    }
    if(is.null(objx$width) || is.null(objx$height)) {
      stop("plotC object must contain numeric width and height (in inches).")
    }
    
    filename_base <- if(!is.null(objx$filename_base)) objx$filename_base else "plot"
    imagename <- paste(filename_base, ".", unclass(Sys.time()), ".png", sep="")
    
    # Save plot using standard defaults (only width/height vary)
    grDevices::png(filename = imagename, bg="transparent",
                   width = objx$width, height = objx$height, units = "in",
                   type = getPNGtype(), pointsize = 10, res = 96, antialias = "default")
    on.exit(grDevices::dev.off(), add = TRUE)
    
    objx$draw()
    
    # Embed using existing "image" behavior
    class(imagename) <- "image"
    nested <- printC(imagename, file = outputfile)
    tables_appended <- tables_appended || isTRUE(nested$tables_appended)
    images_appended <- add_images(images_appended, nested$images_appended)
  }
  else if(isa(objx, "knitr_kable"))
  {
    cat("<div class=\"rcpa4-table-wrap\">\n", file = outputfile, append = TRUE)
    write(knitr::kables(list(objx), format="html"), file=outputfile, append=TRUE)
    cat("</div><BR>\n", file = outputfile, append = TRUE)
    tables_appended <- TRUE
  }
  else if(isa(objx, "html.fragment"))
  {
    cat(as.character(objx)[1], "<BR>\n", file = outputfile, append = TRUE, sep = "")
  }
  else if(isa(objx, "statement"))
  {
    objx[1] <- paste("<strong>", objx[1], "</strong>", sep="")
    cat("<div class=\"rcpa4-statement\"><PRE>", 
        file = outputfile, append = TRUE)
    blob <- paste(objx)
    write(blob, file=outputfile, append=TRUE)
    cat("</PRE></div><BR>\n", file = outputfile, append = TRUE)
  }
  else if(isa(objx, "banner.heading"))
  {
    banner.color <- sample(grDevices::hcl.colors(50, palette="Pastel"), size=1)
    cat("<hr><div class=\"rcpa4-banner\" style=\"background-color:", banner.color, ";\"><h2>", objx[2], "</h2></div>\n\n", file = outputfile, append = TRUE)
    cat("<div class=\"rcpa4-banner-content\">", file = outputfile, append = TRUE)
  }
  else if(is.call(objx))
  {
    cat("</div><BR CLEAR=ALL>", file = outputfile, append = TRUE)
    objx <- utils::capture.output(objx)
    cat("<p class=\"rcpa4-notes\">----------------<BR><i>Notes:</i> Output generated ", format(Sys.time(), "%a %b %d %X %Y")," with R command RCPA4::", objx, ". </p>", file = outputfile, append = TRUE, sep="")
  }
  else if(isa(objx, "image"))
  {
    cat("<div class=\"rcpa4-image-float\">", file = outputfile, append = TRUE, sep="")
    cat("<A HREF=\"", objx, "\"><img src=\"", objx, "\" alt=\"R plot\" class=\"rcpa4-image-thumb\"><p>Click to enlarge</p></A></div>", file = outputfile, append = TRUE, sep="")
    image.label <- gsub("\\\\", "/", as.character(objx)[1])
    images_appended <- add_images(images_appended, image.label)
  }
  else if(isa(objx, "imageonly"))
  {
    cat("<div class=\"rcpa4-image-center\">", file = outputfile, append = TRUE, sep="")
    cat("<img src=\"", objx, "\" alt=\"R plot\" class=\"rcpa4-image-full\">", file = outputfile, append = TRUE, sep="")
    cat("</div>", file = outputfile, append = TRUE, sep="")
    image.label <- gsub("\\\\", "/", as.character(objx)[1])
    images_appended <- add_images(images_appended, image.label)
  }
  else if(is.character(objx))
  {
    objx[1] <- paste("<strong>", objx[1], "</strong>", sep="")
    cat("<div class=\"rcpa4-output-box\">\n<PRE>", 
        file = outputfile, append = TRUE)
    blob <- paste(objx)
    write(blob, file=outputfile, append=TRUE)
    cat("</PRE></div><BR>\n", file = outputfile, append = TRUE)
  }
  else 
  {
    objx <- utils::capture.output(objx)
    cat("<div class=\"rcpa4-output-box\">\n<PRE>", 
        file = outputfile, append = TRUE)
    blob <- paste(objx)
    write(blob, file=outputfile, append=TRUE)
    cat("</PRE></div><BR>\n", file = outputfile, append = TRUE)
  }
  
  invisible(list(
    tables_appended = tables_appended,
    images_appended = images_appended,
    outputfile = outputfile,
    location = output.dir
  ))
}


