#' Welcomes users to RCPA4 and confirms that the package is available
#'
#' @description Welcomes users to RCPA4 package for _An R Companion to Political Analysis, 4th Edition_ and provides basic information about using Companion functions and datasets.
#' @return No value returned
#' @examples  
#'   library(RCPA4)
#'   
#'   # Welcome message from RCPA4 package.
#'   \donttest{
#'   welcomeC()
#'   }
#' @export
#' @section RCPA4 Package Tutorial Videos:
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook Reference:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapter 1. 
#' @section Online Resources:
#' * [R Tutorials & Resources for Getting Started with R](https://www.poliscidata.com/pages/rDemosResources.php?chapter='I'), Compiled by Barry C. Edwards 
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' @md


welcomeC <- function()
  {

    slowConsolePrint("The RCPA4 package bundles datasets and functions featured in ")
    slowConsolePrint("An R Companion to Political Analysis, 4th Edition, ")
    slowConsolePrint("by Philip H. Pollock III and Barry C. Edwards.\n\n")
    slightpause()

    slowConsolePrint(paste("Your current working directory is:", getwd(),"\n"))
    slowConsolePrint("Use the setwd() function to change your working directory.\n\n")
    slightpause()
    slowConsolePrint("This package contains five datasets (anes, debate, gss, states, and world) and many functions.\n")
    slowConsolePrint("To see a list of all objects in this package, enter ls(\"package:RCPA4\")\n\n")
    
    slowConsolePrint("Use helpC() to explore RCPA4 functions, datasets, and package tools. ")
    slowConsolePrint("For help with this function, or any other R function, type ? followed by the function's name, or help(function_name)\n\n")
    
      # slowConsolePrint("These are the functions and objects in the poliscidata package:\n")
      # slightpause()
      # packagecontents <- utils::capture.output(print(ls("package:poliscidata")))
      # for(i in 1:length(packagecontents)) quickConsolePrint(paste(packagecontents[i], "\n"))
      # slightpause()
      
    slowConsolePrint("If you want to play Widget Factory, just type widgetFactory() then press enter.\n\n")
    message("We hope you enjoy using the RCPA4 package!\n")
    slightpause()
    
}

