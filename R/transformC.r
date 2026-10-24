#' Returns new variables by transforming existing dataset variables (e.g. dummy variables, standardized variables, rank orders) 
#'
#' @description Given a variable x, the `transformC` function generates and returns a tranformed version of x. For example, `transformC` can take a variable x and return standardized x, or the log of x.
#' @param type The type of transformation to be made to x. Options include:
#' * "center" 
#' * "cut" use `cutpoints` or `groups` arguments to control cuts
#' * "dummy" use response argument to identify values of x which should be coded 1 (all other non-missing responses will be coded 0)
#' * "dummy.set" 
#' * "ln"
#' * "log10" 
#' * "percent.rank" 
#' * "rank" 
#' * "whole" 
#' * "z" 
#' @param x The variable to be transformed, a variable that already exists, should be in dataset$var form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x variable.
#' @param response (Optional) For `type="dummy"`, response is the value or vector of values to be coded 1. 
#' @param cutpoints (Optional) For `type="cut"`, a vector of values to serve as lower bounds of ranked categories for transformed x variable.
#' @param groups (Optional) For `type="cut"`, the number of (approximately) same sized groups to create based on x values.
#' @param confirm (Optional) By default, `transformC` will ask you to confirm you want transformed variable returned (to prevent data loss). Set `confirm=FALSE` to bypass this check.
#' @param ... (Optional) Additional arguments pass to \code{\link[Hmisc]{cut2}} (for `type="cut"`).
#' @return A transformed version of x variable, a vector with the same length as x, unless `type="dummy.set"` in which case transformC returns a data.frame.
#' @examples 
#'   library(RCPA4)
#'   
#'   # don't use confirm=FALSE until you've tested the function call
#'   transformC("percent.rank", anes$ft_dem_party, confirm=FALSE)
#'   transformC("rank", anes$ft_dem_party, confirm=FALSE)
#'   transformC("whole", runif(min=0,max=100,n=20), confirm=FALSE)
#' @export
#' @template console-output
#' @section Confirmation output:
#' Use `confirm = FALSE` to return the transformed variable without the
#' confirmation prompt or transformation-results heading. Informational
#' messages about the transformation are controlled separately by
#' `suppressMessages()`.
#' @section RCPA4 Package Tutorial Videos:
#' * [Transforming Variables with RCPA4 Package's transformC Function](https://www.youtube.com/watch?v=kYyfvzonFCs) 22:40
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapter 3. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Sage Publications, 2020), pp. 55-64. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Creating and Transforming Variables](https://www.poliscidata.com/pages/rDemosResources.php?chapter=3), Compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications.  
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom Hmisc all.is.numeric cut2
#' @md


transformC = function(type, x, data=NULL, response=NULL, cutpoints=NULL, groups=NULL, confirm=TRUE, ...)
{
  assert_required(type, x)

  set_transform_label <- function(object, label) {
    attr(object, "label") <- label
    class(object) <- setdiff(class(object), "labelled")
    object
  }
  
  ctx <- setup_vars_input(
    var_exprs = list(x = substitute(x)),
    w_expr = NULL,
    data_expr = substitute(data),
    env = parent.frame(),
    data_missing = missing(data),
    drop_complete = FALSE,
    single_vars = "x",
    ref_var = "x"
  )
  x <- ctx$vars$x
  x.name <- ctx$meta$x$name
  

  
  if(type=="dummy")
  {
    # if no response value selected, user should select one
    if(is.null(response)) 
    {
      # cat("response is missing\n")
      response.options <- unique(na.omit(x))
      stop(
        "There's a problem: To create a dummy variable, you need to identify the values of ",
        x.name, " to be coded 1 using the response argument.\n",
        "Here are your options:\n",
        paste0("  - ", as.character(response.options), collapse="\n"),
        "\nPlease complete this function's response argument.",
        call.=FALSE
      )
    }
    # x can't have too many unique values
    suggest_categorical(x, max_unique = 20)
    # if response isn't one of the x values, user should select valid one
    # think about whether it's okay if any response is a value, instead of all of them
    # it would be better to list response option out in sep rows
    if(!(all(response %in% unique(na.omit(x)))))
    {
      response.options <- unique(na.omit(x))
      bad.response <- response[!(response %in% unique(na.omit(x)))]
      stop(
        "There's a problem: ", paste0('"', bad.response, '"', collapse=", "),
        " is not a value of ", x.name, ".\n",
        "These are the values of ", x.name, ":\n",
        paste0('  - "', as.character(response.options), '"', collapse="\n"),
        "\nPlease correct this function's response argument or choose another x variable.",
        call.=FALSE
      )
    }
    x.transformed <- as.numeric(x %in% response)
    x.transformed[is.na(x)] <- NA
    # x.transformed <- factor(x.transformed, levels=c(0,1))
    x.transformed <- set_transform_label(
      x.transformed,
      paste(x.name, " dummy variable (", paste(response, collapse=", "), " values of ", x.name," coded 1)", sep="")
    )
  }
  
  
  else if(type=="dummy.set")
  {
    # user doesn't select response, it's automatic
    suggest_categorical(x, max_unique = 20)
    # x can't have too many unique values
    response.options <- unique(na.omit(x))
    # return(response.options)
    dummy.set <- data.frame(matrix(nrow=length(x), ncol = length(response.options)))
    # return(dummy.set)
    for(j in 1:length(response.options))
    {
      x.transformed <- as.numeric(x == response.options[j])
      x.transformed[is.na(x)] <- NA
      # x.transformed <- factor(x.transformed, levels=c(0,1))
      # print(x.transformed)
      x.transformed <- set_transform_label(
        x.transformed,
        paste(x.name, " dummy variable (", response.options[j], " values of ", x.name," coded 1)", sep="")
      )
      dummy.set[,j] <- x.transformed
    }
    colnames(dummy.set) <- paste(x.name, ".", response.options, sep="")
    dummy.set <- set_transform_label(dummy.set, paste("Set of dummy variables for", x.name))
    x.transformed <- dummy.set
  }
  
  
  else if(type=="z")
  {
    assert_numeric(x)
    x.z <- scale(x, center=TRUE, scale=TRUE)
    if(Hmisc::all.is.numeric(x.z, extras=c(NA))) x.transformed = as.numeric(x.z)
    x.transformed <- set_transform_label(x.transformed, paste("Standardized", x.name))
  }
  else if(type=="center")
  {
    assert_numeric(x)
    x.centered <- scale(x, center=TRUE, scale=FALSE)
    if(Hmisc::all.is.numeric(x.centered, extras=c(NA))) x.transformed <- as.numeric(x.centered)
    x.transformed <- set_transform_label(x.transformed, paste("Mean centered", x.name))
  }
  
  else if(type=="cut")
  {
    if(is.null(groups) && is.null(cutpoints)) stop(wrap_console_text(paste(gettext("There's a problem: To make a cut-type transformation, you need to specify either the number of groups (groups argument) or identify the cutpoints (using the cutpoints argument).", domain = "R-descr"))))
    if(!is.null(groups) && !is.null(cutpoints)) stop(wrap_console_text(paste(gettext("There's a problem: To make a cut-type transformation, you need to specify either the number of groups or identify the cutpoints, but you can't use both groups and cutpoints.", domain = "R-descr"))))
    assert_numeric(x)
    # can you cut an ordered variable? no, cut2 only works on numerics
    # one method if groups is not missing
    if(!is.null(groups)) 
      {
        assert_positive_integer(groups)
        x.transformed <- Hmisc::cut2(x, g=groups, ...)
        x.transformed <- as.ordered(x.transformed)
        x.transformed <- set_transform_label(x.transformed, paste(x.name, "cut into", groups, "equal-sized groups"))
    }
        # another method if cutpoints is not missing
    if(!is.null(cutpoints)) 
      {
        if(max(cutpoints) > max(x, na.rm = T)) stop(wrap_console_text(paste("There's a problem: You've set a cutpoint that's greater than the maximum value of", x.name)))
        if(min(cutpoints) < min(x, na.rm = T)) stop(wrap_console_text(paste("There's a problem: You've set a cutpoint that's less than the minimum value of", x.name)))
        x.transformed <- Hmisc::cut2(x, cuts=cutpoints, ...)
        x.transformed <- as.ordered(x.transformed)
        x.transformed <- set_transform_label(x.transformed, paste(x.name, "values ordered using cutpoints", paste(cutpoints, collapse=", ")))
    }
  }
  else if(type=="rank")
  {
    # make sure x is numeric or ordered
    # this currently has issue with coding NA
    if(!is.numeric(x) && !is.ordered(x)) warning(wrap_console_text("Potential issue: When x is not a number or ordered factor, ranking may be alphabetical and not what you're expecting."))
    # default ties. method is average, user should be able to alter
    x.transformed <- rank(x, na.last="keep", ...)
    if(Hmisc::all.is.numeric(x.transformed, extras=c(NA))) x.transformed = as.numeric(x.transformed)
    x.transformed <- set_transform_label(x.transformed, paste("Ranked values of", x.name))
  }
  else if(type=="percent.rank")
  {
    # make sure x is numeric or ordered
    if(!is.numeric(x) && !is.ordered(x)) warning(wrap_console_text("Potential issue: When x is not a number or ordered factor, ranking may be alphabetical and not what you're expecting."))
    # default ties. method is average, user should be able to alter
    # x.transformed <- dplyr::percent_rank(x)*100
    x.ranks <- rank(x, na.last="keep", ...)
    nobs <- length(na.omit(x.ranks))
    x.transformed <- (x.ranks-1) / (nobs-1) * 100
    if(Hmisc::all.is.numeric(x.transformed, extras=c(NA))) x.transformed = as.numeric(x.transformed)
    x.transformed <- set_transform_label(x.transformed, paste("Percentage ranks of", x.name))
  }
  else if(type=="log10")
  {
    assert_numeric(x)
    if(any(x <= 0, na.rm=TRUE)) message(paste(x.name, "has values zero or less. These values don't have logs."))
    x.transformed <- log10(x)
    if(Hmisc::all.is.numeric(x.transformed, extras=c(NA))) x.transformed = as.numeric(x.transformed)
    x.transformed <- set_transform_label(x.transformed, paste("Log base 10 of", x.name))
  }
  else if(type=="ln")
  {
    assert_numeric(x)
    if(any(x <= 0, na.rm=TRUE)) message(paste(x.name, "has values zero or less. These values don't have logs."))
    x.transformed <- log(x)
    if(Hmisc::all.is.numeric(x.transformed, extras=c(NA))) x.transformed = as.numeric(x.transformed)
    x.transformed <- set_transform_label(x.transformed, paste("Natural log of", x.name))
  }
  else if(type=="whole")
  {
    assert_numeric(x)
    x.transformed <- round(x)
    if(Hmisc::all.is.numeric(x.transformed, extras=c(NA))) x.transformed = as.numeric(x.transformed)
    x.transformed <- set_transform_label(x.transformed, paste(x.name, "rounded to whole numbers"))
  }

  # generate report on the transformation.
  x.transformed.nonmissing <- sum(!is.na(x.transformed))
  x.transformed.missing    <- sum(is.na(x.transformed))

  if(confirm==FALSE) return(x.transformed)
  
  if(confirm==TRUE)
  {
    overwrite.warning <- paste("Please make sure you're not about to overwrite the", x.name, "variable and lose data. You can use confirm=F argument to bypass this message.\nDo you wish to continue? Enter y to continue or enter any other key to cancel.")
    if(tolower(ask(overwrite.warning))=="y")
    {
      # Heading (ALWAYS printed to console)
      heading <- paste("Variable Transformation Results:", x.transformed.nonmissing, "values generated,", x.transformed.missing, "missing values")
      blocks <- list()
      blocks <- render_heading(blocks, heading, marker="=", printC=FALSE)
    
      return(x.transformed)
    }
    else
    {
      message("Variable transformation cancelled.")
    }
  }
}


