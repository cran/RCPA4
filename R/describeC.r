#' Generates table of descriptive statistics for one or more variables in a dataset 
#'
#' @description Generates a table of descriptive statistics for variable(s) specified with x argument, displaying it by default. Works with variables measures at any level but output varies with level of measurement (e.g. you won't get standard deviation for a nominal variable). Character values are analyzed as categorical factors; the function does not convert variables to numeric or ordered classes, but provides class-specific guidance after reporting results. Option for weighting observations.
#' @param x A variable or list of variables, should be in dataset$var form unless dataset specified in optional data argument.
#' @param w (Optional) Sampling weights of variable, must be numeric; should be in dataset$weightvar form unless dataset specified in optional data argument.
#' @param data (Optional) Name of dataset that contains x (and w) variable.
#' @param digits (Optional) Number of decimal places reported in result (defaults to 3).
#' @param printC (Optional) Do you want to print table of descriptive statistics to .html file in working directory? (default: FALSE)
#' @param table (Optional) Display the descriptive-statistics table? Defaults to
#'   `TRUE`. Set to `FALSE` to skip the table, its heading, and HTML export,
#'   while still returning the results invisibly.
#' @return Invisibly returns a table of descriptive statistics, including when
#'   `table = FALSE`. Assign the function call to an object to store the results.
#' @examples  
#'   library(RCPA4)
#'   
#'   # descriptive statistics for qualitative variables
#'   describeC(x=world$region)
#'   
#'   # store results without displaying the table
#'   statistics <- describeC(x=world$infant_mortality, table=FALSE)
#'   
#'   \donttest{
#'   # descriptive statistics for numeric variable
#'   describeC(x=world$infant_mortality)
#'   
#'   # describe multiple variables in list, with optional w argument
#'   describeC(x=list(anes$feel_angry, anes$appr_cong_hndl_its), w=anes$wt)
#'   }
#' @export
#' @template console-output
#' @template result-tables
#' @section Returning results without display:
#' With `table = FALSE`, the table and its heading are omitted and no HTML
#' output is written, even if `printC = TRUE`. Class-specific informational
#' messages can still occur; use `suppressMessages()` to suppress those too.
#' @section RCPA4 Package Tutorial Videos:
#' * [Generate Descriptive Statistics with RCPA4 Package's describeC Function](https://www.youtube.com/watch?v=QbMlUgwG1H4) 17:40  
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapter 2. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Sage Publications, 2020), pp. 39-55. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [Tutorials & Resources for Descriptive Statistics](https://www.poliscidata.com/pages/rDemosResources.php?chapter=2), Compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom Hmisc all.is.numeric wtd.quantile wtd.var
#' @importFrom stats setNames
#' @md
describeC <- function(x, w=NULL, data=NULL, digits=3, printC=FALSE, table=TRUE)
{
  assert_required(x)
  assert_nonnegative_integer(digits)
  
  w_expr <- if (missing(w)) NULL else substitute(w)
  ctx <- setup_vars_input(
    var_exprs = list(x = substitute(x)),
    w_expr = w_expr,
    data_expr = substitute(data),
    env = parent.frame(),
    data_missing = missing(data),
    drop_complete = FALSE,
    single_vars = character(0),
    ref_var = "x",
    strings_as_factors = TRUE
  )
  variable.set <- ctx$vars$x
  variable.names <- colnames(variable.set)
  w.full <- ctx$vars$w
  w.name <- ctx$meta$w$name
  weighted <- ctx$meta$weighted
  w.rawsum <- ctx$meta$w_sum
  
  # set-up descriptors and raw result container
  descriptives.names <- c(
    "Observed values", "Missing values", "Unique values", "Class",
    "Mean", "Median", "Mode",
    "Variance", "Standard deviation", "Minimum", "Maximum", "Range",
    "First quartile (25%)", "Third quartile (75%)", "Interquartile range",
    "Skewness", "Kurtosis"
  )
  descriptives.raw <- setNames(
    lapply(variable.names, function(xn) setNames(vector("list", length(descriptives.names)), descriptives.names)),
    variable.names
  )
  class_messages <- character(0)
  
  for(i in 1:length(variable.names))
  {
    x.name <- variable.names[i]
    x <- variable.set[, x.name]
    keep <- stats::complete.cases(x, w.full)
    x <- x[keep]
    w <- w.full[keep]
    x.nonmissing <- sum(w)
    x.missing <- w.rawsum - x.nonmissing
    
    numeric_like <- Hmisc::all.is.numeric(x, extras=c(NA)) && !is.numeric(x)
    if(numeric_like) {
      if(is.factor(x)) {
        class_messages <- c(
          class_messages,
          paste0(
            "Note: ", x.name,
            " has numeric-looking factor levels and was analyzed as ",
            if(is.ordered(x)) "an ordered factor" else "an unordered factor", ". ",
            "If its labels represent numeric values, create a numeric copy from the labels with as.numeric(as.character(",
            x.name, ")) before calling describeC()."
          )
        )
      } else {
        class_messages <- c(
          class_messages,
          paste0(
            "Note: ", x.name,
            " has numeric-looking values but was analyzed as ",
            paste(class(x), collapse=", "),
            ". If it represents interval-level values, create a numeric copy with as.numeric(",
            x.name, ") before calling describeC()."
          )
        )
      }
    }
    if(is.factor(x) && !is.ordered(x) && !numeric_like) {
      class_messages <- c(
        class_messages,
        paste0(
          "Note: ", x.name,
          " was analyzed as an unordered factor. If its categories have a meaningful order, ",
          "create an ordered version with ordered(", x.name,
          ", levels=...) before calling describeC()."
        )
      )
    }
    
    n.unique <- length(unique(x))
    if(n.unique == length(x)) {
      x.mode <- "None"
    } else {
      if(is.numeric(x)) x.mode <- as.numeric(wtd_mode(x=x, w=w))
      else x.mode <- wtd_mode(x=x, w=w)
    }
    
    x.median <- NULL
    if(is.numeric(x)) x.median <- as.numeric(wtd_median(x=x, w=w))
    if(is.ordered(x)) x.median <- wtd_median(x=x, w=w)
    
    if(is.numeric(x) && length(x) > 0)
    {
      x.mean <- wtd_mean(x=x, w=w)
      x.var  <- wtd_var(x=x, w=w)
      x.sd   <- sqrt(x.var)
      x.min  <- min(x)
      x.max  <- max(x)
      x.range <- (x.max - x.min)
      quantiles <- wtd_quantile(x=x, w=w, q=c(25, 75))
      x.q1 <- unname(quantiles[1])
      x.q3 <- unname(quantiles[2])
      x.iqr <- x.q3 - x.q1
      x.skewness <- wtd_skewness(x, w, digits=NULL)
      x.kurtosis <- wtd_kurtosis(x, w, digits=NULL)
    }
    
    
    
    descriptives.raw[[x.name]][["Observed values"]] <- x.nonmissing
    descriptives.raw[[x.name]][["Missing values"]] <- x.missing
    descriptives.raw[[x.name]][["Unique values"]] <- n.unique
    descriptives.raw[[x.name]][["Class"]] <- paste(class(x), collapse=", ")
    
    if(is.numeric(x) && length(x) > 0) descriptives.raw[[x.name]][["Mean"]] <- x.mean
    if(is.numeric(x) || is.ordered(x)) descriptives.raw[[x.name]][["Median"]] <- x.median
    descriptives.raw[[x.name]][["Mode"]] <- x.mode
    
    if(is.numeric(x) && length(x) > 0)
    {
      descriptives.raw[[x.name]][["Variance"]] <- x.var
      descriptives.raw[[x.name]][["Standard deviation"]] <- x.sd
      descriptives.raw[[x.name]][["Minimum"]] <- x.min
      descriptives.raw[[x.name]][["Maximum"]] <- x.max
      descriptives.raw[[x.name]][["Range"]] <- x.range
      descriptives.raw[[x.name]][["First quartile (25%)"]] <- x.q1
      descriptives.raw[[x.name]][["Third quartile (75%)"]] <- x.q3
      descriptives.raw[[x.name]][["Interquartile range"]] <- x.iqr
      descriptives.raw[[x.name]][["Skewness"]] <- x.skewness
      descriptives.raw[[x.name]][["Kurtosis"]] <- x.kurtosis
    }
    
  }
  
  format_cell <- function(value, digits) {
    if (is.null(value) || length(value) == 0L) return("")
    if (length(value) == 1L && is.na(value)) return("")
    if (is.numeric(value)) {
      return(paste(
        format(
          round(value, digits),
          scientific = FALSE,
          trim = TRUE,
          nsmall = digits
        ),
        collapse = ", "
      ))
    }
    paste(as.character(value), collapse=", ")
  }
  
  raw_cell <- function(value) {
    if (is.null(value) || length(value) == 0L) return("")
    if (length(value) == 1L && is.na(value)) return("")
    if (is.numeric(value)) return(paste(as.character(value), collapse=", "))
    paste(as.character(value), collapse=", ")
  }
  
  row_has_values <- vapply(descriptives.names, function(rn) {
    any(vapply(variable.names, function(vn) {
      val <- descriptives.raw[[vn]][[rn]]
      !(is.null(val) || (length(val) == 1L && is.na(val)))
    }, logical(1)))
  }, logical(1))
  printrows <- descriptives.names[row_has_values]
  
  descriptives.display <- matrix("", nrow=length(printrows), ncol=length(variable.names))
  descriptives.return <- matrix("", nrow=length(printrows), ncol=length(variable.names))
  rownames(descriptives.display) <- printrows
  colnames(descriptives.display) <- variable.names
  rownames(descriptives.return) <- printrows
  colnames(descriptives.return) <- variable.names
  
  for (vn in variable.names) {
    for (rn in printrows) {
      raw_value <- descriptives.raw[[vn]][[rn]]
      display_digits <- if (rn == "Unique values" || (!weighted && rn %in% c("Observed values", "Missing values"))) 0 else digits
      descriptives.display[rn, vn] <- format_cell(raw_value, digits = display_digits)
      descriptives.return[rn, vn] <- raw_cell(raw_value)
    }
  }
  
  descriptives.df <- as.data.frame(descriptives.display, check.names = FALSE, stringsAsFactors = FALSE)
  descriptives.return.df <- as.data.frame(descriptives.return, check.names = FALSE, stringsAsFactors = FALSE)
  attr(descriptives.return.df, "raw_values") <- descriptives.raw
  class(descriptives.return.df) <- c("describeC_table", class(descriptives.return.df))
  
  # ---- collect printC blocks --------------------------------------------
  blocks <- list()
  
  if(table!=FALSE) {
    blocks <- render_heading(blocks, "Descriptive Statistics", marker="=", printC=printC)
  }
  
  variable.names.string <- paste(variable.names, collapse=", ")
  table_caption <- paste("Descriptive Statistics for", variable.names.string)
  if(weighted) table_caption <- paste(table_caption, ", weighted by ", w.name, sep="")
  
  if(table!=FALSE) {
    blocks <- render_table(blocks, x=descriptives.df, caption=table_caption, digits=digits, printC=printC)
    slightpause()
  }
  
  if(length(class_messages) > 0L) message(paste(class_messages, collapse="\n"))
  
  if(table!=FALSE && isTRUE(printC)) {
    blocks <- add_block(blocks, match.call(expand.dots = FALSE))
    print_blocksC(blocks)
  }
  
  invisible(descriptives.return.df)
}


#' @keywords internal
#' @noRd
#' @export
`[.describeC_table` <- function(x, i, j, ..., drop = TRUE) {
  use_scalar_cell <- !missing(i) && !missing(j) && length(i) == 1L && length(j) == 1L
  
  if (use_scalar_cell) {
    rn <- rownames(x)
    cn <- colnames(x)
    row_name <- NULL
    col_name <- NULL
    
    if (is.character(i) && length(i) == 1L && !is.na(i) && (i %in% rn)) {
      row_name <- i
    } else if (is.numeric(i) && length(i) == 1L && !is.na(i)) {
      ii <- as.integer(i)
      if (ii >= 1L && ii <= length(rn)) row_name <- rn[ii]
    }
    
    if (is.character(j) && length(j) == 1L && !is.na(j) && (j %in% cn)) {
      col_name <- j
    } else if (is.numeric(j) && length(j) == 1L && !is.na(j)) {
      jj <- as.integer(j)
      if (jj >= 1L && jj <= length(cn)) col_name <- cn[jj]
    }
    
    raw_vals <- attr(x, "raw_values", exact = TRUE)
    if (!is.null(raw_vals) && !is.null(row_name) && !is.null(col_name)) {
      cell <- raw_vals[[col_name]][[row_name]]
      if (is.null(cell) || (length(cell) == 1L && is.na(cell))) return("")
      return(cell)
    }
  }
  
  NextMethod("[", drop = drop)
}


