#' Sorts dataset observations by user-defined criteria to return case-level information
#'
#' @description Returns case-level information in order specified by user. You can sort by additional criteria to break ties. Useful for learning about units of analysis and selecting cases for qualitative research designs.
#' @param id A variable in the dataset (data) that identfies individual cases, typically the name of states, countries, etc.
#' @param by Variable the cases should be sorted by.
#' @param data (Optional) Dataset to be sorted.
#' @param thenby (Optional) Criteria for sorting cases after sorting with the "by" variable. Useful if many cases tied on first criteria.
#' @param descending (Optional) Should the cases be sorted in descending order?  By default, set to TRUE. When sorting ordered factors, check that the levels higher numerically correspond to the sort order you have in mind. 
#' @param limit (Optional) The number of rows to report. If there are many observations to be sorted, you may want to limit output to 5, 10, etc. rows. 
#' @param digits (Optional) Number of decimal places reported in result (default: 3).
#' @param confirm (Optional) If function is going to return long table of results (more than 20 rows), you'll be asked for confirmation (use `confirm=F` to bypass).
#' @param printC (Optional) Do you want to print table of sorted observations to to .html file in working directory? (default: FALSE)
#' @param table (Optional) Display the sorted-observations table? Defaults to
#'   `TRUE`. Set to `FALSE` to skip the table, its heading, confirmation prompt,
#'   and HTML export, while still returning the results invisibly.
#' @return Invisibly returns a data frame of sorted observations, limited to
#'   `limit` rows when supplied, including when `table = FALSE`. Assign the
#'   function call to an object to store the results.
#' @examples 
#'    library(RCPA4)
#'    
#'    # basic usage
#'    sortC(id=states$state, by=states$volunteer_hrs_pc, limit=10, confirm=FALSE)
#'    
#'    # store sorted results without displaying the table
#'    sorted <- sortC(id=states$state, by=states$volunteer_hrs_pc, table=FALSE)
#'    
#' \donttest{
#'    # options to limit results and sort in ascending order
#'    sortC(id=country, by=gini_index, descending=FALSE, limit=10, data=world)
#'    
#'    # sort by and thenby 
#'    sortC(id=country, by=vdem_4cat, thenby=gini_index, descending=c(FALSE, FALSE), 
#'          data=world, confirm=FALSE)
#'    sortC(id=country, by=vdem_4cat, thenby=gini_index, descending=c(FALSE, TRUE), 
#'          data=world, confirm=FALSE)
#'  }
#' @export
#' @template console-output
#' @template result-tables
#' @section Returning results without display:
#' With `table = FALSE`, the table and its heading are omitted, the long-table
#' confirmation prompt is skipped, and no HTML output is written, even if
#' `printC = TRUE`. The `limit` argument still controls the number of rows
#' returned. With `table = TRUE`, use `confirm = FALSE` to bypass confirmation
#' for a long table. Informational messages remain controlled separately by
#' `suppressMessages()`.
#' @section RCPA4 Package Tutorial Videos:
#' * [Sort and View Case-Level Information with the sortC Function](https://www.youtube.com/watch?v=xdnaFE3kWeU) 12:22   
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapters 2, 6. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Sage Publications, 2020), pp. 122-123. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Descriptive Statistics](https://www.poliscidata.com/pages/rDemosResources.php?chapter=2), compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications.  
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @importFrom Hmisc all.is.numeric
#' @md
sortC <- function(id, by, data=NULL, thenby=NULL, descending=TRUE, limit=NULL, digits=3, confirm=TRUE, printC=FALSE, table=TRUE)
{
  assert_required(id, by)
  assert_nonnegative_integer(digits)
  # ---- Collect printC blocks (only used if printC==TRUE) ----
  blocks <- list()
  
  thenby_expr <- substitute(thenby)
  has_thenby <- !missing(thenby) && !identical(thenby_expr, quote(NULL))
  
  var_exprs <- list(id = substitute(id), by = substitute(by))
  single_vars <- c("id", "by")
  if (has_thenby) {
    var_exprs$thenby <- thenby_expr
    single_vars <- c(single_vars, "thenby")
  }
  
  ctx <- setup_vars_input(
    var_exprs = var_exprs,
    w_expr = NULL,
    data_expr = substitute(data),
    env = parent.frame(),
    data_missing = missing(data),
    drop_complete = FALSE,
    single_vars = single_vars,
    ref_var = "id"
  )
  
  id <- ctx$vars$id
  by <- ctx$vars$by
  id.name <- ctx$meta$id$name
  by.name <- ctx$meta$by$name
  if(has_thenby) {
    thenby <- ctx$vars$thenby
    thenby.name <- ctx$meta$thenby$name
  }
  
  if(id.name=="states" | id.name=="gss" | id.name=="anes" | id.name=="world")
  {
    message(wrap_console_text("It looks like you're using old sortC syntax. The order of arguments has changed.\nTry example(sortC) or help(sortC) for assistance with this function."))
  }
  
  if(has_thenby) 
  {
    if(length(descending)==1) descending <- c(descending, TRUE)    
  }
  
  if(has_thenby) thisorder <- order(by, thenby, na.last=NA, decreasing=descending)
  if(!has_thenby) thisorder <- order(by, na.last=NA, decreasing=descending)
  
  if(!has_thenby) 
  {
    obj1 <- data.frame(id, by)
    colnames(obj1) <- c(id.name, by.name)      
  }
  if(has_thenby) 
  {
    obj1 <- data.frame(id, by, thenby)
    colnames(obj1) <- c(id.name, by.name, thenby.name)      
  }
  
  obj1sort <- data.frame(obj1[thisorder, ])
  rownames(obj1sort) <- NULL
  
  if(table!=FALSE) {
    blocks <- render_heading(blocks, "Sorted Case-Level Information", marker="=", printC=printC)
  }
  
  if(missing(limit)) limit <- nrow(obj1sort)
  assert_positive_integer(limit)
  
  if(table!=FALSE && confirm != FALSE)
  {
    if(limit > 20) 
    {
      select.limit.prompt <- paste(
        "The sorted table has", limit, "rows. Do you want to see them all?\n",
        "If you want to (further) limit the number of rows, enter a number now, enter q to cancel, or any other key to continue.",
        sep=""
      )
      newlimit <- ask(select.limit.prompt)
      if(Hmisc::all.is.numeric(newlimit)) limit <- as.numeric(newlimit)
      if(newlimit=="q" | newlimit=="Q") stop("You have stopped the getC function.")
    }
  }
  
  if(!has_thenby)
  {
    if(descending==TRUE) order.type <- "descending" else order.type <- "ascending"
    table.caption <- paste(id.name, "values sorted by", order.type, "values of", by.name)
  }
  if(has_thenby)
  {
    table.caption <- paste(id.name, "values sorted by")
    if(descending[1]==TRUE) by.type <- "descending" else by.type <- "ascending"
    table.caption <- paste(table.caption, by.type, "values of", by.name)
    if(descending[2]==TRUE) thenby.type <- "descending" else thenby.type <- "ascending"
    table.caption <- paste(table.caption, "and then by", thenby.type, "values of", thenby.name)
  }
  
  # Console table output, HTML table output (for printC)
  if(table!=FALSE) {
    blocks <- render_table(blocks, x=obj1sort[1:limit,], caption=table.caption, digits=digits, printC=printC)
  }
  
  
  if(table!=FALSE && isTRUE(printC)) {
    blocks <- add_block(blocks, match.call(expand.dots = FALSE))
    print_blocksC(blocks)
  }
  
  invisible(obj1sort[1:limit,])
}



