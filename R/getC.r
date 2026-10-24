#' Imports common dataset file types into R environment
#' 
#' @description If you don't complete file argument, you will be prompted to select one. Supports common dataset file formats, including CSV, Stata, SPSS, native R, and SAS files. It previews imported data and asks you to confirm before returning a data frame. You must assign this returned object to a name to work with it. Native R files with multiple objects are returned as a named list. If `getC` doesn't support a file type, it suggests another function or package when possible.
#' @param file (Optional) Local path or URL of the file you want to load into your R session; if you do not specify a file, you will be prompted to select one.
#' @param confirm (Optional) Do you want to confirm getting file before function results returned? (default: TRUE)
#' @param ... (Optional) Additional arguments passed to loading function
#' @return Dataset specified in file argument as a data frame. Native R files containing multiple objects return a named list. You must assign the returned object to a name to work with it.
#' @details A `.sas7bdat` file contains the SAS dataset. If it has a companion `.sas7bcat` catalog file, that catalog may supply value labels. Catalogs commonly have the same file name as the data file but a `.sas7bcat` extension. Provide its path with `catalog_file`, for example `getC("survey.sas7bdat", catalog_file="survey.sas7bcat")`. Files compressed with `.gz`, `.bz2`, or `.xz` are supported when their underlying file extension is supported, such as `.csv.gz` or `.dta.gz`.
#' @examples 
#'    library(RCPA4)
#'    
#'    # basic call will prompt user to choose file
#'    if (interactive()) {
#'    getC()
#'    }
#' @export
#' @template console-output
#' @section Preview output:
#' Use `confirm = FALSE` to skip the data preview and confirmation prompt.
#' File-selection and other informational messages are controlled separately
#' by `suppressMessages()`. To import a specified file without headings,
#' preview output, or informational messages, set `RCPA4.verbose = FALSE` and
#' use `suppressMessages(getC(file, confirm = FALSE))`.
#' @section RCPA4 Package Tutorial Videos:
#' * [Importing Data with RCPA4 Package's getC Function](https://youtu.be/t3UhWDGqkSc), 18:14  
#' * [Complete Playlist of RCPA4 Package Tutorial Videos](https://www.youtube.com/playlist?list=PL3jY4WDTUxoNqrxSSQH4q7XPLPYipeNCu), includes video for this function and many more. 
#' @section Textbook References:
#' * Barry C. Edwards and Philip H. Pollock, _An R Companion to Political Analysis, 4th Edition_ (Political Science Data, Forthcoming 2026), Chapter 15. 
#' * Philip H. Pollock and Barry C. Edwards, _The Essentials of Political Analysis, 6th Edition_ (Sage Publications, 2020), pp. 321-327. ISBN-13: 978-1506379616; ISBN-10: 150637961.
#' @section Online Resources:
#' * [R Tutorials & Resources for Doing Your Own Political Analysis](https://www.poliscidata.com/pages/rDemosResources.php?chapter=15), Compiled by Barry C. Edwards
#' * [Sage Edge Resources for Political Analysis Series](https://edge.sagepub.com/pollock), for streaming videos, flashcards, and more student resources for textbooks by Pollock and Edwards, from Sage Publications. 
#' * [Political Science Data Web Site](https://www.poliscidata.com): Find datasets for your own research and resources to help with the analysis. 
#' @md


getC <- function(file=NULL, confirm=TRUE, ...)
{

  blocks <- render_heading(blocks=list(), "Importing Data into R Session", marker="=", printC=F)
  
 if(missing(file))
 {
   select.file.prompt <- paste("You did not specify the file to get. Would you like to choose it now?\nEnter y to continue or enter any other key to cancel.")
   if(tolower(ask(select.file.prompt))=="y")
   {
     file <- file.choose()
     cat("\n")
   }
   else stop("You have stopped the getC function.")
 }

  if(missing(file) || !is.character(file) || length(file)!=1L || is.na(file) || !nzchar(file)) {
    stop(
      wrap_console_text("Oops. Specify one nonempty file path using the file argument. To see how to use this function, try example(getC) or help(getC)."),
      call.=FALSE
    )
  }
  is_url <- grepl("^(https?|ftps?|file)://", file, ignore.case=TRUE)
  if(!is_url & (!file.exists(file) | dir.exists(file))) {
    stop(
      wrap_console_text(paste0("Oops. getC could not find a data file at '", file, "'. Check the path and file name.")),
      call.=FALSE
    )
  }
  
  file_location <- sub("[?#].*$", "", file)
  file.name <- basename(file_location)
  file.type <- tolower(tools::file_ext(file_location))
  if(file.type %in% c("gz", "bz2", "xz")) {
    file.type <- tolower(tools::file_ext(tools::file_path_sans_ext(file_location)))
  }
  
 message("File selected: ", file.name, "\nFile type:     ", file.type)
 
 verifyReturn <- function(dataframe, file.name)
   {
     is_import_list <- inherits(dataframe, "getC_import_list")
     if(is_import_list) {
       imported_list <- unclass(dataframe)
       if(confirm==FALSE) return(imported_list)
       message("Preview of this file as a named list:\n")
       print(names(imported_list))
       message("\nThis file contains ", length(imported_list), " objects")
       slightpause()
       message("\nBefore getC returns these objects, please verify this is the file you intended to import.\n")
       verify.prompt <- paste("Enter y to continue or enter any other key to cancel.")
       if(tolower(ask(verify.prompt))=="y") {
         message("File returned as a named list. Assign getC output to an object to save it.\n")
         return(imported_list)
       }
       else stop("You have stopped the getC function.")
     }

     dataframe <- data.frame(dataframe)
     if(confirm==FALSE) return(dataframe)
     message(paste("Preview of this file as a data frame:\n"))
     slightpause()
     colpreview <- min(ncol(dataframe), 3)
     print(utils::head(dataframe[, 1:colpreview]))
     message("\nThis dataset has ", nrow(dataframe), " rows and ", ncol(dataframe), " columns")
     # dataframe.name <- deparse(substitute(dataframe))
     slightpause()
     message("\nBefore getC returns this dataset, please verify you're not going to overwrite something.\n")
     verify.prompt <- paste("Enter y to continue or enter any other key to cancel.")
     if(tolower(ask(verify.prompt))=="y") 
     {
       message("File returned as R data frame object. Assign getC output to object to save it.\n")
       return(dataframe)
     }
     else stop("You have stopped the getC function.")
   }
 
 download_native_file <- function(file) {
   if(!is_url) return(file)
   destination <- tempfile(fileext=paste0(".", tools::file_ext(file_location)))
   status <- tryCatch(
     utils::download.file(file, destfile=destination, mode="wb", quiet=TRUE),
     error=function(error) 1L
   )
   if(status != 0L) {
     unlink(destination)
     stop(
       wrap_console_text(paste0("Oops. getC could not download the file at '", file, "'.")),
       call.=FALSE
     )
   }
   destination
 }

 read_rdata <- function(file, ...) {
   input <- download_native_file(file)
   if(is_url) on.exit(unlink(input), add=TRUE)
   import_env <- new.env(parent=emptyenv())
   object_names <- base::load(file=input, envir=import_env, ...)
   if(length(object_names) == 0L) {
     stop(
       "This native R file does not contain any objects.",
       call.=FALSE
     )
   }
   if(length(object_names) > 1L) {
     message(
       "This native R file contains ", length(object_names),
       " objects and will be returned as a named list:\n",
       paste0("  - ", object_names, collapse="\n")
     )
     return(structure(
       mget(object_names, envir=import_env, inherits=FALSE),
       class="getC_import_list"
     ))
   }
   get(object_names[[1]], envir=import_env, inherits=FALSE)
 }

 read_rds <- function(file, ...) {
   input <- download_native_file(file)
   if(is_url) on.exit(unlink(input), add=TRUE)
   base::readRDS(file=input, ...)
 }

 readers <- list(
   csv=function(file, ...) utils::read.csv(file=file, ...),
   csv2=function(file, ...) utils::read.csv2(file=file, ...),
   dif=function(file, ...) utils::read.DIF(file=file, ...),
   dta=function(file, ...) haven::read_dta(file=file, ...),
   por=function(file, ...) haven::read_por(file=file, ...),
   rda=read_rdata,
   rdat=read_rdata,
   rdata=read_rdata,
   rds=read_rds,
   sas7bdat=function(file, ...) haven::read_sas(data_file=file, ...),
   sav=function(file, ...) haven::read_sav(file=file, ...),
   tab=function(file, ...) utils::read.delim(file=file, ...),
   tsv=function(file, ...) utils::read.delim(file=file, ...),
   xpt=function(file, ...) haven::read_xpt(file=file, ...),
   xport=function(file, ...) haven::read_xpt(file=file, ...),
   zsav=function(file, ...) haven::read_sav(file=file, ...)
 )

 reader <- readers[[file.type]]
 if(!is.null(reader)) return(verifyReturn(reader(file=file, ...), file.name))
 

 unsupported_messages <- list(
   arff="Sorry, ARFF-format files are not supported by getC. Try foreign::read.arff().",
   dbf="Sorry, DBF-format files are not supported by getC. Try foreign::read.dbf().",
   epiinfo="Sorry, Epi Info files are not supported by getC. Try foreign::read.epiinfo().",
   rec="Sorry, Epi Info files are not supported by getC. Try foreign::read.epiinfo().",
   mat="Sorry, MATLAB-format files are not supported by getC. Try the R.matlab package.",
   mtp="Sorry, Minitab-format files are not supported by getC. Try foreign::read.mtp().",
   octave="Sorry, Octave-format files are not supported by getC. Try foreign::read.octave().",
   sas="Sorry, this SAS file type is not supported by getC. Use haven::read_sas() with a .sas7bdat data file; supply a companion .sas7bcat file through its catalog_file argument when needed.",
   sas7bcat="Sorry, this SAS file type is not supported by getC. Use haven::read_sas() with a .sas7bdat data file; supply a companion .sas7bcat file through its catalog_file argument when needed.",
   ssd="Sorry, legacy SAS .ssd files are not supported by getC. foreign::read.ssd() may help when SAS is installed and available to convert the file.",
   systat="Sorry, Systat-format files are not supported by getC. Try foreign::read.systat().",
   xls="Sorry, Excel-format files are not supported by getC. Save the worksheet as a .csv file and use getC(), or use readxl::read_excel().",
   xlsx="Sorry, Excel-format files are not supported by getC. Save the worksheet as a .csv file and use getC(), or use readxl::read_excel().",
   txt="Sorry, generic text and .dat files are not supported by getC because their delimiters and layout vary. Use utils::read.table() and specify options such as sep, header, and na.strings as needed.",
   dat="Sorry, generic text and .dat files are not supported by getC because their delimiters and layout vary. Use utils::read.table() and specify options such as sep, header, and na.strings as needed.",
   fwf="Sorry, fixed-width files are not supported by getC because their column widths must be specified. Use utils::read.fwf() and provide the widths argument."
 )
 unsupported_message <- unsupported_messages[[file.type]]
 if(!is.null(unsupported_message)) stop(
   wrap_console_text(unsupported_message),
   call.=FALSE
 )

 stop(
   wrap_console_text(paste0(
     "Sorry \"", file.type,
     "\" is not a recognized data file type. See help(getC) for supported file types."
   )),
   call.=FALSE
 )
  
}
