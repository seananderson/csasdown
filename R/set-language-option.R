#' Parse the file which has YAML option 'french' in it and set option 'french'
#' accordingly in the current workspace
#'
#' @details
#' If the YAML option is not found or there is some other issue, the 'french'
#' option will be set to `FALSE`
#'
#' @param fn The bookdown index filename, typically index.Rmd. This file
#' must have a YAML option called 'french' set to either 'true' or 'false'
#'
#' @return Nothing
#' @export
set_language_option <- function(fn = "index.Rmd"){

  if(!file.exists(fn)){
    stop("File '", fn, "' does not exist",
         call. = FALSE)
  }
  rmd <- readLines(fn)
  ind <- grep("french:", rmd)
  if(!length(rmd)){
    warning("No 'french:' entry was found in the file ", fn,
            ". Setting options(french) to FALSE")
  }
  if(length(ind) > 1){
    warning("More than one 'french:' entry was foun in the file ", fn,
            ". Setting options(french) to FALSE")
  }
  val <- gsub(".*french:\\s*(true|false)", "\\1", rmd[ind])
  if(val == "true"){
    options(french = TRUE)
  }else if(val == "false"){
    options(french = FALSE)
  }else{
    warning("YAML option 'french' found but is something other than 'true' ",
            "or 'false'. Setting options(french) to FALSE")
    options(french = FALSE)
  }
}