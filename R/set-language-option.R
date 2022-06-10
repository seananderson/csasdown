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

  trim_rmd <- trimws(readLines(fn))
  pat <- "^french:\\s*(true|false)\\s*$"
  french_ind <- grep(pat, trim_rmd)

  if(!length(trim_rmd)){
    warning("No 'french:' entry was found in the file ", fn,
            ". Setting options(french) to FALSE")
    options(french = FALSE)
  }
  if(length(french_ind) > 1){
    warning("More than one 'french:' entry was found in the file ", fn,
            ". Setting options(french) to FALSE")
    options(french = FALSE)
  }
  val <- gsub(pat, "\\1", trim_rmd[french_ind])
  if(val == "true"){
    options(french = TRUE)
  }else if(val == "false"){
    options(french = FALSE)
  }else{
    stop("Problem with regular expression in `set_language_option()`,\n",
         "could not extract true/false for 'french:'",
         call. = FALSE)
  }
}