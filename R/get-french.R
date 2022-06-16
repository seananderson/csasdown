#' Get French YAML tag value in the index.Rmd file ('true' or 'false')
#'
#' @param fn The name of the YAML file, typically 'index.Rmd' for bookdown
#'
#' @return `TRUE` if the value is 'true' or `FALSE` if the value is 'false'
#' @export
get_french <- function(fn = "index.Rmd"){

  if(!file.exists(fn)){
    stop("The file '", fn, "' does not exist")
  }
  rmd <- readLines(fn)
  trim_rmd <- trimws(rmd)

  french_pat <- "^\\s*french:\\s*(\\S+)\\s*$"
  french_ind <- grep(french_pat, trim_rmd)
  if(!length(french_ind)){
    stop("`french:` YAML tag of incorrect format or not found in file '",
         fn, "'\n",
         call. = FALSE)
  }
  if(length(french_ind) > 1){
    stop("`french:` YAML tag has more than one entry in file '", fn, "'\n",
         call. = FALSE)
  }

  val <- gsub(french_pat, "\\1", rmd[french_ind])
  if(val == "true"){
    return(TRUE)
  }else if(val == "false"){
    return(FALSE)
  }else{
    stop("The value found was not 'true' or 'false', it was '", val, "'",
         call. = FALSE.)
  }
}