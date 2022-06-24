#' Get French YAML tag value in the index.Rmd file ('true' or 'false')
#'
#' @param fn The name of the YAML file, typically 'index.Rmd' for bookdown
#'
#' @return `TRUE` if the value is 'true' or `FALSE` if the value is 'false'
#' @export
get_french <- function(fn = "index.Rmd"){

  if(!file.exists(fn)){
    bail("The file '", fn, "' does not exist")
  }
  rmd <- readLines(fn)
  trim_rmd <- trimws(rmd)

  french_pat <- "^\\s*french:\\s*(\\S+)\\s*$"
  french_ind <- grep(french_pat, trim_rmd)
  if(!length(french_ind)){
    bail("`french:` YAML tag of incorrect format or not found in file '",
         fn, "'\n")
  }
  if(length(french_ind) > 1){
    bail("`french:` YAML tag has more than one entry in file '", fn, "'\n")
  }

  val <- gsub(french_pat, "\\1", rmd[french_ind])
  if(val == "true"){
    return(TRUE)
  }else if(val == "false"){
    return(FALSE)
  }else{
    bail("The value found was not 'true' or 'false', it was '", val, "'")
  }
}