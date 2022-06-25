#' Get French YAML tag value in the index.Rmd file ('true' or 'false')
#'
#' @param fn The name of the YAML file, typically 'index.Rmd' for bookdown
#'
#' @return `TRUE` if the value is 'true' or `FALSE` if the value is 'false'
#' @export
get_french <- function(fn = "index.Rmd"){

  if(!file.exists(fn)){
    bail("File ", fn_color(fn), " does not exist")
  }
  rmd <- readLines(fn)
  trim_rmd <- trimws(rmd)

  french_pat <- "^\\s*french:\\s*(\\S+)\\s*$"
  french_ind <- grep(french_pat, trim_rmd)
  if(!length(french_ind)){
    bail(tag_color("french:"), " YAML tag of incorrect format or not ",
         "found in file ", fn_color(fn), "\n")
  }
  if(length(french_ind) > 1){
    bail(tag_color("french:"), " YAML tag has more than one entry ",
         "in file ", fn_color(fn), "\n")
  }

  val <- gsub(french_pat, "\\1", rmd[french_ind])
  if(val == "true"){
    return(TRUE)
  }else if(val == "false"){
    return(FALSE)
  }else{
    bail("The value found was not ", csas_color("true"), " or ",
         csas_color("false"), " it was ", csas_color(val))
  }
}