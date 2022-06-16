#' Set French in the index.Rmd file to 'true' or 'false'
#'
#' @param fn The name of the YAML file, typically 'index.Rmd' for bookdown
#' @param val `TRUE` or `FALSE`
#'
#' @return Nothing, write the file `fn`
#' @export
set_french <- function(fn = "index.Rmd", val = TRUE){

  if(!val %in% c(TRUE, FALSE)){
    stop("`val` must be `TRUE` or `FALSE`")
  }

  if(!file.exists(fn)){
    stop("The file '", fn, "' does not exist")
  }
  rmd <- readLines(fn)
  trim_rmd <- trimws(rmd)

  french_ind <- grep("^french:\\s*(false|true)\\s*$", trim_rmd)
  if(!length(french_ind)){
    stop("`french:` YAML tag not found in file '", fn, "'\n",
         call. = FALSE)
  }
  if(length(french_ind) > 1){
    stop("`french:` YAML tag has more than one entry in file '", fn, "'\n",
         call. = FALSE)
  }

  leading_spaces <- gsub("^(\\s*)french:\\s(true|false)\\s*$", "\\1", rmd[french_ind])
  all_spaces <- grep("^\\s*$", leading_spaces)
  if(!length(all_spaces)){
    leading_spaces <- ""
  }
  french_val <- ifelse(val == TRUE, "true", "false")
  rmd[french_ind] <- paste0(leading_spaces, "french: ", french_val)
  writeLines(rmd, fn)
  invisible()
}