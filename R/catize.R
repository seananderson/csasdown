#' Convert a string including inline knitr code chunks to a cat-like string
#'
#' @description
#' Formats the supplied string so that inline knitr code chunks are replaced
#' with the format used in commands such as [cat()] and [paste()] which is a
#' series of comma-separated strings and R code. Use this function to format
#' strings from Rmarkdown to something that can be copy/pasted into a [cat()]
#' command inside a knitr chunk.
#'
#' @details
#' This is used primarily to convert sections of code that have been written
#' in Rmarkdown into simple strings with R code embedded between quoted,
#' comma-separated strings that can be passed to [cat()]. Here is a simple
#' example of the conversion this function does.
#'
#' Original rmarkdown string:
#'
#' "The date is `` `r as.character(Sys.Date())` `` today. You are on
#' a `` `r Sys.info()["sysname"]` `` machine."
#'
#' Modified cat-like string (can be passed to [cat()]):
#'
#' "The date is ", as.character(Sys.Date()), " today. You are on a ",
#' Sys.info()["sysname"], " machine."
#'
#' @param str The string containing inline knitr-style R code
#' (backtick-enclosed)
#' @param verbose Shows two vectors extracted from the input text, which are
#' used to build the new string:
#' 1) the text chunks between the R code chunks
#' 2) the R code chunks between the text chunks
#'
#' @return A non-quoted (see [noquote()]) string which can be
#' enclosed with double-quotes and copy/pasted into a [cat()]
#' or [paste()] command
#'
#' @importFrom stringr str_split str_extract_all
#' @export
catize <- function(str, verbose = FALSE){

  # `pattern` is the official knitr regexp, see $md section,
  # ..$ inline.code: chr "`r[ #]([^`]+)\\s*`" line here:
  # https://rdrr.io/cran/knitr/man/knit_patterns.html
  pattern <- "`r[ #][^`]+\\s*`"
  txt <- str_split(str, pattern)[[1]]
  code <- str_extract_all(str, pattern)[[1]]
  if(verbose){
    message("txt:")
    print(txt)
    message("code:")
    print(code)
    cat("\n")
  }
  if(!length(code)){
    return(str)
  }
  if(length(txt) != length(code) + 1){
    stop("The string did not split up correctly when splitting on ",
         "backtick-surrounded inline R code",
         call. = FALSE)
  }
  # Extract R code(s)
  code <- gsub("^`r ", "", code)
  code <- gsub("`$", "", code)
  if(length(txt) == 2 && txt[1] == "" && txt[2] == ""){
    return(paste('", ', code, ',"'))
  }
  # Paste the string back together
  out <- map2(txt[-length(txt)], code, ~{
    paste0(.x, '", ', .y, ', "')
  })
  out <- map_chr(out, ~{.x})
  out <- paste(out, collapse = "")
  out <- paste0(out, txt[length(txt)])
  noquote(out)
}
