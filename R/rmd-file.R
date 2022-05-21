#' Read in am Rmd file and return it
#'
#' @description
#' Read in am Rmd file and return it verbatim, with a newline added at the end
#'
#' @details
#' This allows the author to write pure Rmarkdown in a file, and have it read
#' into the [cat()] command in a csasdown resdoc Rmd file's code chunk. See
#' 'resdoc-bilingual' type example doc ([csasdown::draft("resdoc-bilingual")])
#'
#' @param fn The Rmd filename. An extension is not needed
#'
#' @return Verbatim text representing the file
#' @export
read_rmd_file <- function(fn){

  if(!length(grep("\\.Rmd$", fn))){
    fn <- paste0(fn, ".Rmd")
  }
  if(!file.exists(fn)){
    stop("File ", fn, " does not exist",
         call. = FALSE)
  }
  lines <- readLines(fn)
  lines[1] <- paste0("cat(\"", lines[1])
  if(lines[length(lines)] == ""){
    lines[length(lines) + 1] <- "\")"
  }else{
    lines[length(lines)] <- paste0(lines[length(lines)], "\")")
  }

  lines
}