#' Convert list lines in Rmd code to have WYSIWYG newlines
#' (WYSIWYG = What You See Is What You Get)
#'
#' @description
#' Convert list lines in Rmd code to WYSIWYG newlines. The provided chunk
#' will start with a list line and possibly more or return NULL as the
#' converted chunk, and the whole `chunk` as `the_rest`. The series of list
#' lines will be converted into a mini-chunk, which will be returned
#' as the first element of a two-element list, the second element is the
#' rest of the Rmd.
#'
#' @param chunk The Rmd chunk to process
#'
#' @return A list of two elements, 1) The corrected part of the chunk and
#' 2) the rest of the chunk starting with the line after the last list line
#' @export
#'
#' @examples
#' library(csasdown)
#' chunk <- c("- Item 1", "- Item 2", "", "1. Item 1", "  a. Item 1a")
#' tmp <- conv_list_lines(chunk)
#' list_line_chunk <- tmp[[1]]
#' the_rest <- tmp[[2]]
conv_list_lines <- function(chunk){

  if(is.null(chunk)){
    return(list(NULL, NULL))
  }

  is_lst_line <- substr(trimws(chunk[1]), 2, 2) == "." ||
                 substr(trimws(chunk[1]), 1, 1) == "-"
  if(!is_lst_line){
    return(list(NULL, chunk))
  }

  if(length(chunk) == 1){
    return(list(c(chunk[1], "\\\\"), NULL))
  }

  new_chunk <- chunk[1]
  is_lst <- TRUE
  i <- 1
  while(is_lst && i < length(chunk)){
    i <- i + 1
    is_lst <- substr(trimws(chunk[i]), 2, 2) == "." ||
              substr(trimws(chunk[i]), 1, 1) == "-"
    if(is_lst){
      new_chunk <- c(new_chunk, chunk[i])
    }
  }

  new_chunk <- c(new_chunk, "\\\\")
  if(i == length(chunk)){
    new_chunk <- c(new_chunk, "\\\\")
    the_rest <- NULL
  }else{
    the_rest <- chunk[i:length(chunk)]
  }

  list(new_chunk, the_rest)
}