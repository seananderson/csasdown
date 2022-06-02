#' Convert blank lines in Rmd code to have WYSIWYG newlines
#' (WYSIWYG = What You See Is What You Get)
#'
#' @description
#' Convert blank lines in Rmd code to WYSIWYG newlines. The provided chunk
#' will start with a blank line and possibly more or return NULL as the
#' converted chunk, and the whole `chunk` as the rest. The series of blank
#' lines will be converted into a mini-chunk, which will be returned
#' as the first element of a two-element list, the second element is the
#' rest of the Rmd.
#'
#' @param chunk The Rmd chunk to process
#'
#' @return A list of two elements, 1) The corrected part of the chunk and
#' 2) the rest of the chunk starting with the line after the last blank line
#' @export
#'
#' @examples
#' library(csasdown)
#' chunk <- c("", "", "", "Some text..", "More Rmd text")
#' tmp <- conv_blank_lines(chunk)
#' blank_line_chunk <- tmp[[1]]
#' the_rest <- tmp[[2]]
conv_blank_lines <- function(chunk){

  if(is.null(chunk)){
    return(list(NULL, NULL))
  }

  if(chunk[1] != ""){
    return(list(NULL, chunk))
  }

  if(length(chunk) == 1){
    return(list(c("\\\\", ""), NULL))
  }

  next_is_blank <- chunk[2] == ""
  if(!next_is_blank){
    next_is_lst_line <- substr(trimws(chunk[2]), 2, 3) == ". " ||
                        substr(trimws(chunk[2]), 1, 2) == "* " ||
                        substr(trimws(chunk[2]), 1, 2) == "+ " ||
                        substr(trimws(chunk[2]), 1, 2) == "- "
    is_header_line <- grepl("^#+", chunk[2])
    if(!next_is_lst_line && !is_header_line){
      return(list("\\\\", chunk[2:length(chunk)]))
    }
  }
  i <- 1
  new_chunk <- "\\\\"
  while(next_is_blank && i < length(chunk)){
    i <- i + 1
    new_chunk <- c(new_chunk, "\\\\")
    next_is_blank <- chunk[i + 1] == ""
  }
  new_chunk <- c(new_chunk, "")

  if(i == length(chunk)){
    the_rest <- NULL
  }else{
    the_rest <- chunk[(i + 1):length(chunk)]
  }
  list(new_chunk, the_rest)
}
