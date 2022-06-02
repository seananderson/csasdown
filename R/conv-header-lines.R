#' Convert header lines in Rmd code to have WYSIWYG newlines
#' (WYSIWYG = What You See Is What You Get)
#'
#' @description
#' Convert header lines in Rmd code to WYSIWYG newlines. The provided chunk
#' will start with a header line and possibly more or return NULL as the
#' converted chunk, and the whole `chunk` as the rest. The series of header
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
#' chunk <- c("#Main", "##Introduction", "", "Some text..", "More Rmd text")
#' tmp <- conv_header_lines(chunk)
#' header_line_chunk <- tmp[[1]]
#' the_rest <- tmp[[2]]
conv_header_lines <- function(chunk){

  if(is.null(chunk)){
    return(list(NULL, NULL))
  }

  pat <- "^#+"
  if(!grepl(pat, chunk[1])){
    return(list(NULL, chunk))
  }

  if(length(chunk) == 1){
    return(list(chunk[1], NULL))
  }

  new_chunk <- chunk[1]
  is_header <- TRUE
  last_header_match <- 1
  i <- 1
  while(is_header && i < length(chunk)){
    i <- i + 1
    is_blank <- chunk[i] == ""
    while(is_blank && i < length(chunk)){
      i <- i + 1
      is_blank <- chunk[i] == ""
    }
    is_header <- grepl(pat, chunk[i])
    if(!is_header){
      break
    }
    last_header_match <- i
    new_chunk <- c(new_chunk, chunk[i])
  }

  if(last_header_match == length(chunk)){
    the_rest <- NULL
  }else{
    if(chunk[last_header_match + 1] == ""){
      if(last_header_match + 2 <= length(chunk)){
        the_rest <- chunk[(last_header_match + 2):length(chunk)]
      }else{
        the_rest <- chunk[(last_header_match + 1):length(chunk)]
      }
    }else{
      the_rest <- chunk[(last_header_match + 1):length(chunk)]

    }
  }

  list(new_chunk, the_rest)
}