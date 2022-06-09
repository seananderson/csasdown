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
#' @param chunk A vector of character strings representing lines for RMD code
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

  if(!is_rmd_list_line(chunk[1])){
    return(list(NULL, chunk))
  }

  if(length(chunk) == 1){
    return(list(chunk[1], NULL))
  }

  new_chunk <- NULL
  curr_is_lst <- TRUE
  i <- 1
  repeat{
    curr_is_lst <- is_rmd_list_line(chunk[i])
    if(!curr_is_lst){
      i <- i - 1
      break
    }
    new_chunk <- c(new_chunk, chunk[i])
    if(i == length(chunk)){
      break
    }
    i <- i + 1
  }
  # removal_len <- length(rmd_nlines(0))
  # start_remove_ind <- length(new_chunk) - removal_len + 1
  # end_removal_ind <- length(new_chunk)
  # # Remove last added space chars (could be langer than one element)
  # new_chunk <- new_chunk[-(start_remove_ind:end_removal_ind)]

  if(i == length(chunk)){
    list(new_chunk, NULL)
  }else{
    list(new_chunk, chunk[(i + 1):length(chunk)])
  }
}

