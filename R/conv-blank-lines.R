#' Convert blank lines in Rmd code to have WYSIWYG newlines
#' (WYSIWYG = What You See Is What You Get)
#'
#' @description
#' Convert (leading) blank lines in Rmd code to WYSIWYG newlines.
#' The provided chunk will start with a blank line and possibly more or return
#' `NULL` as the converted chunk, and the whole `chunk` as the rest. The series
#' of blank lines will be converted into a mini-chunk, which will be returned
#' as the first element of a two-element list, the second element is the rest
#' of the Rmd.
#'
#' @details
#' This function should only be used for Rmd code where there are leading blank
#' lines. The other functions ([conv_list_lines()], [conv_header_lines()],
#' [conv_type_1_table_lines()], [conv_type_2_table_lines()], and
#' [conv_paragraph_lines()] deal with all the blank lines that follow those
#' pieces of rmarkdown code.
#'
#' @param chunk A vector of character strings representing lines for RMD code
#'
#' @return A list of two elements, 1) The corrected part of the chunk and
#' 2) the rest of the chunk starting with the line after the last blank line
#' @export
conv_blank_lines <- function(chunk){

  if(is.null(chunk)){
    return(list(NULL, NULL))
  }

  if(length(chunk) == 1 && chunk[1] == ""){
    return(list(c("", "\\\\ \\\\", ""), NULL))
  }

  if(chunk[1] != ""){
    return(list(NULL, chunk))
  }

  blank_count <- 0
  i <- 1
  repeat{
    curr_is_blank <- chunk[i] == ""
    blank_count <- `if`(curr_is_blank, blank_count + 1, blank_count)
    if(!curr_is_blank){
      i <- i - 1
      break
    }
    if(i == length(chunk)){
      break
    }
    i <- i + 1
  }
  new_chunk <- rmd_nlines(blank_count)
  if(i == length(chunk)){
    list(new_chunk, NULL)
  }else{
    list(new_chunk, chunk[(i + 1):length(chunk)])
  }
}
