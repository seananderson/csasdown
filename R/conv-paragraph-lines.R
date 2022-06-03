#' Convert paragraph lines in Rmd code to have WYSIWYG newlines
#' (WYSIWYG = What You See Is What You Get)
#'
#' @description
#' Convert paragraph lines in Rmd code to WYSIWYG newlines. The provided chunk
#' will start with regular text (or a R inline code chunk) and possibly more
#' lines. The series of paragraph lines will be converted into a mini-chunk,
#' which will be returned as the first element of a two-element list, the
#' second element is the rest of the Rmd.
#'
#' @param chunk A vector of character strings representing lines for RMD code
#'
#' @return A list of two elements, 1) The corrected part of the chunk and
#' 2) the rest of the chunk starting with the line after the last blank line
#' @export
conv_paragraph_lines <- function(chunk){

  if(is.null(chunk)){
    return(list(NULL, NULL))
  }

  if(length(chunk) == 1){
    return(list(chunk[1], NULL))
  }

  # If there is no blank line after the first line, must make sure the next
  # line is not the start of something else
  next_line <- chunk[2]
  next_is_table <- FALSE
  is_table_line <- FALSE
  if(length(chunk) >= 3){
    type <- is_rmarkdown_table_line(chunk[1:3])
    if(type == "type1" || type == "type2"){
      is_table_line <- TRUE
    }
  }
  next_is_header <- is_rmarkdown_header_line(next_line)
  next_is_lst_line <- is_rmarkdown_list_line(next_line)
  if(next_is_lst_line || next_is_header || next_is_table){
    new_chunk <- c(chunk[1], "\\\\", "")
    return(list(new_chunk, chunk[2:length(chunk)]))
  }

  next_is_blank <- FALSE
  new_chunk <- NULL
  i <- 1
  while(!next_is_blank && i < length(chunk)){
    new_chunk <- c(new_chunk, chunk[i])
    i <- i + 1
    next_is_blank <- chunk[i] == ""
  }

  new_chunk <- c(new_chunk, "\\\\")

  if(i == length(chunk)){
    the_rest <- NULL
  }else{
    the_rest <- chunk[i:length(chunk)]
  }

  list(new_chunk, the_rest)
}