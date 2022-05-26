#' Convert table lines in Rmd code to have WYSIWYG newlines
#' (WYSIWYG = What You See Is What You Get)
#'
#' @description
#' Convert table lines in Rmd code to WYSIWYG newlines. There are two types
#' of manually-entered Rmarkdown table supported:
#' 1. The provided chunk will have a beginning and end row of dashes,
#'    and a row of dashes after the table header. All these are directly beside
#'    the text in a vertical sense. The table rows not directly beside the top
#'    and bottom lines must have a blank line between them
#' 2. The provided chunk will have a single dashed line, after the table header.
#'    This line must have text above and below directly, no newlines. the table
#'    rows must be right next to each other, no blank lines between them
#' If there is no table detected, NULL will be returned as the converted chunk,
#' and the whole `chunk` as `the_rest`. The series of list lines will be
#' converted into a mini-chunk, which will be returned as the first element of
#' a two-element list, the second element is the rest of the Rmd.
#'
#' @param chunk The Rmd chunk to process
#'
#' @return A list of two elements, 1) The corrected part of the chunk and
#' 2) the rest of the chunk starting with the line after the last list line
#' @export
#'
#' @examples
#' library(csasdown)
#' chunk <- c("---------- -----------", "  Parameter   Value,
#'            "---------- -----------", "     x          1.0",
#'            "---------- -----------", "     y          2.2")
#' tmp <- conv_table_lines(chunk)
#' table_line_chunk <- tmp[[1]]
#' the_rest <- tmp[[2]]
conv_table_lines <- function(chunk){

  if(is.null(chunk)){
    return(list(NULL, NULL))
  }

  # A type 2 table must be at least three lines
  if(length(chunk) < 3){
    return(list(NULL, chunk))
  }

  # Pattern just means "has some text"
  pat <- "[a-zA-Z0-9_\\-\\s]+"
  # Type 1 OR type 2
  is_type_1 <- substr(trimws(chunk[1]), 1, 5) == "-----" &&
               length(grep(pat, trimws(chunk[2]))) &&
               substr(trimws(chunk[3]), 1, 5) == "-----"

  is_type_2 <- length(grep(pat, trimws(chunk[1]))) &&
               substr(trimws(chunk[2]), 1, 5) == "-----" &&
               length(grep(pat, trimws(chunk[3])))

  if(!is_type_1 && !is_type_2){
    return(list(NULL, chunk))
  }

  end_tbl <- FALSE
  if(is_type_1){
    new_chunk <- c(chunk[1:3])
    i <- 4
    while(!end_tbl && i < length(chunk)){
      end_tbl <- substr(trimws(chunk[i]), 1, 5) == "-----"
      if(end_tbl){
        new_chunk <- c(new_chunk[-length(new_chunk)], chunk[i])
      }
      if(length(grep(pat, trimws(chunk[i])))){
          new_chunk <- c(new_chunk, chunk[i], "")
      }
      i <- i + 1
    }
    if(!end_tbl){
      # Table had no ending row of dashes, so it isn't a table
      return(list(NULL, chunk))
    }
  }else if(is_type_2){
    new_chunk <- c(chunk[1:3])
    i <- 3
    while(!end_tbl && i < length(chunk)){
      i <- i + 1
      new_chunk <- c(new_chunk, chunk[i])
      end_tbl <- chunk[i] == ""
    }
    if(!end_tbl){
      # Table had no ending row of dashes, so add a blank line to make it
      # have an end
      new_chunk <- c(new_chunk, "")
    }
  }

  # Check for optional Table label (line(s) of text followed by a blank line)
  # Same algorithm for both types of table
  end_of_tbl <- i
  tbl_has_label <- FALSE
  # Find start of label if it exists
  while(chunk[i] == "" && i < length(chunk) && !tbl_has_label){
    i <- i + 1
    if(length(grep("^Table:\\s+.+$", chunk[i]))){
      tbl_has_label <- TRUE
    }else if(i + 1 <= length(chunk) &&
             length(grep("^Table:\\s*$", chunk[i])) &&
             length(grep(pat, chunk[i + 1]))){
      tbl_has_label <- TRUE
    }
  }

  while(chunk[i] != "" && i <= length(chunk)){
    new_chunk <- c(new_chunk, chunk[i])
    i <- i + 1
  }

  if(i >= length(chunk)){
    return(list(new_chunk, NULL))
  }else{
    return(list(new_chunk, chunk[i:length(chunk)]))
  }

}