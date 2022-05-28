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
    new_chunk <- chunk[1:3]
    i <- 3
    while(!end_tbl && i < length(chunk)){
      i <- i + 1
      end_tbl <- substr(trimws(chunk[i]), 1, 5) == "-----"
      if(end_tbl){
        new_chunk[length(new_chunk)] <- chunk[i]
      }else if(length(grep(pat, trimws(chunk[i])))){
        new_chunk <- c(new_chunk, chunk[i], "")
      }
    }
    if(!end_tbl){
      # Table had no ending row of dashes, so it isn't a table
      return(list(NULL, chunk))
    }
  }else if(is_type_2){
    new_chunk <- chunk[1:3]
    i <- 3
    if(length(chunk) == 3){
      new_chunk <- c(new_chunk, "\\\\", "")
      return(list(new_chunk, NULL))
    }
    end_tbl_ind <- 3

    i <- i + 1
    while(chunk[i] != "" && i <= length(chunk)){
      new_chunk <- c(new_chunk, chunk[i])
      i <- i + 1
    }
    i <- i - 1

    if(i == length(chunk)){
      new_chunk <- c(new_chunk, "\\\\", "")
      return(list(new_chunk, NULL))
    }
  }

  # ---------------------------------------------------------------------------
  # At this point, the end of the table has been found and i is it's index.
  # - For a type 1 table, this is on the last "--------" line, which could be
  # at the end of the chunk.
  # - For a type 2 table, this is on the last text line which could be
  # at the end of the chunk.
  # Remove the end-of chunk possibilities here, leaving further parsing for
  # labels for after
  if(i == length(chunk)){
    new_chunk <- c(new_chunk, "\\\\", "")
    return(list(new_chunk, NULL))
  }

  # Store the index where the table ends
  end_of_tbl <- i
  # Find start of label if it exists
  i <- i + 1
  while(!length(grep("^Table:.*$", chunk[i])) &&
        i < length(chunk)){
    i <- i + 1
  }

  if(i == length(chunk)){
    # Check to see if last element is a label
    if(length(grep("^Table:\\s*.+$", chunk[i]))){
      if(is_type_1){
        new_chunk <- c(new_chunk, chunk[i], "\\\\", "")
      }else if(is_type_2){
        new_chunk <- c(new_chunk, "", chunk[i], "\\\\", "")
      }
      return(list(new_chunk, NULL))
    }
    # No label was found
    new_chunk <- c(new_chunk, "\\\\", "")
    return(list(new_chunk, chunk[(end_of_tbl + 1):length(chunk)]))
  }

  start_of_label <- i
  # -- Check for number of spaces before (0-3) should be here --
  i <- i + 1
  if(length(grep("^Table: *[a-zA-Z0-9]+", chunk[start_of_label])) && chunk[i] == ""){
    # The label is only Table: with no content and the following line has no
    # content, so there is no label
    return(list(new_chunk, chunk[(end_of_tbl + 1):length(chunk)]))
  }

  # There is a label and there is a caption, read in lines until the caption
  # is read in fully
  i <- start_of_label
  while(chunk[i] != "" && i < length(chunk)){
    #new_chunk <- c(new_chunk, chunk[i])
    i <- i + 1
  }
  end_of_label <- ifelse(chunk[i] == "", i - 1, i)

  if(is_type_1){
    new_chunk <- c(new_chunk, chunk[start_of_label:end_of_label])
  }else if(is_type_2){
    new_chunk <- c(new_chunk, "", chunk[start_of_label:end_of_label])
  }

  new_chunk <- c(new_chunk, "\\\\", "")
  if(i == length(chunk)){
    return(list(new_chunk, NULL))
  }else{
    return(list(new_chunk, chunk[i:length(chunk)]))
  }
}