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

  # `text_pat` matches any sequence of zero or more whitespace characters,
  # followed by 1 or more non-whitespace characters, followed by zero or
  # more whitespace characters all repeating
  # `dash_pat` matches any sequence of zero or more whitespace characters,
  # followed by 1 or more dashes, followed by zero or more whitespace
  # characters all repeating
  text_pat <- "^(\\s*\\S+\\s*)+$"
  dash_pat <- "^(\\s*-+\\s*)+$"
  t1 <- trimws(chunk[1])
  t2 <- trimws(chunk[2])
  t3 <- trimws(chunk[3])

  # Type 1 OR type 2
  is_type_1 <- length(grep(dash_pat, t1)) &&
               length(grep(text_pat, t2)) &&
               length(grep(dash_pat, t3))

  is_type_2 <- length(grep(text_pat, t1)) &&
               length(grep(dash_pat, t2)) &&
               length(grep(text_pat, t3))

  if(!is_type_1 && !is_type_2){
    return(list(NULL, chunk))
  }

  # A type 1 table must be at least 5 lines and a type 2 table must be at
  # least three lines
  if(is_type_1 && length(chunk) < 5){
    return(list(NULL, chunk))
  }
  if(is_type_2 && length(chunk) < 3){
    return(list(NULL, chunk))
  }

  end_tbl <- FALSE
  end_tbl_ind <- NULL
  if(is_type_1){
    new_chunk <- chunk[1:3]
    i <- 4
    repeat{
      tn <- trimws(chunk[i])
      end_tbl <- length(grep(dash_pat, tn))
      if(end_tbl){
        end_tbl_ind <- i
        # Remove previous row's extra blank line while adding ending row
        # of dashes
        new_chunk <- c(new_chunk[-length(new_chunk)], chunk[i])
        break
      }

      if(chunk[i] != ""){
        new_chunk <- c(new_chunk, chunk[i], "")
      }
      if(i == length(chunk)){
        break
      }
      i <- i + 1
    }
    if(!end_tbl && i == length(chunk)){
      stop("A table is missing the final row of dashes:\n\n",
           paste(chunk, collapse = "\n"),
           "\n\n",
           call. = FALSE)
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
  # Add correct newlines for the type orf table and return if at the end of
  # the chunk.
  if(i == length(chunk)){
    new_chunk <- c(new_chunk, "\\\\", "")
    return(list(new_chunk, NULL))
  }

  # Not at end of chunk, need to search for labels
  # Store the index where the table ends
  end_tbl_ind <- i
  # Find start of label if it exists and if table has a caption label
  # (Table: Caption here)
  i <- i + 1
  has_label <- FALSE
  start_label_ind <- NULL
  # `lbl_def_pat` matches any sequence of zero or more whitespace characters,
  # followed by 1 or more dashes, followed by zero or more whitespace characters,
  # preceded by "Table:" and stands for 'Label defined'
  # `lbl_undef_pat` matches any sequence of zero or more whitespace characters,
  # preceded by "Table:" and stands for 'Label undefined '
  lbl_def_pat <- "^Table:(\\s*\\S+\\s*)+$"
  lbl_undef_pat <- "^Table:\\s*$"
  repeat{
    if(length(grep(lbl_def_pat , chunk[i]))){
      has_label <- TRUE
      start_label_ind <- i
      break
    }
    if(length(grep(lbl_undef_pat , chunk[i])) &&
       length(grep(text_pat, chunk[i + 1]))){
      has_label <- TRUE
      start_label_ind <- i
      break
    }
    # If a non-caption text line is found, stop
    if(length(grep(text_pat, chunk[i]))){
      break
    }
    if(i == length(chunk)){
      break
    }
    i <- i + 1
  }

  if(has_label){
    # Add in lines between the end of table and the beginning of label
    # to new_chunk
    if(end_tbl_ind + 1 < start_label_ind - 1){
      new_chunk <- c(new_chunk, chunk[(end_tbl_ind + 1):(start_label_ind - 1)])
    }
  }else{
    # Add lines after the table, before the next text to new_chunk and
    # return that as the table part and 'the rest' of the chunk
    if(i == length(chunk)){
      new_chunk <- c(new_chunk, chunk[(end_tbl_ind + 1):length(chunk)])
      return(list(new_chunk, NULL))
    }else{
      new_chunk <- c(new_chunk, "")
      chunk <- chunk[(end_tbl_ind + 1):length(chunk)]
      # Used to make the number of newlines in the PDF doc match exactly what is
      # in the input code
      if(chunk[1] == ""){
        chunk <- chunk[-1]
      }
      return(list(new_chunk, chunk))
    }
  }

  # Table has a label at the end of the chunk
  if(start_label_ind == length(chunk)){
    new_chunk <- c(new_chunk, chunk[i], "")
    return(list(new_chunk, NULL))
  }
  # Table has a label, find the end of the label
  i <- start_label_ind + 1
  repeat{
    if(chunk[i] == ""){
      end_label_ind <- i - 1
      break
    }
    if(i == length(chunk)){
      end_label_ind <- i
      break
    }
    i <- i + 1
  }

  new_chunk <- c(new_chunk, chunk[start_label_ind:end_label_ind])
  new_chunk <- c(new_chunk, "")
  chunk <- chunk[(end_label_ind + 1):length(chunk)]
  # Used to make the number of newlines in the PDF doc match exactly what is
  # in the input code
  if(chunk[1] == ""){
    chunk <- chunk[-1]
  }
  return(list(new_chunk, chunk))
}