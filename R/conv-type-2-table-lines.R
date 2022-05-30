#' Convert an Rmarkdown type 2 table into a simplified form with WYSIWYG
#' newlines
#'
#' @description
#' Convert an Rmarkdown type 2 table into a simplified form with WYSIWYG
#' newlines. If there is no table detected, NULL will be returned as the
#' converted chunk, or an error will be thrown depending on the situation.
#'
#' @details
#' A type 2 table is defined as a manually-entered Rmarkdown table with a
#' minimum of three lines with this format:
#' #  a     b     c
#' #----- ----- -----
#' # abc   def   ghi
#'
#' There is no whitespace in between any of these lines. The first text row is
#' the column headers for the table and the second text row is the table data.
#' The second row can be multiple rows which must not be separated by blank
#' lines. They all must be together without whitespace in between.
#' Here is an example with three table data rows in acceptable format, along
#' with the optional table caption text which must start with 'Table:'
#' #  a     b     c
#' #----- ----- -----
#' # abc   def   ghi
#' # jkl   mno   pqr
#' # stu   vwx   yza
#'
#' #Table: Table caption (0 or more blank lines between table and this caption)
#' #A second line of table caption here (no blank lines in between)
#'
#' @param chunk A vector of character strings, representing the Rmarkdown chunk
#' to process. Each element is a line of Rmarkdown text
#'
#' @return A list of two elements, which are the modified type 1 table and the
#' rest of the chunk starting with the line after the end of the table
#' @export
#'
#' @examples
#' library(csasdown)
#' chunk <- c("  Parameter   Value", "---------- -----------",
#'            "     x          1.0", "     y          2.2")
#' tmp <- conv_type_2_table_lines(chunk)
#' the_rest <- tmp[[2]]
conv_type_2_table_lines <- function(chunk){

  if(is.null(chunk)){
    return(list(NULL, NULL))
  }

  if(length(chunk) < 3){
    stop("A type 2 table must have at least 3 lines. Input table is:\n\n",
         paste(chunk, collapse = "\n"),
         "\n\n",
         call. = FALSE)
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

  # Confirm type 1 table
  is_type_2 <- length(grep(text_pat, t1)) &&
               length(grep(dash_pat, t2)) &&
               length(grep(text_pat, t3))

  if(!is_type_2){
    stop("The following table is not a type 2 table based on the first three ",
         "rows:\n\n", paste(chunk, collapse = "\n"),
         "\n\n",
         "They must start with:\n",
         "- a row of text representing headers\n",
         "- a row of dashes\n",
         "- a row of text representing row 1 of table data\n",
         call. = FALSE)
  }

  start_tbl_ind <- 1
  end_tbl_ind <- NULL
  start_lbl_ind <- NULL
  end_lbl_ind <- NULL
  # Add the first three rows as they have been checked already
  tbl_chunk <- chunk[1:3]
  end_tbl <- FALSE
  if(length(chunk) == 3){
    return(list(c(chunk, ""), NULL))
  }
  i <- 4
  repeat{
    tn <- trimws(chunk[i])
    if(i == 4 && chunk[i] == ""){
      end_tbl <- TRUE
      end_tbl_ind <- 3
      break
    }
    if(i == length(chunk)){
      tbl_chunk <- c(tbl_chunk, chunk[i])
      end_tbl <- TRUE
      end_tbl_ind <- i
      break
    }
    end_tbl <- chunk[i + 1] == ""
    if(end_tbl){
      tbl_chunk <- c(tbl_chunk, chunk[i])
      end_tbl_ind <- i
      break
    }else{
      tbl_chunk <- c(tbl_chunk, chunk[i])
    }
    i <- i + 1
  }

  if(end_tbl){
    # Basic table without a table caption string included
    if(end_tbl_ind == length(chunk)){
      ret_chunk <- NULL
      tbl_chunk <- c(tbl_chunk, "")
      return(list(tbl_chunk, ret_chunk))
    }
  }else{
    stop("A table appears to have been started but not finished:\n\n",
         paste(chunk, collapse = "\n"),
         "\n\n",
         call. = FALSE)
  }
  tbl_chunk <- c(tbl_chunk, "")

  # ---------------------------------------------------------------------------
  # At this point, the end of the table has been found and its index is
  # `end_tbl_ind`

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
    # If the caption def looks like this:
    # Table: A caption is here.
    # More caption here.
    if(length(grep(lbl_def_pat , trimws(chunk[i])))){
      n_lead_spaces <- nchar(gsub("^(\\s*).*$", "\\1", chunk[i]))
      if(n_lead_spaces > 3){
        # Rmarkdown specs say a table caption line must be indented 3 or less
        # spaces. If more, it is just a regular text line
        warning("A line that looks like a table caption was found but it is ",
                "indented ", n_lead_spaces, " spaces. The Rmarkdown ",
                "specification says it must be 3 or less:\n\n",
                chunk[i],
                "\n\n",
                call. = FALSE)
        return(list(tbl_chunk, chunk[(end_tbl_ind + 1):length(chunk)]))
      }
      has_label <- TRUE
      start_label_ind <- i
      while(length(grep(text_pat , trimws(chunk[i])))){
        tbl_chunk <- c(tbl_chunk, chunk[i])
        end_lbl_ind <- i
        if(i == length(chunk)){
          break
        }
        i <-  i + 1
      }
      break
    }
    # If the caption def looks like this:
    # Table:
    # A caption is here.
    # More caption here.
    if(length(grep(lbl_undef_pat , trimws(chunk[i]))) &&
       length(grep(text_pat, trimws(chunk[i + 1])))){
      n_lead_spaces <- nchar(gsub("^(\\s*).*$", "\\1", chunk[i]))
      if(n_lead_spaces > 3){
        # Rmarkdown specs say a table caption line must be indented 3 or less
        # spaces. If more, it is just a regular text line
        warning("A line that looks like a table caption was found but it is ",
                "indented ", n_lead_spaces, " spaces. The Rmarkdown ",
                "specification says it must be 3 or less:\n\n",
                chunk[i],
                "\n\n",
                call. = FALSE)
        return(list(tbl_chunk, chunk[(end_tbl_ind + 1):length(chunk)]))
      }
      has_label <- TRUE
      start_label_ind <- i
      tbl_chunk <- c(tbl_chunk, chunk[i])
      i <- i + 1
      while(length(grep(text_pat , trimws(chunk[i])) && i < length(chunk))){
        tbl_chunk <- c(tbl_chunk, chunk[i])
        end_lbl_ind <- i
        if(i == length(chunk)){
          break
        }
        i <-  i + 1
      }
      break
    }
    if(i == length(chunk)){
      break
    }
    i <- i + 1
  }

  if(has_label){
    tbl_chunk <- c(tbl_chunk, "")
    if(end_lbl_ind == length(chunk)){
      ret_chunk <- NULL
    }else{
      ret_chunk <- chunk[(end_lbl_ind + 1):length(chunk)]
      if(ret_chunk[1] == ""){
        ret_chunk <- ret_chunk[-1]
      }
      return(list(tbl_chunk, ret_chunk))
    }
  }else{
    if(end_tbl_ind == length(chunk)){
      ret_chunk <- NULL
    }else{
      ret_chunk <- chunk[(end_tbl_ind + 1):length(chunk)]
    }
  }

  return(list(tbl_chunk, ret_chunk))
}