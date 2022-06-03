#' Convert an Rmarkdown type 1 table into a simplified form with WYSIWYG
#' newlines
#'
#' @description
#' Convert an Rmarkdown type 1 table into a simplified form with WYSIWYG
#' newlines. If there is no table detected, NULL will be returned as the
#' converted chunk, or an error will be thrown depending on the situation.
#'
#' @details
#' A type 1 table is defined as a manually-entered Rmarkdown table with a
#' minimum of five lines with this format:
#' #----- ----- -----
#' #  a     b     c
#' #----- ----- -----
#' # abc   def   ghi
#' #----- ----- -----
#'
#' There is no whitespace in between any of these lines. The first text row is
#' the column headers for the table and the second text row is the table data.
#' The second row can be multiple rows separated by an arbitrary number of
#' blank lines, but there cannot be blank lines before the table data or after.
#' Here is an example with three table data rows in acceptable format, along
#' with the optional table caption text which must start with 'Table:'
#' #----- ----- -----
#' #  a     b     c
#' #----- ----- -----
#' # abc   def   ghi
#'
#' # jkl   mno   pqr
#'
#'
#' # stu   vwx   yza
#' #----- ----- -----
#' #Table: Table caption (0 or more blank lines between table and this caption)
#' #A second line of table caption here (no blank lines in between)
#'
#' @param chunk A vector of character strings representing lines for RMD code
#'
#' @return A list of two elements, which are the modified type 1 table and the
#' rest of the chunk starting with the line after the end of the table
#' @export
#'
#' @examples
#' library(csasdown)
#' chunk <- c("---------- -----------", "  Parameter   Value",
#'            "---------- -----------", "     x          1.0",
#'            "     y          2.2", "---------- -----------")
#' tmp <- conv_type_1_table_lines(chunk)
#' the_rest <- tmp[[2]]
conv_type_1_table_lines <- function(chunk){

  if(is.null(chunk)){
    return(list(NULL, NULL))
  }

  if(length(chunk) < 5){
    stop("A type 1 table must have at least 5 lines. Input table is:\n\n",
         paste(chunk, collapse = "\n"),
         "\n\n",
         call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # `text_pat` matches any sequence of zero or more whitespace characters,
  #  followed by 1 or more non-whitespace characters, followed by zero or more
  #  whitespace characters
  # `dash_pat` matches any sequence of zero or more whitespace characters,
  #  followed by 1 or more dashes, followed by zero or more whitespace
  #  characters
  text_pat <- "^(\\s*\\S+\\s*)+$"
  dash_pat <- "^(\\s*-+\\s*)+$"
  is_table <- is_rmarkdown_table_line(chunk[1:3])
  is_type_1 <- is_table == "type1"
  if(!is_type_1){
    stop("The following table is not a type 1 table based on the first three ",
         "rows:\n\n", paste(chunk, collapse = "\n"),
         "\n\n",
         "They must start with:\n",
         "- a row of dashes\n",
         "- a row of text representing headers\n",
         "- a row of dashes.",
         call. = FALSE)
  }

  start_tbl_ind <- 1
  end_tbl_ind <- NULL
  start_lbl_ind <- NULL
  end_lbl_ind <- NULL
  # Add the first three rows as they have been checked already
  tbl_chunk <- chunk[1:3]
  i <- 4
  repeat{
    tn <- trimws(chunk[i])
    end_tbl <- length(grep(dash_pat, tn))
    if(end_tbl){
      end_tbl_ind <- i
      # Remove previous row's extra blank line while adding ending row
      # of dashes
      tbl_chunk <- c(tbl_chunk[-length(tbl_chunk)], chunk[i])
      break
    }

    if(chunk[i] != ""){
      tbl_chunk <- c(tbl_chunk, chunk[i], "")
    }
    if(i == length(chunk)){
      break
    }
    i <- i + 1
  }

  if(end_tbl){
    # Basic table without a table caption string included
    if(end_tbl_ind == length(chunk)){
      the_rest <- NULL
      tbl_chunk <- c(tbl_chunk, "\\\\", "")
      return(list(tbl_chunk, the_rest))
    }
  }else{
    stop("A table appears to have been started but not finished:\n\n",
         paste(chunk, collapse = "\n"),
         "\n\n",
         call. = FALSE)
  }

  # Add label if it exists
  lbl <- extract_rmarkdown_table_label(chunk[(end_tbl_ind + 1):length(chunk)])
browser()
  # Add the trailing whitespace
  #

  return(list(tbl_chunk, the_rest))
}