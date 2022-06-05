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
#' @param chunk A vector of character strings representing lines for RMD code
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

  # ---------------------------------------------------------------------------
  # `dash_pat` matches any sequence of zero or more whitespace characters,
  #  followed by 1 or more dashes, followed by zero or more whitespace
  #  characters
  dash_pat <- "^(\\s*-+\\s*)+$"
  is_table <- is_rmarkdown_table_line(chunk[1:3])
  is_type_2 <- is_table == "type2"
  if(!is_type_2){
    stop("The following table is not a type 2 table based on the first three ",
         "rows:\n\n", paste(chunk, collapse = "\n"),
         "\n\n",
         "They must start with:\n",
         "- a row of text representing headers\n",
         "- a row of dashes\n",
         "- a row of text representing a row of table data.",
         call. = FALSE)
  }

  start_tbl_ind <- 1
  end_tbl_ind <- NULL
  start_lbl_ind <- NULL
  end_lbl_ind <- NULL
  # Add the first three rows as they have been checked already
  tbl_chunk <- chunk[1:3]
  if(length(chunk) == 3){
    tbl_chunk <- c(chunk, "")
    return(list(tbl_chunk, NULL))
  }
  i <- 4
  repeat{
    tn <- trimws(chunk[i])
    if(chunk[i] == "" ||
       is_rmarkdown_list_line(chunk[i]) ||
       is_rmarkdown_header_line(chunk[i]) ||
       grepl("^Table:.*$", tn)){
      end_tbl_ind <- i - 1
      break
    }
    tbl_chunk <- c(tbl_chunk, chunk[i])
    if(i == length(chunk)){
      tbl_chunk <- c(tbl_chunk, chunk[i], "")
      return(list(tbl_chunk, NULL))
    }
    i <- i + 1
  }

  # Add label if it exists
  lbl <- extract_rmd_table_label(chunk[(end_tbl_ind + 1):length(chunk)])
  tbl_chunk <- c(tbl_chunk, lbl[[1]])
  post_chunk <- lbl[[2]]

  # Add the post-table trailing whitespace
  if(is.null(post_chunk)){
    tbl_chunk <- c(tbl_chunk, "")
    return(list(tbl_chunk, NULL))
  }

  if(post_chunk[1] != ""){
    tbl_chunk <- c(tbl_chunk, "")
    return(list(tbl_chunk, post_chunk))
  }
  start_blank_ind <- 1
  end_blank_ind <- 1
  i <- 1
  repeat{
    if(i == length(post_chunk)){
      if(post_chunk[i] == ""){
        end_blank_ind <- i
        break
      }
      break
    }
    if(post_chunk[i] != ""){
      break
    }
    end_blank_ind <- i
    i <- i + 1
  }

  num_blank_lines <- end_blank_ind - start_blank_ind + 1
  if(num_blank_lines == 1){
    # Way too special syntax required to have only a single line
    # This took forever to figure out.
    tbl_chunk <- c(tbl_chunk, "" ,"\\\\ \\\\", "")
  }else{
    tbl_chunk <- c(tbl_chunk, "", rep("\\\\", num_blank_lines - 1), "")
  }
  if(end_blank_ind == length(post_chunk)){
    return(list(tbl_chunk, NULL))
  }
  the_rest <- post_chunk[(end_blank_ind + 1):length(post_chunk)]
  return(list(tbl_chunk, the_rest))
}