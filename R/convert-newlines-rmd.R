#' Remove all the constraints in Rmd involving newlines
#'
#' @description
#' Rmarkdown code requires several different entries to render newlines
#' in a final document. These are frustrating because the algorithm
#' is not clear on what you need for which case. For example, header lines
#' (lines starting with #) do not need a newline, but if you want 'N' blank
#' lines between the header and next text, you need 'N lines containing
#' only \\, without any empty lines following.
#' For other text, you need N + 1 lines containing only \\ with an empty line
#' following, before the next text occurs. List elements must not have newlines
#' between them.
#' This allows for 'What you see is what you get' code in your Rmarkdown code.
#'
#' @param text_chunk A vector of strings, one line of Rmarkdown per element
#'
#' @return A modified vector of strings
#' @export
#' @examples
#' convert_newlines_rmd(c("#header", "hello world", "", "", "1. A list", "  a. Sublist"))
convert_newlines_rmd <- function(text_chunk){

  # Strip all lines with just spaces and nothing else. Rstudio tends to add
  # spaces when you press 'Enter' in text in a [cat()] function call.
  # Also, sometimes a space can halt everything because you can't see it
  text_chunk <- map_chr(text_chunk, ~{
    `if`(grepl("^\\s+$", .x), "", .x)
  })
  new_tc <- NULL
  i <- 1
  while(i < length(text_chunk)){
    contains_text <- text_chunk[i] != ""
    is_header <- contains_text && grepl("^[\"]?#", text_chunk[i])
    is_lst_elem <- contains_text &&
      substr(trimws(text_chunk[i]), 2, 2) == "." ||
      substr(trimws(text_chunk[i]), 1, 1) == "-"
    is_tbl_elem <- contains_text &&
      substr(trimws(text_chunk[i]), 1, 5) == "-----"
    next_is_tbl_elem <- contains_text &&
      substr(trimws(text_chunk[i + 1]), 1, 5) == "-----"
    next_next_is_tbl_elem <- contains_text &&
      substr(trimws(text_chunk[i + 2]), 1, 5) == "-----"
    if(is.na(next_next_is_tbl_elem)){
      next_next_is_tbl_elem <- FALSE
    }

    if(contains_text){
        new_tc <- c(new_tc, text_chunk[i])
    }
    if(i != length(text_chunk)){
      next_contains_text <- text_chunk[i + 1] != ""
      next_is_lst_elem <- next_contains_text &&
                          substr(trimws(text_chunk[i + 1]), 2, 2) == "." ||
                          substr(trimws(text_chunk[i + 1]), 1, 1) == "-"

      if(contains_text &&
         next_contains_text &&
         !is_header &&
         !is_tbl_elem &&
         !next_is_tbl_elem &&
         !next_next_is_tbl_elem &&
         (!is_lst_elem || (is_lst_elem && !next_is_lst_elem))){
        new_tc <- c(new_tc, "\\\\", "")
      }else if(is_header){
        cnt <- 0
        if(!next_contains_text){
          while(!next_contains_text){
            cnt <- cnt + 1
            i <- i + 1
            next_contains_text <- text_chunk[i + 1] != ""
          }
          new_tc <- c(new_tc, rep("\\\\", cnt))
        }
      }else if(is_tbl_elem){
        # Two kids of manual tables:
        # 1. Has a beginning and end row of dashes, and a row of dashes after
        #    the table header. All these are directly beside the text. The
        #    table text not directly beside the top and bottom lines must have
        #    a blank line between them
        # 2. Has a single dashed line, after the table header. This line must
        #    have text above and below directly, no newlines. the table rows
        #    must be right next to each other, no blank lines between them
        is_type_1 <- text_chunk[i - 1] == "" ||
                     text_chunk[i - 1] == "\"" ||
                     text_chunk[i - 1] == "'"

        is_type_2 <- text_chunk[i - 1] != ""

        if(is_type_1){
          if(text_chunk[i + 1] != "" &&
             substr(trimws(text_chunk[i + 2]), 1, 5) == "-----"){
            # Add header surrounded by dashes lines
            new_tc <- c(new_tc, text_chunk[(i + 1):(i + 2)])
            i <- i + 3
            while(substr(trimws(text_chunk[i]), 1, 5) != "-----"){
              # Add table row
              new_tc <- c(new_tc, text_chunk[i])
              i <- i + 1
            }
            new_tc <- c(new_tc, text_chunk[i])
          }else{
            stop("Malformed table found: ",
                 paste(text_chunk[i:(i + 2)], collapse = "\n"),
                 call. = FALSE)
          }
        }else if(is_type_2){
          i <- i + 1
          while(text_chunk[i] != ""){
            # Add table row
            new_tc <- c(new_tc, text_chunk[i])
            i <- i + 1
          }
          new_tc <- c(new_tc, text_chunk[i])
        }
      }else{
        cnt <- 0
        if(!next_contains_text){
          while(!next_contains_text){
            cnt <- cnt + 1
            i <- i + 1
            next_contains_text <- text_chunk[i + 1] != ""
            next_is_lst_elem <- next_contains_text &&
              substr(trimws(text_chunk[i + 1]), 2, 2) == "." ||
              substr(trimws(text_chunk[i + 1]), 1, 1) == "-"
          }
          new_tc <- c(new_tc, rep("\\\\", cnt + 1), "")
        }
      }
      i <- i + 1
    }
  }
  c(new_tc, text_chunk[length(text_chunk)])
}