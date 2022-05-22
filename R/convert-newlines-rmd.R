#' Fix newlines in text that is from inside [cat()] in an Rmd document
#'
#' @description
#' A vector of lines of text strings made up of Rmarkdown code, 1 line per
#' vector element will contain some newlines which are represented in the
#' vector as empty strings (""). These are replaced in a somewhat complex
#' way so that the worry of how newlines in Rmarkdown is no more.
#' This allows for 'What you see is what you get' code in the text strings.
#'
#' @param text_chunk A vector of strings, one line of Rmarkdown per
#' element
#'
#' @return A modified vector of strings
#' @export
#' @examples
#' convert_newlines_rmd(c("hello world", "", "", "Welcome!"))
convert_newlines_rmd <- function(text_chunk){
  # inject break after each non-blank line

  # Problem, need blank before list starts. Like this there has to be two blank lines before list
  new_tc <- NULL
  i <- 1
  while(i < length(text_chunk)){
    contains_text <- text_chunk[i] != ""
    is_header <- contains_text && grepl("^[\"]?#", text_chunk[i])
    is_lst_elem <- contains_text &&
      substr(trimws(text_chunk[i]), 2, 2) == "." ||
      substr(trimws(text_chunk[i]), 1, 1) == "-"
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
         (!is_lst_elem || (is_lst_elem && !next_is_lst_elem))){
        new_tc <- c(new_tc, "\\\\", "")
      }
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
  new_tc <- c(new_tc, text_chunk[length(text_chunk)])
  new_tc
}