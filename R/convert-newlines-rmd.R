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
  for(i in seq_along(text_chunk)){
    new_tc <- c(new_tc, text_chunk[i])
    if(i != length(text_chunk)){
      contains_text <- text_chunk[i] != ""
      next_contains_text <- text_chunk[i + 1] != ""
      is_header <- contains_text && grepl("^[\"]?#", text_chunk[i])
      is_lst_elem <- contains_text &&
                     substr(trimws(text_chunk[i]), 2, 2) == "." ||
                     substr(trimws(text_chunk[i]), 1, 1) == "-"
      next_is_lst_elem <- next_contains_text &&
                          substr(trimws(text_chunk[i + 1]), 2, 2) == "." ||
                          substr(trimws(text_chunk[i + 1]), 1, 1) == "-"
      # Put a newline (\n) after every non-list element line
      if(contains_text && !is_header && (!is_lst_elem || (is_lst_elem && !next_is_lst_elem))){
        new_tc <- c(new_tc, "\\n")
      }else if(!contains_text && next_is_lst_elem){
        new_tc <- c(new_tc, "\\\\", "\\\\", "__mark_newline__")
      }
    }
  }

  # rl <- rle(text_chunk)
  # group_sizes <- rl$lengths[rl$values == ""]
  # newline_strs <- map(group_sizes, ~{
  #   rep("\\\\", .x)
  # })
  # i <- 1
  # grp_ind <- 1
  # x <- NULL
  # while(i <= length(text_chunk)){
  #   if(text_chunk[i] == ""){
  #     # Start group
  #     x <- c(x, unlist(newline_strs[grp_ind]))
  #     i <- i + group_sizes[grp_ind]
  #     grp_ind <- grp_ind + 1
  #   }else{
  #     x <- c(x, text_chunk[i])
  #     i <- i + 1
  #   }
  # }
  new_tc <- map_chr(new_tc, ~{
    `if`(.x == "", "\\\\", .x)
  })
  new_tc <- map_chr(new_tc, ~{
    `if`(.x == "__mark_newline__", "", .x)
  })
}