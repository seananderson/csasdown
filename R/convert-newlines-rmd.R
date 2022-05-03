#' Fix newlines in text that is from inside [cat()] in an Rmd document
#'
#' @description
#' A vector of lines of text strings made up of Rmarkdown code, 1 line per
#' vector element will contain some newlines which are represented in the
#' vector as empty strings (""). These are replaced in a somewhat complex
#' way so that the worry of how newlines in Rmarkdown works is removed.
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
  rl <- rle(text_chunk)
  group_sizes <- rl$lengths[rl$values == ""]

  make_strs <- function(sz){
    if(sz == 1){
      strs <- c("\\n", "")
    }else{
      strs <- c("\\n", rep("\\\\", sz - 1), "")
    }
  }
  newline_strs <- map(group_sizes, ~{
    make_strs(.x)
  })
  i <- 1
  grp_ind <- 1
  x <- NULL
  while(i <= length(text_chunk)){
    if(text_chunk[i] == ""){
      # Start group
      x <- c(x, unlist(newline_strs[grp_ind]))
      i <- i + group_sizes[grp_ind]
      grp_ind <- grp_ind + 1
    }else{
      x <- c(x, text_chunk[i])
      i <- i + 1
    }
  }
  x
}