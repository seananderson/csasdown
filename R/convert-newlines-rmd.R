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

  if(is.null(text_chunk)){
    return(NULL)
  }
  text_chunk <- map_chr(text_chunk, ~{
    `if`(grepl("^\\s+$", .x), "", .x)
  })
  new_tc <- NULL

  is_blank_line <- text_chunk[1] == ""
  if(is_blank_line){
    tmp <- conv_blank_lines(text_chunk)
    new_tc <- c(new_tc, tmp[[1]], convert_newlines_rmd(tmp[[2]]))
  }

  is_header_line <- grepl("^#+", text_chunk[1])
  if(is_header_line){
    tmp <- conv_header_lines(text_chunk)
    new_tc <- c(new_tc, tmp[[1]], convert_newlines_rmd(tmp[[2]]))
  }

  is_lst_line <- substr(trimws(text_chunk[1]), 2, 3) == ". " ||
                 substr(trimws(text_chunk[1]), 1, 2) == "* " ||
                 substr(trimws(text_chunk[1]), 1, 2) == "+ " ||
                 substr(trimws(text_chunk[1]), 1, 2) == "- "
  if(is_lst_line){
    tmp <- conv_list_lines(text_chunk)
    new_tc <- c(new_tc, tmp[[1]], convert_newlines_rmd(tmp[[2]]))
  }

  pat <- "[a-zA-Z0-9_\\-]+"
  if(length(text_chunk) >= 3){
    is_type_1 <- substr(trimws(text_chunk[1]), 1, 5) == "-----" &&
      length(grep(pat, trimws(text_chunk[2])))
    is_type_2 <- length(grep(pat, trimws(text_chunk[1]))) &&
      substr(trimws(text_chunk[2]), 1, 5) == "-----"
    if(is_type_1 || is_type_2){
      tmp <- conv_table_lines(text_chunk)
      new_tc <- c(new_tc, tmp[[1]], convert_newlines_rmd(tmp[[2]]))
    }

  }

  # Regular text paragraphs
  if(length(text_chunk) > 0 &&
     !is_blank_line &&
     !is_header_line &&
     !is_lst_line &&
     !is_type_1 &&
     !is_type_2){
    tmp <- conv_paragraph_lines(text_chunk)
    new_tc <- c(new_tc, tmp[[1]], convert_newlines_rmd(tmp[[2]]))
  }

  new_tc
}