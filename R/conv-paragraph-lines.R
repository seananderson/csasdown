#' Convert paragraph lines in Rmd code to have WYSIWYG newlines
#' (WYSIWYG = What You See Is What You Get)
#'
#' @description
#' Convert paragraph lines in Rmd code to WYSIWYG newlines. The provided chunk
#' will start with regular text (or a R inline code chunk) and possibly more
#' lines. The series of paragraph lines will be converted into a mini-chunk,
#' which will be returned as the first element of a two-element list, the
#' second element is the rest of the Rmd.
#'
#' @param chunk A vector of character strings representing lines for RMD code
#'
#' @return A list of two elements, 1) The corrected part of the chunk and
#' 2) the rest of the chunk starting with the line after the last blank line
#' @export
conv_paragraph_lines <- function(chunk){

  if(is.null(chunk)){
    return(list(NULL, NULL))
  }

  if(chunk[1] == "" ||
     is_rmd_list_line(chunk[1]) ||
     is_rmd_header_line(chunk[1])){
    return(list(NULL, chunk))
  }

  if(length(chunk) == 1){
    return(list(chunk[1], NULL))
  }

  i <- 1
  start_text_ind <- 1
  end_text_ind <- 1
  repeat{
    if(chunk[i] == "" ||
       is_rmd_list_line(chunk[i]) ||
       is_rmd_header_line(chunk[i])){
      i <- i - 1
      break
    }
    if(i == length(chunk)){
      end_text_ind <- i
      break
    }
    end_text_ind <- i
    i <- i + 1
  }

  new_chunk <- chunk[start_text_ind:end_text_ind]
  # Interleave blank lines between the text lines
  new_chunk <- imap(new_chunk, ~{
    if(.y == length(new_chunk)){
      return(.x)
    }
    c(.x, "\\\\", "")
  }) %>%
    unlist

  if(end_text_ind == length(chunk)){
    post_chunk <- NULL
  }else{
    post_chunk <- chunk[(end_text_ind + 1):length(chunk)]
  }

  list(new_chunk, post_chunk)
}