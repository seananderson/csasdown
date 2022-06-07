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
     is_rmarkdown_list_line(chunk[1]) ||
     is_rmarkdown_header_line(chunk[1])){
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
       is_rmarkdown_list_line(chunk[i]) ||
       is_rmarkdown_header_line(chunk[i])){
      i <- i - 1
      break
    }
    if(i == length(chunk)){
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
  post_chunk <- chunk[(end_text_ind + 1):length(chunk)]
  # Add the post-paragraph trailing whitespace
  if(post_chunk[1] != ""){
    new_chunk <- c(new_chunk, "")
    return(list(new_chunk, post_chunk))
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
    new_chunk <- c(new_chunk, "" ,"\\\\ \\\\", "")
  }else{
    new_chunk <- c(new_chunk, "", rep("\\\\", num_blank_lines - 1), "")
  }
  if(end_blank_ind == length(post_chunk)){
    return(list(new_chunk, NULL))
  }
  the_rest <- post_chunk[(end_blank_ind + 1):length(post_chunk)]

  return(list(new_chunk, the_rest))
}