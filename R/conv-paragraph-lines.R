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
    new_chunk <- c(chunk[1], "", "\\\\", "")
    return(list(new_chunk, NULL))
  }

  next_line <- chunk[2]
  next_is_table <- FALSE
  is_table_line <- FALSE
  if(length(chunk) >= 3){
    type <- is_rmarkdown_table_line(chunk[1:3])
    if(type != "false"){
      is_table_line <- TRUE
    }
  }
  next_is_header <- is_rmarkdown_header_line(next_line)
  next_is_lst_line <- is_rmarkdown_list_line(next_line)
  if(next_is_lst_line ||
     next_is_header ||
     next_is_table){
    new_chunk <- c(chunk[1], "\\\\", "")
    return(list(new_chunk, chunk[2:length(chunk)]))
  }

  next_is_text <- TRUE
  new_chunk <- NULL
  i <- 1

  repeat{
    if(i == length(chunk)){
      if(chunk[i] == "" ||
         is_rmarkdown_header_line(chunk[i]) ||
         is_rmarkdown_list_line(chunk[i])){
          return(list(new_chunk, chunk[i]))
      }else{
        new_chunk <- c(new_chunk, chunk[i], "\\\\", "")
        return(list(new_chunk, NULL))
      }
    }
    if(chunk[i] == "" ||
       is_rmarkdown_header_line(chunk[i]) ||
       is_rmarkdown_list_line(chunk[i])){
      break
    }else{
      new_chunk <- c(new_chunk, chunk[i], "\\\\", "")
    }
    i <- i + 1
  }
  # Remove last additional newlines
  if(!is.null(new_chunk)){
    new_chunk <- new_chunk[-c((length(new_chunk) - 1):length(new_chunk))]
  }
  last_text_match <- i - 1

  post_chunk <- chunk[(last_text_match + 1):length(chunk)]
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