#' Convert header lines in Rmd code to have WYSIWYG newlines
#' (WYSIWYG = What You See Is What You Get)
#'
#' @description
#' Convert header lines in Rmd code to WYSIWYG newlines. The provided chunk
#' will start with a header line and possibly more or return NULL as the
#' converted chunk, and the whole `chunk` as the rest. The series of header
#' lines will be converted into a mini-chunk, which will be returned
#' as the first element of a two-element list, the second element is the
#' rest of the Rmd.
#'
#' @param chunk A vector of character strings representing lines for RMD code
#'
#' @return A list of two elements, 1) The corrected part of the chunk and
#' 2) the rest of the chunk starting with the line after the last blank line
#' @export
#'
#' @examples
#' library(csasdown)
#' chunk <- c("#Main", "##Introduction", "", "Some text..", "More Rmd text")
#' tmp <- conv_header_lines(chunk)
#' header_line_chunk <- tmp[[1]]
#' the_rest <- tmp[[2]]
conv_header_lines <- function(chunk){

  if(is.null(chunk)){
    return(list(NULL, NULL))
  }

  if(!is_rmarkdown_header_line(chunk[1])){
    return(list(NULL, chunk))
  }

  if(length(chunk) == 1){
    return(list(chunk[1], NULL))
  }

  new_chunk <- chunk[1]
  is_header <- TRUE
  last_header_match <- 1
  i <- 1
  blanks_detected <- NULL
  blank_iter <- 1
  blanks_detected[blank_iter] <- FALSE
  while(is_header && i < length(chunk)){
    i <- i + 1
    is_blank <- chunk[i] == ""
    blanks_detected[blank_iter] <- is_blank
    blank_iter <- blank_iter + 1
    while(is_blank && i < length(chunk)){
      i <- i + 1
      is_blank <- chunk[i] == ""
    }
    is_header <- is_rmarkdown_header_line(chunk[i])
    if(!is_header){
      break
    }
    last_header_match <- i
    new_chunk <- c(new_chunk, chunk[i])
  }

  if(!is_header){
    blanks_detected <- blanks_detected[-length(blanks_detected)]
  }
  if(any(blanks_detected)){
    warning("All blank lines between header lines are ignored:\n\n",
            paste(chunk, collapse = "\n"),
            "\n\n",
            call. = FALSE)
  }

  if(last_header_match == length(chunk)){
    the_rest <- NULL
    return(list(new_chunk, the_rest))
  }

  post_chunk <- chunk[(last_header_match + 1):length(chunk)]
  # Add the post-table trailing whitespace
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
    # Way too special syntax required to have only a single line
    # This took forever to figure out.
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