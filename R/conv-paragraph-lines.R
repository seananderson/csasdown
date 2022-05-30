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
#' @param chunk The Rmd chunk to process
#'
#' @return A list of two elements, 1) The corrected part of the chunk and
#' 2) the rest of the chunk starting with the line after the last blank line
#' @export
conv_paragraph_lines <- function(chunk){

  if(is.null(chunk)){
    return(list(NULL, NULL))
  }

  if(chunk[1] == ""){
    return(list(NULL, chunk))
  }

  if(length(chunk) >= 1 &&
     nchar(chunk[1]) >= 2 &&
     substr(trimws(chunk[1]), 1, 2) == "# "){
    return(list(NULL, chunk))
  }

  if(length(chunk) >= 2 &&
     nchar(chunk[1]) >= 2 &&
     (substr(trimws(chunk[1]), 1, 2) == "* " ||
      substr(trimws(chunk[1]), 1, 2) == "+ " ||
      substr(trimws(chunk[1]), 1, 2) == "- ")){
    return(list(NULL, chunk))
  }

  if(length(chunk) >= 1 &&
     nchar(chunk[1]) >= 3 &&
           substr(trimws(chunk[1]), 2, 3) == ". "){
    return(list(NULL, chunk))
  }

  # `dash_pat` matches any sequence of zero or more whitespace characters,
  # followed by 1 or more dashes, followed by zero or more whitespace
  # characters all repeating
  dash_pat <- "^(\\s*-+\\s*)+$"
  if(length(grep(dash_pat, trimws(chunk[1]))) ||
     length(grep(dash_pat, trimws(chunk[2])))){
    stop("A table appears to have been started but not finished:\n\n",
         paste(chunk, collapse = "\n"),
         "\n\n",
         call. = FALSE)
  }

  if(length(chunk) == 1){
    return(list(chunk[1], NULL))
  }

  next_is_blank <- FALSE
  new_chunk <- NULL
  i <- 1

  while(!next_is_blank && i < length(chunk)){
    new_chunk <- c(new_chunk, chunk[i])
    i <- i + 1
    next_is_blank <- chunk[i] == ""
  }
  new_chunk <- c(new_chunk, "\\\\")

  if(i == length(chunk)){
    the_rest <- NULL
  }else{
    the_rest <- chunk[i:length(chunk)]
  }

  list(new_chunk, the_rest)
}