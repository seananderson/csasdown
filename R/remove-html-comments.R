#' Remove HTML comments (Markdopwn comments) from a chunk of code
#'
#' @param chunk A vector of character strings representing lines for RMD code
#' @param fn Filename that to reference when writing errors only. The file is
#' not used or looked for in any way, all the processing is done for `chunk`
#'
#' @return The modified vector of Rmarkdown code
#' @importFrom purrr map2_chr
#' @export
remove_html_comments <- function(chunk, fn = "unknown"){

  start_comment <- str_extract_all(chunk, "<!--")
  start_lines <- which(lengths(start_comment) > 0)
  num_start_per_line <- lengths(start_comment[start_lines])

  if(length(start_lines)){
    # Remove all comments which have the start and end on the same line
    lines_modified <- map2_chr(start_lines,
         num_start_per_line,
         function(start = .x, num = .y){
           i <- 1
           tmp <- chunk[start]
           repeat{
             tmp <- gsub("(.*)<!--.*-->", "\\1", tmp)
             if(i == num){
               break
             }
             i <- i + 1
           }
           tmp
         })
    if(length(lines_modified)){
      chunk[start_lines] <- lines_modified
      # Remove lines which are now empty strings due to comment removal
      chunk[start_lines] <- ifelse(chunk[start_lines] == "", NA, chunk[start_lines])
      chunk <- chunk[!is.na(chunk)]
    }
  }
  start_comment <- str_extract_all(chunk, "<!--")
  start_lines <- which(lengths(start_comment) > 0)
  num_start_per_line <- lengths(start_comment[start_lines])

  end_comment <- str_extract_all(chunk, "-->")
  end_lines <- which(lengths(end_comment) > 0)
  if(length(start_lines) != length(end_lines)){
    stop("The number of start comment characters and end comment ",
         "characters do not match for comments spanning multiple ",
         "lines in file '", fn, "'\n\nLine numbers for the ",
         "starting comment characters are:\n\n",
         paste(start_lines, collapse = ", "),
         "\n\nLine numbers for the ending comment ",
         "characters are:\n\n",
         paste(end_lines, collapse = ", "),
         "\n\n")
  }
  if(any(end_lines - start_lines < 1)){
    stop("One or more of the end comment characters in file '", fn, "' comes ",
         "before start comment characters for comments spanning multiple ",
         "lines.\n\nLine numbers for the starting ",
         "comment characters are:\n\n",
         paste(start_lines, collapse = ", "),
         "\n\nLine numbers for the ending comment ",
         "characters are:\n\n",
         paste(end_lines, collapse = ", "),
         "\n\n")
  }
  tryCatch({
    # Remove all comments that span multiple lines
    repeat{
      start_comment <- str_extract_all(chunk, "<!--")
      start_lines <- which(lengths(start_comment) > 0)
      if(!length(start_lines)){
        break
      }
      end_comment <- str_extract_all(chunk, "-->")
      end_lines <- which(lengths(end_comment) > 0)
      start <- start_lines[1]
      end <- end_lines[1]
      chunk[start] <- gsub("(<!--.*)", "", chunk[start])
      chunk[end] <- gsub("(.*-->)", "", chunk[end])
      if(chunk[start] == ""){
        if(chunk[end] == ""){
          comment_range <- start:end
        }else{
          comment_range <- start:(end - 1)
        }
      }else{
        if(chunk[end] == ""){
          comment_range <- (start + 1):end
        }else{
          comment_range <- (start + 1):(end - 1)
        }
      }
      chunk <- chunk[-comment_range]
    }
  }, error = function(e){
    stop("General error with the comments in the file ", fn, ".\n",
         "Usually this type of error is caused by interwoven comments or ",
         "comments that are embedded within one another",
         call. = FALSE)
  })

  chunk
}