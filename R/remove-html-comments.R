#' Remove HTML comments (Markdown comments) from a chunk of code
#'
#' @keywords internal
#'
#' @param chunk A vector of character strings representing lines for RMD code
#' @param fn Filename that to reference when writing errors only. The file is
#' not used or looked for in any way, all the processing is done for `chunk`
#'
#' @return The modified vector of Rmarkdown code, which has all comments
#' removed and `NA`'s for lines which were taken up by all comments
#'
#' @importFrom purrr map2_chr
remove_html_comments <- function(chunk, fn = "unknown"){

  start_comment <- str_extract_all(chunk, "<!--")
  start_lines <- which(lengths(start_comment) > 0)
  file_start_lines <- start_lines
  num_start_per_line <- lengths(start_comment[start_lines])
  comments_removed_before_ind <- rep(0, length(chunk))
  run_num_comm_lines <- 0

  if(length(start_lines)){
    # Remove all comments which have the start and end on the same line
    lines_modified <- map2_chr(start_lines, num_start_per_line,
         function(start, num){
           i <- 1
           tmp <- chunk[start]
           repeat{
             tmp <- gsub("(.*)<!--.*-->", "\\1", tmp)
             if(tmp != chunk[start]){
               comments_removed_before_ind[start] <<- 1
             }
             if(tmp == ""){
             }
             if(i == num){
               break
             }
             i <- i + 1
           }
           tmp
         })
    if(length(lines_modified)){
      chunk[start_lines] <- lines_modified
      # Remove lines which are now empty strings due to comment removal,
      # and count lines removed
      num_lines_removed <- 0
      chunk_start_lines_empty_vec <- chunk[start_lines] == ""
      chunk[start_lines] <- ifelse(chunk_start_lines_empty_vec, NA, chunk[start_lines])
    }
  }
  inds_single_lines <- which(comments_removed_before_ind == 1)
  start_lines <- start_lines[!start_lines %in% inds_single_lines]
  end_comment <- str_extract_all(chunk, "-->")
  end_lines <- which(lengths(end_comment) > 0 & map_lgl(chunk, function(.x) {!is.na(.x)}))
  if(length(start_lines) != length(end_lines)){
    bail("The number of start comment characters and end comment ",
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
    bail("One or more of the end comment characters in file '", fn, "' comes ",
         "before start comment characters for comments spanning multiple ",
         "lines.\n\nLine numbers for the starting ",
         "comment characters are:\n\n",
         paste(start_lines, collapse = ", "),
         "\n\nLine numbers for the ending comment ",
         "characters are:\n\n",
         paste(end_lines, collapse = ", "),
         "\n\n")
  }

  if(length(start_lines)){
    comment_inds <- map2(start_lines, end_lines, function(start, end){
      chunk[start] <<- gsub("(<!--.*)", "", chunk[start])
      chunk[end] <<- gsub("(.*-->)", "", chunk[end])
      if(chunk[start] == ""){
        chunk[start] <<- NA
      }
      if(chunk[end] == ""){
        chunk[end] <<- NA
      }
      if(end - start > 1){
        i <- start + 1
        repeat{
          if(i >= end){
            break
          }
          chunk[i] <<- NA
          i <- i + 1
        }
      }
    })
  }

  chunk
}