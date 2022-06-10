#' Remove all comment lines from knitr chunks in the Rmd code
#'
#' @param rmd A vector of character strings representing the lines in an
#' Rmarkdown file
#'
#' @return A modified `rmd` vector
#' @export
#' @importFrom knitr all_patterns
remove_comments_from_chunks <- function(rmd){

  # Extract the knitr code chunks
  pat_code_begin <- all_patterns$md$chunk.begin
  # The following pat code matched chunks with four backticks, which are
  # verbatim code chunks and not meant to be processed
  # pat_code_end <- all_patterns$md$chunk.end
  pat_code_end <- "^[\t >]*```\\s*$"
  start_inds <- grep(pat_code_begin, rmd)
  end_inds <- grep(pat_code_end, rmd)
  if(length(start_inds) != length(!end_inds)){
    stop("The number of knitr starting code chunk header lines does not ",
         "equal the number of ending code chunk lines (triple backtick-lines)",
         call. = FALSE)
  }

  if(!length(start_inds)){
    # No code chunks found
    return(rmd)
  }

  code_chunks <- map2(start_inds, end_inds, ~{
    rmd[.x:.y]
  })
  # Process code chunks here (remove comment lines). Unfortunately,
  # if an Rmarkdown header is inside a `cat()` statement, starting on its own
  # line, it will appear exactly the same as a comment inside the code chunk.
  # To fix this, look for a `cat()` statement inside the code chunk, and
  # while inside that, ignore any lines that start with #
  com_pat <- "^#.*$"
  code_chunks <- map(code_chunks, function(chunk = .x){
    cat_ind <- grep("^cat\\(.*", trimws(chunk))
    if(length(cat_ind)){
      if(length(cat_ind) > 1){
        stop("Can only have one `cat()` call inside a code chunk:\n\n",
             paste(chunk, collapse = "\n"),
             "\n\n",
             call. = FALSE)
      }
      # Get the indices of the chunk which are inside the `cat()` call
      k <- parse_cat_text(chunk[cat_ind:length(chunk)], ret_inds = TRUE)
      k <- cat_ind + k - 1
      # Need to loop through the lines of the chunk
      new_chunk <- NULL
      i <- 1
      repeat{
        if(i %in% k){
          new_chunk <- c(new_chunk, chunk[i])
        }else{
          gr <- grep(com_pat, trimws(chunk[i]))
          if(!length(gr)){
            new_chunk <- c(new_chunk, chunk[i])
          }
        }
        if(i == length(chunk)){
          break
        }
        i <- i + 1
      }
      return(new_chunk)
    }else{
      gr <- grep(com_pat, trimws(chunk))
      if(length(gr)){
        return(chunk[-gr])
      }else{
        return(chunk)
      }
    }
  })

  out_rmd <- NULL
  if(start_inds[1] > 1){
    out_rmd <- c(out_rmd, rmd[1:(start_inds[1] - 1)])
  }
  imap(start_inds, ~{
    out_rmd <<- c(out_rmd, code_chunks[[.y]])
     if(.y < length(start_inds)){
        if(start_inds[.y + 1] - end_inds[.y] > 1){
          out_rmd <<- c(out_rmd, rmd[(end_inds[.y] + 1):(start_inds[.y + 1] - 1)])
      }
    }
  })
  if(end_inds[length(end_inds)] < length(rmd)){
    out_rmd <- c(out_rmd, rmd[(end_inds[length(end_inds)] + 1):length(rmd)])
  }

  out_rmd
}
