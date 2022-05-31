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
  pat_code_end <- all_patterns$md$chunk.end
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
  # Process code chunks here (remove comment lines)
  code_chunks <- map(code_chunks, ~{
    gr <- grep("^#.*$", trimws(.x))
    if(length(gr)){
      .x[-gr]
    }else{
      .x
    }
  })

  if(length(start_inds) == 1){
    if(start_inds == 1 && end_inds == length(rmd)){
      # Only one code chunk, taking up all of rmd
      out_rmd <- code_chunks[[1]]
    }else if(start_inds != 1 && end_inds == length(rmd)){
      # Some text before the code chunk
      out_rmd <- c(rmd[1:(start_inds - 1)], code_chunks[[1]])
    }else if(start_inds == 1 && end_inds != length(rmd)){
      # Some text after the code chunk
      out_rmd <- c(code_chunks[[1]], rmd[(end_inds + 1):length(rmd)])
    }else{
      # There is text both before and after the chunk
      out_rmd <- c(rmd[1:(start_inds - 1)],
                   code_chunks[[1]],
                   rmd[(end_inds + 1):length(rmd)])
    }
  }else if(length(start_inds) > 1){
    # Interleave chunks back together correctly (the hard part)
    out_rmd <- NULL
    code_first <- FALSE
    imap(start_inds, ~{
      if(start_inds[.y] == 1 && .y == 1){
        # It goes code chunk, text chunk, code chunk, etc
        out_rmd <<- code_chunks[[1]]
        if(start_inds[2] - end_inds[1] > 1){
          # If code chunks are right after each other with no lines in betweem,
          # don't do this bit
          out_rmd <<- c(out_rmd, rmd[(end_inds[1] + 1):(start_inds[2] - 1)])
        }
        code_first <<- TRUE
      }else if(.y == 1){
        # It goes text chunk, code chunk, text chunk, etc
        out_rmd <<- c(rmd[1:(start_inds[1] - 1)], code_chunks[[1]])
        code_first <<- FALSE
      }

      if(.y > 1){
        if(code_first){
          out_rmd <<- c(out_rmd,
                        code_chunks[[.y]])

          if(.y == length(start_inds) && end_inds[length(end_inds)] > length(rmd)){
            # One more text chunk needs to be added to the end
            #if(start_inds[.y] - end_inds[.y - 1] > 1){
              # If code chunks are right after each other with no lines in between,
              # don't do this bit
              out_rmd <<- c(out_rmd,
                          rmd[(end_inds[.y] + 1):(start_inds[.y + 1] - 1)])
            #}
          }
        }else{
          if(start_inds[.y] - end_inds[.y - 1] > 1){
            out_rmd <<- c(out_rmd,
                          rmd[(end_inds[.y - 1] + 1):(start_inds[.y] - 1)])
          }
          out_rmd <<- c(out_rmd, code_chunks[[.y]])
          if(.y == length(start_inds) && end_inds[length(end_inds)] < length(rmd)){
            # One more text chunk needs to be added to the end
            out_rmd <<- c(out_rmd,
                          rmd[(end_inds[.y] + 1):length(rmd)])
          }
        }
      }
    })
  }
  out_rmd
}