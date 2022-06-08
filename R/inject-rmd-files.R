#' Inject verbatim text from Rmd files into Rmd code
#'
#' @description
#' Inject verbatim text from Rmd files into Rmd code wrapped in
#' [cat()] so that the bilingual features of csasdown can be used properly
#' but the author can write paragraphs or sections in pure Rmarkdown
#'
#' @param rmd A vector of character strings representing an Rmd file
#'
#' @return A vector of character strings representing an Rmd file
#' @export
inject_rmd_files <- function(rmd){

  # Single or double quotes around filename
  rmd_file_inds <- grep("^rmd_file\\([\"|\'].*[\"|\']\\)$", trimws(rmd))
  nms <- trimws(rmd[rmd_file_inds])
  rmd_file_names <- gsub("^rmd_file\\([\"|\'](.*)[\"|\']\\)$", "\\1", nms)
  rmd_file_names <- trimws(rmd_file_names)

  rmd_code <- map(rmd_file_names, ~{
    rmd <- read_rmd_file(.x)
    # Check the rmd code to make sure there are no triple-tick code chunks in it
    backtick_inds <- grep("^```", trimws(rmd))
    if(length(backtick_inds)){
      message("Triple-backtick code chunk found in file '", basename(.x),
              ".Rmd' on line(s) ",
              paste(backtick_inds, collapse = ", "))
      stop("Triple-backtick code chunks are not allowed in external RMD ",
           "files which have been injected using `rmd_files()`. It ",
           "defeats the purpose of using the bilingual code chunk system",
           call. = FALSE)
    }
    rmd
  })

  if(!length(rmd_file_inds)){
    return(rmd)
  }

  if(length(rmd_file_inds) > 1){
    j <- imap(rmd_file_inds, ~{
      if(.y == length(rmd_file_inds)){
        text_chunk <- rmd[(.x + 1):length(rmd)]
      }else{
        text_chunk <- rmd[(.x + 1):(rmd_file_inds[.y + 1] - 1)]
      }
    })
    j <- append(list(rmd[1:(rmd_file_inds[1] - 1)]), j)
  }else{
    # There is only one rmd_file call
    j <- list(rmd[1:(rmd_file_inds[1] - 1)], rmd[(rmd_file_inds[1] + 1):length(rmd)])
  }

  # Put the code chunks from the Rmd files where the call to insert them was
  k <- NULL
  for(i in seq_along(j)){
    k <- append(k, j[i])
    if(i < length(j)){
      k <- append(k, rmd_code[i])
    }
  }
  unlist(k)
}
