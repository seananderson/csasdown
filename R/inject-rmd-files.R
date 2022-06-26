#' Inject verbatim text from Rmd files into Rmd code
#'
#' @description
#' Inject verbatim text from Rmd files into Rmd code wrapped in
#' [cat()] so that the bilingual features of csasdown can be used properly
#' but the author can write paragraphs or sections in pure Rmarkdown
#'
#' @keywords internal
#'
#' @param rmd_files A vector of character strings representing the names
#' of Rmd files
#' @param verbose Logical. If `TRUE`, print messages
#'
#' @return Nothing
inject_rmd_files <- function(rmd_files, verbose = FALSE){

  if(verbose){
    notify("Attempting to inject Rmd files included with ",
           csas_color("rmd_file('filename')"), " ...")
  }
  modded_files <- map(rmd_files, function(fn){
    rmd <- readLines(fn)
    # Single or double quotes around filename
    rmd_file_inds <- grep("^rmd_file\\([\"|\'].*[\"|\']\\)$", trimws(rmd))
    nms <- trimws(rmd[rmd_file_inds])
    rmd_file_names <- gsub("^rmd_file\\([\"|\'](.*)[\"|\']\\)$", "\\1", nms)
    rmd_file_names <- trimws(rmd_file_names)
    rmd_code <- map(rmd_file_names, function(.x) {
      rmd <- read_rmd_file(.x, src_fn = fn)
      # Strip any HTML comments out of the file
      rmd <- remove_html_comments(rmd, .x)
      # Check the rmd code to make sure there are no triple-tick code chunks in it
      backtick_inds <- grep("^```", trimws(rmd))
      if(length(backtick_inds)){
        notify("Triple-backticks found in file ", fn_color(basename(.x)),
                " on line(s) ",
                paste(backtick_inds, collapse = ", "))
        bail("Triple- or Quadruple-backtick code chunks are not allowed in ",
             "external RMD files which have been injected using ",
             csas_color("rmd_file()"), ". The code is going to be imported ",
             "into a chunk and embedding chunks into other chunks is not ",
             "possible in knitr. ",
             "Delete them or use Markdown comments to comment those chunks out ",
             "<!-- -->.")
      }
      # Need to remove the NA's from the chunk that was returned from `remove_html_comments()`
      rmd <- rmd[!is.na(rmd)]
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
    new_rmd <- unlist(k)
    writeLines(new_rmd, fn)
  })

  if(verbose){
    check_notify("Rmd files injected successfuly\n")
  }
}
