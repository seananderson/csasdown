#' Copy mirrored knitr chunk code into chunks with the mirrored call
#'
#' @description
#' Copy the mirrored chunk code into the chunk with the mirrored call.
#' knitr does this automatically but we need to do it in the pre-processing
#' step so we can modify the newlines in the mirrored code
#'
#' @param rmd A vector of character strings representing the lines in an
#' Rmd file
#'
#' @return A vector of character strings representing the lines in an
#' Rmd file but with the mirrored calls replaced with code
#' @export
copy_mirror_chunks <- function(rmd_files){

  # Make a pasted-together version of all the Rmds files so the chunks are all
  # in one object for searching for code. So if a file has a mirror to a
  # chunk in another file in the project, it will still find the code that is
  # being referred to
  huge_rmd <- unlist(map(rmd_files, ~{
    readLines(.x)
  }))

  # Replace chunk mirrors with code
  modded_files <- map(rmd_files, function(fn = .x){
    txt <- readLines(fn)
    mirror_inds <- grep("<<[a-zA-Z0-9_\\-]+>>", txt)
    if(!length(mirror_inds)){
      return(NULL)
    }
    chunk_names <- unique(gsub("<<([a-zA-Z0-9_\\-]+)>>", "\\1", txt[mirror_inds]))
    map(chunk_names, function(chunk_name = .x){

      # Search for the mirrored chunks in txt (the file)
      pat <- paste0("<<", chunk_name, ">>")
      file_mirror_inds <- grep(pat, txt)

      # Search for the code for this mirrored chunk in huge_rmd
      pat <- paste0(chunk_name, ",")
      k <- grep(pat, huge_rmd)
      if(!length(k)){
        stop("Chunk name '", chunk_name, "' does not seem to have a source chunk in ",
             "the project",
             call. = FALSE)
      }
      if(length(k) > 1){
        stop("Chunk name '", chunk_name, "' seems to have multiple source chunks in ",
             "the project",
             call. = FALSE)
      }
      k <- k + 1
      src_start <- k
      while(substr(trimws(huge_rmd[k]), 1, 3) != "```" ){
        k <- k + 1
      }
      # chunk is the code chunk we will be inserting in place of mirror calls
      chunk <- huge_rmd[src_start:(k - 1)]
      txt <<- inject_vec_in_vec(txt, chunk, file_mirror_inds)
    })
    unlink(fn, force = TRUE)
    writeLines(txt, fn)
  })
}