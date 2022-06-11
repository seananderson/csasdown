#' Copy mirrored knitr chunk code into chunks with the mirrored call
#'
#' @description
#' Copy the mirrored chunk code into the chunk with the mirrored call.
#' knitr does this automatically but we need to do it in the pre-processing
#' step so we can modify the newlines in the mirrored code.
#'
#' @details
#' Search for chunks containing mirror code lines (looks like `<<chunk-name>>`),
#' find the chunk-names and copy the code within those chunks into the chunks
#' with the mirror code lines, replacing them.
#'
#' @param rmd_files A vector of character strings representing the names
#' of Rmd files
#'
#' @return A vector of character strings representing the lines in an
#' Rmd file but with the mirrored calls replaced with code
#'
#' @importFrom magrittr %>%
#' @export
copy_mirror_chunks <- function(rmd_files){

  # Make a pasted-together version of all the Rmds files so the chunks are all
  # in one object for searching for code. So if a file has a mirror to a
  # chunk in another file in the project, it will still find the code that is
  # being referred to
  huge_rmd <- unlist(map(rmd_files, ~{
    readLines(.x)
  }))

  get_mirror_not_in_cat_inds <- function(txt, regex = "<<[a-zA-Z0-9_\\-]+>>"){
    cat_inds <- grep("^(cat|rmd_file)\\(.*", trimws(txt))
    if(length(cat_inds)){
      # Have to make sure mirror code line is not inside a  `cat()` call
      mirror_in_cat_inds <- map(cat_inds, ~{
        chunk <- txt[.x:length(txt)]
        inds <- parse_cat_text(chunk, ret_inds = TRUE) + .x - 1
        if(length(inds)){
          cat_chunk <- txt[inds]
          cat_mirror_inds <- grep(regex, cat_chunk)
          txt_cat_mirror_inds <- cat_mirror_inds + .x - 1
          return(txt_cat_mirror_inds)
        }
        NULL
      }) %>% unlist
    }
    all_mirror_inds <- grep(regex, txt)
    if(!length(all_mirror_inds)){
      return(NULL)
    }
    if(length(cat_inds)){
      all_mirror_inds[!all_mirror_inds %in% mirror_in_cat_inds]
    }
  }
  # Replace chunk mirrors with code
  modded_files <- map(rmd_files, function(fn = .x){
    txt <- readLines(fn)
    mirror_inds <- get_mirror_not_in_cat_inds(txt)

    if(!length(mirror_inds)){
      return(NULL)
    }
    chunk_names <- unique(gsub("<<([a-zA-Z0-9_\\-]+)>>", "\\1", txt[mirror_inds]))
    map(chunk_names, function(chunk_name = .x){

      # Search for the mirrored chunks in txt (the file)
      pat <- paste0("<<", chunk_name, ">>")
      file_mirror_inds <- get_mirror_not_in_cat_inds(txt, pat)
      if(!length(file_mirror_inds)){
        return(NULL)
      }
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
      if(src_start < k){
        # There was code in the chunk
        chunk <- huge_rmd[src_start:(k - 1)]
        txt <<- inject_vec_in_vec(txt, chunk, file_mirror_inds)
      }
    })
    unlink(fn, force = TRUE)
    writeLines(txt, fn)
  })
}