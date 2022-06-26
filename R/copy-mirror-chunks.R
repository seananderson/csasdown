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
#' @keywords internal
#'
#' @param rmd_files A vector of character strings representing the names
#' of Rmd files
#' @param nowrite Don't write the files, instead return the Rmd code as
#' elements of a list, with each element corresponding to the input files
#' in `rmd_files`. Needed for testing.
#' @param verbose Logical. If `TRUE`, print messages
#'
#' @return A vector of character strings representing the lines in an
#' Rmd file but with the mirrored calls replaced with code
copy_mirror_chunks <- function(rmd_files,
                               nowrite = FALSE,
                               verbose = FALSE){

  if(verbose){
    notify("Copying chunk source code into mirror references (",
           csas_color("<<chunk-name>>"), ") ...")
  }
  # Make a pasted-together version of all the Rmds files so the chunks are all
  # in one object for searching for code. So if a file has a mirror to a
  # chunk in another file in the project, it will still find the code that is
  # being referred to
  huge_rmd <- unlist(map(rmd_files, function(.x) {
    readLines(.x)
  }))

  get_mirror_not_in_cat_inds <- function(txt, regex = "<<[a-zA-Z0-9_\\-]+>>"){

    cat_inds <- grep("^(cat|rmd_file)\\(.*", trimws(txt))
    if(length(cat_inds)){
      # Have to make sure mirror code line is not inside a `cat()` call
      mirror_in_cat_inds <- map(cat_inds, ~{
        chunk <- txt[.x:length(txt)]
        inds <- parse_cat_text(chunk, ret_inds = TRUE) + .x - 1
        if(length(inds)){
          cat_chunk <- txt[inds]
          cat_mirror_inds <- grep(regex, cat_chunk)
          txt_cat_mirror_inds <- cat_mirror_inds + .x - 1
          return(txt_cat_mirror_inds)
        }
        NULL # nocov
      }) |>
        unlist()
    }
    all_mirror_inds <- grep(regex, txt)
    if(!length(all_mirror_inds)){
      return(NULL)
    }
    if(length(cat_inds)){
      return(all_mirror_inds[!all_mirror_inds %in% mirror_in_cat_inds])
    }else{
      # No cat() calls, but maybe a mirror code line
      return(all_mirror_inds)
    }
  }
  # Replace chunk mirrors with code
  modded_files <- map(rmd_files, function(fn){
    txt <- readLines(fn)
    mirror_inds <- get_mirror_not_in_cat_inds(txt)

    if(!length(mirror_inds)){
      return(NULL)
    }
    chunk_names <- unique(gsub("<<([a-zA-Z0-9_\\-]+)>>", "\\1", txt[mirror_inds]))
    map(chunk_names, function(chunk_name){

      if(verbose){
        notify("Copying source code from chunk ", csas_color(chunk_name))
      }
      # Search for the mirrored chunks in txt (the file)
      pat <- paste0("<<", chunk_name, ">>")
      file_mirror_inds <- get_mirror_not_in_cat_inds(txt, pat)
      if(!length(file_mirror_inds)){
        return(NULL) # nocov
      }
      # Search for the code for this mirrored chunk in huge_rmd
      pat <- paste0(chunk_name, ",")
      k <- grep(pat, huge_rmd)
      if(!length(k)){
        bail("In file ", fn_color(fn), ", mirrored chunk name ",
             csas_color(paste0("<<", chunk_name, ">>")),
                        " does not appear to have a source chunk in ",
                        "the project")
      }
      if(length(k) > 1){
        bail("In file ", fn_color(fn), ", mirrored chunk name ",
             csas_color(paste0("<<", chunk_name, ">>")),
             " appears to have multiple source chunks in the project")
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
        # If the code to insert is another mirror, issue error.
        chained_mirror_pat <- "^<<(\\S+)>>$"
        if(length(grep(chained_mirror_pat, trimws(chunk)))){
          nm <- gsub(chained_mirror_pat, "\\1", chunk)
          bail("A mirrored chunk has another mirror code line in it.\n",
               "Chained mirror chunks are not supported.\n",
               "Line ", csas_color(file_mirror_inds[1] - 1), " in file ",
               fn_color(fn))
        }

        txt <<- inject_vec_in_vec(txt, chunk, file_mirror_inds)
      }
    })
    if(nowrite){
      return(txt)
    }else{
      unlink(fn, force = TRUE)
      writeLines(txt, fn)
    }
  })

  if(verbose){
    check_notify("Mirror chunks copied successfully\n")
  }

  modded_files
}