#' Copy mirrored knitr chunk code into chunks with the mirrored call
#'
#' @description
#' Copy the mirrored chunk code into the chunk with the mirrored call.
#' knitr does this automatically but we need to do it in the pre-processing
#' step so we can modify the newlines in the mirrored code.
#'
#' @details
#' Search for chunks containing mirror code lines (looks like
#' `<<chunk-name>>`), find the chunk-names and copy the code within those
#' chunks into the chunks with the mirror code lines, replacing them.
#'
#' @keywords internal
#'
#' @param rmd_files A vector of character strings representing the names
#' of Rmd files
#' @param nowrite Don't write the files, instead return the Rmd code as
#' elements of a list, with each element corresponding to the input files
#' in `rmd_files`. Needed for testing.
#' @param line_offsets A [data.frame()] or [tibble::tibble()] containing the
#' line offsets to be added to line numbers in messages in order to give the
#' correct line numbers for the original files. Has columns `fn` (chr),
#' `chunk_header` (chr)  `chunk_ind` (int), `pre_num` (dbl), `post_num` (dbl),
#' and `rmd_num` (dbl). See `return` values for [inject_rmd_files()] and
#' [remove_comments_from_chunks()]
#' @param verbose Logical. If `TRUE`, print messages
#'
#' @return A vector of character strings representing the lines in an
#' Rmd file but with the mirrored calls replaced with code
copy_mirror_chunks <- function(rmd_files,
                               nowrite = FALSE,
                               line_offsets = NULL,
                               verbose = FALSE){

  if(verbose){
    notify("Copying chunk source code into mirror references (",
           csas_color("<<chunk-name>>"), ") ...")
  }

  # Make a pasted-together version of all the Rmd files so the chunks are all
  # in one object for searching for code. So if a file has a mirror to a
  # chunk in another file in the project, it will still find the code that is
  # being referred to
  huge_rmd <- map(rmd_files, function(.x) {
    readLines(.x)
  }) |>
    unlist()

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
  num_lines_df <- imap(rmd_files, function(fn, fn_ind){
    txt <- readLines(fn)
    if(!length(txt)){
      return(NULL)
    }
    mirror_inds <- get_mirror_not_in_cat_inds(txt)

    if(!length(mirror_inds)){
      return(NULL)
    }
    if(verbose){
      notify("Mirror chunks in file: ", fn_color(fn))
    }
    chunk_headers <- txt[mirror_inds - 1]
    chunk_names <- unique(gsub("<<([a-zA-Z0-9_\\-]+)>>", "\\1", txt[mirror_inds]))
    num_lines <- imap(chunk_names, function(chunk_name, chunk_ind = .y){

      # Search for the mirrored chunks in txt (the file)
      pat <- paste0("<<", chunk_name, ">>")
      file_mirror_inds <- get_mirror_not_in_cat_inds(txt, pat)
      if(!length(file_mirror_inds)){
        return(NULL) # nocov
      }
      if(verbose){
        if(is.null(line_offsets)){
          notify("Copying Rmarkdown code into ",
                 csas_color(paste0("<<", chunk_name, ">>")),
                 " in ", fn_color(fn))
        }else{
          ln <- get_real_line_num(chunk_headers[chunk_ind], line_offsets)
          notify("Copying Rmarkdown code into ",
                 csas_color(paste0("<<", chunk_name, ">>")),
                 " at line ", tag_color(ln), " of ",
                 fn_color(gsub("^tmp-(\\S+)$", "\\1", fn)))        }
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
        num_lines <- length(chunk)
        list(fn, num_lines)
      }
    })
    num_lines_df <- map(num_lines, ~{
      tibble(fn = .x[[1]], num_lines = .x[[2]])
    }) |> map_df(~{.x})
    if(nowrite){
      return(txt)
    }else{
      unlink(fn, force = TRUE)
      writeLines(txt, fn)
    }
    num_lines_df
  })
  if(!nowrite){
    num_lines_df <- num_lines_df |>
      map_df(~{.x})
  }

  if(verbose){
    tot_lines_added <- sum(num_lines_df$num_lines)
    if(as.logical(tot_lines_added)){
      line <- ifelse(tot_lines_added == 1, "line", "lines")
      check_notify(tag_color(tot_lines_added),
                   " ", line, " successfully copied from one chunk into another:")

      tot_added_by_file <- num_lines_df |>
        group_by(fn) |>
        summarize(added = sum(num_lines))

      pwalk(tot_added_by_file, ~{
        check_notify(paste0("Copied ", tag_color(..2),
                            " Rmarkdown lines to file ", fn_color(..1)))
      })
    }else{
      check_notify("There were no mirror chunks (", fn_color("<<chunk-name>>"),
                   " ) found, therefore no Rmarkdown code to copy\n")
    }


    check_notify("Mirror chunks copied successfully\n")
  }

  num_lines_df
}