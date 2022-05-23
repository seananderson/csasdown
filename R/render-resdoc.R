#' Render a resdoc with bilingual features
#'
#' @description
#' Render a resdoc with bilingual features. Renders a resdoc using the
#' [bookdown::render_book()] method but includes a
#' pre-processing step to do two things:
#' 1. Inject 'index.Rmd' with special code to allow bilingual features to
#'    be used
#' 2. Convert anything inside [cat()] calls to cat-like
#'    strings instead of rmarkdown strings.
#' This means that any inline R code
#' included with backticks, eg: `` `r Sys.time()` `` will be replaced with
#' a quoted, comma separated string (see [catize()]). This allows the [cat()]
#' function inside a code chunk to contain backtick-quoted R expressions exactly
#' like what knitr processes inline.
#'
#' @details
#' This is a convenience function so users can place [cat()] around
#' already-written rmarkdown code containing inline R code chunks, and
#' place those [cat()] statements inside a large code chunk. It will be
#' invisible to users when using this function to render their document.
#'
#' Temporary files for all the Rmd files and the YAML file which contains these
#' filenames (typically _bookdown.yml) are created with modified code chunks.
#' Anywhere in the Rmd files containing [cat()] with knitr-style inline embedded
#' code chunks included are modified to make a string of text and R code together
#' which can be processed by core R. The official knitr regular expression is used
#' to extract these inline code chunks. The main Rmd file, typically index.Rmd
#' is not modified at all (it is not parsed).
#'
#' Any single-backslash escaped things in rmarkdown such as `` $\pi$ ``, or
#' `` $\alpha$ `` or similar will be converted to double-backslashed inside
#' of the temporary Rmd files to avoid an error from the [cat()] function.
#'
#' You can use either single or double quotes to surround the text passed to [cat()]
#'
#' @param yaml_fn The YAML file name. The default is '_bookdown.yml' for
#' [bookdown::render_book()]
#' @param keep_files If `TRUE`, keep the temporary files created (Rmd files and
#' YAML file)
#' @param ... Additional arguments passed to [bookdown::render_book()]
#'
#' @return Nothing
#' @importFrom purrr prepend imap_chr imap
#' @importFrom stringr str_count
#' @export
render_resdoc <- function(yaml_fn = "_bookdown.yml",
                          keep_files = FALSE,
                          ...){

  tmp_yaml_fn <- create_tmp_yaml_file(yaml_fn)
  book_fn <- get_book_filename(yaml_fn)

  book <- readLines(book_fn)
  fn_process <- rmd_filenames_from_yaml(yaml_fn)
  if(!length(fn_process)){
    stop("No uncommented Rmd files were found in the YAML file ", yaml_fn,
         call. = FALSE)
  }

  # Remove the book_fn from the vector if it is there
  fn_process <- fn_process[fn_process != book_fn]
  tmp_rmd_files <- map(fn_process, ~{
    if(!file.exists(.x)){
      stop("The file ", .x, " does not exist. Check the YAML file entry ", yaml_fn,
           call. = FALSE)
    }
    rmd <- readLines(.x)
    rmd <- inject_rmd_files(rmd)

    cat_inds <- grep("cat\\(.*", rmd)
    blank_inds <- NULL

    # Match ending parens
    chunk_end_inds <- NULL
    cat_chunks <- imap(cat_inds, ~{
      if(.y == length(cat_inds)){
        text_chunk <- parse_cat_text(rmd[.x:length(rmd)])
      }else{
        text_chunk <- parse_cat_text(rmd[.x:(cat_inds[.y + 1] - 1)])
      }
      chunk_end_inds <<- c(chunk_end_inds, .x + length(text_chunk) - 1)
      # Convert any single backslashes to double. All the extra ones are needed here
      # because of escaping and because we are inside a cat() layer so it is a double-
      # double situation
      text_chunk <- gsub("\\\\", "\\\\\\\\", text_chunk)
      text_chunk <- convert_newlines_rmd(text_chunk)
      # If single quotes were used to surround the text in [cat()], make them double
      # so that they match the quotes used to make the embedded code parts
      if(substr(text_chunk[1], 1, 1) == "'"){
        substr(text_chunk[1], 1, 1) <- "\""
      }
      if(substr(text_chunk[length(text_chunk)],
                nchar(text_chunk[length(text_chunk)]),
                nchar(text_chunk[length(text_chunk)])) == "'"){
        substr(text_chunk[length(text_chunk)],
               nchar(text_chunk[length(text_chunk)]),
               nchar(text_chunk[length(text_chunk)])) <- "\""
      }

      text_chunk <- map_chr(text_chunk, ~{
        catize(.x)
      })
      text_chunk[1] <- paste0("cat(", text_chunk[1])
      text_chunk[length(text_chunk)] <- paste0(text_chunk[length(text_chunk)], ")")
      text_chunk
    })
    if(length(cat_chunks)){
      out_rmd <- NULL
      out_rmd <- rmd[1:(cat_inds[1] - 1)]
      # extract chunks not part of 'cat()'s
      cat_start_inds <- cat_inds[-1]
      cat_start_inds <- c(cat_start_inds, length(rmd))
      noncat_chunks <- imap(seq_along(cat_start_inds), ~{
        end <- ifelse(.x == length(cat_start_inds),
                      cat_start_inds[.x],
                      (cat_start_inds[.x] - 1))
        rmd[(chunk_end_inds[.x] + 1):end]
      })
      # Interlace the two, cat_chunks and noncat_chunks
      map(seq_along(cat_chunks), ~{
        out_rmd <<- c(out_rmd, cat_chunks[[.x]], noncat_chunks[[.x]])
      })
    }else{
      out_rmd <- rmd
    }

    # Make references work, they were modified above in the code that changes
    # the backslashes
    out_rmd <- gsub("\\\\\\\\\\\\\\\\@ref", "\\\\\\\\@ref", out_rmd)

    # If any chunks do not have a blank line between them, make it so
    # The lack of blank line between them messes up the typesetting in a bad
    # way in some cases
    bt_inds <- grep("```", out_rmd)
    spaced_out_rmd <- NULL
    i <- 1
    while(i < length(out_rmd)){
      if(substr(trimws(out_rmd[i]), 1, 3) == "```" &&
         substr(trimws(out_rmd[i + 1]), 1, 3) == "```"){
        spaced_out_rmd <- c(spaced_out_rmd, out_rmd[i], "", out_rmd[i + 1])
        i <- i + 2
      }else{
        spaced_out_rmd <- c(spaced_out_rmd, out_rmd[i])
        i <- i + 1
      }
    }
    spaced_out_rmd <- c(spaced_out_rmd, out_rmd[i])

    tmp_fn <- paste0("tmp-", .x)
    unlink(tmp_fn, force = TRUE)
    writeLines(spaced_out_rmd, tmp_fn)
    tmp_fn
  })

  # Modify index.Rmd (actually tmp-index.Rmd)
  tmp_book_fn <- get_book_filename(tmp_yaml_fn)
  unlink(tmp_book_fn, force = TRUE)
  file.copy(book_fn, tmp_book_fn)
  inject_bilingual_code(tmp_book_fn)

  render_book(tmp_book_fn, config_file = tmp_yaml_fn, ...)
  if(!keep_files){
    map(tmp_rmd_files, ~{
      unlink(.x, force = TRUE)
    })
    unlink(tmp_yaml_fn)
  }
}
