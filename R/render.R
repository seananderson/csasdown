#' Render a csasdown document with bilingual features
#'
#' @description
#' Render a csasdown document with bilingual features. Renders a csasdown
#' document (resodc, sr, techreport) using the [bookdown::render_book()]
#' method but includes a pre-processing step to do several things:
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
#' @param doc_type The type of document to render. Either 'pdf' or 'word'
#' @param ... Additional arguments passed to [bookdown::render_book()] and
#' [validate_chunk_headers()]
#'
#' @return Nothing
#' @importFrom purrr prepend imap_chr imap
#' @importFrom stringr str_count
#' @importFrom knitr all_patterns
#' @export
render <- function(yaml_fn = "_bookdown.yml",
                   keep_files = FALSE,
                   doc_type = c("pdf", "word"),
                   ...){

  doc_type <- match.arg(doc_type)

  # Create the temporary YAML and Rmd files and store their names
  tmp_yaml_rmd_fns <- create_tmp_yaml_rmd_files(yaml_fn)
  tmp_yaml_fn <- tmp_yaml_rmd_fns[[1]]
  tmp_rmd_fns <- tmp_yaml_rmd_fns[[2]]

  book_fn <- get_book_filename(tmp_yaml_fn)

  # Set the render type
  set_render_type(book_fn, doc_type)

  # Find out what language is set to and set the option 'french' here
  # so that it works on the first compilation in a workspace
  set_language_option(book_fn)
  book <- readLines(book_fn)

  if(!length(tmp_rmd_fns)){
    stop("No uncommented Rmd files were found in the YAML file ", yaml_fn,
         call. = FALSE)
  }

  # Get CSAS document type
  doc_type_pat <- "^csasdown::(\\S+):\\s*$"
  doc_ind <- grep(doc_type_pat, trimws(book))
  if(!length(doc_ind)){
    stop("Document type not found in file '", book_fn, "'\n",
         "A line'csasdown::resdoc_pdf:' was not found",
         call. = FALSE)
  }
  if(length(doc_ind) > 1){
    warning("Document type defined more than once in file '", book_fn, "'\n",
         "A line like 'csasdown::resdoc_pdf:' is multiply defined.\n",
         "Using the first instance.",
         call. = FALSE)
    doc_ind <- doc_ind[1]
  }
  doc_type <- gsub(doc_type_pat, "\\1", trimws(book[doc_ind]))
  # Make sure all YAML entries are present in `index.Rmd`
  check_yaml(doc_type)
  message("\nRendering document ...")

  # Process all Rmd files except for the `book_fn` (index.Rmd)
  fn_process <- tmp_rmd_fns[tmp_rmd_fns != book_fn]

  # Make sure all chunk headers are of the correct language and have
  # `needs_trans` chunk headers set correctly
  validate_chunk_headers(fn_process, ...)
  # Remove all comments from code chunks in all files
  remove_comments_from_chunks(fn_process)
  # Inject the Rmd code in referenced files into the actual code in all files
  inject_rmd_files(fn_process)
  # Copy mirrored code chunks as real code into the chunks where they are
  # mirrored in all files:
  # e.g replace instances of <<char-01-para-06-chunk>> with code from that
  # actual chunk. This works project wide, i.e. a chunk can mirror a chunk
  # from a different file as long as both are in `fn_process`
  copy_mirror_chunks(fn_process)

  tmp_rmd_files <- map(fn_process, ~{
    if(!file.exists(.x)){
      stop("The file ", .x, " does not exist. Check the YAML file entry ",
           yaml_fn,
           call. = FALSE)
    }
    rmd <- readLines(.x)

    cat_inds <- grep("^cat\\(.*", trimws(rmd))
    # Find `needs_trans = TRUE` chunks. Those will not have newlines converted
    # Need to make sure they are in chunk headers and not in text
    # (inside `cat()` calls) so have to pre-match the chunk header lines
    # then match from those
    chunk_head_inds <- grep(all_patterns$md$chunk.begin, rmd)
    if(!length(chunk_head_inds)){
      # No chunks in the file, return the file
      return(NULL)
    }
    nt_inds <- chunk_head_inds[grep("needs_trans\ *=\ *TRUE", rmd[chunk_head_inds])]
    nt_chunks <- NULL
    if(length(nt_inds)){
      if(!all((nt_inds + 1) %in% cat_inds)){
        # Name the chunks which are missing `cat()`
        bad_chunk_names <- map(nt_inds, ~{
          x <- .x + 1
          if(!x %in% cat_inds){
            # Extract chunk name
            chunk_head <- gsub(all_patterns$md$chunk.begin, "\\1", rmd[.x])
            pat <- "r\\s*(\\s*\\S+\\s*)+?\\s*,\\s*.*$"
            chunk_name <- gsub(pat, "\\1", chunk_head)
            return(chunk_name)
          }
          NULL
        })
        bad_chunk_names <- bad_chunk_names[lengths(bad_chunk_names) > 0]
        message("Not all chunks in the file ", .x, " with `needs_trans = TRUE` have ",
                "`cat(` immediately following. If the chunks mirror other chunks, ",
                "make sure that the mirrored chunk has a `cat()` call in it.\n")
        message("The chunk name(s) missing `cat()` are:\n\n",
                paste(bad_chunk_names, collapse = "\n"),
                "\n")
        stop("Chunks missing `cat()`",
             call. = FALSE)
      }
      cat_inds <- cat_inds[!(cat_inds %in% (nt_inds + 1))]

      # Deal with needs-trans chunks
      nt_inds <- nt_inds + 1
      chunk_end_inds <- NULL
      nt_chunks <- imap(nt_inds, ~{
        if(.y == length(nt_inds)){
          text_chunk <- parse_cat_text(rmd[.x:length(rmd)])
        }else{
          text_chunk <- parse_cat_text(rmd[.x:(nt_inds[.y + 1] - 1)])
        }
        chunk_end_inds <<- c(chunk_end_inds, .x + length(text_chunk) - 1)
        # Convert any single backslashes to double. All the extra ones are needed here
        # because of escaping and because we are inside a cat() layer so it is a double-
        # double situation
        text_chunk <- gsub("\\\\", "\\\\\\\\", text_chunk)
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
    }
    if(length(nt_chunks)){
      out_rmd <- NULL
      out_rmd <- rmd[1:(nt_inds[1] - 1)]
      # extract chunks not part of chunks with `needs_trans = TRUE`
      nt_start_inds <- nt_inds[-1]
      nt_start_inds <- c(nt_start_inds, length(rmd))
      nonnt_chunks <- imap(seq_along(nt_start_inds), ~{
        end <- ifelse(.x == length(nt_start_inds),
                      nt_start_inds[.x],
                      (nt_start_inds[.x] - 1))
        rmd[(chunk_end_inds[.x] + 1):end]
      })
      # Interlace the two, nt_chunks and nonnt_chunks
      map(seq_along(nt_chunks), ~{
        out_rmd <<- c(out_rmd, nt_chunks[[.x]], nonnt_chunks[[.x]])
      })
    }else{
      out_rmd <- rmd
    }
    rmd <- out_rmd

    # Deal with non-needs_trans chunks, the same way as we dealt with the
    # need_trans chunks
    chunk_end_inds <- NULL
    cat_chunks <- NULL

    if(length(cat_inds)){
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
        if(!(length(text_chunk) == 1 && text_chunk[1] == "\"\"")){
          # There will be a leading quote and ending quote, but they may not be on
          # their own line. Remove them and keep track if they shared a line with other
          # values.
          if(text_chunk[1] == "\""){
            text_chunk <- text_chunk[-1]
            # Toggle to paste the quote to the beginning of the first element
            # If FALSE, add a new element to the beginning of the vector with
            # the quote character only after calling convert_newlines_rmd()
            paste_beg <- FALSE
          }
          if(text_chunk[length(text_chunk)] == "\""){
            text_chunk <- text_chunk[-length(text_chunk)]
            # Toggle to paste the quote to the end of the last element
            # If FALSE, add a new element to the end of the vector with
            # the quote character only after calling convert_newlines_rmd()
            paste_end <- FALSE
          }
          if(length(grep("^\".+$", text_chunk[1]))){
            text_chunk[1] <- gsub("\"(.*)", "\\1", text_chunk[1])
            paste_beg <- TRUE
          }
          if(length(grep("^.+\"$", text_chunk[length(text_chunk)]))){
            text_chunk[length(text_chunk)] <- gsub("(.*)\"", "\\1", text_chunk[length(text_chunk)])
            paste_end <- TRUE
          }

          text_chunk <- convert_newlines_rmd(text_chunk)

          if(paste_beg){
            text_chunk[1] <- paste0("\"", text_chunk[1])
          }else{
            text_chunk <- c("\"", text_chunk)
          }
          if(paste_end){
            text_chunk[length(text_chunk)] <- paste0(text_chunk[length(text_chunk)], "\"")
          }else{
            text_chunk <- c(text_chunk, "\"")
          }
        }

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
    }

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

    if(length(out_rmd)){
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
      if(i <= length(out_rmd)){
        spaced_out_rmd <- c(spaced_out_rmd, out_rmd[i])
      }
    }else{
      spaced_out_rmd <- ""
    }
    unlink(.x, force = TRUE)
    writeLines(spaced_out_rmd, .x)
    .x
  })

  # Modify index.Rmd (actually tmp-index.Rmd)
  tmp_book_fn <- get_book_filename(tmp_yaml_fn)
  # doc_type defined at beginning of this function
  inject_bilingual_code(tmp_book_fn, doc_type)

  render_book(tmp_book_fn, config_file = tmp_yaml_fn, ...)
  if(!keep_files){
    map(tmp_rmd_files, ~{
      unlink(.x, force = TRUE)
    })
    unlink(tmp_yaml_fn)
  }
}
