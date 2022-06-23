#' Render any csasdown document with bilingual features
#'
#' @description
#' Render a csasdown document with bilingual features. Renders a csasdown
#' document (autodetects resdoc, sr, techreport) using the
#' [bookdown::render_book()] method but includes a pre-processing step to
#' do several things:
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
#' @param en_chunk_regex A regular expression to match for the chunk
#' name for English chunks. Default is 'ends in -en'. The `$` means anchor
#' to the end, so '-en' must be at the end. `\\S+` means match one or more
#' any other non-whitespace characters. Passed to [validate_chunk_headers()]
#' @param fr_chunk_regex A regular expression to match for the chunk
#' name for French chunks. Default is 'ends in -fr'. Passed to
#' [validate_chunk_headers()]
#' @param suppress_warnings If `TRUE`, [base::suppressWarnings()] will wrap
#' the call to [bookdown::render_book()]
#' @param ... Additional arguments passed to [bookdown::render_book()]
#'
#' @return Nothing
#' @importFrom purrr prepend imap_chr imap
#' @importFrom stringr str_count
#' @importFrom knitr all_patterns
#' @export
render <- function(yaml_fn = "_bookdown.yml",
                   keep_files = FALSE,
                   en_chunk_regex = "^\\S+-en$",
                   fr_chunk_regex = "^\\S+-fr$",
                   suppress_warnings = TRUE,
                   ...){

  # Create the temporary YAML and Rmd files and store their names
  tmp_yaml_rmd_fns <- create_tmp_yaml_rmd_files(yaml_fn)
  tmp_yaml_fn <- tmp_yaml_rmd_fns[[1]]
  tmp_rmd_fns <- tmp_yaml_rmd_fns[[2]]

  index_fn <- get_index_filename(tmp_yaml_fn)
  render_type <- get_render_type(index_fn)
  doc_format <- gsub("^\\S+_(\\S+)$", "\\1", render_type)
  pdf_or_word <- `if`(doc_format == "pdf", "PDF", "Word")

  # Set the render type
  set_render_type(index_fn, doc_format)

  # Find out what language is set to and set the option 'french' here
  # so that it works on the first compilation in a workspace
  # It sets `options(french)` to the value in the file
  set_language_option(index_fn)

  # Render type (resdoc_pdf, sr_word, etc)
  render_type <- get_render_type(index_fn)

  # Make sure all YAML entries are present in `index.Rmd`
  check_yaml(render_type)

  csas_render_type <- gsub("(.*)_\\S+$", "\\1", render_type)
  if(csas_render_type == "resdoc"){
    csas_render_type <- "Research Document"
  }else if(csas_render_type == "sr"){
    csas_render_type <- "Science Response"
  }else if(csas_render_type == "techreport"){
    csas_render_type <- "Technical Report"
  }else{
    csas_render_type <- "CSAS Document" # nocov
  }

  message(paste0("\nRendering the ", csas_render_type, " as a ", pdf_or_word,
                 " document in ", `if`(fr(), "French", "English"), "..."))

  # Process all Rmd files except for the `index_fn` (index.Rmd)
  fn_process <- tmp_rmd_fns[tmp_rmd_fns != index_fn]

  # Make sure all chunk headers are of the correct language and have
  # `needs_trans` chunk headers set correctly
  validate_chunk_headers(fn_process,
                         en_chunk_regex = en_chunk_regex,
                         fr_chunk_regex = fr_chunk_regex)

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

  # Run the pre-processor on all the chunks
  preprocess_chunks(fn_process, yaml_fn)

  # Modify index.Rmd (actually tmp-index.Rmd)
  tmp_index_fn <- get_index_filename(tmp_yaml_fn)

  # render_type defined at beginning of this function
  inject_bilingual_code(tmp_index_fn, render_type)

  if(suppress_warnings){
    suppressWarnings(
      render_book(tmp_index_fn,
                  config_file = tmp_yaml_fn,
                  ...)
    )
  }else{
    render_book(tmp_index_fn,
                config_file = tmp_yaml_fn,
                ...)
  }
  # Delete the temporary files
  if(!keep_files){
    map(fn_process, ~{
      unlink(.x, force = TRUE)
    })
    unlink(tmp_yaml_fn)
  }

  rename_output_files(index_fn)
  invisible()
}
