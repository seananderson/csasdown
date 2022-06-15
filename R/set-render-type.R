#' Set the the type of document to render by modifying the YAML tags in
#' the index file
#'
#' @keywords internal
#'
#' @param fn The name of the YAML file, typically 'index.Rmd' for bookdown
#' @param doc_type The type of document to set for rendering. Either 'pdf' or
#' 'word'
#'
#' @return Nothing
set_render_type <- function(fn = "index.Rmd",
                            doc_type = "pdf"){

  if(!file.exists(fn)){
    stop("The file '", fn, "' does not exist")
  }
  rmd <- readLines(fn)
  trim_rmd <- trimws(rmd)

  # Get the document type from the `output:` YAML tag
  doc_type_pat <- "^csasdown::(\\S+):\\s*$"
  doc_ind <- grep(doc_type_pat, trim_rmd)
  if(!length(doc_ind)){
    stop("Document type not found in file '", fn, "'\n",
         "A line'csasdown::resdoc_pdf:' was not found",
         call. = FALSE)
  }
  if(length(doc_ind) > 1){
    warning("Document type defined more than once in file '", fn, "'\n",
            "A line like 'csasdown::resdoc_pdf:' is multiply defined.\n",
            "Using the first instance.",
            call. = FALSE)
    doc_ind <- doc_ind[1]
  }


  full_doc_type <- gsub(doc_type_pat, "\\1", trimws(rmd[doc_ind]))
  old_doc_type <- gsub(".*_(\\S+)$", "\\1", full_doc_type)
  if(old_doc_type == doc_type){
    # No need to re-write the file
    return(invisible())
  }
  csas_doc_type <- gsub("(.*)_\\S+$", "\\1", full_doc_type)
  full_type_line <- paste0(" csasdown::", csas_doc_type, "_", doc_type, ":")
  rmd[doc_ind] <- full_type_line
  unlink(fn, force = TRUE)
  writeLines(rmd, fn)
  invisible()
}