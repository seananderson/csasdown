#' Get the the type of document to render by extracting from the YAML tags in
#' the index file
#'
#' @keywords internal
#' @param fn The name of the YAML file, typically 'index.Rmd' for bookdown
#'
#' @return A character string representing the render type. One of
#' `resdoc_pdf`, `resdoc_word`, `sr_pdf`, `sr_word`, `techreport_pdf` or
#' `techreport_word`
get_render_type <- function(fn = "index.Rmd"){

  if(!file.exists(fn)){
    bail("The file ", fn_color(fn), " does not exist")
  }
  rmd <- readLines(fn)
  trim_rmd <- trimws(rmd)

  # Get the document type from the `output:` YAML tag
  doc_type_pat <- "^csasdown::(\\S+):\\s*$"
  doc_ind <- grep(doc_type_pat, trim_rmd)
  if(!length(doc_ind)){
    bail("Document type not found in file ", fn_color(fn), "\n",
         "The line ", tag_color("csasdown::resdoc_pdf:"), " was not found")
  }
  if(length(doc_ind) > 1){
    alert("Document type defined more than once in file ", fn_color(fn), "\n",
          "A line like ", tag_color("csasdown::resdoc_pdf:"),
          " is multiply defined.\n",
          "Using the first instance.")
    doc_ind <- doc_ind[1]
  }

  gsub(doc_type_pat, "\\1", trimws(rmd[doc_ind]))
}