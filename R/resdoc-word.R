#' Creates an Microsoft Word CSAS-formatted document
#'
#' @description
#' This is a function called in output in the YAML of the `index.Rmd` file
#' to specify the creation of a Microsoft Word version of the Research
#' Document or Science Response.
#'
#' @param ... Other arguments to [bookdown::word_document2()]
#' @import bookdown
#' @rdname csas_docx
#' @return A Word Document in the `.docx` format based on the CSAS Res Doc
#' template
#' @export
resdoc_word <- function(...) {
  file <- if (fr()) "RES2021-fra-content.docx" else "RES2021-eng-content.docx"
  base <- word_document2(...,
                         reference_docx = system.file("csas-docx",
                                                      file,
                                                      package = "csasdown"))

  # Mostly copied from knitr::render_sweave
  base$knitr$opts_chunk$comment <- NA
  base$knitr$opts_chunk$fig.align <- "center"
  base
}



#' Add frontmatter to Res Doc word file
#'
#' Add title page and table of contents to a Res Doc Word document.
#'
#' @keywords internal
#' @param index_fn The name of the YAML file, typically 'index.Rmd' for bookdown
#'
#' @return A merged .docx
add_resdoc_word_frontmatter <- function(index_fn, verbose = FALSE) {

  if (verbose) notify("Adding frontmatter to the ", csas_color("Research Document"), "using the officer package...")

  x <- rmarkdown::yaml_front_matter(index_fn)

  ## Note: bookmarks (bkm) were manually added to the "resdoc-frontmatter.docx" file
  ## Also needed to copy and paste character strings like "french_title", otherwise
  ## officer may not detect that string if manually typed (b/c Word operates in chunks)
  file <- "resdoc-frontmatter.docx"
  front <- officer::read_docx(system.file("csas-docx", file, package = "csasdown")) |>
    officer::headers_replace_text_at_bkm("region", x$region) |>
    officer::headers_replace_text_at_bkm("year", as.character(x$year)) |>
    officer::headers_replace_text_at_bkm("report_number", as.character(x$report_number)) |>
    officer::footers_replace_text_at_bkm("date", paste(x$month, x$year)) |>
    officer::body_replace_all_text("^title$", x$title) |>
    officer::body_replace_all_text("^french_title$", x$french_title) |>
    officer::body_replace_all_text("^author$", x$author) |>
    officer::body_replace_all_text("^address$", x$address) |>
    officer::body_replace_all_text("^author_list$", x$author_list) |>
    officer::body_replace_all_text("^year$", as.character(x$year)) |>
    officer::body_replace_all_text("^report_number$", as.character(x$report_number)) # |>
    # officer::body_replace_all_text("^abstract$", x$abstract)

  print(front, target = "TEMP-frontmatter.docx")

  ## Drop title from resdoc.docx
  content <- officer::read_docx("_book/resdoc.docx") |>
    officer::cursor_begin() |>
    officer::body_remove() |>
    officer::cursor_reach(keyword = "INTRODUCTION") |>
    officer::cursor_backward() |>
    officer::body_add_break()

  print(content, target = "TEMP-content.docx")

  doc <- officer::read_docx("TEMP-frontmatter.docx") |>
    officer::body_add_docx("TEMP-content.docx") |>
    officer::cursor_reach(keyword = "TABLE OF CONTENTS") |>
    officer::body_add_toc() |>
    officer::cursor_reach(keyword = "ABSTRACT") |>
    officer::cursor_forward() |>
    officer::body_remove() # drop return

  print(doc, target = "_book/resdoc.docx")

  unlink("TEMP-frontmatter.docx")
  unlink("TEMP-content.docx")

  invisible()

}
