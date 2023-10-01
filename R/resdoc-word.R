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



#' Add title page to Res Doc word file
#'
#' Add title page and table of contents to a Res Doc Word document.
#'
#' @keywords internal
#' @param fn The name of the YAML file, typically 'index.Rmd' for bookdown
#'
#' @return A merged .docx
add_resdoc_word_titlepage <- function(index_fn) {

  x <- rmarkdown::yaml_front_matter(index_fn)

  file <- "resdoc-titlepage.docx"
  doc <- officer::read_docx(system.file("csas-docx", file, package = "csasdown"))

  # d <- officer::headers_replace_text_at_bkm(doc, "report_year", x$report_year)
  # d <- officer::headers_replace_text_at_bkm(doc, "report_number", x$report_number)
  # d <- officer::headers_replace_text_at_bkm(doc, "region_name", x$region)

  ## Note: bookmarks (bkm) were manually added to the "resdoc-titlepage.docx" file
  doc <- doc |>
    officer::headers_replace_text_at_bkm("region", x$region) |>
    officer::headers_replace_text_at_bkm("year", as.character(x$year)) |>
    officer::headers_replace_text_at_bkm("number", as.character(x$report_number)) |>
    officer::body_replace_all_text("TITLE", x$title) |>
    officer::body_replace_all_text("AUTHOR", x$author) |>
    officer::body_replace_all_text("ADDRESS", x$address) |>
    officer::body_replace_all_text("ALIST", x$author_list) |>
    officer::body_replace_all_text("YEAR", as.character(x$year)) |>
    officer::body_replace_all_text("NUMBER", as.character(x$report_number)) |>
    officer::body_end_block_section(officer::block_section(officer::prop_section(type = "nextPage"))) |>

  print(doc, target = "TEMP-titlepage.docx")

  doc <- officer::read_docx("TEMP-titlepage.docx") |>
    officer::body_end_block_section(officer::block_section(officer::prop_section(type = "nextPage"))) |>
    officer::body_add_docx("_book/resdoc.docx")

  print(doc, target = "_book/resdoc.docx")

  unlink("TEMP-titlepage.docx")

  invisible()

}

# add_resdoc_word_frontmatter <- function(index_fn) {
#
# }
