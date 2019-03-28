#' Creates an R Markdown PDF with CSAS formatting
#'
#' This is a function called in output in the YAML of the driver Rmd file
#' to specify using the CSAS LaTeX template and cls files.
#'
#' @export
#' @param toc A Boolean (`TRUE` or `FALSE`) specifying whether table of contents
#'   should be created.
#' @param toc_depth A positive integer.
#' @param highlight Syntax highlighting style. Supported styles include
#'   "default", "tango", "pygments", "kate", "monochrome", "espresso",
#'   "zenburn", and "haddock". Pass `NULL` to prevent syntax highlighting.
#' @param latex_engine LaTeX engine.
#' @param french Logical for French (vs. English).
#' @param ... other arguments to [bookdown::pdf_book()].
#' @return A modified `pdf_document` based on the CSAS LaTeX template.
#' @import bookdown
#' @rdname csas_pdf
#' @examples
#' \dontrun{
#'  output: csasdown::resdoc_pdf
#' }
resdoc_pdf <- function(toc = TRUE, toc_depth = 3, highlight = "default",
                       latex_engine = "pdflatex", french = FALSE, ...) {

  if (french) {
    file <- system.file("csas-tex", "res-doc-french.tex", package = "csasdown")
  } else {
    file <- system.file("csas-tex", "res-doc.tex", package = "csasdown")
  }

  base <- bookdown::pdf_book(
    template = file,
    toc = toc,
    toc_depth = toc_depth,
    highlight = highlight,
    keep_tex = TRUE,
    pandoc_args = c("--top-level-division=chapter", "--wrap=none"),
    latex_engine = latex_engine,
    ...
  )
  update_csasstyle()

  # Mostly copied from knitr::render_sweave
  base$knitr$opts_chunk$comment <- NA
  # base$knitr$opts_chunk$fig.align <- "center"

  old_opt <- getOption("bookdown.post.latex")
  options(bookdown.post.latex = fix_envs)
  on.exit(options(bookdown.post.late = old_opt))

  base
}

#' Creates an R Markdown Word CSAS-formatted document
#'
#' This is a function called in output in the YAML of the driver Rmd file
#' to specify the creation of a Microsoft Word version of the Research
#' Document or Science Response.
#'
#' @param french Logical for French (vs. English).
#' @param ... other arguments to [bookdown::word_document2()]
#' @import bookdown
#' @rdname csas_docx
#' @export
#' @return A Word Document based on the CSAS Res Doc template.
resdoc_word <- function(french = FALSE, ...) {
  file <- if (french) "RES2016-fra.docx" else "RES2016-eng-content-only.docx"
  base <- word_document2(...,
    reference_docx = system.file("csas-docx", file, package = "csasdown")
  )

  # Mostly copied from knitr::render_sweave
  base$knitr$opts_chunk$comment <- NA
  base$knitr$opts_chunk$fig.align <- "center"
  base
}

#' @export
#' @rdname csas_pdf
sr_pdf <- function(latex_engine = "pdflatex", ...) {
  base <- bookdown::pdf_book(
    template = system.file("csas-tex", "sr.tex", package = "csasdown"),
    keep_tex = TRUE,
    pandoc_args = c("--top-level-division=chapter", "--wrap=none"),
    latex_engine = latex_engine,
    ...
  )
  update_csasstyle()
  base$knitr$opts_chunk$comment <- NA
  old_opt <- getOption("bookdown.post.latex")
  options(bookdown.post.latex = fix_envs)
  on.exit(options(bookdown.post.late = old_opt))
  base
}

#' @export
#' @rdname csas_docx
sr_word <- function(french = FALSE, ...) {
  file <- if (french) "SRR-RS2016-fra.docx" else "SRR-RS2016-eng.docx"
  base <- word_document2(...,
    reference_docx = system.file("csas-docx", file, package = "csasdown")
  )
  base$knitr$opts_chunk$comment <- NA
  base$knitr$opts_chunk$fig.align <- "center"
  base
}

#' @export
#' @rdname csas_docx
techreport_word <- function(french = FALSE, ...) {
  file <- if (french) "PRO-CR2016-fra.docx" else "PRO-CR2016-eng.docx"
  base <- word_document2(...,
    reference_docx = system.file("csas-docx", file, package = "csasdown")
  )
  base$knitr$opts_chunk$comment <- NA
  base$knitr$opts_chunk$fig.align <- "center"
  base
}


#' @export
#' @rdname csas_pdf
techreport_pdf <- function(latex_engine = "pdflatex", ...) {
  base <- bookdown::pdf_book(
    template = system.file("csas-tex", "tech-report.tex", package = "csasdown"),
    keep_tex = TRUE,
    pandoc_args = c("--top-level-division=chapter", "--wrap=none"),
    latex_engine = latex_engine,
    ...
  )

  update_csasstyle()
  base$knitr$opts_chunk$comment <- NA
  old_opt <- getOption("bookdown.post.latex")
  options(bookdown.post.latex = fix_envs)
  on.exit(options(bookdown.post.late = old_opt))
  base
}

update_csasstyle <- function() {
 f <- system.file("csas-style", package = "csasdown")
 dir.create("csas-style", showWarnings = FALSE)
 ignore <- file.copy(f, ".", overwrite = TRUE, recursive = TRUE)
}

fix_envs <- function(x) {
  ## Change csas-style to use the sty file found in csasdown repo
  g <- grep("csas-style", x)

  ## Find beginning and end of the abstract text
  abs_beg <- grep("begin_abstract_csasdown", x)
  abs_end <- grep("end_abstract_csasdown", x)
  if (length(abs_beg) == 0L || length(abs_end) == 0L) {
    warning("`% begin_abstract_csasdown` or `% end_abstract_csasdown`` not found ",
      "in `templates/csas.tex`", call. = FALSE)
  } else {
    abs_vec <- x[seq(abs_beg + 1, abs_end - 1)]
    abs_vec <- abs_vec[abs_vec != ""]
    abstract <- paste(abs_vec, collapse = " \\break \\break ")
    first_part <- x[seq_len(abs_beg - 1)]
    second_part <- x[seq(abs_end + 1, length(x))]
    x <- c(first_part, abstract, second_part)
  }

  beg_reg <- "^\\s*\\\\begin\\{.*\\}"
  end_reg <- "^\\s*\\\\end\\{.*\\}"
  i3 <- if (length(i1 <- grep(beg_reg, x))) (i1 - 1)[grepl("^\\s*$", x[i1 - 1])]

  i3 <- c(
    i3,
    if (length(i2 <- grep(end_reg, x))) (i2 + 1)[grepl("^\\s*$", x[i2 + 1])]
  )
  if (length(i3)) x <- x[-i3]

  g <- grep("\\\\Appendices$", x)
  if (identical(length(g), 0L)){
    appendix_line <- length(x) - 1 # no appendix
  }else{
    appendix_line <- min(g)
  }

  for (i in seq(1, appendix_line)) {
    x[i] <- gsub("\\\\subsection\\{", "\\\\subsubsection\\{", x[i])
    x[i] <- gsub("\\\\section\\{", "\\\\subsection\\{", x[i])
    x[i] <- gsub("\\\\chapter\\{", "\\\\section\\{", x[i])
  }

  for (i in seq(appendix_line + 1, length(x))) {
    x[i] <- gsub("\\\\section\\{", "\\\\appsection\\{", x[i])
    x[i] <- gsub("\\\\chapter\\{",
      "\\\\starredchapter\\{APPENDIX~\\\\thechapter. ", x[i])
  }
  x <- inject_refstepcounters(x)

  # Need to remove hypertarget four references to appendices to work:
  # rs_line <- grep("\\\\refstepcounter", x)
  # FIXME: make more robust
  rs_line <- grep("\\\\hypertarget\\{app:", x)
  x[rs_line + 0] <- gsub("hypertarget", 'label', x[rs_line + 0])
  x[rs_line + 0] <- gsub("\\{%", '', x[rs_line + 0])
  x[rs_line + 1] <- gsub("\\}$", '', x[rs_line + 1])
  x[rs_line + 1] <- gsub("\\}.*\\}$", "}", x[rs_line + 1])

  x <- gsub("itemize\\}", "resdoclist\\}", x)
  x <- gsub("enumerate\\}", "resdoclist\\}", x)

  x <- gsub("^\\\\tightlist$", "", x)
  # Non-breaking spaces:
  x <- gsub(" \\\\ref\\{", "~\\\\ref\\{", x)

  x
}

inject_refstepcounters <- function(x) {
  chpts <- grep("^\\\\starredchapter\\{", x)
  for (i in chpts) {
    x <- c(
      x[seq(1, i - 3)],
      paste0(x[i - 2], "\n\n\\clearpage\n\n\\refstepcounter{chapter}"),
      x[seq(i - 1, length(x))])
  }
  x
}

#' Add a Res Doc titlepage to a docx file
#'
#' Add a Res Doc titlepage. Must hand edit `templates/RES2016-eng-titlepage.docx`
#' to have your desired title and authors etc.
#'
#' @param titlepage Filename
#' @param resdoc Filename
#'
#' @return A merged .docx
#' @export
add_resdoc_docx_titlepage <- function(titlepage = "templates/RES2016-eng-titlepage.docx",
                                 resdoc = "_book/resdoc.docx") {
  title_doc <- officer::read_docx(titlepage)
  x <- officer::body_add_docx(title_doc, resdoc, pos = "before")
  print(x, target = resdoc)
}

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

#' Check to make sure index.Rmd contains all current YAML options
#'
#' As the csasdown package is updated, sometimes new mandatory YAML options are added
#' to the `index.Rmd` file. Running this function will compare your file to the
#' version built into the currently installed version of csasdown and issue
#' a `stop()` statement telling you what doesn't match if needed.
#'
#' @param type Type of document. Currently this is only implemented for research
#'   documents.
#'
#' @export
check_yaml <- function(type = "resdoc") {
  x_skeleton <- names(rmarkdown::yaml_front_matter(
    system.file("rmarkdown", "templates", "resdoc", "skeleton", "skeleton.Rmd",
      package = "csasdown")))
  x_index <- names(rmarkdown::yaml_front_matter("index.Rmd"))
  .diff <- setdiff(x_skeleton, x_index)
  if (length(.diff) > 0L) {
    stop("Your `index.Rmd` file is missing: ", paste(.diff, collapse = ", "), ".")
  } else {
    message("Your `index.Rmd` file contains all necessary YAML options.")
  }
}
