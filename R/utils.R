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
#' @param ... other arguments to [bookdown::pdf_book()].
#' @return A modified `pdf_document` based on the CSAS LaTeX template.
#' @import bookdown
#' @rdname csas_pdf
#' @examples
#' \dontrun{
#'  output: csasdown::resdoc_pdf
#' }
resdoc_pdf <- function(toc = TRUE, toc_depth = 3, highlight = "default",
                       latex_engine = "pdflatex", ...) {
  base <- bookdown::pdf_book(
    template = system.file("csas-tex", "res-doc.tex", package = "csasdown"),
    toc = toc,
    toc_depth = toc_depth,
    highlight = highlight,
    keep_tex = TRUE,
    pandoc_args = c("--top-level-division=chapter", "--wrap=none"),
    latex_engine = latex_engine,
    ...
  )

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
#' @param ... other arguments to [bookdown::word_document2()]
#' @import bookdown
#' @rdname csas_docx
#' @export
#' @return A Word Document based on the CSAS Res Doc template.
resdoc_word <- function(...) {
  base <- word_document2(...,
    reference_docx = system.file("csas-docx", "RES2016-eng-content-only.docx",
      package = "csasdown")
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
  base$knitr$opts_chunk$comment <- NA
  old_opt <- getOption("bookdown.post.latex")
  options(bookdown.post.latex = fix_envs)
  on.exit(options(bookdown.post.late = old_opt))
  base
}

#' @export
#' @rdname csas_docx
sr_word <- function(...) {
  base <- word_document2(...,
    reference_docx = system.file("csas-docx", "RES2016-eng-content-only.docx",
      package = "csasdown")
  )
  base$knitr$opts_chunk$comment <- NA
  base$knitr$opts_chunk$fig.align <- "center"
  base
}

fix_envs <- function(x) {
  ## Change csas-style to use the sty file found in csasdown repo
  g <- grep("csas-style", x)
  x[g] <- gsub("csas-style",
               system.file("csas-style", package = "csasdown"),
               x[g])

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
  # Non-breaking spaces:
  x <- gsub(" \\\\ref\\{", "~\\\\ref\\{", x)

  x
}

inject_refstepcounters <- function(x) {
  chpts <- grep("^\\\\starredchapter\\{", x)
  for (i in chpts) {
    x <- c(
      x[seq(1, i - 3)],
      paste0(x[i - 2], "\\clearpage\n\\refstepcounter{chapter}"),
      x[seq(i - 1, length(x))])
  }
  x
}

#' Add Res Doc titlepage
#'
#' @param titlepage Filename
#' @param resdoc Filename
#'
#' @return A merged .docx
#' @export
add_resdoc_titlepage <- function(titlepage = "templates/RES2016-eng-titlepage.docx",
                                 resdoc = "_book/resdoc.docx") {
  title_doc <- officer::read_docx(titlepage)
  x <- officer::body_add_docx(title_doc, resdoc, pos = "before")
  print(x, target = resdoc)
}

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

#' Add Arial LaTeX fonts for Windows users
#'
#' @export
#' @examples
#' \dontrun{
#' add_arial()
#' }
add_arial <- function() {
  if (!is_windows()) {
    warning("This function is only for use on Windows machines.")
    return(invisible())
  }

  arial_folder <- system.file("texlocal", package = "csasdown")
  arial_folder_to <- file.path("C:/texlocal")

  ok <- utils::menu(c("Yes", "No"),
    title = "OK to copy LaTeX font files to your computer?"
  ) == 1

  if (!ok) {
    return(invisible())
  }

  if (!file.exists(arial_folder_to)) {
    file.copy(arial_folder, "C:/", recursive = TRUE, overwrite = FALSE)
  } else {
    warning("We detected an existing folder at C:\\texlocal.\n",
      "Either manually copy the Arial font files into C:\\texlocal",
      "or rename/delete your C:\\texlocal and run this function again.")
  }

  ok <- utils::menu(c("Continue", "Quit"),
    title = paste0(
      "Please do the following:\n\n",
      "1. Go to Start->Programs->Miktex->",
      "Maintenance(Admin)->MikTex Settings(Admin)\n",
      "2. Go to the 'Roots' tab\n",
      "3. Click 'Add' and add C:\\texlocal\n",
      "4. Go to 'General' tab\n",
      "5. Click 'Refresh FNDB' button\n",
      "6. Click 'Update Formats' button\n",
      "7. Click 'OK'\n",
      "When done, press 1 to continue."
    )
  ) == 1

  if (!ok) {
    return(invisible())
  }

  system("initexmf --admin --update-fndb")

  ok <- utils::menu(c("Continue", "Quit"),
    title = paste0(
      "Please paste the following into the editor ",
      "that opened:\n",
      "Map ua1.map\n\n",
      "Then save and close the file.\n",
      "When done, press 1 to continue."
    )
  ) == 1

  if (!ok) {
    return(invisible())
  }

  system("initexmf --mkmaps")
}

