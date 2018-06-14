#' Creates an R Markdown PDF Res Doc
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
#' @examples
#' \dontrun{
#'  output: csasdown::resdoc_pdf
#' }
resdoc_pdf <- function(toc = TRUE, toc_depth = 3, highlight = "default",
                       latex_engine = "xelatex", ...) {
  base <- bookdown::pdf_book(
    template = "templates/csas.tex",
    toc = toc,
    toc_depth = toc_depth,
    highlight = highlight,
    keep_tex = TRUE,
    pandoc_args = "--top-level-division=chapter",
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

#' Creates an R Markdown gitbook Res Doc
#'
#' This is a function called in output in the YAML of the driver Rmd file
#' to specify the creation of a webpage version of the resdoc.
#'
#' @param ... other arguments to [bookdown::gitbook()]
#' @export
#' @return A gitbook webpage
#' @import bookdown
resdoc_gitbook <- function(...) {
  base <- gitbook(
    split_by = "chapter+number",
    config = list(toc = list(
      collapse = "section",
      before = '<li><a href="./"></a></li>',
      after = '<li><a href="https://github.com/rstudio/bookdown" target="blank">Published with bookdown</a></li>',
      ...
    ))
  )
  # Mostly copied from knitr::render_sweave
  base$knitr$opts_chunk$comment <- NA
  base$knitr$opts_chunk$fig.align <- "center"
  base
}

#' Creates an R Markdown Word Res Doc
#'
#' This is a function called in output in the YAML of the driver Rmd file
#' to specify the creation of a Microsoft Word version of the resdoc.
#' @param ... other arguments to [bookdown::word_document2()]
#' @import bookdown
#' @export
#' @return A Word Document based on the CSAS Res Doc template.
resdoc_word <- function(...) {
  base <- word_document2(...,
    reference_docx = "templates/RES2016-eng-content-only.docx"
  )

  # Mostly copied from knitr::render_sweave
  base$knitr$opts_chunk$comment <- NA
  base$knitr$opts_chunk$fig.align <- "center"
  base
}

#' Creates an R Markdown epub Res Doc
#'
#' This is a function called in output in the YAML of the driver Rmd file
#' to specify the creation of a epub version of the resdoc.
#'
#' @param ... other arguments to [bookdown::epub_book()]
#' @import bookdown
#' @export
#' @return A ebook version of the resdoc. Not formatted to CSAS standards.
resdoc_epub <- function(...) {
  base <- epub_book(...)

  # Mostly copied from knitr::render_sweave
  base$knitr$opts_chunk$comment <- NA
  base$knitr$opts_chunk$fig.align <- "center"
  base
}

fix_envs <- function(x) {
  beg_reg <- "^\\s*\\\\begin\\{.*\\}"
  end_reg <- "^\\s*\\\\end\\{.*\\}"
  i3 <- if (length(i1 <- grep(beg_reg, x))) (i1 - 1)[grepl("^\\s*$", x[i1 - 1])]

  i3 <- c(
    i3,
    if (length(i2 <- grep(end_reg, x))) (i2 + 1)[grepl("^\\s*$", x[i2 + 1])]
  )
  if (length(i3)) x <- x[-i3]

  appendix_line <- min(grep("^\\\\Appendices$", x))

  for (i in seq(1, appendix_line)) {
    x[i] <- gsub("^\\\\subsection\\{", "\\\\subsubsection\\{", x[i])
    x[i] <- gsub("^\\\\section\\{", "\\\\subsection\\{", x[i])
    x[i] <- gsub("^\\\\chapter\\{", "\\\\section\\{", x[i])
  }

  # uppercase sections:
  x <- gsub("(section\\{)([a-zA-Z0-9\\.\\-\\'\\?\\!\\,]*)(\\})",
    "\\1\\U\\2\\3", x, perl = TRUE)
  x <- gsub("(^\\\\chapter\\{)([a-zA-Z0-9\\.\\-\\'\\?\\!\\,]*)(\\})",
    "\\1\\U\\2\\3", x, perl = TRUE)

  for (i in seq(appendix_line + 1, length(x))) {
    # x[i] <- gsub("^\\\\subsubsection\\{", "\\\\appsubsubsection\\{", x[i])
    # x[i] <- gsub("^\\\\subsection\\{", "\\\\appsubsection\\{", x[i])
    x[i] <- gsub("^\\\\section\\{", "\\\\appsection\\{", x[i])
    x[i] <- gsub("^\\\\chapter\\{",
      "\\\\starredchapter\\{APPENDIX~\\\\thechapter. ", x[i])
  }
  x <- inject_refstepcounters(x)

  x <- gsub("itemize\\}", "resdoclist\\}", x)

  x
}

inject_refstepcounters <- function(x) {
  chpts <- grep("^\\\\starredchapter\\{", x)
  for (i in chpts) {
    x <- c(
      x[seq(1, i - 2)],
      "\\refstepcounter{chapter}",
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

  ok <- menu(c("Yes", "No"),
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

  ok <- menu(c("Continue", "Quit"),
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

  ok <- menu(c("Continue", "Quit"),
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
