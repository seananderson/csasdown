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
  if (french)
    options(bookdown.post.latex = fix_envs_resdoc_french)
  else
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
sr_pdf <- function(latex_engine = "pdflatex", french = FALSE, ...) {

  if (french) {
    file <- system.file("csas-tex", "sr-french.tex", package = "csasdown")
  } else {
    file <- system.file("csas-tex", "sr.tex", package = "csasdown")
  }

  base <- bookdown::pdf_book(
    template = file,
    keep_tex = TRUE,
    pandoc_args = c("--top-level-division=chapter", "--wrap=none"),
    latex_engine = latex_engine,
    ...
  )
  update_csasstyle()

  base$knitr$opts_chunk$comment <- NA
  old_opt <- getOption("bookdown.post.latex")

  if (french)
    options(bookdown.post.latex = fix_envs_sr_french)
  else
    options(bookdown.post.latex = fix_envs_sr)
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
techreport_pdf <- function(french = FALSE, latex_engine = "pdflatex", ...) {

  if (french) {
    file <- system.file("csas-tex", "tech-report-french.tex", package = "csasdown")
  } else {
    file <- system.file("csas-tex", "tech-report.tex", package = "csasdown")
  }
  base <- bookdown::pdf_book(
    template = file,
    keep_tex = TRUE,
    pandoc_args = c("--top-level-division=chapter", "--wrap=none"),
    latex_engine = latex_engine,
    ...
  )

  update_csasstyle()

  base$knitr$opts_chunk$comment <- NA
  old_opt <- getOption("bookdown.post.latex")
  if (french)
	options(bookdown.post.latex = fix_envs_tr_french)
  else
	options(bookdown.post.latex = fix_envs_tr)
  on.exit(options(bookdown.post.late = old_opt))
  base
}

update_csasstyle <- function() {
 f <- system.file("csas-style", package = "csasdown")
 dir.create("csas-style", showWarnings = FALSE)
 ignore <- file.copy(f, ".", overwrite = TRUE, recursive = TRUE)
}

fix_envs_sr <- function(x) {
  fix_envs(x, include_abstract = FALSE, join_abstract = FALSE)
}

fix_envs_sr_french <- function(x) {
  fix_envs(x, include_abstract = FALSE, join_abstract = FALSE, french = TRUE)
}

fix_envs_tr <- function(x) {
  fix_envs(x, join_abstract = FALSE)
}

fix_envs_tr_french <- function(x) {
  fix_envs(x, join_abstract = FALSE, french = TRUE)
}

fix_envs_resdoc_french <- function(x) {
  fix_envs(x, join_abstract = TRUE, french = TRUE)
}

fix_envs <- function(x,
                     include_abstract = TRUE,
                     join_abstract = TRUE,
                     french = FALSE) {

  # Switch up email for some regions:
  pac_region <- grepl("rdRegion\\}\\{Pacific Region}$", x)
  if (length(pac_region) > 0) {
    x <- gsub("mailto:csas-sccs@dfo-mpo.gc.ca\\}\\{csas-sccs@dfo-mpo.gc.ca\\}",
      "mailto:csap@dfo-mpo.gc.ca\\}\\{csap@dfo-mpo.gc.ca\\}", x)
  }
  # FIXME: if we want to go this route we should add the other regions

  ## Change csas-style to use the sty file found in csasdown repo
  g <- grep("csas-style", x)

  ## Find beginning and end of the abstract text is it is not a Science Response document
  if(include_abstract){
    abs_beg <- grep("begin_abstract_csasdown", x)
    abs_end <- grep("end_abstract_csasdown", x)
    if (join_abstract) {
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
    }
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
    if (!french) {
      x[i] <- gsub("\\\\chapter\\{",
        "\\\\starredchapter\\{APPENDIX~\\\\thechapter. ", x[i])
    } else {
      x[i] <- gsub("\\\\chapter\\{",
        "\\\\starredchapter\\{ANNEXE~\\\\thechapter. ", x[i])
    }
  }
  x <- inject_refstepcounters(x)

  # Need to remove hypertarget for references to appendices to work:
  # rs_line <- grep("\\\\refstepcounter", x)
  # FIXME: make more robust
  rs_line <- grep("\\\\hypertarget\\{app:", x)
  x[rs_line + 0] <- gsub("hypertarget", 'label', x[rs_line + 0])
  x[rs_line + 0] <- gsub("\\{%", '', x[rs_line + 0])
  x[rs_line + 1] <- gsub("\\}$", '', x[rs_line + 1])
  x[rs_line + 1] <- gsub("\\}.*\\}$", "}", x[rs_line + 1])

  x <- gsub("^.*\\\\tightlist$", "", x)

  # Non-breaking spaces:
  x <- gsub(" \\\\ref\\{", "~\\\\ref\\{", x)

  # ----------------------------------------------------------------------
  # Add tooltips so that figures have alternative text for read-out-loud
  figlabel_lines <- x[grep("\\\\label\\{fig:", x)]
  fig_labels <- gsub("\\\\caption\\{(.*?)\\}\\\\label\\{fig:(.*?)\\}",
    "\\2", figlabel_lines)
  all_include_graphics <- grep("(\\\\includegraphics\\[(.*?)\\]\\{(.*?)\\})", x)

  # is this a true figure with a caption in Pandoc style?
  all_include_graphics <-
    all_include_graphics[grep("\\\\centering", x[all_include_graphics])]

  if (identical(length(fig_labels), length(all_include_graphics))) {
    for (i in seq_along(all_include_graphics)) {
      x[all_include_graphics[i]] <-
        gsub("(\\\\includegraphics\\[(.*?)\\]\\{(.*?)\\})",
        paste0("\\\\pdftooltip{\\1}{", "Figure \\\\ref{fig:", fig_labels[i], "}}"),
          x[all_include_graphics[i]])
    }
  } else {
    warning("The number of detected figure captions did not match the number of ",
    "detected figures. Reverting to unnumbered alternative text figures.",
      call. = FALSE)
    x <- gsub("(\\\\includegraphics\\[(.*?)\\]\\{(.*?)\\})",
      "\\\\pdftooltip{\\1}{Figure}", x)
  }
  # ----------------------------------------------------------------------

  regexs <- c(
    "^\\\\CHAPTER\\*\\{R\\p{L}F\\p{L}RENCES", # French or English
    "^\\\\SECTION{SOURCES DE RENSEIGNEMENTS}",
    "^\\\\SECTION{SOURCES OF INFORMATION}")
  .matches <- lapply(regexs, function(.x) grep(.x, toupper(x), perl = TRUE) + 1)
  references_insertion_line <- unlist(.matches)

  x[references_insertion_line - 1] <- sub("chapter", "section", x[references_insertion_line - 1])
  x[references_insertion_line] <- sub("chapter", "section", x[references_insertion_line])

  # Move the bibliography to before the appendices:
  if (length(references_insertion_line) > 0) {
    references_begin <- grep("^\\\\hypertarget\\{refs\\}\\{\\}$", x)
    if (length(references_begin) > 0) {
      references_end <- length(x) - 1
      x <- c(x[seq(1, references_insertion_line - 1)],
        "\\phantomsection",
        x[references_insertion_line],
        "% This manually sets the header for this unnumbered chapter.",
        # "\\markboth{References}{References}",
        "\\noindent",
        "\\vspace{-2em}",
        "\\setlength{\\parindent}{-0.2in}",
        "\\setlength{\\leftskip}{0.2in}",
        "\\setlength{\\parskip}{8pt}",
        "",
        x[seq(references_begin, references_end)],
        "\\setlength{\\parindent}{0in} \\setlength{\\leftskip}{0in} \\setlength{\\parskip}{4pt}",
        x[seq(references_insertion_line + 1, references_begin - 1)],
        x[length(x)])
    } else {
      warning("Did not find the beginning of the LaTeX bibliography.", call. = FALSE)
    }
  }

  # Tech Report Appendices:
  x <- gsub("\\% begin csasdown appendix",
    paste0(
      "\\begin{appendices}\n",
      "\\\\counterwithin{figure}{section}\n",
      "\\\\counterwithin{table}{section}\n",
      "\\\\counterwithin{equation}{section}"),
    x
  )
  x <- gsub("\\% end csasdown appendix", "\\end{appendices}", x)

  label_app <- grep("^\\\\label\\{app:", x)
  for (i in seq_along(label_app)) {
    if (grepl("^\\\\section\\{", x[label_app[i] + 1])) {
      x[seq(label_app[i], label_app[i] + 1)] <- x[seq(label_app[i] + 1, label_app[i])]
    }
  }

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

#' Insert regional CSAS email address to contact for information
#'
#' @param region Region in which the document is published. Character vector
#' (i.e., Pacific)
#' @param type Type of document. Currently this is only implemented for Science
#' Response documents.
#'
#' @importFrom tibble tribble
#'
#' @export
#'
#' @return Email address as a character vector
get_email <- function( region, type="sr" ) {
  # Create a table with region name, region name if french, and email address
  dat <- tribble( ~Region, ~RegionFr, ~Email,
    "National Capital Region", "Région de la capitale nationale", "csas-sccs@dfo-mpo.gc.ca",
    "Pacific Region", "Région du Pacifique", "csap@dfo-mpo.gc.ca",
    "Newfoundland and Labrador Region", "Région de Terre-Neuve et Labrador", "DFONLCentreforScienceAdvice@dfo-mpo.gc.ca",
    "Quebec", "Québec", "bras@dfo-mpo.gc.ca",
    "Gulf Region", "Région du Golfe", "Gerald.Chaput@dfo-mpo.gc.ca",
    "Maritimes Region", "Région des Maritimes", "XMARMRAP@dfo-mpo.gc.ca",
    "Central and Arctic Region", "Centre & Arctique", "xcna-csa-cas@dfo-mpo.gc.ca" )
  # If french
  if( french ) {
    # Get index for region (row)
    ind <- which( dat$RegionFr == region )
  } else{  # End if french, otherwise
    # Get index for region (row)
    ind <- which( dat$Region == region )
  }  # End if not french
  # If region not detected
  if( length(ind) == 0 ) {
    # Use national email address
    email <- "csas-sccs@dfo-mpo.gc.ca"
    # Warning
    warning( "Region not detected; use national CSAS email" )
  } else {  # End if no region, otherwise get email
    # Get email address
    email <- dat$Email[ind]
  }  # End if region detected
  # Return email address
  return( email )
}  # End get_email

