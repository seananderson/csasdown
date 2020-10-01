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
#' @param prepub Logical for whether this is a prepublication version
#'  (currently not implemented for ResDocs)
#' @param copy_sty Copy the .sty files every time? Set to `FALSE` to "freeze" the
#'   .sty file if you need to edit it.
#' @param line_nums Include line numbers in the document? Logical.
#' @param line_nums_mod Numerical. Which modulo line numbers to label, 2 = every second line, etc.
#' @param pandoc_args Any other arguments to pandoc.
#' @param ... other arguments to [bookdown::pdf_book()].
#' @return A modified `pdf_document` based on the CSAS LaTeX template.
#' @import bookdown
#' @rdname csas_pdf
#' @examples
#' \dontrun{
#' output:csasdown::resdoc_pdf
#' }
resdoc_pdf <- function(toc = TRUE, toc_depth = 3, highlight = "default",
                       latex_engine = "pdflatex", french = FALSE,
                       prepub = FALSE, copy_sty = TRUE,
                       line_nums = FALSE, line_nums_mod = 1,
                       pandoc_args = c("--top-level-division=chapter", "--wrap=none", "--default-image-extension=png"),
                       ...) {
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
    pandoc_args = pandoc_args,
    latex_engine = latex_engine,
    ...
  )

  if(!class(line_nums_mod) %in% c("integer", "numeric")){
    stop("line_nums_mod must be a numeric or integer value.", call. = FALSE)
  }
  update_csasstyle(copy = copy_sty,
                   line_nums = line_nums,
                   line_nums_mod = line_nums_mod,
                   which_sty = ifelse(french, "res-doc-french.sty", "res-doc.sty"))

  # Mostly copied from knitr::render_sweave
  base$knitr$opts_chunk$comment <- NA
  # base$knitr$opts_chunk$fig.align <- "center"

  old_opt <- getOption("bookdown.post.latex")
  options(bookdown.post.latex = function(x) {
    fix_envs(
      x = x,
      french = french,
      prepub = prepub,
      include_abstract = TRUE,
      join_abstract = TRUE
    )
  })
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
sr_pdf <- function(latex_engine = "pdflatex", french = FALSE, prepub = FALSE,
                   copy_sty = TRUE,
                   line_nums = FALSE, line_nums_mod = 1,
                   pandoc_args = c("--top-level-division=chapter", "--wrap=none", "--default-image-extension=png"),
                   ...) {
  if (french) {
    file <- system.file("csas-tex", "sr-french.tex", package = "csasdown")
  } else {
    file <- system.file("csas-tex", "sr.tex", package = "csasdown")
  }

  base <- bookdown::pdf_book(
    template = file,
    keep_tex = TRUE,
    pandoc_args = pandoc_args,
    latex_engine = latex_engine,
    ...
  )
  update_csasstyle(copy = copy_sty,
                   line_nums = line_nums,
                   line_nums_mod = line_nums_mod,
                   which_sty = ifelse(french, "sr-french.sty", "sr.sty"))

  base$knitr$opts_chunk$comment <- NA
  old_opt <- getOption("bookdown.post.latex")

  options(bookdown.post.latex = function(x) {
    fix_envs(
      x = x,
      french = french,
      prepub = prepub,
      include_abstract = FALSE,
      join_abstract = FALSE
    )
  })

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
  file <- ifelse(french, "tech-report-cover-fra.docx", "tech-report-cover-eng.docx")
  base <- word_document2(...,
    reference_docx = system.file("csas-docx", file, package = "csasdown")
  )
  base$knitr$opts_chunk$comment <- NA
  base$knitr$opts_chunk$fig.align <- "center"
  base
}

#' @export
#' @rdname csas_pdf
techreport_pdf <- function(french = FALSE, latex_engine = "pdflatex",
                           copy_sty = TRUE,
                           line_nums = FALSE, line_nums_mod = 1,
                           pandoc_args = c("--top-level-division=chapter", "--wrap=none", "--default-image-extension=png"), ...) {
  if (french) {
    file <- system.file("csas-tex", "tech-report-french.tex", package = "csasdown")
  } else {
    file <- system.file("csas-tex", "tech-report.tex", package = "csasdown")
  }

  base <- bookdown::pdf_book(
    template = file,
    keep_tex = TRUE,
    pandoc_args = pandoc_args,
    latex_engine = latex_engine,
    ...
  )

  cover_file_pdf <- if (french) "tech-report-cover-french.pdf" else "tech-report-cover.pdf"
  cover_file_docx <- if (french) "tech-report-cover-french.docx" else "tech-report-cover.docx"
  if (!file.exists(cover_file_pdf)) {
    cover_docx <- system.file("rmarkdown", "templates", "techreport", "skeleton", cover_file_docx, package = "csasdown")
    cover_pdf <- system.file("rmarkdown", "templates", "techreport", "skeleton", cover_file_pdf, package = "csasdown")
    warning("Missing the Tech Report cover page. Copying in the files...", call. = FALSE)
    file.copy(cover_docx, ".", overwrite = FALSE)
    file.copy(cover_pdf, ".", overwrite = FALSE)
  }
  update_csasstyle(copy = copy_sty,
                   line_nums = line_nums,
                   line_nums_mod = line_nums_mod,
                   which_sty = ifelse(french, "tech-report-french.sty", "tech-report.sty"))

  base$knitr$opts_chunk$comment <- NA
  old_opt <- getOption("bookdown.post.latex")
  options(bookdown.post.latex = function(x) {
    fix_envs(
      x = x,
      french = french,
      join_abstract = FALSE
    )
  })
  on.exit(options(bookdown.post.late = old_opt))
  base
}

#' Copy the csas-style directory from the local library location to
#' the current directory, overwriting and edit the style file if necessary
#'
#' @param copy Logical. If TRUE, copy and overwrite if the directory already exists.
#' If FALSE, only copy if the directory does not exist in the current directory
#' @param line_nums Logical. Include line numbering in the document
#' @param line_nums_mod Numerical. Which modulo line numbers to label, 2 = every second line, etc.
#' @param which_sty Name of the style file to modify
#'
#' @return Nothing
update_csasstyle <- function(copy = TRUE, line_nums = TRUE, line_nums_mod = 1, which_sty = "res-doc.sty") {
  fn <- system.file("csas-style", package = "csasdown")
  if (copy || (!dir.exists("csas-style") && !copy)) {
    dir.create("csas-style", showWarnings = FALSE)
    ignore <- file.copy(fn, ".", overwrite = TRUE, recursive = TRUE)
  }
  if(line_nums){
    csas_style <- readLines(here::here("csas-style", which_sty))
    if(length(grep("res-doc", which_sty))){
      frontmatter_loc <- grep("frontmatter\\{", csas_style)
      beg_of_file <- csas_style[1:(frontmatter_loc - 1)]
      end_of_file <- csas_style[frontmatter_loc:length(csas_style)]
      modulo <- paste0("\\modulolinenumbers[", line_nums_mod, "]")
      csas_style <- c(beg_of_file, "\\linenumbers", modulo, end_of_file)
      writeLines(csas_style, here::here("csas-style", which_sty))
    }else{
      modulo <- paste0("\\modulolinenumbers[", line_nums_mod, "]")
      csas_style <- c(csas_style, "\\linenumbers", modulo)
      writeLines(csas_style, here::here("csas-style", which_sty))
    }
  }
}

fix_envs <- function(x,
                     include_abstract = TRUE,
                     join_abstract = TRUE,
                     french = FALSE,
                     prepub = FALSE) {


  # fix equations:
  x <- gsub("^\\\\\\[$", "\\\\begin{equation}", x)
  x <- gsub("^\\\\\\]$", "\\\\end{equation}", x)
  x <- gsub("^\\\\\\]\\.$", "\\\\end{equation}.", x)
  x <- gsub("^\\\\\\],$", "\\\\end{equation},", x)

  # Get region line
  region_line <- grep(pattern = "% Region", x) + 1
  # If region is specified (currently only SRs)
  if (length(region_line) > 0) {
    # Get region name and contact info
    region <- regmatches(
      x = x[region_line],
      m = regexec(
        pattern = "\\\\rdRegion\\}\\{(.*?)\\}+$",
        text = x[region_line]
      )
    )[[1]][2]
    contact_info <- get_contact_info(region = region, isFr = french)
    # Insert contact info
    x <- sub(
      pattern = "AddressPlaceholder", replacement = contact_info$address,
      x = x
    )
    x <- sub(pattern = "PhonePlaceholder", replacement = contact_info$phone, x = x)
    x <- sub(
      pattern = "EmailPlaceholder",
      replacement = paste0(
        "\\\\link\\{mailto:", contact_info$email,
        "\\}\\{", contact_info$email, "\\}"
      ), x = x
    )
  } # End if region exists (SRs)
  ## Change csas-style to use the sty file found in csasdown repo
  g <- grep("csas-style", x)

  ## Find beginning and end of the abstract text is it is not a Science Response document
  if (include_abstract) {
    abs_beg <- grep("begin_abstract_csasdown", x)
    abs_end <- grep("end_abstract_csasdown", x)
    if (join_abstract) {
      if (length(abs_beg) == 0L || length(abs_end) == 0L) {
        warning("`% begin_abstract_csasdown` or `% end_abstract_csasdown`` not found ",
          "in `templates/csas.tex`",
          call. = FALSE
        )
      } else {
        abs_vec <- x[seq(abs_beg + 1, abs_end - 1)]
        abs_vec <- abs_vec[abs_vec != ""]
        abstract <- paste(abs_vec, collapse = " \\vspace{1.5mm} \\break ")
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
  if (identical(length(g), 0L)) {
    appendix_line <- length(x) - 1 # no appendix
  } else {
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
      x[i] <- gsub(
        "\\\\chapter\\{",
        "\\\\starredchapter\\{APPENDIX~\\\\thechapter. ", x[i]
      )
    } else {
      x[i] <- gsub(
        "\\\\chapter\\{",
        "\\\\starredchapter\\{ANNEXE~\\\\thechapter. ", x[i]
      )
    }
  }
  x <- inject_refstepcounters(x)

  # Need to remove hypertarget for references to appendices to work:
  # rs_line <- grep("\\\\refstepcounter", x)
  # FIXME: make more robust
  rs_line <- grep("\\\\hypertarget\\{app:", x)
  x[rs_line + 0] <- gsub("hypertarget", "label", x[rs_line + 0])
  x[rs_line + 0] <- gsub("\\{%", "", x[rs_line + 0])
  x[rs_line + 1] <- gsub("\\}$", "", x[rs_line + 1])
  x[rs_line + 1] <- gsub("\\}.*\\}$", "}", x[rs_line + 1])

  x <- gsub("^.*\\\\tightlist$", "", x)

  # \eqref needs to be \ref so the equation references don't have () around them
  # https://tex.stackexchange.com/a/107425
  x <- gsub("\\\\eqref\\{", "\\\\ref\\{", x)

  # Non-breaking spaces:
  x <- gsub(" \\\\ref\\{", "~\\\\ref\\{", x)

  # ----------------------------------------------------------------------
  # Add tooltips so that figures have alternative text for read-out-loud
  figlabel_lines <- x[grep("\\\\label\\{fig:", x)]
  fig_labels <- gsub(
    "\\\\caption\\{(.*?)\\}\\\\label\\{fig:(.*?)\\}",
    "\\2", figlabel_lines
  )
  all_include_graphics <- grep("(\\\\includegraphics\\[(.*?)\\]\\{(.*?)\\})", x)

  # is this a true figure with a caption in Pandoc style?
  all_include_graphics <-
    all_include_graphics[grep("\\\\centering", x[all_include_graphics])]

  if (identical(length(fig_labels), length(all_include_graphics))) {
    for (i in seq_along(all_include_graphics)) {
      x[all_include_graphics[i]] <-
        gsub(
          "(\\\\includegraphics\\[(.*?)\\]\\{(.*?)\\})",
          paste0("\\\\pdftooltip{\\1}{", "Figure \\\\ref{fig:", fig_labels[i], "}}"),
          x[all_include_graphics[i]]
        )
    }
  } else {
    warning("The number of detected figure captions did not match the number of ",
      "detected figures. Reverting to unnumbered alternative text figures.",
      call. = FALSE
    )
    x <- gsub(
      "(\\\\includegraphics\\[(.*?)\\]\\{(.*?)\\})",
      "\\\\pdftooltip{\\1}{Figure}", x
    )
  }
  # ----------------------------------------------------------------------

  regexs <- c(
    "^\\\\CHAPTER\\*\\{R\\p{L}F\\p{L}RENCES", # French or English
    "^\\\\SECTION{SOURCES DE RENSEIGNEMENTS}",
    "^\\\\SECTION{SOURCES OF INFORMATION}"
  )
  .matches <- lapply(regexs, function(.x) grep(.x, toupper(x), perl = TRUE) + 1)
  references_insertion_line <- unlist(.matches)

  x[references_insertion_line - 1] <- sub("chapter", "section", x[references_insertion_line - 1])
  x[references_insertion_line] <- sub("chapter", "section", x[references_insertion_line])

  # Move the bibliography to before the appendices:
  if (length(references_insertion_line) > 0) {
    references_begin <- grep("^\\\\hypertarget\\{refs\\}\\{\\}$", x)
    if (length(references_begin) > 0) {
      references_end <- length(x) - 1
      x <- c(
        x[seq(1, references_insertion_line - 1)],
        #"\\phantomsection",
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
        x[length(x)]
      )
      # Modify References from starred chapter to regular chapter so that it is numbered
      starred_references_line <- grep("\\\\section\\*\\{REFERENCES\\}\\\\label\\{references\\}\\}", x)
      x[starred_references_line] <- gsub("\\*", "", x[starred_references_line])
      # Remove the add contents line which was used to add the unnumbered section before
      add_toc_contents_line <- grep("\\\\addcontentsline\\{toc\\}\\{section\\}\\{REFERENCES\\}", x)
      x[add_toc_contents_line] <- ""
    } else {
      warning("Did not find the beginning of the LaTeX bibliography.", call. = FALSE)
    }
  }

  # Tech Report Appendices:
  x <- gsub(
    "\\% begin csasdown appendix",
    paste0(
      "\\begin{appendices}\n",
      "\\\\counterwithin{figure}{section}\n",
      "\\\\counterwithin{table}{section}\n",
      "\\\\counterwithin{equation}{section}"
    ),
    x
  )
  x <- gsub("\\% end csasdown appendix", "\\end{appendices}", x)

  label_app <- grep("^\\\\label\\{app:", x)
  for (i in seq_along(label_app)) {
    if (grepl("^\\\\section\\{", x[label_app[i] + 1])) {
      x[seq(label_app[i], label_app[i] + 1)] <- x[seq(label_app[i] + 1, label_app[i])]
    }
  }

  # Implement "Approved pre-publication" version (science response)
  if (prepub) {
    # Text to add
    addText <- ifelse(french, " -- PR\u00C9-PUBLICATION APPROUV\u00C9E}",
      " -- APPROVED PRE-PUBLICATION}"
    )
    # 1. Modify header first page (report number)
    rn_loc_1 <- grep(pattern = "\\% Report number", x = x) + 1
    rn_loc_2 <- grep(pattern = "\\% End of report number", x = x) - 1
    if (rn_loc_1 != rn_loc_2) {
      stop("Can't find report number (report_number)")
    }
    rn_text <- x[rn_loc_1]
    rn_text_clean <- gsub(pattern = "\\}+$", replacement = "", x = rn_text)
    rn_text_new <- paste0(rn_text_clean, "}", addText)
    x[rn_loc_1] <- rn_text_new
    # 2. Modify short title
    st_loc_1 <- grep(pattern = "\\% Title short", x = x) + 1
    st_loc_2 <- grep(pattern = "\\% End of title short", x = x) - 1
    if (st_loc_1 != st_loc_2) stop("Can't find short title (title_short)")
    st_text <- x[st_loc_1]
    st_text_clean <- gsub(pattern = "\\}+$", replacement = "", x = st_text)
    st_text_new <- paste0(st_text_clean, addText)
    x[st_loc_1] <- st_text_new
    # 3. Modify citation (2 things)
    if (french) {
      # Edit french citation
      cite_head_fr <- grep(
        pattern = "La pr\\\\\'\\{e\\}sente publication doit \\\\\\^\\{e\\}tre cit\\\\\'\\{e\\}e comme suit:",
        x = x
      )
      if (length(cite_head_fr) == 0) stop("Can't find French citation header")
      x[cite_head_fr] <- "Cite comme ceci (jusqu'\u00E0 la publication):"
      cite_loc_fr <- grep(
        pattern = "\\\\citeFr\\{\\\\rdYear\\{\\}/\\\\rdNumber\\{\\}\\}", x = x
      )
      if (length(cite_loc_fr) == 0) stop("Can't find French citation")
      x[cite_loc_fr] <- "\\citeFr{Sous presse}"
      # Nuke english citation
      cite_head_eng <- grep(
        pattern = "\\\\emph\\{Also available in English:\\}",
        x = x
      )
      if (length(cite_head_eng) == 0) stop("Can't find English citation header")
      x[cite_head_eng] <- ""
      cite_loc_eng <- grep(
        pattern = "\\\\citeEng\\{\\\\rdYear\\{\\}/\\\\rdNumber\\{\\}\\}", x = x
      )
      if (length(cite_loc_eng) == 0) stop("Can't find English citation")
      x[cite_loc_eng] <- ""
    } else {
      # Edit english citation
      cite_head_eng <- grep(
        pattern = "Correct citation for this publication:",
        x = x
      )
      if (length(cite_head_eng) == 0) stop("Can't find English citation header")
      x[cite_head_eng] <- "Correct citation (until published):"
      cite_loc_eng <- grep(
        pattern = "\\\\citeEng\\{\\\\rdYear\\{\\}/\\\\rdNumber\\{\\}\\}", x = x
      )
      if (length(cite_loc_eng) == 0) stop("Can't find English citation")
      x[cite_loc_eng] <- "\\citeEng{In press}"
      # Nuke french citation
      cite_head_fr <- grep(
        pattern = "\\\\emph\\{Aussi disponible en fran\\\\c\\{c\\}ais:\\}",
        x = x
      )
      if (length(cite_head_fr) == 0) stop("Can't find French citation header")
      x[cite_head_fr] <- ""
      cite_loc_fr <- grep(
        pattern = "\\\\citeFr\\{\\\\rdYear\\{\\}/\\\\rdNumber\\{\\}\\}", x = x
      )
      if (length(cite_loc_fr) == 0) stop("Can't find French citation")
      x[cite_loc_fr] <- ""
    } # End modify citations
  } # End if prepub

  # Fix Res. Doc. 2013/092: -> Res. Doc. 2013/092.
  x <- gsub("Res\\. Doc\\. ([0-9]{4}/[0-9]{2,}):", "Res. Doc. \\1.", x)
  x <- gsub("MPO\\. Doc\\. de rech ([0-9]{4}/[0-9]{2,}):", "MPO. Doc. de rech \\1.", x)
  x
}

inject_refstepcounters <- function(x) {
  chpts <- grep("^\\\\starredchapter\\{", x)
  for (i in chpts) {
    # in very rare setups hypertarget doesn't appear(?):
    .i <- if (grepl("hypertarget", x[i - 1])) i else i + 1
    x <- c(
      x[seq(1, .i - 3)],
      paste0(x[.i - 2], "\n\n\\clearpage\n\n\\refstepcounter{chapter}"),
      x[seq(.i - 1, length(x))]
    )
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

#' Add a titlepage to a Tech report docx file
#'
#' Must hand edit the first two pages of your file afterwards to have your desired title and authors.
#'
#' @param titlepage Filename
#' @param resdoc Filename
#' @param french Logical. If TRUE, Add the French title page
#'
#' @return A merged .docx
#' @export
add_techreport_docx_titlepage <- function(titlepage = ifelse(french, "templates/tech-report-cover-fra.docx", "templates/tech-report-cover-eng.docx"),
                                          doc = "_book/techreport.docx",
                                          french = FALSE) {
  title_doc <- officer::read_docx(titlepage)
  x <- officer::body_add_docx(title_doc, doc, pos = "before")
  print(x, target = doc)
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
      package = "csasdown"
    )
  ))
  x_index <- names(rmarkdown::yaml_front_matter("index.Rmd"))
  .diff <- setdiff(x_skeleton, x_index)
  if (length(.diff) > 0L) {
    stop("Your `index.Rmd` file is missing: ", paste(.diff, collapse = ", "), ".")
  } else {
    message("Your `index.Rmd` file contains all necessary YAML options.")
  }
}

#' Return regional CSAS email address, phone number, and mailing address for
#' the last page in the section "This report is available from the." Return
#' contactinformation for the national CSAS office if regional information is
#' not available (with a warning).
#'
#' @param region Region in which the document is published; character vector.
#' (i.e., Pacific). Default is "National Capital Region."
#' @param isFr Logical (default FALSE). Is the report in French or not?
#'
#' @export
#'
#' @return Email address, phone number, and mailing address as list of character
#' vectors.
get_contact_info <- function(region = "National Capital Region", isFr = FALSE) {
  # Region name (English and French), email, phone, and address
  dat <- tibble::tribble(
    ~Region, ~RegionFr, ~Email, ~Phone, ~Address,
    "Central and Arctic Region", "R\u00E9gion du Centre et de l'Arctique", "xcna-csa-cas@dfo-mpo.gc.ca", "(204) 983-5232", "501 University Cres.\\\\\\\\Winnipeg, MB, R3T 2N6",
    "Gulf Region", "R\u00E9gion du Golfe", "Gerald.Chaput@dfo-mpo.gc.ca", "(506) 851-2022", "343 Universit\u00E9 Ave.\\\\\\\\Moncton, NB, E1C 9B6",
    "Maritimes Region", "R\u00E9gion des Maritimes", "XMARMRAP@dfo-mpo.gc.ca", "(902) 426-3246", "1 Challenger Dr.\\\\\\\\Dartmouth, NS, B2Y 4A2",
    "National Capital Region", "R\u00E9gion de la capitale nationale", "csas-sccs@dfo-mpo.gc.ca", "(613) 990-0194", "200 Kent St.\\\\\\\\Ottawa, ON, K1A 0E6",
    "Newfoundland and Labrador Region", "R\u00E9gion de Terre-Neuve et Labrador", "DFONLCentreforScienceAdvice@dfo-mpo.gc.ca", "(709) 772-8892", "P.O. Box 5667\\\\\\\\St. John's, NL, A1C 5X1",
    "Pacific Region", "R\u00E9gion du Pacifique", "csap@dfo-mpo.gc.ca", "(250) 756-7088", "3190 Hammond Bay Rd.\\\\\\\\Nanaimo, BC, V9T 6N7",
    "Quebec Region", "R\u00E9gion du Qu\u00E9bec", "bras@dfo-mpo.gc.ca", "(418) 775-0825", "850 route de la Mer, P.O. Box 1000\\\\\\\\Mont-Joli, QC, G5H 3Z4"
  )
  # If french
  if (isFr) {
    # Get index for region (row)
    ind <- which(dat$RegionFr == region)
  } else { # End if french, otherwise
    # Get index for region (row)
    ind <- which(dat$Region == region)
  } # End if not french
  # If region not detected
  if (length(ind) == 0) {
    # Use national contact info
    email <- dat$Email[dat$Region == "National Capital Region"]
    phone <- dat$Phone[dat$Region == "National Capital Region"]
    address <- dat$Address[dat$Region == "National Capital Region"]
    warning("Region not detected; use national CSAS contact info")
  } else { # End if no region, otherwise get regional contact info
    # Get regional contact info
    email <- dat$Email[ind]
    phone <- dat$Phone[ind]
    address <- dat$Address[ind]
  } # End if region detected
  return(list(email = email, phone = phone, address = address))
} # End get_contact_info

#' Creates a temporary directory for compiling the latex file with latex commands for a csasdown type
#'
#' @details The compiled tex file will be copied from either the root directory or the _book directory, depending
#' on the value of `where`. The necessary directories knitr-figs-pdf, knitr-figs-word, knitr-cache-pdf, and
#' knitr-cache-word will be copied recursively into the temporary directory, preserving the directory structure
#' necessary for the build.
#'
#' @param type The csasdown document type. See [draft()]
#' @param where Where to look for the tex file. If "r", look in root directory, if "b", look in the _book
#' subdirectory. Any other value will cause an error
#' @param tmp_dir A temporary directory. If NULL, once will be created in the filesystem using [tempdir()]
#' @param root_dir A directory where everything will be copied from
#'
#' @return The temporary directory's full path
#' @export
#'
#' @examples
#' \dontrun{
#' root_dir <- getwd()
#' tmp_dir <- create_tempdir_for_latex("resdoc", "b")
#' setwd(tmp_dir)
#' tinytex::latexmk("resdoc.tex")
#' setwd(root_dir)
#' }
create_tempdir_for_latex <- function(type = NULL,
                                     where = "r",
                                     tmp_dir = NULL,
                                     root_dir = here::here()) {
  stopifnot(type == "resdoc" ||
    type == "sr" ||
    type == "techreport")
  stopifnot(where == "r" ||
    where == "b")

  if (is.null(tmp_dir)) {
    tmp_dir <- tempdir()
  }

  copy_dir <- function(from_dir, to_dir, recursive = TRUE) {
    dir.create(to_dir, showWarnings = FALSE)
    to_dir <- file.path(to_dir, from_dir)
    dir.create(to_dir, showWarnings = FALSE)
    from_dir <- file.path(root_dir, from_dir)
    from_files <- file.path(from_dir, dir(from_dir))
    invisible(file.copy(from_files, to_dir, recursive = recursive))
  }

  # Copy required directories and files recursively
  copy_dir("csas-style", tmp_dir)
  copy_dir("knitr-cache-pdf", tmp_dir)
  copy_dir("knitr-cache-word", tmp_dir)
  copy_dir("knitr-figs-pdf", tmp_dir)
  copy_dir("knitr-figs-word", tmp_dir)

  # Copy the TEX file
  tex_file_name <- paste0(type, ".tex")
  if (where == "b") {
    tex_file <- file.path(root_dir, "_book", tex_file_name)
  } else if (where == "r") {
    tex_file <- file.path(root_dir, tex_file_name)
  }
  if (!file.exists(tex_file)) {
    stop(paste0(type, ".tex"), " does not exist in the ", ifelse(where == "b", "_book", "root"), " directory")
  }
  invisible(file.copy(tex_file, tmp_dir))
  tmp_dir
}
