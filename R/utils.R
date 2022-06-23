#' Convert a hex string into a vector of three decimal values (RGB)
#'
#' @keywords internal
#'
#' @param hex The hex string of 6 or 8 digits (if alpha included). May of may
#' not begin with  #
#' @param rel If `TRUE`, divide the RGB values by 255 for relative values
#' @param ret_alpha if `TRUE` alpha value will be included in the output
#' vector as the last item, so it will be length 4 instead of 3
#'
#' @return A vector of three RGB decimal values, or three relative values
hex2rgb <- function(hex, rel = FALSE, ret_alpha = FALSE){

  if(is.null(hex)){
    return(NULL)
  }
  if(is.na(hex)){
    return(NA_character_)
  }

  hex <- gsub("#", "", hex)
  if(nchar(hex) != 6 && nchar(hex) != 8){
    stop("hex must be a 6- or 8-digit number", call. = FALSE)
  }
  if(ret_alpha && nchar(hex) != 8){
    hex <- paste0(hex, "ff")
  }
  for(i in 1:nchar(hex)){
    if(!substr(hex, i, i) %in% c(0:9, letters[1:6], LETTERS[1:6])){
      stop("`hex` contains non-hexadecimal digits",
           call. = FALSE)
    }
  }
  if(ret_alpha){
    hex_vec <- substring(hex, seq(1, 7, 2), seq(2, 8, 2))
  }else{
    hex_vec <- substring(hex, seq(1, 5, 2), seq(2, 6, 2))
  }
  dec <- strtoi(hex_vec, 16)
  if(rel){
    dec <- dec / 255
  }
  dec
}

#' Supply the Rmarkdown newline code for a given number of newlines
#'
#' @keywords internal
#'
#' @param num_blank_lines A single value for the number of newlines,
#' or actual blank lines required
#'
#' @return A character vector containing the sequence of code necessary
#' to create the number of newlines required
rmd_nlines <- function(num_blank_lines){
  if(is.null(num_blank_lines)){
    stop("`num_blank_lines` must not be `NULL`")
  }
  if(num_blank_lines < 0){
    stop("`num_blank_lines` must be zero or greater")
  }
  if(num_blank_lines == 0){
    return("")
  }
  if(num_blank_lines == 1){
    return(c("", "\\\\ \\\\", ""))
  }
  if(num_blank_lines > 1){
    return(c("", rep("\\\\", num_blank_lines - 1), ""))
  }
}

#' Checks to see if character strings are Rmarkdown header lines
#'
#' @description
#' Checks to see if character strings are Rmarkdown header lines which are
#' text with at least one character, possibly containing whitespace. In other
#' words, typical paragraph or sentence text with punctuation.
#'
#' @details
#' The one thing that is not a text line is a header line (Starts with a #
#' followed by a space)
#'
#' @keywords internal
#'
#' @param lines The vector of character strings to check
#'
#' @return A logical vector representing whether or not the lines are
#' Rmarkdown header lines, which are normal text
is_rmd_text_line <- function(lines){
  if(is.null(lines)){
    return(NULL)
  }
  if(any(is.na(lines))){
    stop("An NA is present in the vector of strings:\n\n",
         paste(lines, collapse = "\n"),
         "\n\n",
         call. = FALSE)
  }

  map_lgl(lines, ~{
    (!is_rmd_header_line(.x) &&
      grepl("^(\\s*\\S+\\s*)+$", trimws(.x)))
  })
}

#' Checks to see if character strings are dashed lines
#'
#' @description
#' Checks to see if character strings are dashed lines of any length
#' (including one)
#'
#' @keywords internal
#'
#' @param lines The vector of character strings to check
#'
#' @return A logical vector representing whether or not the lines are
#' dashed lines
is_rmd_dashed_line <- function(lines){

  if(is.null(lines)){
    return(NULL)
  }
  if(any(is.na(lines))){
    stop("An NA is present in the vector of strings:\n\n",
         paste(lines, collapse = "\n"),
         "\n\n",
         call. = FALSE)
  }

  map_lgl(lines, ~{
    grepl("^(\\s*-+\\s*)+$", trimws(.x))
  })

}

#' Checks to see if character strings are Rmarkdown header lines
#'
#' @param lines The vector of character strings to check
#'
#' @keywords internal
#'
#' @details
#' A header line must be indented less than 4 spaces and start with a #
#' followed by one or more spaces, and the n text
#'
#' @return A logical vector representing whether or not the lines are
#' Rmarkdown header lines
is_rmd_header_line <- function(lines){
  if(is.null(lines)){
    return(NULL)
  }
  if(any(is.na(lines))){
    stop("An NA is present in the vector of strings:\n\n",
         paste(lines, collapse = "\n"),
         "\n\n",
         call. = FALSE)
  }

  map_lgl(lines, ~{
    leading_spaces <- nchar(gsub("^(\\s*)#+.*$", "\\1", .x))
    if(leading_spaces > 3){
      return(FALSE)
    }
    grepl("^#+\\s+.*$", trimws(.x))
  })
}

#' Checks to see if character strings are Rmarkdown list lines
#'
#' @keywords internal
#'
#' @param lines The vector of character strings to check
#'
#' @return A logical vector representing whether or not the lines are
#' Rmarkdown list lines
is_rmd_list_line <- function(lines){
  if(is.null(lines)){
    return(NULL)
  }
  if(any(is.na(lines))){
    stop("An NA is present in the vector of strings:\n\n",
         paste(lines, collapse = "\n"),
         "\n\n",
         call. = FALSE)
  }
  map_lgl(lines, function(.x) {
    substr(trimws(.x), 2, 3) == ". " ||
      substr(trimws(.x), 1, 2) == "* " ||
      substr(trimws(.x), 1, 2) == "+ " ||
      substr(trimws(.x), 1, 2) == "- "
  })
}

#' Checks to see if character strings represent the start of a Rmarkdown
#' tables
#'
#' @keywords internal
#'
#' @param lines_lst A list of character strings vectors of at least length 5
#' for a type 1 table and 3 for a type 2 table
#'
#' @details
#' Three lines from the beginning of the table are required to determine if a
#' table is possibly valid
#'
#' @return A character vector representing which Rmarkdown table type each
#' element in `lines_lst` is
is_rmd_table_line <- function(lines_lst){

  if(is.null(lines_lst)){
    return(NULL)
  }
  if(!is.list(lines_lst)){
    lines_lst <- list(lines_lst)
  }
  if(any(is.na(lines_lst))){
    stop("An NA is present in the vector of strings:\n\n",
         paste(lines_lst, collapse = "\n"),
         "\n\n",
         call. = FALSE)
  }

  map_chr(lines_lst, ~{
    is_type_1 <- FALSE
    is_type_2 <- FALSE
    if(length(.x) >= 5){
      is_type_1 <- is_rmd_dashed_line(.x[1]) &&
                   is_rmd_text_line(.x[2]) &&
                   !is_rmd_header_line(.x[2]) &&
                   !is_rmd_list_line(.x[2]) &&
                   is_rmd_dashed_line(.x[3]) &&
                   is_rmd_text_line(.x[4]) &&
                   !is_rmd_header_line(.x[4]) &&
                   !is_rmd_list_line(.x[4])
    }
    if(length(.x) >= 3){
      is_type_2 <- is_rmd_text_line(.x[1]) &&
                   !is_rmd_header_line(.x[1]) &&
                   !is_rmd_list_line(.x[1]) &&
                   is_rmd_dashed_line(.x[2]) &&
                   is_rmd_text_line(.x[3]) &&
                   !is_rmd_header_line(.x[3]) &&
                   !is_rmd_list_line(.x[3])
    }
    if(is_type_1){
      "type1"
    }else if(is_type_2){
      "type2"
    }else{
      "false"
    }
  })
}

#' Detect which columns are year columns based on the range and type
#'
#' @keywords internal
#'
#' @param df A data frame with column names
#' @param year_range The range to use for year column acceptance. All values
#' in the column must be in this range
#'
#' @return A vector of column names, or NULL if no year columns were found
year_cols <- function(df, year_range = 1800:4000){

  col_is_year <- map2(df, names(df), ~{
    if(is.numeric(.x)){
      # Check that all values are in the year range and that they are integers
      # even if the type has not been set to integer, i.e. `is.integer(.x)` is FALSE
      # but `is.numeric(.x)` is TRUE.
      if(all(.x %in% year_range) && all(sapply(.x, `%%`, 1) == 0)){
        .y
      }
    }
  })
  # Remove all NULLs from the list and make the list a character vector
  col_is_year[sapply(col_is_year, is.null)] <- NULL
  col_is_year <- map_chr(col_is_year, ~{.x})
  if(!length(col_is_year)){
    return(NULL)
  }
  # Remove names because testing is easier to code, and they are the same
  # as the values anyway
  names(col_is_year) <- NULL
  col_is_year
}

#' Return value of the 'french' option in the current environment
#'
#' @description
#' Used to retrieve `TRUE` or `FALSE` for whether
#' or not to render the document in French
#'
#' @return The french option value. If the french option is `NULL`,
#' `FALSE` will be returned
#' @export
fr <- function(){
  getOption("french", default = FALSE)
}

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
#'  "tango", "pygments", "kate", "monochrome", "espresso",
#'  "zenburn", and "haddock". If not in this list of styles, the directory in which
#'  the build process was called will be searched for the given theme file. You can
#'  copy one from the csasdown library install on your machine and modify as necessary.
#'  Find them by looking here:
#'  file.path(.libPaths(), "csasdown", "themes")
#'  Pass `NULL` to prevent syntax highlighting (uses 'monochrome' theme) for slightly
#'  different text format but no highlighting
#' @param latex_engine LaTeX engine to render with. 'pdflatex' or 'xelatex'
#' @param prepub Logical for whether this is a pre-publication version
#'  (currently not implemented for ResDocs)
#' @param draft_watermark If `TRUE` show a DRAFT watermark on all pages of the output document
#' @param include_section_nums If `TRUE` include the section and subsection numbers in the body titles.
#' The table of contents will still show the numbers.
#' @param copy_sty Copy the .sty files every time? Set to `FALSE` to "freeze" the
#'   .sty file if you need to edit it.
#' @param line_nums Include line numbers in the document? Logical.
#' @param line_nums_mod Numerical. Which modulo line numbers to label, 2 = every second line, etc.
#' @param lot_lof Include list of tables and list of figures in the document? Logical.
#'  (implemented only for ResDocs and TechReports)
#' @param pandoc_args Any other arguments to pandoc.
#' @param ... other arguments to [bookdown::pdf_book()].
#' @return A modified `pdf_document` based on the CSAS LaTeX template.
#' @import bookdown
#' @importFrom here here
#' @rdname csas_pdf
#' @examples
#' \dontrun{
#' output:csasdown::resdoc_pdf
#' }
resdoc_pdf <- function(toc = TRUE,
                       toc_depth = 3,
                       highlight = "tango",
                       latex_engine = "pdflatex",
                       prepub = FALSE,
                       copy_sty = TRUE,
                       line_nums = FALSE,
                       line_nums_mod = 1,
                       lot_lof = FALSE,
                       draft_watermark = FALSE,
                       include_section_nums = TRUE,
                       pandoc_args = c("--top-level-division=chapter",
                                       "--wrap=none",
                                       "--default-image-extension=png"),
                       ...) {

  themes <- c("pygments", "tango", "espresso",
              "zenburn", "kate", "monochrome",
              "breezedark", "haddock")

  if(is.null(highlight)){
    highlight = "monochrome" # nocov
  }

  if((!highlight %in% themes) && !file.exists(here(highlight))){
    stop("in YAML, `csasdown:resdoc_pdf: highlight` must be one of ",
         paste(themes, collapse = ", "),
         "\nor a filename for a custom latex theme file.",
         "\nSee pandoc documentation, --highlight-style argument.",
         call. = FALSE)
  }

  if (fr()) {
    file <- system.file("csas-tex", "res-doc-french.tex", package = "csasdown")
  } else {
    file <- system.file("csas-tex", "res-doc.tex", package = "csasdown")
  }

  base <- pdf_book(
    template = file,
    toc = toc,
    toc_depth = toc_depth,
    keep_tex = TRUE,
    pandoc_args = pandoc_args,
    latex_engine = latex_engine,
    ...
  )
  tmp_hl <- grep("--highlight-style", base$pandoc$args)
  base$pandoc$args <- base$pandoc$args[-c(tmp_hl[1], tmp_hl[1] + 1)]

  if (!class(line_nums_mod) %in% c("integer", "numeric")) {
    stop("line_nums_mod must be a numeric or integer value.", call. = FALSE)
  }

  update_csasstyle(
    copy = copy_sty,
    line_nums = line_nums,
    line_nums_mod = line_nums_mod,
    draft_watermark = draft_watermark,
    lot_lof = lot_lof,
    which_sty = ifelse(fr(), "res-doc-french.sty", "res-doc.sty")
  )

  # Mostly copied from knitr::render_sweave
  base$knitr$opts_chunk$comment <- NA
  # base$knitr$opts_chunk$fig.align <- "center"

  old_opt <- getOption("bookdown.post.latex")
  options(bookdown.post.latex = function(x) {
    fix_envs(
      x = x,
      prepub = prepub,
      highlight = highlight,
      include_section_nums = include_section_nums,
      include_abstract = TRUE,
      fix_ref_section_name = TRUE
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
#' @param ... other arguments to [bookdown::word_document2()]
#' @import bookdown
#' @rdname csas_docx
#' @export
#' @return A Word Document based on the CSAS Res Doc template.
resdoc_word <- function(...) {
  file <- if (fr()) "RES2021-fra-content.docx" else "RES2021-eng-content.docx"
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
sr_pdf <- function(latex_engine = "pdflatex",
                   prepub = FALSE,
                   copy_sty = TRUE,
                   line_nums = FALSE,
                   line_nums_mod = 1,
                   draft_watermark = FALSE,
                   highlight = "tango",
                   pandoc_args = c("--top-level-division=chapter", "--wrap=none", "--default-image-extension=png"),
                   ...) {

  themes <- c("pygments", "tango", "espresso", "zenburn", "kate", "monochrome", "breezedark", "haddock")

  if(is.null(highlight)){
    highlight = "monochrome"
  }

  if((!highlight %in% themes) && !file.exists(here(highlight))){
    stop("in YAML, `csasdown:sr_pdf: highlight` must be one of ", paste(themes, collapse = ", "),
         "\nor a filename for a custom latex theme file.",
         "\nSee pandoc documentation, --highlight-style argument.", call. = FALSE)
  }

  if (fr()) {
    file <- system.file("csas-tex", "sr-french.tex", package = "csasdown")
  } else {
    file <- system.file("csas-tex", "sr.tex", package = "csasdown")
  }

  base <- pdf_book(
    template = file,
    keep_tex = TRUE,
    pandoc_args = pandoc_args,
    latex_engine = latex_engine,
    ...
  )
  tmp_hl <- grep("--highlight-style", base$pandoc$args)
  base$pandoc$args <- base$pandoc$args[-c(tmp_hl[1], tmp_hl[1] + 1)]

  if (!class(line_nums_mod) %in% c("integer", "numeric")) {
    stop("line_nums_mod must be a numeric or integer value.", call. = FALSE)
  }

  update_csasstyle(
    copy = copy_sty,
    line_nums = line_nums,
    line_nums_mod = line_nums_mod,
    draft_watermark = draft_watermark,
    which_sty = ifelse(fr(), "sr-french.sty", "sr.sty")
  )

  base$knitr$opts_chunk$comment <- NA
  old_opt <- getOption("bookdown.post.latex")

  options(bookdown.post.latex = function(x) {
    fix_envs(
      x = x,
      prepub = prepub,
      highlight = highlight,
      include_abstract = FALSE
    )
  })

  on.exit(options(bookdown.post.late = old_opt))
  base
}

#' @export
#' @rdname csas_docx
sr_word <- function(...) {
  file <- if (fr()) "SRR-RS2021-fra.docx" else "SRR-RS2021-eng.docx"
  base <- word_document2(...,
    reference_docx = system.file("csas-docx", file, package = "csasdown")
  )
  base$knitr$opts_chunk$comment <- NA
  base$knitr$opts_chunk$fig.align <- "center"
  base
}

#' @export
#' @rdname csas_docx
techreport_word <- function(...) {
  file <- "tech-report.docx"
  base <- word_document2(...,
    reference_docx = system.file("csas-docx", file, package = "csasdown")
  )
  base$knitr$opts_chunk$comment <- NA
  base$knitr$opts_chunk$fig.align <- "center"
  base
}

#' @export
#' @rdname csas_pdf
techreport_pdf <- function(latex_engine = "pdflatex",
                           copy_sty = TRUE,
                           line_nums = FALSE,
                           line_nums_mod = 1,
                           lot_lof = FALSE,
                           draft_watermark = FALSE,
                           highlight = "tango",
                           pandoc_args = c("--top-level-division=chapter",
                                           "--wrap=none",
                                           "--default-image-extension=png"),
                           ...) {

  themes <- c("pygments", "tango", "espresso",
              "zenburn", "kate", "monochrome",
              "breezedark", "haddock")

  if(is.null(highlight)){
    highlight = "monochrome" # nocov
  }

  if((!highlight %in% themes) && !file.exists(here(highlight))){
    stop("in YAML, `csasdown:techreport_pdf: highlight` must be one of ",
         paste(themes, collapse = ", "),
         "\nor a filename for a custom latex theme file.",
         "\nSee pandoc documentation, --highlight-style argument.",
         call. = FALSE)
  }

  if (fr()) {
    file <- system.file("csas-tex", "tech-report-french.tex", package = "csasdown") # nocov
  } else {
    file <- system.file("csas-tex", "tech-report.tex", package = "csasdown")
  }

  base <- pdf_book(
    template = file,
    keep_tex = TRUE,
    pandoc_args = pandoc_args,
    latex_engine = latex_engine,
    ...
  )
  tmp_hl <- grep("--highlight-style", base$pandoc$args)
  base$pandoc$args <- base$pandoc$args[-c(tmp_hl[1], tmp_hl[1] + 1)]

  cover_file_pdf <- if (fr()) "tech-report-cover-french.pdf" else "tech-report-cover.pdf"
  cover_file_docx <- if (fr()) "tech-report-cover-french.docx" else "tech-report-cover.docx"
  if (!file.exists(cover_file_pdf)) {
    cover_docx <- system.file("rmarkdown", "templates", "techreport", "skeleton", cover_file_docx, package = "csasdown")
    cover_pdf <- system.file("rmarkdown", "templates", "techreport", "skeleton", cover_file_pdf, package = "csasdown")
    warning("Missing the Tech Report cover page. Copying in the files...", call. = FALSE)
    file.copy(cover_docx, ".", overwrite = FALSE)
    file.copy(cover_pdf, ".", overwrite = FALSE)
  }

  if (!class(line_nums_mod) %in% c("integer", "numeric")) {
    stop("line_nums_mod must be a numeric or integer value.", call. = FALSE)
  }

  update_csasstyle(
    copy = copy_sty,
    line_nums = line_nums,
    line_nums_mod = line_nums_mod,
    lot_lof = lot_lof,
    draft_watermark = draft_watermark,
    which_sty = ifelse(fr(), "tech-report-french.sty", "tech-report.sty")
  )

  base$knitr$opts_chunk$comment <- NA
  old_opt <- getOption("bookdown.post.latex")
  options(bookdown.post.latex = function(x) {
    fix_envs(
      x = x,
      highlight = highlight
    )
  })
  on.exit(options(bookdown.post.late = old_opt))
  base
}

#' Copy the csas-style directory from the local library location to
#' the current directory, overwriting and edit the style file if necessary
#'
#' @keywords internal
#'
#' @param copy Logical. If TRUE, copy and overwrite if the directory already exists.
#' If FALSE, only copy if the directory does not exist in the current directory
#' @param line_nums Logical. Include line numbering in the document
#' @param line_nums_mod Numerical. Which modulo line numbers to label, 2 = every second line, etc.
#' @param which_sty Name of the style file to modify
#' @param lot_lof Include list of tables and list of figures in the document? Logical.
#'  (implemented only for ResDocs and TechReports)
#' @param draft_watermark If `TRUE` show a DRAFT watermark on all pages of the output document
#'
#' @importFrom utils tail
#' @return Nothing
update_csasstyle <- function(copy = TRUE,
                             line_nums = TRUE,
                             line_nums_mod = 1,
                             lot_lof = FALSE,
                             draft_watermark = FALSE,
                             which_sty = "res-doc.sty") {

  fn <- system.file("csas-style", package = "csasdown")
  if(!copy && line_nums){
    stop("You have set `copy_sty` to `FALSE` and `line_nums` to `TRUE` in ",
         "the index.Rmd YAML header. The permanent style file cannot be ",
         "modified as needed to include line numbering. ",
         "Either set `copy_sty` to `TRUE` or `line_nums` to `FALSE` to build.",
         call. = FALSE)
  }
  if(!copy && lot_lof){
    stop("You have set `copy_sty` to `FALSE` and `lot_lof` to `TRUE` in the ",
         "index.Rmd YAML header. The permanent style file cannot be ",
         "modified as needed to include the lists of tables and figures. ",
         "Either set `copy_sty` to `TRUE` or `lot_lof` to `FALSE` to build.",
         call. = FALSE)
  }
  if(!copy && draft_watermark){
    stop("You have set `copy_sty` to `FALSE` and `draft_watermark` to `TRUE` ",
         "in the index.Rmd YAML header. The permanent style file cannot be ",
         "modified as needed to include the DRAFT watermark. ",
         "Either set `copy_sty` to `TRUE` or `draft_watermark` to `FALSE` to build.",
         call. = FALSE)
  }

  if (copy || !dir.exists("csas-style")) {
    dir.create("csas-style", showWarnings = FALSE)
    ignore <- file.copy(fn, ".", overwrite = TRUE, recursive = TRUE)
    if(line_nums || lot_lof || draft_watermark){
      csas_style <- readLines(file.path("csas-style", which_sty))
    }
    if (line_nums) {
      if (grepl("res-doc", which_sty)) {
        frontmatter_loc <- grep("frontmatter\\{", csas_style)
        beg_of_file <- csas_style[seq(1, (frontmatter_loc - 1))]
        end_of_file <- csas_style[seq(frontmatter_loc, length(csas_style))]
        modulo <- paste0("\\modulolinenumbers[", line_nums_mod, "]")
        csas_style <- c(beg_of_file, "\\linenumbers", modulo, end_of_file)
        writeLines(csas_style, file.path("csas-style", which_sty))
      } else {
        modulo <- paste0("\\modulolinenumbers[", line_nums_mod, "]")
        csas_style <- c(csas_style, "\\linenumbers", modulo)
        writeLines(csas_style, file.path("csas-style", which_sty))
      }
    }
    if (lot_lof) {
      if (grepl("res-doc", which_sty) | grepl("tech-report", which_sty)) {
        pagenumbering_loc <- grep("pagenumbering\\{arabic", csas_style)
        beg_of_file <- csas_style[seq(1, (pagenumbering_loc - 1))]
        end_of_file <- csas_style[seq(pagenumbering_loc, length(csas_style))]
        lot <- "\\listoftables"
        cp <- "\\clearpage"
        lof <- "\\listoffigures"
        csas_style <- c(beg_of_file, lot, cp, lof, cp, end_of_file)
        writeLines(csas_style, file.path("csas-style", which_sty))
      } else { # nocov start
        warning("`lot_lof` is only implemented for Res Docs and TechReports.", call. = FALSE)
      } # nocov end
    }
    if(draft_watermark){
      last_usepackage_ind <- tail(grep("usepackage", csas_style), 1)
      beg_of_file <- csas_style[seq(1, last_usepackage_ind)]
      end_of_file <- csas_style[seq(last_usepackage_ind + 1, length(csas_style))]
      draft_watermark_include <- "\\usepackage{draftwatermark}"
      if(last_usepackage_ind == length(csas_style)){
        csas_style <- c(beg_of_file, draft_watermark_include)
      }else{
        csas_style <- c(beg_of_file, draft_watermark_include, end_of_file)
      }
      writeLines(csas_style, file.path("csas-style", which_sty))
    }
  }
}

#' Fix the appendix subsections so they can be referenced properly in text
#'
#' @keywords internal
#'
#' @param x A vector of lines of the TEX file
#'
#' @return Modified TEX lines (a vector of lines of the TEX file)
add_appendix_subsection_refs <- function(x){

  # Need a new counter for each appendix
  star_chap_inds <- grep("^\\\\starredchapter\\{", x)
  # If there are Appendices (Resdoc and SR only, the techreport has a totally different TEX structure)
  if(length(star_chap_inds)){
    counters <- paste0("app_counter_", seq_along(star_chap_inds))
    pre_starred_x <- x[1:(star_chap_inds[1] - 3)]
    appendix_chunks <- list()
    for(i in seq_along(star_chap_inds)){
      if(i == length(star_chap_inds)){
        appendix_chunks[[i]] <- x[(star_chap_inds[i] - 2):length(x)]
      }else{
        appendix_chunks[[i]] <- x[(star_chap_inds[i] - 2):(star_chap_inds[i + 1] - 3)]
      }
    }
    # At this point the TEX file is broken into several chunks, `pre_starred_x` which
    # is everything before the appendices, and N chunks in the list `appendix_chunks`,
    # one element for each appendix

    # Apply mods to the appendix chunks
    for(h in seq_along(appendix_chunks)){
      appsection_inds <- grep("^\\\\appsection\\{", appendix_chunks[[h]])
      if(length(appsection_inds)){
        # Strip appendix header away and call function on the rest
        app_chunk <- appendix_chunks[[h]]
        app_header <- app_chunk[1:(appsection_inds[1] - 2)]
        app_chunk <- app_chunk[(appsection_inds[1] - 1):length(app_chunk)]
        app_chunk_inds <- grep("^\\\\appsection\\{", app_chunk)
        # Now, break each into section chunks
        sec_chunks <- list()
        sec_header <- list()
        for(i in seq_along(app_chunk_inds)){
          if(i == length(app_chunk_inds)){
            sec_chunks[[i]] <- app_chunk[(app_chunk_inds[i] - 1):length(app_chunk)]
          }else{
            sec_chunks[[i]] <- app_chunk[(app_chunk_inds[i] - 1):(app_chunk_inds[i + 1] - 2)]
          }
          # Check for a label and allow missing label
          if(!length(grep("^\\\\hypertarget\\{", sec_chunks[[i]][1]))){
            # An auto-generated label was not added (using manually-added label) so switching the
            # label and appsection is necessary
            tmp_label <- sec_chunks[[i]][1]
            tmp_section <- sec_chunks[[i]][2]
            sec_chunks[[i]][1] <- tmp_section
            sec_chunks[[i]][2] <- tmp_label
          }

          # Iterate through each section chunk and create a list for the subsection chunks
          subsection_inds <- grep("^\\\\subsection\\{", sec_chunks[[i]])
          if(length(subsection_inds)){
            sec_chunk <- sec_chunks[[i]]
            sec_header[[i]] <- sec_chunk[1:(subsection_inds[1] - 2)]
            sec_chunk <- sec_chunk[(subsection_inds[1] - 1):length(sec_chunk)]
            sec_chunk_inds <- grep("^\\\\subsection\\{", sec_chunk)
            subsec_chunks <- list()
            subsec_header <- list()
            for(j in seq_along(sec_chunk_inds)){
              if(j == length(sec_chunk_inds)){
                subsec_chunks[[j]] <- sec_chunk[(sec_chunk_inds[j] - 1):length(sec_chunk)]
              }else{
                subsec_chunks[[j]] <- sec_chunk[(sec_chunk_inds[j] - 1):(sec_chunk_inds[j + 1] - 2)]
              }
              if(!length(grep("^\\\\hypertarget\\{", subsec_chunks[[j]][1]))){
                # An auto-generated label was not added (using manually-added label) so switching the
                # label and subsection is necessary
                tmp_label <- subsec_chunks[[j]][1]
                tmp_subsection <- subsec_chunks[[j]][2]
                subsec_chunks[[j]][1] <- tmp_subsection
                subsec_chunks[[j]][2] <- tmp_label
              }
              # Iterate through each section chunk and create a list for the subsection chunks
              subsubsection_inds <- grep("^\\\\subsubsection\\{", subsec_chunks[[j]])
              if(length(subsubsection_inds)){
                subsec_chunk <- subsec_chunks[[j]]
                subsec_header[[j]] <- subsec_chunk[1:(subsubsection_inds[1] - 2)]
                subsec_chunk <- subsec_chunk[(subsubsection_inds[1] - 1):length(subsec_chunk)]
                subsec_chunk_inds <- grep("^\\\\subsubsection\\{", subsec_chunk)
                subsubsec_chunks <- list()
                for(k in seq_along(subsec_chunk_inds)){
                  if(k == length(subsec_chunk_inds)){
                    subsubsec_chunks[[k]] <- subsec_chunk[(subsec_chunk_inds[k] - 1):length(subsec_chunk)]
                  }else{
                    subsubsec_chunks[[k]] <- subsec_chunk[(subsec_chunk_inds[k] - 1):(subsec_chunk_inds[k + 1] - 2)]
                  }
                  if(!length(grep("^\\\\hypertarget\\{", subsubsec_chunks[[k]][1]))){
                    # An auto-generated label was not added (using manually-added label) so switching the
                    # label and subsection is necessary
                    tmp_sublabel <- subsubsec_chunks[[k]][1]
                    tmp_subsubsection <- subsubsec_chunks[[k]][2]
                    subsubsec_chunks[[k]][1] <- tmp_subsubsection
                    subsubsec_chunks[[k]][2] <- tmp_sublabel
                  }
                }
                subsubsec_chunks <- unlist(subsubsec_chunks)
                counter_lines <- c(paste0("\\newcounter{appendix_",
                                          h,
                                          "_appsection_",
                                          i,
                                          "_subsection_",
                                          j,
                                          "_counter}"),
                                   paste0("\\refstepcounter{appendix_",
                                          h,
                                          "_appsection_",
                                          i,
                                          "_subsection_",
                                          j,
                                          "_counter}"))
                subsubsec_chunks <- c(counter_lines, subsubsec_chunks)
                names(subsubsec_chunks) <- NULL
                subsec_chunks[[j]] <- c(subsec_header[[j]], subsubsec_chunks)
              }
            }
            subsec_chunks <- unlist(subsec_chunks)
            counter_lines <- c(paste0("\\newcounter{appendix_",
                                      h,
                                      "_appsection_",
                                      i,
                                      "_counter}"),
                               paste0("\\refstepcounter{appendix_",
                                      h,
                                      "_appsection_",
                                      i,
                                      "_counter}"))
            subsec_chunks <- c(counter_lines, subsec_chunks)
            names(subsec_chunks) <- NULL
            sec_chunks[[i]] <- c(sec_header[[i]], subsec_chunks)
          }
        }
        sec_chunks <- unlist(sec_chunks)
        sec_header <- NULL
        counter_lines <- c(paste0("\\newcounter{appendix_",
                                  h,
                                  "_counter}"),
                           paste0("\\refstepcounter{appendix_",
                                  h,
                                  "_counter}"))
        sec_chunks <- c(counter_lines, sec_chunks)
        names(sec_chunks) <- NULL
        appendix_chunks[[h]] <- c(app_header, sec_chunks)
      }
    }
    appendix_chunks <- unlist(appendix_chunks)
    names(appendix_chunks) <- NULL
    x <- c(pre_starred_x, appendix_chunks)

  }
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

# nocov start
#' Add a Res Doc titlepage to a docx file
#'
#' Add a Res Doc titlepage. Must hand edit `templates/RES2021-eng-titlepage.docx`
#' to have your desired title and authors etc.
#'
#' @param titlepage Filename
#' @param resdoc Filename
#'
#' @return A merged .docx
#' @importFrom officer read_docx body_add_docx cursor_reach body_add_toc
#' @export
add_resdoc_docx_titlepage <- function(titlepage = "templates/RES2021-eng-titlepage.docx",
                                      resdoc = "_book/resdoc-english.docx") {
  title_doc <- read_docx(titlepage)
  x <- body_add_docx(title_doc, resdoc, pos = "before")
  print(x, target = resdoc)
}

#' Add front matter to Res Doc docx file
#'
#' Add title page and table of contents to a Res Doc. Must hand edit
#' `templates/RES2021-eng-frontmatter.docx`to have your desired title and authors etc.
#'
#' @param frontmatter  Path to title page file included with resdoc template
#' @param resdoc       Path to content generated using resdoc_word
#'
#' @return A merged .docx
#' @export
add_resdoc_docx_frontmatter <- function(frontmatter = "templates/RES2021-eng-frontmatter.docx",
                                        resdoc = "_book/resdoc.docx") {
  frontmatter_doc <- read_docx(frontmatter)
  x <- body_add_docx(frontmatter_doc, resdoc, pos = "before")
  x <- cursor_reach(x, keyword = "TABLE OF CONTENTS")
  x <- body_add_toc(x)
  print(x, target = resdoc)
}

#' Add a titlepage to a Tech report docx file
#'
#' Must hand edit the first two pages of your file afterwards to have your desired title and authors.
#'
#' @param titlepage Filename
#' @param doc Filename
#'
#' @return A merged .docx
#' @export
add_techreport_docx_titlepage <- function(titlepage = ifelse(fr(), "templates/tech-report-cover-fra.docx", "templates/tech-report-cover-eng.docx"),
                                          doc = "_book/techreport.docx") {
  title_doc <- read_docx(titlepage)
  x <- body_add_docx(title_doc, doc, pos = "before")
  print(x, target = doc)
}

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}
# nocov end
#
#' Check to make sure index.Rmd contains all current YAML options
#'
#' @description
#' As the csasdown package is updated, sometimes new mandatory YAML options are added
#' to the `index.Rmd` file. Running this function will compare your file to the
#' version built into the currently installed version of csasdown and issue
#' a `stop()` statement telling you what doesn't match if needed.
#'
#' @param type Type of document
#'
#' @importFrom rmarkdown yaml_front_matter
#' @export
check_yaml <- function(type = c("resdoc", "resdoc_pdf", "resdoc_word",
                                "sr", "sr_pdf", "sr_word",
                                "techreport", "techreport_pdf",
                                "techreport_word")) {

  type <- match.arg(type)
  if(type %in% c("resdoc", "resdoc_pdf", "resdoc_word")){
    type <- "resdoc"
  }else if(type %in% c("sr", "sr_pdf", "sr_word")){
    type <- "sr"
  }else if(type %in% c("techreport", "techreport_pdf", "techreport_word")){
    type <- "techreport"
  }

  message("Checking that YAML options are all present for document type '",
          type, "' ...")

  x_skeleton <- names(yaml_front_matter(
    system.file("rmarkdown", "templates", type, "skeleton", "skeleton.Rmd",
      package = "csasdown"
    )
  ))
  x_index <- names(yaml_front_matter("index.Rmd"))
  .diff <- setdiff(x_skeleton, x_index)
  if (length(.diff) > 0L) {
    stop("Your `index.Rmd` file is missing: ", paste(.diff, collapse = ", "))
  } else {
    message("Your `index.Rmd` file contains all necessary YAML options")
  }
}

#' Return regional CSAS email address and mailing address for the last page in
#' the section "This report is available from the." Return contact information
#' for the national CSAS office if regional information is not available (with a
#' warning).
#'
#' @param region Region in which the document is published; character vector.
#' (i.e., Pacific). Default is "National Capital Region."
#'
#' @export
#'
#' @return Email address and mailing address as list of character vectors.
get_contact_info <- function(region = "National Capital Region") {

  if (fr()) {
    # Get index for region (row)
    ind <- which(region_info$RegionFr == region)
    if(!length(ind)){
      # Maybe the author used English for the region name
      ind <- which(region_info$Region == region)
    }
  } else {
    # Get index for region (row)
    ind <- which(region_info$Region == region)
    if(!length(ind)){
      # Maybe the author used French for the region name
      ind <- which(region_info$RegionFr == region)
    }
  }
  # If region not detected, use national contact info
  if (length(ind) == 0) {
    default_region <- "National Capital Region"
    email <- region_info$Email[region_info$Region == default_region]
    if(fr()){
      address <- region_info$AddressFr[region_info$Region == default_region]
    }else{
      address <- region_info$Address[region_info$Region == default_region]
    }
    warning("Region not detected; using national CSAS contact info")
  } else {
    # Get regional contact info
    email <- region_info$Email[ind]
    if (fr())
      address <- region_info$AddressFr[ind]
    else
      address <- region_info$Address[ind]
  }
  list(email = email, address = address)
}

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
create_tempdir_for_latex <- function(type = c("resdoc", "sr", "techreport"),
                                     where = c("r", "b"),
                                     tmp_dir = NULL,
                                     root_dir = here()) {

  type <- match.arg(type)
  where <- match.arg(where)

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

  lang <- ifelse(fr(), "french", "english")

  # Copy the TEX file
  tex_file_name <- paste0(type, "-", lang, ".tex")
  if (where == "b") {
    tex_file <- file.path(root_dir, "_book", tex_file_name)
  } else if (where == "r") {
    tex_file <- file.path(root_dir, tex_file_name)
  }
  if (!file.exists(tex_file)) {
    stop(paste0(type, ".tex"), " does not exist in the ",
         ifelse(where == "b", "_book", "root"), " directory")
  }
  copy_success <- file.copy(tex_file, tmp_dir)
  if(!copy_success){
    stop("Copy of file '",tex_file, "' to directory '", tmp_dir, "' failed.",
         call. = FALSE)
  }
  tmp_dir
}

#' Redefinition of [cat()] with separator set to empty string
#'
#' @param ... `R` objects (see `Details` for the types of objects allowed)
#' @param file A connection, or a character string naming the file to print
#' to. If "" (the default), cat prints to the standard output connection
#' the console unless redirected by sink
#' @param sep A character vector of strings to append after each element.
#' @param fill A logical or (positive) numeric controlling how the output is
#' broken into successive lines. If `FALSE` (default), only newlines created
#' explicitly by ‘⁠\\n"⁠’ are printed. Otherwise, the output is broken in
#' to lines with print width equal to the option width if fill is `TRUE`,
#' or the value of `fill` if this is numeric. Linefeeds are only inserted
#' between elements, strings wider than `fill` are not wrapped. Non-positive
#' `fill` values are ignored, with a warning
#' @param labels A character vector of labels for the lines printed. Ignored
#' if `fill` is `FALSE`
#' @param append Logical. Only used if the argument file is the name of file
#' (and not a connection or "`|cmd`"). If `TRUE` output will be appended to file;
#' otherwise, it will overwrite the contents of file.
#'
#' @inherit base::cat details note references seealso examples
#' @export
cat <- function(...,
                file = "",
                sep = " ",
                fill = FALSE,
                labels = NULL,
                append = FALSE){

  base::cat(...,
            file = file,
            sep = "",
            fill = fill,
            labels = labels,
            append = append)
}

# nocov start
#' Capture multiple log messages from a function call
#'
#' @details
#' Capture all messages from a function.
#' Use like this:
#' `` x <- capture_log(function_call(args)) ``
#' `` j <- x(1) ``
#' `` j <- j$logs ``
#' `` messages <- map_chr(j, ~{.x$message}) ``
#'
#' @keywords internal
#' @param f The function call
#'
#' @return A function which you call like this (if you name sit `x`): `x(1)`
#' to extract the messages
capture_log <- function(f) {

  function(...) {
    logs <- list()
    add_log <- function(type, message) {
      new_l <- logs
      new_log <- list(timestamp = format(Sys.time(),
                                         tz = 'UTC', format = '%Y-%m-%d %H:%M:%S'),
                      type = type,
                      message = message)
      new_l[[length(new_l) + 1]]  <- new_log
      logs <<- new_l
    }
    res <- withCallingHandlers(
      tryCatch(f(...), error = function(e) {
        add_log("error", conditionMessage(e))
        NULL
      }), warning = function(w) {
        add_log("warning", conditionMessage(w))
        invokeRestart("muffleWarning")
      }, message = function(m) {
        add_log("message", conditionMessage(m))
        invokeRestart("muffleMessage")
      })
    list(res, logs = logs)
  }
}
# nocov end
