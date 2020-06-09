#' Custom table for csasdown
#'
#' This is a custom wrapper for [knitr::kable()] with some arguments set so that
#' the tables work with CSAS formatting in both LaTeX and Word documents.
#'
#' @param x An R object, typically a matrix or data frame.
#' @param format As defined by [knitr::kable()].
#' @param booktabs As defined by [knitr::kable()].
#' @param linesep As defined by [knitr::kable()].
#' @param longtable As defined by [knitr::kable()].
#' @param font_size Font size in pts. If NULL, document font size is used.
#' @param landscape Make this table in landscape orientation?
#' @param bold_header Make headers bold. Logical
#' @param repeat_header If landscape, repeat the header on subsequent pages?
#' @param repeat_header_text Use to write a Continued.. messgae continuing pages
#'   with the long table
#' @param repeat_header_method As defined by [kableExtra::kable_styling()].
#' @param col_names Names for the columns to show on table.
#' @param col_names_align As defined in [kableExtra::linebreak()].
#' @param escape As defined by [kableExtra::kable_styling()].
#' @param hold_position force the table placement to be where the code is called
#'   (don't let latex position the table where it wants)
#' @param extra_header character vector of extra headers to be placed above the headers
#' @param ... Other arguments passed to [knitr::kable()] and kableExtra:::pdfTable_add_header_above()
#' @param ex_bold See `bold` in kableExtra:::pdfTable_add_header_above()
#' @param ex_italic See `italic` in kableExtra:::pdfTable_add_header_above()
#' @param ex_monospace See `monospace` in kableExtra:::pdfTable_add_header_above()
#' @param ex_underline See `underline` in kableExtra:::pdfTable_add_header_above()
#' @param ex_strikeout See `strikeout` in kableExtra:::pdfTable_add_header_above()
#' @param ex_align See `align` in kableExtra:::pdfTable_add_header_above()
#' @param ex_color See `color` in kableExtra:::pdfTable_add_header_above()
#' @param ex_background See `background` in kableExtra:::pdfTable_add_header_above()
#' @param ex_font_size See `font_size` in kableExtra:::pdfTable_add_header_above()
#' @param ex_angle See `angle` in kableExtra:::pdfTable_add_header_above()
#' @param ex_escape See `escape` in kableExtra:::pdfTable_add_header_above()
#' @param ex_line See `line` in kableExtra:::pdfTable_add_header_above()
#' @param ex_line_sep See `line_sep` in kableExtra:::pdfTable_add_header_above()
#'
#' @importFrom knitr kable
#' @importFrom kableExtra row_spec kable_styling landscape linebreak
#' @examples
#' csas_table(head(iris))
#' @export
csas_table <- function(x,
                       format = "pandoc",
                       booktabs = TRUE,
                       linesep = "",
                       longtable = TRUE,
                       font_size = NULL,
                       landscape = FALSE,
                       bold_header = TRUE,
                       repeat_header = TRUE,
                       repeat_header_text = "",
                       repeat_header_method = "replace",
                       col_names = NULL,
                       col_names_align = "c",
                       escape = FALSE,
                       hold_position = TRUE,
                       extra_header = NULL,
                       ex_bold = FALSE,
                       ex_italic = FALSE,
                       ex_monospace = FALSE,
                       ex_underline = FALSE,
                       ex_strikeout = FALSE,
                       ex_align = "c",
                       ex_color = NULL,
                       ex_background = NULL,
                       ex_font_size = NULL,
                       ex_angle = NULL,
                       ex_escape = TRUE,
                       ex_line = TRUE,
                       ex_line_sep = 3,
                       ...) {
  if (!is.null(col_names)) {
    # Check for newlines in column headers and convert to proper latex linebreaks
    # See 'Insert linebreak in table' section in the following
    # http://haozhu233.github.io/kableExtra/best_practice_for_newline_in_latex_table.pdf
    if (length(grep("\n", col_names))) {
      ## Only use kableExtra if there are newlines
      col_names <- linebreak(col_names, align = col_names_align)
    }
    k <- kable(x = x,
               format = format,
               booktabs = booktabs,
               linesep = linesep,
               longtable = longtable,
               col.names = col_names,
               escape = escape,
               ...)
    k <- kable_styling(k, font_size = font_size)
  } else {
    k <- kable(x = x,
               format = format,
               booktabs = booktabs,
               linesep = linesep,
               longtable = longtable,
               escape = escape,
               ...)
    k <- kable_styling(k, font_size = font_size)
  }
  if (bold_header) {
    k <- row_spec(k, 0, bold = TRUE)
  }
  if (landscape) {
    k <- landscape(k)
  }
  if (repeat_header) {
    k <- kable_styling(k,
                       latex_options = "repeat_header",
                       repeat_header_text = repeat_header_text,
                       repeat_header_method = repeat_header_method)
  }
  k <- kable_styling(k, font_size = font_size)
  if (hold_position) {
    k <- kable_styling(k, latex_options = "hold_position")
  }
  k <- sub("\\caption\\[\\]\\{\\}", "\\caption*{}", k)
  if(!is.null(extra_header)){
    kable_format <- attr(k, "format")
    if(kable_format != "latex"){
      stop("Adding an extra header is only supported for latex builds.", call. = FALSE)
    }
    k <- add_extra_header(k,
                          header = extra_header,
                          ex_bold,
                          ex_italic,
                          ex_monospace,
                          ex_underline,
                          ex_strikeout,
                          ex_align,
                          ex_color,
                          ex_background,
                          ex_font_size,
                          ex_angle,
                          ex_escape,
                          ex_line,
                          ex_line_sep)
  }
  k
}

#' Adds an extra header to the top of a [csas_table()]. Works for longpages.
#'
#' @param kable_input An R object, typically a matrix or data frame.
#' @param header a vector of character strings to use for the extra header names
#' @param bold See kableExtra:::pdfTable_add_header_above()
#' @param italic See kableExtra:::pdfTable_add_header_above()
#' @param monospace See kableExtra:::pdfTable_add_header_above()
#' @param underline See kableExtra:::pdfTable_add_header_above()
#' @param strikeout See kableExtra:::pdfTable_add_header_above()
#' @param align See kableExtra:::pdfTable_add_header_above()
#' @param color See kableExtra:::pdfTable_add_header_above()
#' @param background See kableExtra:::pdfTable_add_header_above()
#' @param font_size See kableExtra:::pdfTable_add_header_above()
#' @param angle See kableExtra:::pdfTable_add_header_above()
#' @param escape See kableExtra:::pdfTable_add_header_above()
#' @param line See kableExtra:::pdfTable_add_header_above()
#' @param line_sep See kableExtra:::pdfTable_add_header_above()
#'
#' @return See kableExtra:::pdfTable_add_header_above()
add_extra_header <- function(kable_input,
                             header = NULL,
                             bold = FALSE,
                             italic = FALSE,
                             monospace = FALSE,
                             underline = FALSE,
                             strikeout = FALSE,
                             align = "c",
                             color = NULL,
                             background,
                             font_size,
                             angle,
                             escape,
                             line = TRUE,
                             line_sep = 3){

  table_info <- kableExtra::magic_mirror(kable_input)
  header <- kableExtra:::standardize_header_input(header)
  if(length(table_info$colnames) != nrow(header)){
    stop("The number of extra headers supplied is not the same as the number of columns in the table", call. = FALSE)
  }
  if(escape){
    header$header <- kableExtra:::input_escape(header$header, align)
  }
  align <- match.arg(align, c("c", "l", "r"))
  hline_type <- switch(table_info$booktabs + 1,
                       "\\\\hline",
                       "\\\\toprule")
  new_header_split <-
    kableExtra:::pdfTable_new_header_generator(header,
                                               table_info$booktabs,
                                               bold,
                                               italic,
                                               monospace,
                                               underline,
                                               strikeout,
                                               align,
                                               color,
                                               background,
                                               font_size,
                                               angle,
                                               line_sep)
  if(line){
    new_header <- paste0(new_header_split[1], "\n",
                         new_header_split[2])
  }else{
    new_header <- new_header_split[1]
  }
  j <- utf8_inp <- kableExtra:::solve_enc(kable_input)
  out <- stringr::str_replace_all(utf8_inp,
                                  hline_type,
                                  paste0(hline_type, "\n", new_header))
  out <- structure(out,
                   format = "latex",
                   class = "knitr_kable")

  if(is.null(table_info$new_header_row)){
    table_info$new_header_row <- new_header_split[1]
    table_info$header_df <- list(header)
  }else{
    table_info$new_header_row <- c(table_info$new_header_row,
                                   new_header_split[1])
    table_info$header_df[[length(table_info$header_df) + 1]] <- header
  }
  attr(out, "kable_meta") <- table_info
  out
}
