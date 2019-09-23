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
#' @param col_names Names for the columns to show on table. If there are any
#'   \\\ns, they will be replaced with the
#' @param col_names_align As defined in [kableExtra::linebreak()].
#' @param escape As defined by [kableExtra::kable_styling()].
#' @param ... Other arguments to pass to [knitr::kable()].
#' @param hold_position force the table placement to be where the code is called
#'   (don't let latex positino the table where it wants)
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
                       ...) {
  if (!is.null(col_names)) {
    # Check for newlines in column headers and convert to proper latex linebreaks
    # See 'Insert linebreak in table' section in the following
    # http://haozhu233.github.io/kableExtra/best_practice_for_newline_in_latex_table.pdf
    if (length(grep("\n", col_names))) {
      ## Only use kableExtra if there are newlines
      col_names <- linebreak(col_names, align = col_names_align)
    }
    k <- kable(
      x = x,
      format = format,
      booktabs = booktabs,
      linesep = linesep,
      longtable = longtable,
      col.names = col_names,
      escape = escape,
      ...
    )
  } else {
    k <- kable(
      x = x,
      format = format,
      booktabs = booktabs,
      linesep = linesep,
      longtable = longtable,
      escape = escape,
      ...
    )
  }
  if (bold_header) {
    k <- row_spec(k, 0, bold = TRUE)
  }
  if (landscape) {
    k <- landscape(k)
    if (repeat_header) {
      k <- kable_styling(k,
        latex_options = "repeat_header",
        repeat_header_text = repeat_header_text,
        repeat_header_method = repeat_header_method
      )
    }
  }
  k <- kable_styling(k, font_size = font_size)
  if (hold_position) {
    k <- kable_styling(k, latex_options = "hold_position")
  }
  k <- sub("\\caption\\[\\]\\{\\}", "\\caption*{}", k)
  k
}
