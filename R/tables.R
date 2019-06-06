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
#' @param font_size Font size in pts.
#' @param landscape Make this table in landscape orientation?
#' @param repeat_header If landscape, repeat the header on subsequent pages?
#' @param ... Other arguments to pass to [knitr::kable()].
#'
#' @examples
#' csas_table(head(iris))
#' @export
csas_table <- function(x,
                       format = "pandoc",
                       booktabs = TRUE,
                       linesep = "",
                       longtable = TRUE,
                       font_size = 10,
                       landscape = FALSE,
                       repeat_header = TRUE,
                       ...){
  k <- knitr::kable(x = x,
                    format = format,
                    booktabs = booktabs,
                    linesep = linesep,
                    longtable = longtable,
                    ...)
  if(landscape){
    k <- k %>%
      kableExtra::landscape()
    if(repeat_header){
      k <- k %>%
        kableExtra::kable_styling(font_size = font_size,
                                  latex_options = "repeat_header",
                                  repeat_header_text = "",
                                  repeat_header_method = "replace")
    }else{
      k <- k %>%
        kableExtra::kable_styling(font_size = font_size)
    }
    k <- k %>%
      sub("\\caption\\[\\]\\{\\}", "\\caption*{}", .)
  }
  k
}
