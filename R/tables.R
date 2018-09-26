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
#' @param ... Other arguments to pass to [knitr::kable()].
#'
#' @examples
#' csas_table(head(iris))
#' @export

csas_table <- function(x, format = "pandoc", booktabs = TRUE, linesep = "",
  longtable = TRUE, ...)  {
  knitr::kable(x = x, format = format, booktabs = booktabs, linesep = linesep,
    longtable = longtable, ...)
}
