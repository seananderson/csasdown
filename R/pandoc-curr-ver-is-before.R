#' Checks whether or not the current version of Pandoc installed is below a
#' given version
#'
#' @param ver The version to compare the current version against. If `NULL`,
#' an error is thrown
#'
#' @return If the current version of Pandoc is prior to this dotted string
#' version, `TRUE`. If not `FALSE` is returned.
#' @export
pandoc_curr_ver_is_before <- function(ver = "3.1.8"){

  if(is.null(ver)){
    stop("Must supply a version string to compare (`ver`)")
  }
  ver_curr <- rmarkdown::pandoc_version()
  ver <- numeric_version(ver)

  ver_curr < ver
}
