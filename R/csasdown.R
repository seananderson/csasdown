#' csasdown: A package for creating CSAS Research Document with R Markdown/bookdown
#'
#' Uses the 'bookdown' R package to generate CSAS (Canadian Science Advisory
#' Secretariat) Research Documents, Science Responses, and Technical Reports in
#' PDF or Word format. The package is based on Chester Ismay's thesisdown
#' package and Ben Marwick's huskydown package.
#'
#' @name csasdown
#' @keywords internal
"_PACKAGE"
NULL

if(getRversion() >= "2.15.1")
  utils::globalVariables(c("region_info", ".x", ".y",
                           "fn", "num_lines", "post_num",
                           "pre_num"))
