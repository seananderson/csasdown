# nocov start
#' Create a new CSAS document based on a template
#'
#' Create a draft of an R Markdown CSAS document
#'
#' @param type The type of document to start.
#' @param create_dir `TRUE` to create a new directory for the document.
#' @param edit `TRUE` to edit the template immediately.
#' @param ... Other arguments to pass to [rmarkdown::draft()].
#'
#' @details This is a light wrapper around [rmarkdown::draft()]. Consult that
#'   function for further details.
#'
#' @examples
#' \dontrun{
#' csasdown::draft("resdoc")
#' csasdown::draft("sr")
#' csasdown::draft("techreport")
#' }
#' @export
draft <- function(type = c("resdoc", "sr", "techreport"),
                  create_dir = FALSE,
                  edit = FALSE,
                  ...) {
  type <- match.arg(type)
  rmarkdown::draft("index.Rmd",
    template = type,
    package = "csasdown",
    create_dir = create_dir,
    edit = edit,
    ...)
  # rmarkdown::draft does not copy files that begin with a dot (on Windows)
  # so we just rename the git ignore file
  if(file.exists("_gitignore")){
    file.rename("_gitignore", ".gitignore")
  }
  if(file.exists("_here")){
    file.rename("_here", ".here")
  }
  create_rstudio_project_file()
}

#' Creates a blank Rstudio project file in the current directory with the same name
#' as the current directory.
create_rstudio_project_file <- function(){
  wd <- getwd()
  fn <- file.path(wd, paste0(basename(wd), ".Rproj"))
  txt <- c("Version: 1.0",
           "\n",
           "\n",
           "RestoreWorkspace: Default",
           "SaveWorkspace: Default",
           "AlwaysSaveHistory: Default",
           "\n",
           "EnableCodeIndexing: Yes",
           "UseSpacesForTab: Yes",
           "NumSpacesForTab: 2",
           "Encoding: UTF-8",
           "\n",
           "RnwWeave: knitr",
           "LaTeX: pdfLaTeX",
           "\n",
           "StripTrailingWhitespace: Yes")
  writeLines(txt, fn)
}
# nocov end
