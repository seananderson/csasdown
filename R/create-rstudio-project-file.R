#' Creates a blank Rstudio project file in the current directory with the
#' same name as the current directory.
#'
#' @keywords internal
#' @family drafting
#'
#' @param directory The directory in which to create the RStudio project file
#' @param verbose Logical. If `TRUE`, print messages
create_rstudio_project_file <- function(directory = getwd(),
                                        verbose = TRUE) {

  if(verbose){
    notify("Creating a new RStudio project file ...")
  }

  if(!dir.exists(directory)){
    bail("The directory ", fn_color(directory), " does not exist so ",
         "an RStudio project file cannot be created there")
  }

  fn <- file.path(directory, paste0(basename(directory), ".Rproj"))
  txt <- c(
    "Version: 1.0",
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
    "AutoAppendNewLine: Yes",
    "StripTrailingWhitespace: Yes"
  )
  writeLines(txt, fn)

  if(verbose){
    check_notify("Created a new RStudio project file: ",
                 fn_color(fn), "\n")
  }
}
