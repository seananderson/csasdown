#' Create a new CSAS document based on a template
#'
#' Create a draft of an R Markdown CSAS document
#'
#' @param type The type of document to draft
#' @param verbose Logical. If `TRUE`, print messages
#' @param directory The directory to place the draft document files
#' @param edit Logical. If `TRUE`, edit the template file immediately
#' (See `edit` in [rmarkdown::draft()])
#' @param testing Logical. If `TRUE`, the question about overwriting
#' files which already exist will be set to `yes` automatically to
#' ensure unit test work correctly
#' @param testing_affirm Logical. If `TRUE`, set overwrite to `TRUE`
#' for testing if `testing` is `TRUE`. If `TRUE` set overwrite to `FALSE`
#' for testing if `testing` is `TRUE`. If `testing` is `FALSE`, this
#' will be ignored
#' @param ... Other arguments to pass to [rmarkdown::draft()].
#'
#' @details This is a light wrapper around [rmarkdown::draft()]. Consult that
#' function for further details.
#'
#' @family drafting
#' @examples
#' \dontrun{
#' csasdown::draft("resdoc")
#' csasdown::draft("resdoc-b")
#' csasdown::draft("sr")
#' csasdown::draft("techreport")
#' }
#' @export
draft <- function(type = c("resdoc", "resdoc-b", "sr", "techreport"),
                  directory = getwd(),
                  edit = FALSE,
                  verbose = TRUE,
                  testing = FALSE,
                  testing_affirm = FALSE,
                  ...) {

  if(!dir.exists(directory)){
    bail("The directory ", fn_color(directory), " does not exist so ",
         "the csasdown project cannot be created there")
  }

  package <- NULL
  if (!grepl("\\/rmarkdown\\/templates", type)) {
    # so it also works with unit testing
    if (!type %in% c("resdoc", "resdoc-b", "sr", "techreport")) {
      alert(csas_color("type"), " should be one of ",
            csas_color("resdoc, resdoc-b, sr, or techreport"), ".")
    }
    package <- "csasdown"
  }

  if(verbose){
    notify("Drafting a new ", csas_color(type), " project ...")
  }

  all_files <- dir()
  # Keep RStudio project files during deletion and don't include them
  # in the check to see if the directory is empty
  fns <- all_files[-grep("^.*(RProj|Rproj|rproj)$", all_files)]
  if(testing){
    affirm_delete <- ifelse(testing_affirm, "y", "n")
  }else{
    # nocov start
    if(!length(fns)){
      affirm_delete <- "y"
    }else{
      affirm_delete <- readline(
        question("This action will delete the contents ",
                 "of the directory ", fn_color(directory), " ...\n",
                 "Are you sure you want to do this (y/n)?"))
    }
    # nocov end
  }
  if(affirm_delete == "y" || affirm_delete == "Y"){
    unlink(fns, recursive = TRUE, force = TRUE)
    check_notify("Deleted all non-.Rproj files and directories in directory ",
                 fn_color(directory), "\n")
  }else{
    bail("New document not drafted, you decided to keep the old files.")
  }

  rmarkdown::draft("index.Rmd",
    template = type,
    package = package,
    edit = edit,
    ...)

  # rmarkdown::draft() does not copy files that begin with a dot (on Windows)
  # so we just rename the git ignore file
  if (file.exists("_gitignore")) {
    file.rename("_gitignore", ".gitignore")
  }
  if (file.exists("_here")) {
    file.rename("_here", ".here")
  }
  create_rstudio_project_file(directory)

  if(verbose){
    dirs <- list.dirs()
    dirs <- dirs[-grep("^(\\.\\/\\.Rproj.*|\\.)$", dirs)]
    check_notify("Directories copied:\n",
                 paste(dirs, collapse = "\n"), "\n")
    files <- setdiff(list.files(), list.dirs(recursive = FALSE, full.names = FALSE))
    check_notify("Files copied:\n",
                 paste(files, collapse = "\n"), "\n")
    check_notify("Successfully drafted a new ", csas_color(type),
                 " project in ", fn_color(directory), "\n",
                 "To render it into a document, run ",
                 csas_color("csasdown::render()"))
  }
}
