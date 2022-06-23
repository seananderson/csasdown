#' Creates a temporary directory for compiling the latex file with latex
#' commands for a csasdown type
#'
#' @details
#' The compiled tex file will be copied from either the root directory or
#' the `_book` directory, depending on the value of `where`. The necessary
#' directories `knitr-figs-pdf`, `knitr-figs-word`, `knitr-cache-pdf`, and
#' `knitr-cache-word` will be copied recursively into the temporary directory,
#' preserving the directory structure necessary for the build.
#'
#' @param type The csasdown document type. See [draft()]
#' @param where Where to look for the `.tex` file. If 'r', look in root
#' directory, if 'b', look in the `_book` subdirectory. Any other value will
#' cause an error
#' @param tmp_dir A temporary directory. If `NULL`, once will be created in
#' the filesystem using [tempdir()]
#' @param root_dir The directory where everything will be copied from
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
