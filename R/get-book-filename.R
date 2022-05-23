#' Extract the book filename from the YAML file (usually index.Rmd)
#'
#' @details
#' `book_file` is the "skeleton.Rmd" file, which is typically called
#' "index.Rmd" by default in a project. It is found in the bookdown YAML file

#' @param yaml_fn The YAML file name. The default is '_bookdown.yml' for
#'
#' @return The book filename (typically index.Rmd) for [bookdown]
#' @importFrom stringr str_extract_all
#' @export
get_book_filename <- function(yaml_fn = "_bookdown.yml"){

  yaml <- readLines(yaml_fn)
  book_fn_ind <- grep("^rmd_files: \\[", yaml)
  book_fn <- str_extract_all(yaml[book_fn_ind], "[a-zA-Z0-9_\\-]+\\.(R|r)md")[[1]]
  if(!length(book_fn)){
    stop("Book filename not found in ", yaml_fn, ". ",
         "This is typically index.Rmd and should be the first entry after ",
         "`rmd_files:[` and on the same line as it",
         call. = FALSE)
  }
  # In case there were more than one Rmd files on the first line
  book_fn[1]
}