#' Run latexmk/pdflatex on a csasdown report
#'
#' Run latexmk/pdflatex on a csasdown report with extra pdflatex runs.
#'
#' @details
#' This can be useful in edge cases where [tinytex::latexmk()] doesn't
#' run enough times on its own.
#' For example <https://github.com/pbs-assess/csasdown/issues/151>.
#'
#' @param extra_pdflatex Extra number of times to run [tinytex::pdflatex()].
#' @param ... Arguments to pass to [tinytex::latexmk()].
#'
#' @return
#' The PDF in the `_book` folder should be updated.
#' @export
#'
#' @examples
#' \dontrun{
#' # After running `bookdown::render_book("index.Rmd")` or knitting, run:
#' run_pdflatex()
#' }
run_pdflatex <- function(extra_pdflatex = 1, ...) {
  dir_file <- list.files("_book", pattern = "*.tex", full.names = TRUE)[[1]]
  file <- list.files("_book", pattern = "*.tex", full.names = FALSE)[[1]]
  if (file.exists(file) || file.exists(gsub("\\.tex", "\\.pdf", file))) {
    stop("The file '", file,
      "' (or its PDF version) already exists\nin the main report folder.",
      " Delete it before running this function.",
      call. = FALSE
    )
  }
  file.copy(dir_file, ".", overwrite = FALSE)
  tinytex::latexmk(file, clean = FALSE, ...)
  # The point of this function is that latexmk sometimes misses a run:
  for (i in seq_len(extra_pdflatex - 1)) {
    tinytex::pdflatex(file, clean = FALSE)
  }
  tinytex::pdflatex(file, clean = TRUE)
  file.copy(gsub("\\.tex", "\\.pdf", file), "_book", overwrite = TRUE)
  file.remove(file)
  file.remove(gsub("\\.tex", "\\.pdf", file))
}
