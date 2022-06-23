test_that("run_pdflatex() works", {

  testing_path <- file.path(tempdir(), "resdoc-get-book-filename")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(csasdown::draft(
    system.file("rmarkdown", "templates", "resdoc", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  csasdown::render()

  # -----------------------------------------------------------------------------
  expect_error(csasdown::run_pdflatex(),
               paste0("The file '_book/resdoc-english.pdf' exists. Delete it ",
                      "before running this function."))

  # -----------------------------------------------------------------------------
  unlink("_book/*.pdf", force = TRUE)

  suppressWarnings(csasdown::run_pdflatex(extra_pdflatex = 2))
})
