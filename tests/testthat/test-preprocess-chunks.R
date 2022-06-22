test_that("csasdown:::preprocess_chunks() works", {

  rmd_dir <- testthat::test_path("preprocess-chunks-files")

  testing_path <- file.path(tempdir(), "resdoc-preprocess-chunks")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  file.copy(file.path(rmd_dir, "empty-file.Rmd"),
            file.path(testing_path, "empty-file.Rmd"))
  file.copy(file.path(rmd_dir, "double-chunk-cat.Rmd"),
            file.path(testing_path, "double-chunk-cat.Rmd"))
  file.copy(file.path(rmd_dir, "double-chunk-cat-extra-blanks.Rmd"),
            file.path(testing_path, "double-chunk-cat-extra-blanks.Rmd"))
  file.copy(file.path(rmd_dir, "double-chunk-cat-single-quotes.Rmd"),
            file.path(testing_path, "double-chunk-cat-single-quotes.Rmd"))
  setwd(testing_path)
  suppressMessages(csasdown::draft(
    system.file("rmarkdown", "templates", "resdoc-b", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  expect_invisible(csasdown:::preprocess_chunks(NULL))

  expect_error(csasdown:::preprocess_chunks("unknown-file.Rmd"),
               "The file 'unknown-file.Rmd' does not exist. Check the YAML file entry")


  csasdown:::preprocess_chunks("double-chunk-cat.Rmd")

  expect_identical(readLines("double-chunk-cat.Rmd"),
                   c("```{r chunk-1-en, eval = !fr(), results = 'asis'}",
                     "cat(\"# Header",
                     "",
                     "Test text \", print('Hello World!'), \"\")",
                     "```",
                     "",
                     "```{r chunk-1-fr, eval = fr(), results = 'asis', needs_trans = TRUE}",
                     "cat(\"## Header 2",
                     "Test text \", print('Hello World 2!'), \"\")",
                      "```"))


  expect_error(expect_message(csasdown:::preprocess_chunks("double-chunk-cat-extra-blanks.Rmd"),
                              paste0("Not all chunks in the file double-chunk-cat-extra-blanks.Rmd ",
                                     "with `needs_trans = TRUE` have `cat(` immediately following. ",
                                     "If the chunks mirror other chunks, make sure that the mirrored ",
                                     "chunk has a `cat()` call in it.\n\nThe chunk name(s) missing ",
                                     "`cat()` are:\n\nchunk-1-fr"), fixed = TRUE),
               "Chunks missing `cat()`", fixed = TRUE)

  csasdown:::preprocess_chunks("double-chunk-cat-single-quotes.Rmd")
  actual <- readLines("double-chunk-cat-single-quotes.Rmd")
  expect_identical(actual, c("```{r chunk-1-en, eval = !fr(), results = 'asis'}",
                             "cat(\"# Header",
                             "",
                             "Test text \", print(\"Hello World!\"), \"\")",
                             "```",
                             "",
                             "```{r chunk-1-fr, eval = fr(), results = 'asis', needs_trans = TRUE}",
                             "cat(\"## Header 2",
                              "Test text \", print('Hello World 2!'), \"\")",
                              "```"))

  csasdown:::preprocess_chunks("empty-file.Rmd")
  expect_identical(readLines("empty-file.Rmd"), character(0))
})
