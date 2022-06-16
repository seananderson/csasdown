test_that("get_book_filename() works for the resdoc", {
  testing_path <- file.path(tempdir(), "resdoc-get-book-filename")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(csasdown::draft(
    system.file("rmarkdown", "templates", "resdoc", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  expect_error(csasdown:::get_book_filename(NULL))
  expect_error(csasdown:::get_book_filename(""))
  expect_identical(csasdown:::get_book_filename(), "index.Rmd")
  file.copy("_bookdown.yml", "tmp-bookdown.yml")
  expect_identical(csasdown:::get_book_filename("tmp-bookdown.yml"), "index.Rmd")

  yaml <- readLines("_bookdown.yml")
  yaml <- gsub("index.Rmd", "tmp-index.Rmd", yaml)
  writeLines(yaml, "_bookdown.yml")
  expect_identical(csasdown:::get_book_filename(), "tmp-index.Rmd")

  yaml <- gsub("^rmd_files:.*$", "", yaml)
  writeLines(yaml, "_bookdown.yml")
  expect_error(csasdown:::get_book_filename())
})

test_that("get_book_filename() works for the SR", {
  testing_path <- file.path(tempdir(), "sr-get-book-filename")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(csasdown::draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  expect_error(csasdown:::get_book_filename(NULL))
  expect_error(csasdown:::get_book_filename(""))
  expect_identical(csasdown:::get_book_filename(), "index.Rmd")
  file.copy("_bookdown.yml", "tmp-bookdown.yml")
  expect_identical(csasdown:::get_book_filename("tmp-bookdown.yml"), "index.Rmd")

  yaml <- readLines("_bookdown.yml")
  yaml <- gsub("index.Rmd", "tmp-index.Rmd", yaml)
  writeLines(yaml, "_bookdown.yml")
  expect_identical(csasdown:::get_book_filename(), "tmp-index.Rmd")

  yaml <- gsub("^rmd_files:.*$", "", yaml)
  writeLines(yaml, "_bookdown.yml")
  expect_error(csasdown:::get_book_filename())
})

test_that("get_book_filename() works for the techreport", {
  testing_path <- file.path(tempdir(), "techreport-get-book-filename")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(csasdown::draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  expect_error(csasdown:::get_book_filename(NULL))
  expect_error(csasdown:::get_book_filename(""))
  expect_identical(csasdown:::get_book_filename(), "index.Rmd")
  file.copy("_bookdown.yml", "tmp-bookdown.yml")
  expect_identical(csasdown:::get_book_filename("tmp-bookdown.yml"), "index.Rmd")

  yaml <- readLines("_bookdown.yml")
  yaml <- gsub("index.Rmd", "tmp-index.Rmd", yaml)
  writeLines(yaml, "_bookdown.yml")
  expect_identical(csasdown:::get_book_filename(), "tmp-index.Rmd")

  yaml <- gsub("^rmd_files:.*$", "", yaml)
  writeLines(yaml, "_bookdown.yml")
  expect_error(csasdown:::get_book_filename())
})
