test_that("get_index_filename() works for the resdoc", {
  testing_path <- file.path(tempdir(), "resdoc-get-book-filename")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(csasdown::draft(
    system.file("rmarkdown", "templates", "resdoc", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  # -----------------------------------------------------------------------------
  expect_error(csasdown:::get_index_filename(NULL),
               "The `yaml_fn` argument (filename) cannot be `NULL`",
               fixed = TRUE)
  expect_error(csasdown:::get_index_filename(""),
               "The `yaml_fn` argument (filename) cannot be an empty string",
               fixed = TRUE)
  expect_identical(csasdown:::get_index_filename(), "index.Rmd")
  file.copy("_bookdown.yml", "tmp-bookdown.yml")
  expect_identical(csasdown:::get_index_filename("tmp-bookdown.yml"), "index.Rmd")

  # -----------------------------------------------------------------------------
  yaml <- readLines("_bookdown.yml")
  yaml <- gsub("index.Rmd", "tmp-index.Rmd", yaml)
  writeLines(yaml, "_bookdown.yml")
  expect_identical(csasdown:::get_index_filename(), "tmp-index.Rmd")

  # -----------------------------------------------------------------------------
  yaml <- gsub("^rmd_files:.*$", "", yaml)
  writeLines(yaml, "_bookdown.yml")
  expect_error(csasdown:::get_index_filename(),
               paste0("Index filename not found in _bookdown.yml. This is ",
                      "typically index.Rmd and should be the first entry after ",
                      "`rmd_files:[` and on the same line as it"),
               fixed = TRUE)

})

test_that("get_index_filename() works for the SR", {
  testing_path <- file.path(tempdir(), "sr-get-book-filename")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(csasdown::draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  # -----------------------------------------------------------------------------
  expect_error(csasdown:::get_index_filename(NULL))
  expect_error(csasdown:::get_index_filename(""))
  expect_identical(csasdown:::get_index_filename(), "index.Rmd")
  file.copy("_bookdown.yml", "tmp-bookdown.yml")
  expect_identical(csasdown:::get_index_filename("tmp-bookdown.yml"), "index.Rmd")

  # -----------------------------------------------------------------------------
  yaml <- readLines("_bookdown.yml")
  yaml <- gsub("index.Rmd", "tmp-index.Rmd", yaml)
  writeLines(yaml, "_bookdown.yml")
  expect_identical(csasdown:::get_index_filename(), "tmp-index.Rmd")

  # -----------------------------------------------------------------------------
  yaml <- gsub("^rmd_files:.*$", "", yaml)
  writeLines(yaml, "_bookdown.yml")
  expect_error(csasdown:::get_index_filename())
})

test_that("get_index_filename() works for the techreport", {
  testing_path <- file.path(tempdir(), "techreport-get-book-filename")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(csasdown::draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  # -----------------------------------------------------------------------------
  expect_error(csasdown:::get_index_filename(NULL))
  expect_error(csasdown:::get_index_filename(""))
  expect_identical(csasdown:::get_index_filename(), "index.Rmd")
  file.copy("_bookdown.yml", "tmp-bookdown.yml")
  expect_identical(csasdown:::get_index_filename("tmp-bookdown.yml"), "index.Rmd")

  # -----------------------------------------------------------------------------
  yaml <- readLines("_bookdown.yml")
  yaml <- gsub("index.Rmd", "tmp-index.Rmd", yaml)
  writeLines(yaml, "_bookdown.yml")
  expect_identical(csasdown:::get_index_filename(), "tmp-index.Rmd")

  # -----------------------------------------------------------------------------
  yaml <- gsub("^rmd_files:.*$", "", yaml)
  writeLines(yaml, "_bookdown.yml")
  expect_error(csasdown:::get_index_filename())
})
