test_that("inject_rmd_files() works", {
  rmd_dir <- testthat::test_path("inject-rmd-files")

  testing_path <- file.path(tempdir(), "test-inject-rmd")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  file.copy(file.path(rmd_dir, "injected-into.Rmd"),
            file.path(testing_path, "injected-into.Rmd"))
  file.copy(file.path(rmd_dir, "injected-into-2.Rmd"),
            file.path(testing_path, "injected-into-2.Rmd"))
  file.copy(file.path(rmd_dir, "injected-into-multiple.Rmd"),
            file.path(testing_path, "injected-into-multiple.Rmd"))
  file.copy(file.path(rmd_dir, "injected.Rmd"),
            file.path(testing_path, "injected.Rmd"))
  file.copy(file.path(rmd_dir, "injected-triple-backticks.Rmd"),
            file.path(testing_path, "injected-triple-backticks.Rmd"))
  setwd(testing_path)

  csasdown:::inject_rmd_files("injected-into.Rmd")
  actual <- readLines("injected-into.Rmd")
  expect_identical(actual, c("```{r chunk-1-en, eval = !fr(), results = 'asis'}",
                             "cat(\"First chunk\")",
                             "```",
                             "```{r chunk-2-en, eval = !fr(), results = 'asis'}",
                             "cat(\"# Header",
                             "This text will be injected into the chunk in the other file. `r as.character(Sys.time())` is the time.\")",
                             "```" ))

  mess <- "Triple-backticks found in file 'injected-triple-backticks' on line(s) 4, 6"
  err <- paste0("Triple- or Quadruple-backtick code chunks are not allowed in ",
                "external RMD files which have been injected using `rmd_files()`. ",
                "The code is going to be imported into a chunk and embedding chunks ",
                "into other chunks is not possible in knitr. Delete them or use ",
                "Markdown comments to comment those chunks out <!-- -->.")
  expect_error(expect_message(csasdown:::inject_rmd_files("injected-into-2.Rmd"),
                              mess, fixed = TRUE),
               err, fixed = TRUE)

  csasdown:::inject_rmd_files("injected-into-multiple.Rmd")
  actual <- readLines("injected-into-multiple.Rmd")
  expected <- c("```{r chunk-1-en, eval = !fr(), results = 'asis'}",
                "cat(\"First chunk\")",
                "```",
                "```{r chunk-2-en, eval = !fr(), results = 'asis'}",
                "cat(\"# Header",
                "This text will be injected into the chunk in the other file. `r as.character(Sys.time())` is the time.\")",
                "```",
                "",
                "```{r chunk-3-en, eval = !fr(), results = 'asis'}",
                 "cat(\"# Header",
                 "This text will be injected into the chunk in the other file. `r as.character(Sys.time())` is the time.\")",
                 "```",
                 "",
                 "",
                 "",
                 "```{r chunk-4-en, eval = !fr(), results = 'asis'}",
                 "cat(\"# Header",
                 "This text will be injected into the chunk in the other file. `r as.character(Sys.time())` is the time.\")",
                 "```")
  expect_identical(actual, expected)
})
