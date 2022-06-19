test_that("create_tmp_yaml_rmd_files() throws errors", {
  testing_path <- file.path(tempdir(), "resdoc-create-ymp-yaml-rmd-files-errors")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(csasdown::draft(
    system.file("rmarkdown", "templates", "resdoc", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  expect_error(csasdown:::create_tmp_yaml_rmd_files("nonexistent-file.yml"),
               "YAML file nonexistent-file.yml does not exist")

  file.create("empty.yml")
  expect_error(csasdown:::create_tmp_yaml_rmd_files("empty.yml"),
               "YAML file empty.yml is empty")
  rmd <- readLines("_bookdown.yml")
  back_rmd <- rmd

  ind <- grep("\\[", rmd)
  tmp <- rmd[ind]
  rmd[ind] <- ""
  writeLines(rmd, "_bookdown.yml")
  expect_error(csasdown:::create_tmp_yaml_rmd_files(),
               "`rmd_files: \\[` not found")

  rmd[ind] <- tmp
  rmd[ind + 1] <- tmp
  writeLines(rmd, "_bookdown.yml")
  expect_error(csasdown:::create_tmp_yaml_rmd_files(),
               "More than one `rmd_files: \\[` found")

  rmd <- back_rmd
  writeLines(rmd, "_bookdown.yml")

  ind <- grep("\\]", rmd)
  tmp <- rmd[ind]
  rmd[ind] <- ""
  writeLines(rmd, "_bookdown.yml")
  expect_error(csasdown:::create_tmp_yaml_rmd_files(),
               "`\\]` not found")

  rmd[2:length(rmd)] <- ""
  rmd[2] <- "rmd_files: ["
  rmd[3] <- "]"
  writeLines(rmd, "_bookdown.yml")
  expect_error(csasdown:::create_tmp_yaml_rmd_files(),
               "No .Rmd filenames found")

  rmd <- back_rmd
  rmd <- rmd[-1]
  rmd <- rmd[-length(rmd)]
  writeLines(rmd, "_bookdown.yml")
  tmp <- csasdown:::create_tmp_yaml_rmd_files()
  expect_identical(tmp[[1]], c("tmp_bookdown.yml"))
  expect_identical(tmp[[2]], c("tmp-index.Rmd",
                               "tmp-01-chap1.Rmd",
                               "tmp-02-chap2.Rmd",
                               "tmp-03-chap3.Rmd",
                               "tmp-04-references.Rmd",
                               "tmp-05-appendix.Rmd"))
})

test_that("create_tmp_yaml_rmd_files() works", {
  testing_path <- file.path(tempdir(), "resdoc-create-ymp-yaml-rmd-files")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(csasdown::draft(
    system.file("rmarkdown", "templates", "resdoc", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  csasdown:::create_tmp_yaml_rmd_files()
  expect_true(file.exists("tmp_bookdown.yml"))
  expect_true(file.exists("tmp-index.Rmd"))
  expect_true(file.exists("tmp-01-chap1.Rmd"))
  expect_true(file.exists("tmp-02-chap2.Rmd"))
  expect_true(file.exists("tmp-03-chap3.Rmd"))
  expect_true(file.exists("tmp-04-references.Rmd"))
  expect_true(file.exists("tmp-05-appendix.Rmd"))

})