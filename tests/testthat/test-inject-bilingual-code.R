test_that("inject_bilingual_code works properly and doesn't inject more than once", {
  testing_path <- file.path(tempdir(), "inject-bilingual-code")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(csasdown::draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  # -----------------------------------------------------------------------------
  csasdown::render(keep_files = TRUE)

  csasdown::render("tmp_bookdown.yml", keep_files = TRUE)
  expect_true(file.exists("tmptmp_bookdown.yml"))
  expect_true(file.exists("tmp-tmp-01_context.Rmd"))
  expect_true(file.exists("tmp-tmp-08_appendix.Rmd"))
  expect_true(file.exists("tmp-01_context.Rmd"))
  expect_true(file.exists("tmp-08_appendix.Rmd"))

  csasdown::render("tmptmp_bookdown.yml", keep_files = TRUE)
  expect_true(file.exists("tmptmptmp_bookdown.yml"))
  expect_true(file.exists("tmp-tmp-tmp-01_context.Rmd"))
  expect_true(file.exists("tmp-tmp-tmp-08_appendix.Rmd"))

  csasdown::render("tmptmp_bookdown.yml", keep_files = FALSE)
  expect_false(file.exists("tmptmptmp_bookdown.yml"))
  expect_false(file.exists("tmp-tmp-tmp-01_context.Rmd"))
  expect_false(file.exists("tmp-tmp-tmp-08_appendix.Rmd"))

  csasdown::render("tmp_bookdown.yml", keep_files = FALSE)
  expect_false(file.exists("tmptmp_bookdown.yml"))
  expect_false(file.exists("tmp-tmp-01_context.Rmd"))
  expect_false(file.exists("tmp-tmp-08_appendix.Rmd"))
  expect_true(file.exists("tmp-01_context.Rmd"))
  expect_true(file.exists("tmp-08_appendix.Rmd"))

})
