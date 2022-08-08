test_that("rename_output_files() works", {

  testing_path <- file.path(tempdir(), "test-rename")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(csasdown::draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  d <- "_book"
  dir.create(d)
  file.create(file.path(d, "sr.pdf"))
  file.create(file.path(d, "sr.tex"))
  options(french = FALSE)
  csasdown:::set_render_type(doc_type = "pdf")
  csasdown:::rename_output_files("index.Rmd")
  expect_true(file.exists(file.path("_book", "sr-english.pdf")))
  expect_true(file.exists(file.path("_book", "sr-english.tex")))

  w <- capture_warnings(csasdown:::rename_output_files("index.Rmd"))
  expect_match(w[1], paste0("Could not copy file from"))
  expect_match(w[2], paste0("Could not copy file from"))

  file.create(file.path(d, "sr.docx"))
  file.create(file.path(d, "reference-keys.txt"))
  csasdown:::set_render_type(doc_type = "word")
  csasdown:::rename_output_files("index.Rmd")
  expect_true(file.exists(file.path("_book", "sr-english.docx")))
  expect_true(file.exists(file.path("_book", "reference-keys-docx-english.txt")))

  w <- capture_warnings(csasdown:::rename_output_files("index.Rmd"))
  expect_match(w[1], paste0("Could not copy file from"))
  expect_match(w[2], paste0("Could not copy file from"))

})
