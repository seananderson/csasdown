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

  x <- csasdown:::capture_log(csasdown:::rename_output_files("index.Rmd"))
  j <- x(1)
  j <- j$logs
  mess <- purrr::map_chr(j, ~{.x$message})
  types <- purrr::map_chr(j, ~{.x$type})
  expect_match(mess[1], paste0("cannot rename file '_book/sr.tex' ",
                               "to '_book/sr-english.tex'"))
  expect_match(mess[2], paste0("cannot rename file '_book/sr.pdf' ",
                               "to '_book/sr-english.pdf'"))
  expect_match(mess[3], paste0("Could not rename the file \\S+ ",
                               "to \\S+"))
  expect_match(mess[4], paste0("Could not rename the file \\S+ ",
                               "to \\S+"))

  file.create(file.path(d, "sr.docx"))
  file.create(file.path(d, "reference-keys.txt"))
  csasdown:::set_render_type(doc_type = "word")
  csasdown:::rename_output_files("index.Rmd")
  expect_true(file.exists(file.path("_book", "sr-english.docx")))
  expect_true(file.exists(file.path("_book", "reference-keys-docx-english.txt")))

  x <- csasdown:::capture_log(csasdown:::rename_output_files("index.Rmd"))
  j <- x(1)
  j <- j$logs
  mess <- purrr::map_chr(j, ~{.x$message})
  expect_match(mess[1],
               paste0("cannot rename file '_book/reference-keys.txt' ",
                      "to '_book/reference-keys-docx-english.txt'"))
  expect_match(mess[2],
               paste0("cannot rename file '_book/sr.docx' ",
                      "to '_book/sr-english.docx'"))
  expect_match(mess[3],
                   paste0("Could not rename the file \\S+ ",
                   "to \\S+"))
  expect_match(mess[4],
                   paste0("Could not rename the file \\S+ to ",
                          "\\S+"))

})
