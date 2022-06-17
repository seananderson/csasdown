testing_path <- file.path(tempdir(), "sr")
unlink(testing_path, recursive = TRUE, force = TRUE)
dir.create(testing_path, showWarnings = FALSE)
setwd(testing_path)
suppressMessages(csasdown::draft(
  system.file("rmarkdown", "templates", "sr", package = "csasdown"),
  create_dir = FALSE,
  edit = FALSE
))

# ----------------------------------------------------
# Make sure all YAML options are contained in index.Rmd
expect_message(csasdown::check_yaml(type = "sr"), "contains all")

# ----------------------------------------------------
# Render the PDF sr
test_that("csasdown::render generates the PDF of the sr", {
  csasdown::set_french(val = FALSE)
  csasdown:::set_render_type(doc_type = "pdf")
  csasdown::render()
  expect_true(file.exists(file.path(testing_path, "_book", "sr-english.pdf")))
})

# ----------------------------------------------------
# Render the Word sr
test_that("csasdown::render generates the .docx of the sr", {
  csasdown::set_french(val = FALSE)
  csasdown:::set_render_type(doc_type = "word")
  csasdown::render()
  expect_true(file.exists(file.path(testing_path, "_book", "sr-english.docx")))
})

# ----------------------------------------------------
# Render the French PDF sr
test_that("csasdown::render generates the French PDF of the sr", {
  csasdown::set_french(val = TRUE)
  csasdown:::set_render_type(doc_type = "pdf")
  csasdown::render()
  expect_true(file.exists(file.path(testing_path, "_book", "sr-french.pdf")))
})

# ----------------------------------------------------
# Render the French Word sr
test_that("csasdown::render generates the French .docx of the sr", {
  csasdown::set_french(val = TRUE)
  csasdown:::set_render_type(doc_type = "word")
  csasdown::render()
  expect_true(file.exists(file.path(testing_path, "_book", "sr-french.docx")))
})
