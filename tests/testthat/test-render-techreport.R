rnd_num <- as.integer(abs(rnorm(1) * 1e6))
testing_path <- file.path(tempdir(), paste0("techreport_", rnd_num))
unlink(testing_path, recursive = TRUE, force = TRUE)
dir.create(testing_path, showWarnings = FALSE)
setwd(testing_path)
suppressMessages(csasdown::draft(
  system.file("rmarkdown", "templates", "techreport", package = "csasdown"),
  create_dir = FALSE,
  edit = FALSE
))

# ----------------------------------------------------
# Make sure all YAML options are contained in index.Rmd
expect_message(csasdown::check_yaml(type = "techreport"), "contains all")

# ----------------------------------------------------
# Render the PDF techreport
test_that("csasdown::render generates the PDF of the techreport", {
  csasdown::set_french(val = FALSE)
  csasdown:::set_render_type(doc_type = "pdf")
  csasdown::render()
  expect_true(file.exists(file.path(testing_path, "_book", "techreport-english.pdf")))
})

# ----------------------------------------------------
# Render the Word techreport
test_that("csasdown::render generates the .docx of the techreport", {
  csasdown::set_french(val = FALSE)
  csasdown:::set_render_type(doc_type = "word")
  csasdown::render()
  expect_true(file.exists(file.path(testing_path, "_book", "techreport-english.docx")))
})


# French techreport does not currently work, and never has
#
# ----------------------------------------------------
# Render the French PDF techreport
#test_that("csasdown::render generates the French PDF of the techreport", {
  # csasdown::set_french(val = TRUE)
  # csasdown:::set_render_type(doc_type = "pdf")
  # expect_warning(csasdown::render())
  # expect_true(file.exists(file.path(testing_path, "_book", "techreport-french.pdf")))
#})

# ----------------------------------------------------
# Render the French Word techreport
# test_that("csasdown::render generates the French .docx of the techreport", {
#   csasdown::set_french(val = TRUE)
#   csasdown:::set_render_type(doc_type = "word")
#   suppressWarnings(csasdown::render())
#   expect_true(file.exists(file.path(testing_path, "_book", "techreport-french.docx")))
# })
