testing_path <- file.path(tempdir(), "resdoc")
unlink(testing_path, recursive = TRUE, force = TRUE)
dir.create(testing_path, showWarnings = FALSE)
setwd(testing_path)
suppressMessages(csasdown::draft(
  system.file("rmarkdown", "templates", "resdoc", package = "csasdown"),
  create_dir = FALSE,
  edit = FALSE
))

# -----------------------------------------------------------------------------
# Make sure all YAML options are contained in index.Rmd
expect_message(csasdown::check_yaml(), "contains all")

# -----------------------------------------------------------------------------
# Render the PDF resdoc
test_that("csasdown::render generates the PDF of the resdoc", {
  csasdown::set_french(val = FALSE)
  csasdown:::set_render_type(doc_type = "pdf")
  csasdown::render()
  expect_true(file.exists(file.path(testing_path, "_book",
                                    "resdoc-english.pdf")))
})

# -----------------------------------------------------------------------------
# Render the Word resdoc
test_that("csasdown::render generates the .docx of the resdoc", {
  csasdown::set_french(val = FALSE)
  csasdown:::set_render_type(doc_type = "word")
  csasdown::render()
  expect_true(file.exists(file.path(testing_path, "_book",
                                    "resdoc-english.docx")))
})

# -----------------------------------------------------------------------------
# Add the title page to the Word resdoc
# Fails on macos on GitHub
#Add_resdoc_docx_titlepage()

# test_that("add_resdoc_docx_titlepage() generates the .docx of the resdoc", {
#   expect_true(file.exists(file.path(testing_path, "_book", "resdoc.docx")))
# })

# -----------------------------------------------------------------------------
# Render the French PDF resdoc
test_that("csasdown::render generates the French PDF of the resdoc", {
  csasdown::set_french(val = TRUE)
  csasdown:::set_render_type(doc_type = "pdf")
  csasdown::render()
  expect_true(file.exists(file.path(testing_path, "_book",
                                    "resdoc-french.pdf")))
})

# -----------------------------------------------------------------------------
# Render the French Word resdoc
test_that("csasdown::render generates the French .docx of the resdoc", {
  csasdown::set_french(val = TRUE)
  csasdown:::set_render_type(doc_type = "word")
  csasdown::render()
  expect_true(file.exists(file.path(testing_path, "_book",
                                    "resdoc-french.docx")))
})

# -----------------------------------------------------------------------------
# Render the PDF resdoc, with `NULL` highlight - fails on GHA probably due to
# some Pandoc type difference. Not that important.
# test_that("csasdown::render generates monochrome code PDF of the resdoc", {
#   csasdown::set_french(val = FALSE)
#   csasdown:::set_render_type(doc_type = "pdf")
#   rmd <- readLines("index.Rmd")
#   ind <- grep("highlight:", rmd)
#   rmd[ind] <- "   highlight: "
#   writeLines(rmd, "index.Rmd")
#   csasdown::render()
#   expect_true(file.exists(file.path(testing_path, "_book",
#                                     "resdoc-english.pdf")))
#   # Checked manually that the code chunks are monochrome
# })

# -----------------------------------------------------------------------------
# Render the PDF resdoc, with bogus highlight
test_that("csasdown::render detects bogus highlight", {
  csasdown::set_french(val = FALSE)
  csasdown:::set_render_type(doc_type = "pdf")
  rmd <- readLines("index.Rmd")
  ind <- grep("highlight:", rmd)
  rmd[ind] <- "   highlight: bogus"
  writeLines(rmd, "index.Rmd")
  expect_error(csasdown::render(), paste0("must be one of"))
})

# -----------------------------------------------------------------------------
# Render the PDF resdoc, with character line number mod
test_that("csasdown::render detects character line number mod value", {
  csasdown::set_french(val = FALSE)
  csasdown:::set_render_type(doc_type = "pdf")
  rmd <- readLines("index.Rmd")
  ind <- grep("highlight:", rmd)
  rmd[ind] <- "   highlight: tango"
  ind <- grep("line_nums_mod:", rmd)
  rmd[ind] <- "   line_nums_mod: A"
  writeLines(rmd, "index.Rmd")
  expect_error(csasdown::render(), paste0("must be a numeric ",
                                          "or integer value."))
})
