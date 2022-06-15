testing_path <- file.path(tempdir(), "sr")
unlink(testing_path, recursive = TRUE, force = TRUE)
dir.create(testing_path, showWarnings = FALSE)
setwd(testing_path)
unlink("index", recursive = TRUE, force = TRUE)
suppressMessages(csasdown::draft(
  system.file("rmarkdown", "templates", "sr", package = "csasdown"),
  create_dir = FALSE,
  edit = FALSE
))
files <- file.path(testing_path, "index", dir("index"))
invisible(file.copy(files, testing_path, recursive = TRUE))

# ----------------------------------------------------
# Render the PDF sr
expect_warning({
  csasdown:::set_render_type(doc_type = "pdf")
  csasdown::render()
})

test_that("csasdown::render generates the PDF of the sr", {
  expect_true(file.exists(file.path(testing_path, "_book", "sr.pdf")))
})

# ----------------------------------------------------
# Render the Word sr
csasdown:::set_render_type(doc_type = "word")
csasdown::render()

test_that("csasdown::render generates the .docx of the sr", {
  expect_true(file.exists(file.path(testing_path, "_book", "sr.docx")))
})

# ----------------------------------------------------
# Render the French PDF sr
options(french = TRUE)
expect_warning({
  csasdown:::set_render_type(doc_type = "pdf")
  csasdown::render()
})

test_that("csasdown::render generates the PDF of the sr", {
  expect_true(file.exists(file.path(testing_path, "_book", "sr.pdf")))
})

# ----------------------------------------------------
# Render the French Word sr
csasdown:::set_render_type(doc_type = "word")
csasdown::render()

test_that("csasdown::render generates the .docx of the sr", {
  expect_true(file.exists(file.path(testing_path, "_book", "sr.docx")))
})

# ----------------------------------------------------
# Creation and copying of test files to a temporary directory
unlink(testing_path, recursive = TRUE, force = TRUE)
dir.create(testing_path, showWarnings = FALSE)
setwd(testing_path)

options(french = FALSE)
suppressMessages(csasdown::draft(
  system.file("rmarkdown", "templates", "sr", package = "csasdown"),
  create_dir = FALSE,
  edit = FALSE
))

csasdown:::set_render_type(doc_type = "pdf")
suppressWarnings(csasdown::render())

files <- file.path(testing_path, "index", dir("index"))
invisible(file.copy(files, testing_path, recursive = TRUE))

tmp_dir <- create_tempdir_for_latex("sr",
                                    "b",
                                    tmp_dir = file.path(testing_path, "test"),
                                    root_dir = getwd())

tmp_csas_dir <- file.path(tmp_dir, "csas-style")

expect_true(file.exists(file.path(tmp_csas_dir, "res-doc.sty")))
expect_true(file.exists(file.path(tmp_csas_dir, "res-doc-french.sty")))
expect_true(file.exists(file.path(tmp_csas_dir, "sr.sty")))
expect_true(file.exists(file.path(tmp_csas_dir, "sr-french.sty")))
expect_true(file.exists(file.path(tmp_csas_dir, "tech-report.sty")))
expect_true(file.exists(file.path(tmp_csas_dir, "tech-report-french.sty")))
expect_true(dir.exists(file.path(tmp_csas_dir, "images")))
expect_true(dir.exists(file.path(tmp_dir, "knitr-cache-pdf")))
expect_true(dir.exists(file.path(tmp_dir, "knitr-cache-word")))
expect_true(dir.exists(file.path(tmp_dir, "knitr-figs-pdf")))
expect_true(dir.exists(file.path(tmp_dir, "knitr-figs-word")))
expect_true(file.exists(file.path(tmp_dir, "sr.tex")))

# Test copying of the tex file from the root directory instead of the _book directory
# to a user-assigned directory
file.copy(file.path("_book", "sr.tex"), "sr.tex")
tmp_dir <- create_tempdir_for_latex("sr",
                                    "r",
                                    tmp_dir = file.path(testing_path, "test"),
                                    root_dir = getwd())

expect_true(file.exists(file.path(testing_path, "test", "sr.tex")))

# Test correct application of system-created directory
tmp_dir <- create_tempdir_for_latex("sr",
                                    "r",
                                    tmp_dir = NULL,
                                    root_dir = getwd())

expect_true(file.exists(file.path(tmp_dir, "sr.tex")))

unlink(testing_path, recursive = TRUE, force = TRUE)
