testing_path <- file.path(tempdir(), "resdoc")
unlink(testing_path, recursive = TRUE, force = TRUE)
dir.create(testing_path, showWarnings = FALSE)
setwd(testing_path)
unlink("index", recursive = TRUE, force = TRUE)
suppressMessages(csasdown::draft(
  system.file("rmarkdown", "templates", "resdoc", package = "csasdown"),
  create_dir = FALSE,
  edit = FALSE
))
files <- file.path(testing_path, "index", dir("index"))
invisible(file.copy(files, testing_path, recursive = TRUE))

# ----------------------------------------------------
# Render the PDF resdoc
expect_warning({
  bookdown::render_book("index.Rmd",
    csasdown::resdoc_pdf(),
    envir = globalenv()
  )
})

test_that("bookdown::render_book generates the PDF of the resdoc", {
  expect_true(file.exists(file.path(testing_path, "_book", "resdoc.pdf")))
})

# ----------------------------------------------------
# Render the Word resdoc
suppressWarnings(bookdown::render_book("index.Rmd",
  csasdown::resdoc_word(),
  envir = globalenv()
))

test_that("bookdown::render_book generates the .docx of the resdoc", {
  expect_true(file.exists(file.path(testing_path, "_book", "resdoc.docx")))
})

# ----------------------------------------------------
# Add the title page to the Word resdoc
add_resdoc_docx_titlepage()

test_that("add_resdoc_docx_titlepage() generates the .docx of the resdoc", {
  expect_true(file.exists(file.path(testing_path, "_book", "resdoc.docx")))
})

# ----------------------------------------------------
# Make sure all YAML options are contained in index.Rmd
expect_message(check_yaml(), "contains all")

# ----------------------------------------------------
# Check that French versions build
# First, using the french argument of resdoc_pdf()
expect_warning({
  bookdown::render_book("index.Rmd",
    csasdown::resdoc_pdf(french = TRUE),
    envir = globalenv()
  )
})

test_that("bookdown::render_book generates the PDF of the French resdoc", {
  expect_true(file.exists(file.path(testing_path, "_book", "resdoc.pdf")))
})

# ----------------------------------------------------
# Creation and copying of test files to a temporary directory

unlink(testing_path, recursive = TRUE, force = TRUE)
dir.create(testing_path, showWarnings = FALSE)
setwd(testing_path)
unlink("index", recursive = TRUE, force = TRUE)
suppressMessages(csasdown::draft(
  system.file("rmarkdown", "templates", "resdoc", package = "csasdown"),
  create_dir = FALSE,
  edit = FALSE
))

suppressWarnings(bookdown::render_book("index.Rmd",
  csasdown::resdoc_pdf(),
  envir = globalenv()
))
files <- file.path(testing_path, "index", dir("index"))
invisible(file.copy(files, testing_path, recursive = TRUE))

tmp_dir <- create_tempdir_for_latex("resdoc",
  "b",
  tmp_dir = file.path(testing_path, "test"),
  root_dir = getwd()
)
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
expect_true(file.exists(file.path(tmp_dir, "resdoc.tex")))

# Test copying of the tex file from the root directory instead of the _book directory
# to a user-assigned directory
file.copy(file.path("_book", "resdoc.tex"), "resdoc.tex")
tmp_dir <- create_tempdir_for_latex("resdoc",
  "r",
  tmp_dir = file.path(testing_path, "test"),
  root_dir = getwd()
)
expect_true(file.exists(file.path(testing_path, "test", "resdoc.tex")))

# Test correct application of system-created directory
tmp_dir <- create_tempdir_for_latex("resdoc",
  "r",
  tmp_dir = NULL,
  root_dir = getwd()
)
expect_true(file.exists(file.path(tmp_dir, "resdoc.tex")))

unlink(testing_path, recursive = TRUE, force = TRUE)
