context("Test the sr document generation and create_tempdir_for_latex()")

testing_path <- file.path(tempdir(), "sr")
dir.create(testing_path, showWarnings = FALSE)
setwd(testing_path)
unlink("index", recursive = TRUE, force = TRUE)
suppressMessages(csasdown::draft(
  system.file("rmarkdown", "templates", "sr", package = "csasdown"),
  create_dir = TRUE,
  edit = FALSE
))
files <- file.path(testing_path, "index", dir("index"))
invisible(file.copy(files, testing_path, recursive = TRUE))

# ----------------------------------------------------
# Render the PDF sr
expect_warning({
  bookdown::render_book("index.Rmd",
    csasdown::sr_pdf(),
    envir = globalenv()
  )
})

test_that("bookdown::render_book generates the PDF of the sr", {
  expect_true(file.exists(file.path(testing_path, "_book", "sr.pdf")))
})

# ----------------------------------------------------
# Render the Word sr
suppressWarnings(bookdown::render_book("index.Rmd",
  csasdown::sr_word(),
  envir = globalenv()
))

test_that("bookdown::render_book generates the .docx of the sr", {
  expect_true(file.exists(file.path(testing_path, "_book", "sr.docx")))
})

# ----------------------------------------------------
# Check that French versions build
# First, using the french argument of sr_pdf()
expect_warning({
  bookdown::render_book("index.Rmd",
    csasdown::sr_pdf(french = TRUE),
    envir = globalenv()
  )
})

test_that("bookdown::render_book generates the PDF of the French sr", {
  expect_true(file.exists(file.path(testing_path, "_book", "sr.pdf")))
})

# Second, changing the french YAML option in index.Rmd
x <- readLines("index.Rmd")
x[grep("french:", x)] <- gsub("false", "true", x[grep("french:", x)])
writeLines(x, con = "index.Rmd")
file.remove(file.path(testing_path, "_book", "sr.pdf"))

expect_warning({
  bookdown::render_book(
    system.file("rmarkdown", "templates", "sr", package = "csasdown"),
    csasdown::sr_pdf(),
    envir = globalenv()
  )
})

test_that("bookdown::render_book generates the PDF of the French sr", {
  expect_true(file.exists(file.path(testing_path, "_book", "sr.pdf")))
})

# ----------------------------------------------------
# Creation and copying of test files to a temporary directory
setwd(testing_path)
unlink("index", recursive = TRUE, force = TRUE)
suppressMessages(csasdown::draft(
  system.file("rmarkdown", "templates", "sr", package = "csasdown"),
  create_dir = TRUE,
  edit = FALSE
))

suppressWarnings(bookdown::render_book("index.Rmd",
  csasdown::sr_pdf(),
  envir = globalenv()
))
files <- file.path(testing_path, "index", dir("index"))
invisible(file.copy(files, testing_path, recursive = TRUE))

tmp_dir <- create_tempdir_for_latex("sr",
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
expect_true(file.exists(file.path(tmp_dir, "sr.tex")))

# Test copying of the tex file from the root directory instead of the _book directory
# to a user-assigned directory
file.copy(file.path("_book", "sr.tex"), "sr.tex")
tmp_dir <- create_tempdir_for_latex("sr",
  "r",
  tmp_dir = file.path(testing_path, "test"),
  root_dir = getwd()
)
expect_true(file.exists(file.path(testing_path, "test", "sr.tex")))

# Test correct application of system-created directory
tmp_dir <- create_tempdir_for_latex("sr",
  "r",
  tmp_dir = NULL,
  root_dir = getwd()
)
expect_true(file.exists(file.path(tmp_dir, "sr.tex")))
