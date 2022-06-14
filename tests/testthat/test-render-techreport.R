temp_path <- tempdir()
testing_path <- file.path(temp_path, "techreport")

# ----------------------------------------------------
# Creation and copying of test files to a temporary directory

unlink(testing_path, recursive = TRUE, force = TRUE)
dir.create(testing_path, showWarnings = FALSE)
setwd(testing_path)
csasdown::draft(
  system.file("rmarkdown", "templates", "techreport", package = "csasdown"),
  create_dir = FALSE,
  edit = FALSE
)

# ----------------------------------------------------
# Render the PDF techreport
expect_warning({csasdown::render(doc_type = "pdf")})

unlink("_book/techreport.pdf", force = TRUE, recursive = TRUE)
options(french = FALSE)
test_that("run_pdflatex() works", {
  expect_warning(run_pdflatex())
})

test_that("csasdown::render generates the PDF of the techreport", {
  expect_true(file.exists(file.path(testing_path, "_book", "techreport.pdf")))
})

# ----------------------------------------------------
# Render a Word techreport
options(french = FALSE)
test_that("csasdown::render generates the .docx of the techreport", {
  expect_warning(csasdown::render(doc_type = "word"))
  expect_true(file.exists(file.path(testing_path, "_book", "techreport.docx")))
})

# ----------------------------------------------------
# Render the French PDF techreport
options(french = TRUE)
unlink("_book/techreport.pdf", force = TRUE, recursive = TRUE)
test_that("run_pdflatex() works", {
  expect_warning(csasdown::render(doc_type = "pdf"))
  expect_warning(run_pdflatex())
})

test_that("csasdown::render generates the PDF of the techreport", {
  expect_true(file.exists(file.path(testing_path, "_book", "techreport.pdf")))
})

# ----------------------------------------------------
# Render a French Word techreport
options(french = TRUE)
test_that("csasdown::render generates the Word version of the techreport", {
  expect_warning(csasdown::render(doc_type = "word"))
})

test_that("csasdown::render generates the .docx of the techreport", {
  expect_true(file.exists(file.path(testing_path, "_book", "techreport.docx")))
})

# ----------------------------------------------------
# Creation and copying of test files to a temporary directory

tmp_dir <- create_tempdir_for_latex("techreport",
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
expect_true(file.exists(file.path(tmp_dir, "techreport.tex")))

# Test correct application of system-created directory
file.copy(file.path("_book", "techreport.tex"), "techreport.tex")
tmp_dir <- create_tempdir_for_latex("techreport",
                                    "r",
                                    tmp_dir = NULL,
                                    root_dir = getwd())

expect_true(file.exists(file.path(tmp_dir, "techreport.tex")))

# Test copying of the tex file from the root directory instead of the _book directory
# to a user-assigned directory
tmp_dir <- create_tempdir_for_latex("techreport",
                                    "r",
                                    tmp_dir = file.path(testing_path, "test"),
                                    root_dir = getwd())

expect_true(file.exists(file.path(testing_path, "test", "techreport.tex")))

# ----------------------------------------------------
# Testing of other input in latex copying function

expect_error(create_tempdir_for_latex("techrepotr",
                                      "b",
                                      tmp_dir = file.path(testing_path, "test"),
                                      root_dir = getwd()))

expect_error(create_tempdir_for_latex("techreport",
                                      "s",
                                      tmp_dir = file.path(testing_path, "test"),
                                      root_dir = getwd()))

expect_error(create_tempdir_for_latex(NULL,
                                      "b",
                                      tmp_dir = file.path(testing_path, "test"),
                                      root_dir = getwd()))

expect_error(create_tempdir_for_latex("techreport",
                                      NULL,
                                      tmp_dir = file.path(testing_path, "test"),
                                      root_dir = getwd()))

unlink(file.path(testing_path, "techreport.tex"), force = TRUE)
expect_error(create_tempdir_for_latex("techreport",
                                      "r",
                                      tmp_dir = file.path(testing_path, "test"),
                                      root_dir = getwd()))

unlink(testing_path, recursive = TRUE, force = TRUE)
