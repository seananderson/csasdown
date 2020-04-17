context("Test the techreport document generation and create_tempdir_for_latex()")

testing_path <- tempdir()
setwd(testing_path)
unlink("index", recursive = TRUE, force = TRUE)
suppressMessages(rmarkdown::draft("index.Rmd",
                                  system.file("rmarkdown",
                                              "templates",
                                              "techreport",
                                              package = "csasdown"
                                  ),
                                  create_dir = TRUE,
                                  edit = FALSE)
                 )
files <- file.path(testing_path, "index", dir("index"))
invisible(file.copy(files, testing_path, recursive = TRUE))

# ----------------------------------------------------
# Render the PDF techreport
expect_warning({
  bookdown::render_book("index.Rmd",
                        csasdown::techreport_pdf(),
                        envir = globalenv())
  })

test_that("bookdown::render_book generates the PDF of the techreport", {
  expect_true(file.exists(file.path(testing_path, "_book", "techreport.pdf")))
})

# ----------------------------------------------------
# Render a Word techreport
suppressWarnings(bookdown::render_book("index.Rmd",
                                       csasdown::techreport_word(),
                                       envir = globalenv())
                 )

test_that("bookdown::render_book generates the .docx of the techreport", {
  expect_true(file.exists(file.path(testing_path, "_book", "techreport.docx")))
})

# ----------------------------------------------------
# Check that French versions build
# First, using the french argument of techreport_pdf()
expect_warning({
  bookdown::render_book("index.Rmd",
                        csasdown::techreport_pdf(french = TRUE),
                        envir = globalenv())
})

test_that("bookdown::render_book generates the PDF of the French techreport", {
  expect_true(file.exists(file.path(testing_path, "_book", "techreport.pdf")))
})

# Second, changing the french YAML option in index.Rmd
x <- readLines("index.Rmd")
x[grep("french:", x)] <- gsub("false", "true", x[grep("french:", x)])
writeLines(x, con = "index.Rmd")
file.remove(file.path(testing_path, "_book", "techreport.pdf"))

expect_warning({
  bookdown::render_book("index.Rmd",
                        csasdown::techreport_pdf(),
                        envir = globalenv())
})

test_that("bookdown::render_book generates the PDF of the French techreport", {
  expect_true(file.exists(file.path(testing_path, "_book", "techreport.pdf")))
})

# ----------------------------------------------------
# Creation and copying of test files to a temporary directory
testing_path <- tempdir()
setwd(testing_path)
unlink("index", recursive = TRUE, force = TRUE)
suppressMessages(rmarkdown::draft("index.Rmd",
                                  system.file("rmarkdown",
                                              "templates",
                                              "techreport",
                                              package = "csasdown"
                                  ),
                                  create_dir = TRUE,
                                  edit = FALSE)
                 )

suppressWarnings(bookdown::render_book("index.Rmd",
                                       csasdown::techreport_pdf(),
                                       envir = globalenv())
                 )
files <- file.path(testing_path, "index", dir("index"))
invisible(file.copy(files, testing_path, recursive = TRUE))

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
