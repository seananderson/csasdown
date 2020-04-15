library(tidyverse)
is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}

# create temporary directory in file system
testing_path <- paste0(tempdir(), "/testing_directory")
#testing_path <- paste0("C:/junk/testing_directory")
dir.create(testing_path, showWarnings = FALSE)

context("check for prerequisites")

if (is_windows()) {
  x <- system("where xelatex", intern = TRUE)
} else {
  x <- system("which xelatex", intern = TRUE)
}
if (!grepl("latex", x)) {
  if (!require(tinytex)) install.packages("tinytex")
  if (!tinytex:::is_tinytex()) tinytex::install_tinytex(force = TRUE)
  test_that("LaTeX is installed", {
    expect_true(tinytex:::is_tinytex())
  })
}

context("check that the pkg template files are present")

template_files <- list.files(system.file("rmarkdown", package = "csasdown"),
  recursive = TRUE)

test_that("Template files are present", {
  expect_true(length(template_files) > 10)
})

context("create the resdoc directories and files")

if (getwd() != testing_path) setwd(testing_path)
if (dir.exists("index")) unlink("index", recursive = TRUE)
suppressMessages(rmarkdown::draft("index.Rmd",
  system.file("rmarkdown",
    "templates",
    "resdoc",
    package = "csasdown"
  ),
  create_dir = TRUE,
  edit = FALSE
))

if (getwd() != file.path(testing_path, "index"))
  setwd(file.path(testing_path, "index"))

context("render into a PDF")

expect_message(check_yaml(), "contains all")

expect_warning({
  bookdown::render_book("index.Rmd",
    csasdown::resdoc_pdf(),
    envir = globalenv()
  )
})

test_that("bookdown::render_book generates the PDF of the resdoc", {
  expect_true(file.exists(file.path(testing_path, "index/_book/resdoc.pdf")))
})

if (getwd() != file.path(testing_path, "index"))
  setwd(file.path(testing_path, "index"))

context("render into a French PDF")

expect_message(check_yaml(), "contains all")

expect_warning({
  bookdown::render_book("index.Rmd",
                        csasdown::resdoc_pdf(french = TRUE),
                        envir = globalenv()
  )})

test_that("bookdown::render_book generates the French PDF of the resdoc", {
  expect_true(file.exists(file.path(testing_path, "index/_book/resdoc.pdf")))
})


x <- readLines("index.Rmd")
x[grep("french:", x)] <- gsub("false", "true", x[grep("french:", x)])
writeLines(x, con = "index.Rmd")
file.remove(file.path(testing_path, "index/_book/resdoc.pdf"))

expect_warning({
  bookdown::render_book("index.Rmd",
    csasdown::resdoc_pdf(),
    envir = globalenv()
  )})

test_that("bookdown::render_book generates the PDF of the French resdoc", {
  expect_true(file.exists(file.path(testing_path, "index/_book/resdoc.pdf")))
})

context("render into a .docx")

suppressWarnings(bookdown::render_book("index.Rmd",
  csasdown::resdoc_word(),
  envir = globalenv()
))

test_that("bookdown::render_book generates the .docx of the resdoc", {
  expect_true(file.exists(file.path(testing_path, "index/_book/resdoc.docx")))
})

add_resdoc_docx_titlepage()

test_that("add_resdoc_docx_titlepage() generates the .docx of the resdoc", {
  expect_true(file.exists(file.path(testing_path, "index/_book/resdoc.docx")))
})

# ----------------------------------------------------
# SR:

# create temporary directory in file system
testing_path <- paste0(tempdir(), "/testing_directory_sr")
dir.create(testing_path, showWarnings = FALSE)

context("create the SR directories and files")

if (getwd() != testing_path) setwd(testing_path)
if (dir.exists("index")) unlink("index", recursive = TRUE)
suppressMessages(rmarkdown::draft("index.Rmd",
  system.file("rmarkdown",
  "templates",
  "sr",
  package = "csasdown"
  ),
  create_dir = TRUE,
  edit = FALSE
))

if (getwd() != file.path(testing_path, "index"))
  setwd(file.path(testing_path, "index"))

context("render into a PDF")

expect_warning({ # warning expected because of currently missing abstract
  bookdown::render_book("index.Rmd",
    csasdown::sr_pdf(),
    envir = globalenv()
  )})

test_that("bookdown::render_book generates the PDF of the SR", {
  expect_true(file.exists(file.path(testing_path, "index/_book/sr.pdf")))
})

context("render SR into a .docx")

suppressWarnings(bookdown::render_book("index.Rmd",
  csasdown::sr_word(),
  envir = globalenv()
))

test_that("bookdown::render_book generates the .docx of the SR", {
  expect_true(file.exists(file.path(testing_path, "index/_book/sr.docx")))
})

expect_true(file.exists(file.path(testing_path, "index/_book/sr.docx")))

# ----------------------------------------------------
# TR:

# create temporary directory in file system
testing_path <- paste0(tempdir(), "/testing_directory_tr")
dir.create(testing_path, showWarnings = FALSE)

context("create the TR directories and files")

if (getwd() != testing_path) setwd(testing_path)
if (dir.exists("index")) unlink("index", recursive = TRUE)
suppressMessages(rmarkdown::draft("index.Rmd",
  system.file("rmarkdown",
  "templates",
  "techreport",
  package = "csasdown"
  ),
  create_dir = TRUE,
  edit = FALSE
))

if (getwd() != file.path(testing_path, "index"))
  setwd(file.path(testing_path, "index"))

context("render into a PDF")

expect_warning({
  bookdown::render_book("index.Rmd",
    csasdown::techreport_pdf(),
    envir = globalenv()
  )})

test_that("bookdown::render_book generates the PDF of the SR", {
  expect_true(file.exists(file.path(testing_path, "index/_book/techreport.pdf")))
})

context("render TR into a .docx")

suppressWarnings(bookdown::render_book("index.Rmd",
  csasdown::techreport_word(),
  envir = globalenv()
))

test_that("bookdown::render_book generates the .docx of the techreport", {
  expect_true(file.exists(file.path(testing_path, "index/_book/techreport.docx")))
})

expect_true(file.exists(file.path(testing_path, "index/_book/techreport.docx")))

# ----------------------------------------------------
# Creation and copying of test files to a temporary directory for resdoc

testing_path <- paste0(tempdir(), "/testing_directory_resdoc_file_copying")
dir.create(testing_path, showWarnings = FALSE)

context("Create a resdoc, and then copy the files to a new, temporary directory for latex compilation and debugging")

setwd(testing_path)
if (dir.exists("index")) unlink("index", recursive = TRUE)
suppressMessages(rmarkdown::draft("index.Rmd",
                                  system.file("rmarkdown",
                                              "templates",
                                              "resdoc",
                                              package = "csasdown"
                                  ),
                                  create_dir = TRUE,
                                  edit = FALSE
))
setwd(file.path(testing_path, "index"))

suppressWarnings(bookdown::render_book("index.Rmd",
                      csasdown::resdoc_pdf(),
                      envir = globalenv()))

tmp_dir <- create_tempdir_for_latex("resdoc",
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
expect_true(file.exists(file.path(tmp_dir, "resdoc.tex")))

# ----------------------------------------------------
# Creation and copying of test files to a temporary directory for sr

testing_path <- paste0(tempdir(), "/testing_directory_sr_file_copying")
dir.create(testing_path, showWarnings = FALSE)

context("Create a sr, and then copy the files to a new, temporary directory for latex compilation and debugging")

setwd(testing_path)
if (dir.exists("index")) unlink("index", recursive = TRUE)
suppressMessages(rmarkdown::draft("index.Rmd",
                                  system.file("rmarkdown",
                                              "templates",
                                              "sr",
                                              package = "csasdown"
                                  ),
                                  create_dir = TRUE,
                                  edit = FALSE
))
setwd(file.path(testing_path, "index"))

suppressWarnings(bookdown::render_book("index.Rmd",
                      csasdown::sr_pdf(),
                      envir = globalenv()))

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

# ----------------------------------------------------
# Creation and copying of test files to a temporary directory for techreport

testing_path <- paste0(tempdir(), "/testing_directory_tr_file_copying")
dir.create(testing_path, showWarnings = FALSE)

context("Create a tech report, and then copy the files to a new, temporary directory for latex compilation and debugging")

setwd(testing_path)
if (dir.exists("index")) unlink("index", recursive = TRUE)
suppressMessages(rmarkdown::draft("index.Rmd",
                                  system.file("rmarkdown",
                                              "templates",
                                              "techreport",
                                              package = "csasdown"
                                  ),
                                  create_dir = TRUE,
                                  edit = FALSE
))
setwd(file.path(testing_path, "index"))

suppressWarnings(bookdown::render_book("index.Rmd",
                      csasdown::techreport_pdf(),
                      envir = globalenv()))

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

expect_error(create_tempdir_for_latex("techreport",
                                    "r",
                                    tmp_dir = file.path(testing_path, "test"),
                                    root_dir = getwd()))

file.copy("_book/techreport.tex", "techreport.tex")
tmp_dir <- create_tempdir_for_latex("techreport",
                                    "r",
                                    tmp_dir = file.path(testing_path, "test"),
                                    root_dir = getwd())
expect_true(file.exists(file.path(testing_path, "test", "techreport.tex")))

tmp_dir <- create_tempdir_for_latex("techreport",
                                    "r",
                                    tmp_dir = NULL,
                                    root_dir = getwd())
expect_true(file.exists(file.path(tmp_dir, "techreport.tex")))

