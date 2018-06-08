# This code runs before the tests. It creates a testpackage in
# the temporary directory where all the functions of rrtools
# can be applied safely and subsequently tested.

# create temporary directory in file system
testing_path <- paste0(tempdir(), "/testing_directory")
dir.create(testing_path, showWarnings = FALSE)

context("check for prerequisites")

if (!require(tinytex)) install.packages("tinytex")
if (!tinytex:::is_tinytex()  ) tinytex::install_tinytex(force = TRUE)

test_that("LaTeX is installed", {
  expect_true(tinytex:::is_tinytex())
})

context("check that the pkg template files are present")

template_files <- list.files(system.file("rmarkdown", package = "csasdown"),
  recursive = TRUE)

test_that("Template files are present", {
  expect_true(length(template_files) > 10)
})

context("create the thesis directories and files")

if (getwd() != testing_path) setwd(testing_path)
if (dir.exists("index")) unlink("index", recursive = TRUE)
suppressMessages(rmarkdown::draft("index.Rmd",
  system.file("rmarkdown",
    "templates",
    "thesis",
    package = "csasdown"
  ),
  create_dir = TRUE,
  edit = FALSE
))

# these are the files that we expect it to make
the_files <- c(
  "_bookdown.yml",
  "01-chap1.Rmd",
  "02-chap2.Rmd",
  "03-chap3.Rmd",
  "04-conclusion.Rmd",
  "05-appendix.Rmd",
  "98-colophon.Rmd",
  "99-references.Rmd",
  "bib",
  "csas-style",
  "csl",
  "data",
  "figure",
  "index.Rmd",
  "template.tex"
)

#### check results ####

test_that("rmarkdown::draft generates the thesis directories and files", {
  expect_equal(
    list.files(file.path(testing_path, "index")),
    the_files
  )
})

context("render into a PDF")

if (getwd() != file.path(testing_path, "index"))
  setwd(file.path(testing_path, "index"))
bookdown::render_book("index.Rmd",
  csasdown::resdoc_pdf(latex_engine = "xelatex"),
  envir = globalenv()
)

test_that("bookdown::render_book generates the PDF of the thesis", {
  expect_true(file.exists(file.path(testing_path, "index/_book/resdoc.pdf")))
})
