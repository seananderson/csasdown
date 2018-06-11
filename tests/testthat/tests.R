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

bookdown::render_book("index.Rmd",
  csasdown::resdoc_pdf(latex_engine = "xelatex"),
  envir = globalenv()
)

test_that("bookdown::render_book generates the PDF of the resdoc", {
  expect_true(file.exists(file.path(testing_path, "index/_book/resdoc.pdf")))
})

context("render into a .docx")

bookdown::render_book("index.Rmd",
  csasdown::resdoc_word(),
  envir = globalenv()
)

test_that("bookdown::render_book generates the .docx of the resdoc", {
  expect_true(file.exists(file.path(testing_path, "index/_book/resdoc.docx")))
})

add_resdoc_titlepage()
