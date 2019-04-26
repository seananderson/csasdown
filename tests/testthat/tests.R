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
)})

test_that("bookdown::render_book generates the PDF of the resdoc", {
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

bookdown::render_book("index.Rmd",
  csasdown::techreport_pdf(),
  envir = globalenv()
)

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
