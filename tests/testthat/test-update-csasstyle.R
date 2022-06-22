test_that("update_csasstyle() works", {

  testing_path <- file.path(tempdir(), "test-update-csasstyle")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(csasdown::draft(
    system.file("rmarkdown", "templates", "resdoc", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  # ---------------------------------------------------------------------------
  expect_error(csasdown:::update_csasstyle(FALSE, TRUE),
               paste0("You have set `copy_sty` to `FALSE` and `line_nums` to ",
                      "`TRUE` in the index.Rmd YAML header. The permanent ",
                      "style file cannot be modified as needed to include ",
                      "line numbering. Either set `copy_sty` to `TRUE` or ",
                      "`line_nums` to `FALSE` to build."),
               fixed = TRUE)

  # ---------------------------------------------------------------------------
  expect_error(csasdown:::update_csasstyle(FALSE, lot_lof = TRUE),
               paste0("You have set `copy_sty` to `FALSE` and `line_nums` ",
                      "to `TRUE` in the index.Rmd YAML header. The ",
                      "permanent style file cannot be modified as needed to ",
                      "include line numbering. Either set `copy_sty` to ",
                      "`TRUE` or `line_nums` to `FALSE` to build."),
               fixed = TRUE)

  # ---------------------------------------------------------------------------
  expect_error(csasdown:::update_csasstyle(FALSE,
                                line_nums = FALSE,
                                draft_watermark = TRUE),
               paste0("You have set `copy_sty` to `FALSE` and ",
                      "`draft_watermark` to `TRUE` in the index.Rmd YAML ",
                      "header. The permanent style file cannot be modified ",
                      "as needed to include the DRAFT watermark. Either set ",
                      "`copy_sty` to `TRUE` or `draft_watermark` to `FALSE` ",
                      "to build."),
               fixed = TRUE)

  # ---------------------------------------------------------------------------
  # Set lot_lof (toggle show List of tables/List of Figures in doc)
  rmd <- readLines("index.Rmd")
  ind <- grep("lot_lof:", rmd)
  rmd[ind] <- "   lot_lof: true"
  writeLines(rmd, "index.Rmd")
  csasdown::render()
  expect_true(file.exists("_book/resdoc-english.pdf"))

  # ---------------------------------------------------------------------------
  # Set draft_watermark
  unlink("_book/resdoc-english.pdf", force = TRUE)
  unlink("_book/resdoc-english.tex", force = TRUE)
  rmd <- readLines("index.Rmd")
  ind <- grep("lot_lof:", rmd)
  rmd[ind] <- "   lot_lof: false"
  ind <- grep("draft_watermark:", rmd)
  rmd[ind] <- "   draft_watermark: true"
  writeLines(rmd, "index.Rmd")
  csasdown::render()
  expect_true(file.exists("_book/resdoc-english.pdf"))

})
