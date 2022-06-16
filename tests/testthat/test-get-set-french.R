test_that("set_french() and get_french() works", {
  testing_path <- file.path(tempdir(), "sr-latex-test-1")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(csasdown::draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  expect_false(csasdown::get_french())
  csasdown::set_french(val = TRUE)
  expect_true(csasdown::get_french())
  csasdown::set_french(val = FALSE)
  expect_false(csasdown::get_french())

  fn <- 'index.Rmd'
  rmd <- readLines(fn)
  trim_rmd <- trimws(rmd)

  french_pat <- "^\\s*french:\\s*(false|true)\\s*$"
  french_ind <- grep(french_pat, trim_rmd)
  leading_spaces <- gsub("^(\\s*)french:\\s(true|false)\\s*$", "\\1", rmd[french_ind])
  all_spaces <- grep("^\\s*$", leading_spaces)
  if(!length(all_spaces)){
    leading_spaces <- ""
  }
  rmd[french_ind] <- paste0(leading_spaces, "french: truee")
  writeLines(rmd, fn)
  expect_error(csasdown::get_french())

  csasdown::set_french(val = TRUE)
  expect_true(csasdown::get_french())

})
