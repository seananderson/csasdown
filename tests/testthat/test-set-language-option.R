test_that("set_language_option() works", {
  testing_path <- file.path(tempdir(), "sr-set-language-option")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(csasdown::draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  # ---------------------------------------------------------------------------
  options(french = FALSE)
  expect_error(csasdown:::set_language_option("junk.file"))
  csasdown:::set_french(val = TRUE)
  csasdown:::set_language_option()
  expect_true(getOption("french"))

  # ---------------------------------------------------------------------------
  csasdown:::set_french(val = FALSE)
  csasdown:::set_language_option()
  expect_false(getOption("french"))

  # ---------------------------------------------------------------------------
  rmd <- readLines("index.Rmd")
  ind <- grep("french:", rmd)
  rmd[ind] <- ""
  writeLines(rmd, "index.Rmd")
  expect_error(csasdown:::set_language_option(),
               "No \\S+ entry was found")

  # ---------------------------------------------------------------------------
  rmd[ind] <- "   french: truee"
  writeLines(rmd, "index.Rmd")
  expect_error(csasdown:::set_language_option(),
               "Could not extract \\S+")

  # ---------------------------------------------------------------------------
  rmd_prev <- rmd[1:ind]
  rmd_post <- rmd[(ind + 1):length(rmd)]
  rmd <- c(rmd_prev, "   french: true", "   french: false", rmd_post)
  writeLines(rmd, "index.Rmd")
  expect_error(csasdown:::set_language_option(),
               "More than one \\S+ entry")

  # ---------------------------------------------------------------------------

})
