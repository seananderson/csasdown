test_that("draft() works", {
  testing_path <- file.path(tempdir(), "resdoc-draft-test")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)

  # ---------------------------------------------------------------------------
  expect_message(csasdown::draft("sr",
                  verbose = TRUE,
                  testing = TRUE,
                  testing_affirm = TRUE),
  paste0("Created a new RStudio project file"))

  # ---------------------------------------------------------------------------
  expect_error(csasdown::draft("resdoc-b",
                               verbose = TRUE,
                               testing = TRUE,
                               testing_affirm = FALSE),
         paste0("New document not drafted, you decided to keep"))

  # ---------------------------------------------------------------------------
  # Bad directory
  expect_error(csasdown::draft("resdoc-b",
                               directory = "baddir",
                               verbose = TRUE,
                               testing = TRUE,
                               testing_affirm = FALSE),
    paste0("does not exist so the csasdown project cannot be created there"))


  expect_error(csasdown:::create_rstudio_project_file(directory = "baddir",
                                                      verbose = TRUE),
               paste0("does not exist so an RStudio project file cannot ",
                      "be created there"))
})
