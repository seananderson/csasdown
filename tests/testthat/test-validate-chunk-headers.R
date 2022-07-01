test_that("validate_chunk_headers() works", {
  rmd_dir <- testthat::test_path("validate-chunk-headers-files")

  testing_path <- file.path(tempdir(), "test-validate-chunk-headers")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  file.copy(file.path(rmd_dir, "regex-tests-0.Rmd"),
            file.path(testing_path, "regex-tests-0.Rmd"))
  file.copy(file.path(rmd_dir, "regex-tests-1.Rmd"),
            file.path(testing_path, "regex-tests-1.Rmd"))
  file.copy(file.path(rmd_dir, "regex-tests-2.Rmd"),
            file.path(testing_path, "regex-tests-2.Rmd"))
  file.copy(file.path(rmd_dir, "regex-tests-3.Rmd"),
            file.path(testing_path, "regex-tests-3.Rmd"))
  file.copy(file.path(rmd_dir, "regex-tests-5.Rmd"),
            file.path(testing_path, "regex-tests-5.Rmd"))
  file.copy(file.path(rmd_dir, "regex-tests-6.Rmd"),
            file.path(testing_path, "regex-tests-6.Rmd"))
  file.copy(file.path(rmd_dir, "regex-tests-7.Rmd"),
            file.path(testing_path, "regex-tests-7.Rmd"))
  file.copy(file.path(rmd_dir, "regex-tests-8.Rmd"),
            file.path(testing_path, "regex-tests-8.Rmd"))
  setwd(testing_path)

  expect_invisible(csasdown:::validate_chunk_headers(NULL))
  # ---------------------------------------------------------------------------

  expect_error(csasdown:::validate_chunk_headers("regex-tests-0.Rmd"),
               paste0("is not of correct format for English chunks"))
  # ---------------------------------------------------------------------------

  expect_error(csasdown:::validate_chunk_headers("regex-tests-8.Rmd"),
               paste0("is not of correct format for French chunks"))
  # ---------------------------------------------------------------------------

  expect_warning(csasdown:::validate_chunk_headers("regex-tests-3.Rmd"),
                 paste0("is missing for this French chunk and has been added"))
  # ---------------------------------------------------------------------------

  rmd_files <-  c("regex-tests-1.Rmd", "regex-tests-2.Rmd")
  w <- capture_warnings(csasdown:::validate_chunk_headers(
    rmd_files,
    en_chunk_regex = NULL,
    fr_chunk_regex = NULL))
  expect_match(w[1], "is missing for this French chunk and has been added")
  expect_match(w[2], "is missing for this French chunk and has been added")
  expect_match(w[3], "is missing for this French chunk and has been added")
  expect_match(w[4], "is not allowed for English chunks and has been removed")
  expect_match(w[5], "is not allowed for English chunks and has been removed")
  expect_match(w[6], "is not allowed for English chunks and has been removed")
  expect_match(w[7], "is not allowed for English chunks and has been removed")
  expect_match(w[8], "is missing for this French chunk and has been added")
  expect_match(w[9], "is missing for this French chunk and has been added")
  expect_match(w[10], "is missing for this French chunk and has been added")
  expect_match(w[11], "is not allowed for English chunks and has been removed")
  expect_match(w[12], "is not allowed for English chunks and has been removed")
  expect_match(w[13], "is not allowed for English chunks and has been removed")
  expect_match(w[14], "is not allowed for English chunks and has been removed")
  # ---------------------------------------------------------------------------

  expect_warning(csasdown:::validate_chunk_headers("regex-tests-5.Rmd"),
                 "is missing and has been added automatically")
  # ---------------------------------------------------------------------------

  expect_warning(csasdown:::validate_chunk_headers("regex-tests-6.Rmd"),
                 "is not of correct format for neutral chunks")
  # ---------------------------------------------------------------------------

  expect_warning(csasdown:::validate_chunk_headers("regex-tests-7.Rmd"),
                 "is missing and has been added automatically")

})
