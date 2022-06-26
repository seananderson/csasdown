test_that("validate_chunk_headers() works", {

  dr <- file.path(testthat::test_path(), "validate-chunk-headers-files")
  expect_invisible(csasdown:::validate_chunk_headers(NULL))
  # ---------------------------------------------------------------------------

  rmd_file <- file.path(dr, "regex-tests-0.Rmd")
  expect_match(csasdown:::validate_chunk_headers(rmd_file),
    paste0("is not of correct format for English chunks"))
  # ---------------------------------------------------------------------------

  rmd_file <- file.path(dr, "regex-tests-3.Rmd")
  expect_match(csasdown:::validate_chunk_headers(rmd_file),
    paste0("is not of correct format for English chunks"))
  # ---------------------------------------------------------------------------

  rmd_files <- file.path(dr, c("regex-tests-1.Rmd", "regex-tests-2.Rmd"))
  expected <- c(
    paste0("must include"),
    paste0("must include"),
    paste0("must include"),
    paste0("must not include"),
    paste0("must not include"),
    paste0("must not include"),
    paste0("must not include"),
    paste0("must include"),
    paste0("must include"),
    paste0("must include"),
    paste0("must not include"),
    paste0("must not include"),
    paste0("must not include"),
    paste0("must not include"))

  actual <- csasdown:::validate_chunk_headers(rmd_files,
                                              en_chunk_regex = NULL,
                                              fr_chunk_regex = NULL)
  purrr::map(seq_along(actual), ~{
    expect_match(actual[.x], expected[.x])
  })
  # ---------------------------------------------------------------------------

  rmd_files <- file.path(dr, c("regex-tests-1.Rmd", "regex-tests-2.Rmd"))
  expected <- c(
    paste0("is not of correct format for French chunks"),
    paste0("must include"),
    paste0("is not of correct format for French chunks"),
    paste0("must include"),
    paste0("is not of correct format for English chunks"),
    paste0("is not of correct format for French chunks"),
    paste0("must include"),
    paste0("must not include"),
    paste0("must not include"),
    paste0("is not of correct format for French chunks"),
    paste0("must not include"),
    paste0("must not include"),
    paste0("is not of correct format for French chunks"),
    paste0("must include"),
    paste0("is not of correct format for French chunks"),
    paste0("must include"),
    paste0("is not of correct format for French chunks"),
    paste0("must include"),
    paste0("must not include"),
    paste0("must not include"),
    paste0("is not of correct format for French chunks"),
    paste0("must not include"),
    paste0("must not include"))

  actual <- csasdown:::validate_chunk_headers(rmd_files)

  purrr::map(seq_along(actual), ~{
    expect_match(actual[.x], expected[.x])
  })
  # ---------------------------------------------------------------------------

  rmd_file <- file.path(dr, "regex-tests-5.Rmd")
  expected <- c(
    paste0("is not of correct format for English chunks"),
    paste0("is not of correct format for English chunks"),
    paste0("is not of correct format for French chunks"),
    paste0("must include"),
    paste0("is not of correct format for English chunks"),
    paste0("must not include"),
    paste0("is not of correct format for French chunks"),
    paste0("must not include"),
    paste0("must not include"))
  actual <- csasdown:::validate_chunk_headers(rmd_file,
                                   en_chunk_regex = "^en-$",
                                   fr_chunk_regex = "^fr-$")
  purrr::map(seq_along(actual), ~{
    expect_match(actual[.x], expected[.x])
  })
  # ---------------------------------------------------------------------------

})
