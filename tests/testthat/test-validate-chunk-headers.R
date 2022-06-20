test_that("validate_chunk_headers() works", {

  dr <- file.path(testthat::test_path(), "validate-chunk-headers-files")
  expect_invisible(csasdown:::validate_chunk_headers(NULL))
  # ---------------------------------------------------------------------------

  rmd_file <- file.path(dr, "regex-tests-0.Rmd")
  expect_identical(csasdown:::validate_chunk_headers(rmd_file),
    paste0("Chunk name 'x-2-english' in ",
           "file '", file.path(dr, "regex-tests-0.Rmd"), "'\n is not of ",
           "correct format for English chunks (^\\S+-en$)"))
  # ---------------------------------------------------------------------------

  rmd_file <- file.path(dr, "regex-tests-3.Rmd")
  expect_identical(csasdown:::validate_chunk_headers(rmd_file),
    paste0("Chunk name 'x-1-english' in file '", file.path(dr,
    "regex-tests-3.Rmd"), "'\n is not of correct format for English chunks ",
    "(^\\S+-en$)"))
  # ---------------------------------------------------------------------------

  rmd_files <- file.path(dr, c("regex-tests-1.Rmd", "regex-tests-2.Rmd"))
  expected <- c(
    paste0("Chunk 'x-1-en' in file '", file.path(dr, "regex-tests-1.Rmd"),
           "'\n must include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk 'x-2-en' in file '", file.path(dr, "regex-tests-1.Rmd"),
           "'\n must include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk 'x-6-en' in file '", file.path(dr, "regex-tests-1.Rmd"),
           "'\n must include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk 'x-8-en' in file '", file.path(dr, "regex-tests-1.Rmd"),
           "'\n must not include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk 'x-9-en' in file '", file.path(dr, "regex-tests-1.Rmd"),
           "'\n must not include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk 'x-11-en' in file '", file.path(dr, "regex-tests-1.Rmd"),
           "'\n must not include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk 'x-12-en' in file '", file.path(dr, "regex-tests-1.Rmd"),
           "'\n must not include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk 'y-1-end' in file '", file.path(dr, "regex-tests-2.Rmd"),
           "'\n must include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk 'y-2-en' in file '", file.path(dr, "regex-tests-2.Rmd"),
           "'\n must include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk 'y-6-en' in file '", file.path(dr, "regex-tests-2.Rmd"),
           "'\n must include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk 'y-8-en' in file '", file.path(dr, "regex-tests-2.Rmd"),
           "'\n must not include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk 'y-9-en' in file '", file.path(dr, "regex-tests-2.Rmd"),
           "'\n must not include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk 'y-11-en' in file '", file.path(dr, "regex-tests-2.Rmd"),
           "'\n must not include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk 'y-12-en' in file '", file.path(dr, "regex-tests-2.Rmd"),
           "'\n must not include `needs_trans = TRUE` or `needs_trans = FALSE`"))

  actual <- csasdown:::validate_chunk_headers(rmd_files,
                                   en_chunk_regex = NULL,
                                   fr_chunk_regex = NULL)
  expect_identical(actual, expected)
  # ---------------------------------------------------------------------------

  rmd_files <- file.path(dr, c("regex-tests-1.Rmd", "regex-tests-2.Rmd"))
  expected <- c(
    paste0("Chunk name 'x-1-en' in file '", file.path(dr, "regex-tests-1.Rmd"),
           "'\n is not of correct format for French chunks (^\\S+-fr$)"),
    paste0("Chunk 'x-1-en' in file '", file.path(dr, "regex-tests-1.Rmd"),
           "'\n must include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk name 'x-2-en' in file '", file.path(dr, "regex-tests-1.Rmd"),
           "'\n is not of correct format for French chunks (^\\S+-fr$)"),
    paste0("Chunk 'x-2-en' in file '", file.path(dr, "regex-tests-1.Rmd"),
           "'\n must include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk name 'x-3' in file '", file.path(dr, "regex-tests-1.Rmd"),
           "'\n is not of correct format for English chunks (^\\S+-en$)"),

    paste0("Chunk name 'x-6-en' in file '", file.path(dr, "regex-tests-1.Rmd"),
           "'\n is not of correct format for French chunks (^\\S+-fr$)"),
    paste0("Chunk 'x-6-en' in file '", file.path(dr, "regex-tests-1.Rmd"),
           "'\n must include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk 'x-8-en' in file '", file.path(dr, "regex-tests-1.Rmd"),
           "'\n must not include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk 'x-9-en' in file '", file.path(dr, "regex-tests-1.Rmd"),
           "'\n must not include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk name 'x-10-en' in file '", file.path(dr, "regex-tests-1.Rmd"),
           "'\n is not of correct format for French chunks (^\\S+-fr$)"),

    paste0("Chunk 'x-11-en' in file '", file.path(dr, "regex-tests-1.Rmd"),
           "'\n must not include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk 'x-12-en' in file '", file.path(dr, "regex-tests-1.Rmd"),
           "'\n must not include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk name 'y-1-end' in file '", file.path(dr, "regex-tests-2.Rmd"),
           "'\n is not of correct format for French chunks (^\\S+-fr$)"),
    paste0("Chunk 'y-1-end' in file '", file.path(dr, "regex-tests-2.Rmd"),
           "'\n must include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk name 'y-2-en' in file '", file.path(dr, "regex-tests-2.Rmd"),
           "'\n is not of correct format for French chunks (^\\S+-fr$)"),
    paste0("Chunk 'y-2-en' in file '", file.path(dr, "regex-tests-2.Rmd"),
           "'\n must include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk name 'y-6-en' in file '", file.path(dr, "regex-tests-2.Rmd"),
           "'\n is not of correct format for French chunks (^\\S+-fr$)"),
    paste0("Chunk 'y-6-en' in file '", file.path(dr, "regex-tests-2.Rmd"),
           "'\n must include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk 'y-8-en' in file '", file.path(dr, "regex-tests-2.Rmd"),
           "'\n must not include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk 'y-9-en' in file '", file.path(dr, "regex-tests-2.Rmd"),
           "'\n must not include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk name 'y-10-en' in file '", file.path(dr, "regex-tests-2.Rmd"),
           "'\n is not of correct format for French chunks (^\\S+-fr$)"),

    paste0("Chunk 'y-11-en' in file '", file.path(dr, "regex-tests-2.Rmd"),
           "'\n must not include `needs_trans = TRUE` or `needs_trans = FALSE`"),

    paste0("Chunk 'y-12-en' in file '", file.path(dr, "regex-tests-2.Rmd"),
           "'\n must not include `needs_trans = TRUE` or `needs_trans = FALSE`"))

  actual <- csasdown:::validate_chunk_headers(rmd_files)
  expect_identical(actual, expected)
  # ---------------------------------------------------------------------------

  rmd_file <- file.path(dr, "regex-tests-5.Rmd")
  expected <- c(
    paste0("Chunk name 'x-1-en' in file '", file.path(dr,
      "regex-tests-5.Rmd"), "'\n is not of correct format for English chunks ",
      "(^en-\\S+$)"),
    paste0("Chunk 'fr-x-2' in file '", file.path(dr,
      "regex-tests-5.Rmd"), "'\n must include `needs_trans = TRUE` or ",
      "`needs_trans = FALSE`"),
    paste0("Chunk 'en-x-3' in file '", file.path(dr,
      "regex-tests-5.Rmd"), "'\n must not include `needs_trans = TRUE` or ",
      "`needs_trans = FALSE`"),
    paste0("Chunk 'x-5' in file '", file.path(dr,
      "regex-tests-5.Rmd"), "'\n must not include `needs_trans = TRUE` or ",
      "`needs_trans = FALSE`"),
    paste0("Chunk 'x-6' in file '", file.path(dr,
      "regex-tests-5.Rmd"), "'\n must not include `needs_trans = TRUE` or ",
      "`needs_trans = FALSE`"))
  actual <- csasdown:::validate_chunk_headers(rmd_file,
                                   en_chunk_regex = "^en-\\S+$",
                                   fr_chunk_regex = "^fr-\\S+$")
  expect_identical(actual, expected)
  # ---------------------------------------------------------------------------

})
