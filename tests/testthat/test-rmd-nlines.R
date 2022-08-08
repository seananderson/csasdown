test_that("rnd_nlines() works", {

  # -----------------------------------------------------------------------------
  expect_error(csasdown:::rmd_nlines(NULL),
               "`num_blank_lines` must not be `NULL`",
               fixed = TRUE)

  # -----------------------------------------------------------------------------
  expect_error(csasdown:::rmd_nlines(-1),
               "`num_blank_lines` must be zero or greater",
               fixed = TRUE)

  # -----------------------------------------------------------------------------
  expect_identical(csasdown:::rmd_nlines(0), "")

  # -----------------------------------------------------------------------------
  expect_identical(csasdown:::rmd_nlines(1), c("", "\\\\ \\\\", ""))

  # -----------------------------------------------------------------------------
  expect_identical(csasdown:::rmd_nlines(2), c("", "\\\\", ""))

  # -----------------------------------------------------------------------------
  expect_identical(csasdown:::rmd_nlines(4), c("", "\\\\", "\\\\", "\\\\", ""))

})
