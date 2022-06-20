test_that("csasdown:::is_rmd_text_line() works", {

  k <- csasdown:::is_rmd_text_line(NULL)
  expect_identical(k, NULL)
  # ---------------------------------------------------------------------------

  k <- csasdown:::is_rmd_text_line("This is a text line")
  expect_true(k)
  # ---------------------------------------------------------------------------

  k <- csasdown:::is_rmd_text_line("# This is not a text line")
  expect_false(k)
  # ---------------------------------------------------------------------------

  k <- csasdown:::is_rmd_text_line("#This is a text line")
  expect_true(k)
  # ---------------------------------------------------------------------------

  k <- csasdown:::is_rmd_text_line(c("    # header", "#non header", "     regular text.",
                          ""))
  expect_identical(k, c(TRUE, TRUE, TRUE, FALSE))
  # ---------------------------------------------------------------------------

  k <- csasdown:::is_rmd_text_line(c("   # header", "#non header", "     regular text.",
                          ""))
  expect_identical(k, c(FALSE, TRUE, TRUE, FALSE))
  # ---------------------------------------------------------------------------

  expect_error(k <- csasdown:::is_rmd_text_line(c(NA, "   # header", "#non header",
                                       "     regular text.", "")))
  # ---------------------------------------------------------------------------

})
