test_that("csasdown:::is_rmd_header_line() works", {

  expect_null(csasdown:::is_rmd_header_line(NULL))
  # ---------------------------------------------------------------------------

  expect_false(csasdown:::is_rmd_header_line(""))
  # ---------------------------------------------------------------------------

  expect_false(csasdown:::is_rmd_header_line("1. List Item"))
  # ---------------------------------------------------------------------------

  expect_false(csasdown:::is_rmd_header_line("plaintext"))
  # ---------------------------------------------------------------------------

  expect_true(csasdown:::is_rmd_header_line("# No indentation"))
  # ---------------------------------------------------------------------------

  expect_true(csasdown:::is_rmd_header_line(" # One indentation"))
  # ---------------------------------------------------------------------------

  expect_true(csasdown:::is_rmd_header_line("  # Two indentation"))
  # ---------------------------------------------------------------------------

  expect_true(csasdown:::is_rmd_header_line("   # Three indentation (max"))
  # ---------------------------------------------------------------------------

  expect_false(csasdown:::is_rmd_header_line("    # Four indentation (too much)"))
  # ---------------------------------------------------------------------------

  expect_false(csasdown:::is_rmd_header_line("#No space between hash and text"))
  # ---------------------------------------------------------------------------

  expect_true(csasdown:::is_rmd_header_line("## Second header"))
  # ---------------------------------------------------------------------------

  expect_true(csasdown:::is_rmd_header_line("### Third header"))
  # ---------------------------------------------------------------------------

  expect_true(csasdown:::is_rmd_header_line("   ### Third header indented 3"))
  # ---------------------------------------------------------------------------

  expect_false(csasdown:::is_rmd_header_line("###Third header, no space"))
  # ---------------------------------------------------------------------------

  j <- c("# Header 1", "#   Header 2", " ## Secondary header 1",
         "## Secondary header 2", "#Incorrect header")
  ret <- csasdown:::is_rmd_header_line(j)
  expect_identical(ret, c(TRUE, TRUE, TRUE, TRUE, FALSE))
  # ---------------------------------------------------------------------------
})
