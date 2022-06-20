test_that("Conversion of list lines in Rmd works correctly", {

  chunk <- NULL
  tmp <- csasdown:::conv_list_lines(chunk)
  expect_null(tmp[[1]])
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- "1. Item"
  tmp <- csasdown:::conv_list_lines(chunk)
  expect_identical(tmp[[1]], c("1. Item"))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- c("1. Item", "", "", "Text")
  tmp <- csasdown:::conv_list_lines(chunk)
  expect_identical(tmp[[1]], c("1. Item"))
  expect_identical(tmp[[2]], c("", "", "Text"))
  # ---------------------------------------------------------------------------

  chunk <- c("1. Item", "", "", "", "", "", "Text")
  tmp <- csasdown:::conv_list_lines(chunk)
  expect_identical(tmp[[1]], c("1. Item"))
  expect_identical(tmp[[2]], c("", "", "", "", "", "Text"))
  # ---------------------------------------------------------------------------

  chunk <- c("1. Item", "Text", "here")
  tmp <- csasdown:::conv_list_lines(chunk)
  expect_identical(tmp[[1]], c("1. Item"))
  expect_identical(tmp[[2]], c("Text", "here"))
  # ---------------------------------------------------------------------------

  chunk <- c("1. Item", "# Header", "here")
  tmp <- csasdown:::conv_list_lines(chunk)
  expect_identical(tmp[[1]], c("1. Item"))
  expect_identical(tmp[[2]], c("# Header", "here"))
  # ---------------------------------------------------------------------------

  chunk <- c("1. Item", "-----", "  A  ", "-----", "  B  ", "-----", "", "")
  tmp <- csasdown:::conv_list_lines(chunk)
  expect_identical(tmp[[1]], c("1. Item"))
  expect_identical(tmp[[2]], c("-----", "  A  ", "-----", "  B  ", "-----",
                               "", ""))
  # ---------------------------------------------------------------------------

  chunk <- c("1. Item 1", "2. Item 2", "  a. Item 2a", "  b. Item 2b",
             "3. Item 3")
  tmp <- csasdown:::conv_list_lines(chunk)
  expect_identical(tmp[[1]], c("1. Item 1", "2. Item 2", "  a. Item 2a",
                               "  b. Item 2b", "3. Item 3"))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- c("1. Item 1", "", "2. Item 2", "  a. Item 2a", "  b. Item 2b",
             "3. Item 3", "", "", "", "", "# Header", "")
  tmp <- csasdown:::conv_list_lines(chunk)
  expect_identical(tmp[[1]], c("1. Item 1"))
  expect_identical(tmp[[2]], c("", "2. Item 2", "  a. Item 2a", "  b. Item 2b",
                               "3. Item 3", "", "", "", "", "# Header", ""))
  # ---------------------------------------------------------------------------

})
