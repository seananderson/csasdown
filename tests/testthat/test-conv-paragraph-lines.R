test_that("csasdown:::conv_paragraph_lines() works", {

  chunk <- NULL
  tmp <- csasdown:::conv_paragraph_lines(chunk)
  expect_null(tmp[[1]])
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- c("")
  tmp <- csasdown:::conv_paragraph_lines(chunk)
  expect_null(tmp[[1]])
  expect_identical(tmp[[2]], "")
  # ---------------------------------------------------------------------------

  chunk <- c("Text line")
  tmp <- csasdown:::conv_paragraph_lines(chunk)
  expect_identical(tmp[[1]], c("Text line"))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- c("Text line", "")
  tmp <- csasdown:::conv_paragraph_lines(chunk)
  expect_identical(tmp[[1]], c("Text line"))
  expect_identical(tmp[[2]], "")
  # ---------------------------------------------------------------------------

  chunk <- c("", "Text line")
  tmp <- csasdown:::conv_paragraph_lines(chunk)
  expect_null(tmp[[1]])
  expect_identical(tmp[[2]], c("", "Text line"))
  # ---------------------------------------------------------------------------

  chunk <- c("1. Item", "")
  tmp <- csasdown:::conv_paragraph_lines(chunk)
  expect_null(tmp[[1]])
  expect_identical(tmp[[2]], c("1. Item", ""))
  # ---------------------------------------------------------------------------

  chunk <- c("# Header", "")
  tmp <- csasdown:::conv_paragraph_lines(chunk)
  expect_null(tmp[[1]])
  expect_identical(tmp[[2]], c("# Header", ""))
  # ---------------------------------------------------------------------------

  chunk <- c("", "", "", "Text line")
  tmp <- csasdown:::conv_paragraph_lines(chunk)
  expect_null(tmp[[1]])
  expect_identical(tmp[[2]], c("", "", "", "Text line"))
  # ---------------------------------------------------------------------------

  chunk <- c("Text line", "continues on the second line")
  tmp <- csasdown:::conv_paragraph_lines(chunk)
  expect_identical(tmp[[1]], c("Text line", "\\\\", "",
                               "continues on the second line"))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- c("Text line", "continues on the second line", "and the third")
  tmp <- csasdown:::conv_paragraph_lines(chunk)
  expect_identical(tmp[[1]], c("Text line", "\\\\", "",
                               "continues on the second line", "\\\\", "",
                               "and the third"))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------

  chunk <- c("Text line", "continues on the second line", "", "and the third")
  tmp <- csasdown:::conv_paragraph_lines(chunk)
  expect_identical(tmp[[1]], c("Text line", "\\\\", "",
                               "continues on the second line"))
  expect_identical(tmp[[2]], c("", "and the third"))
  # ---------------------------------------------------------------------------

  chunk <- c("Text line", "", "", "", "continues on the second line")
  tmp <- csasdown:::conv_paragraph_lines(chunk)
  expect_identical(tmp[[1]], c("Text line"))
  expect_identical(tmp[[2]], c("", "", "", "continues on the second line"))
  # ---------------------------------------------------------------------------

  chunk <- c("Text line", "1. Item 1", "2. Item 2")
  tmp <- csasdown:::conv_paragraph_lines(chunk)
  expect_identical(tmp[[1]], c("Text line", ""))
  expect_identical(tmp[[2]], c("1. Item 1", "2. Item 2"))
  # ---------------------------------------------------------------------------

})
