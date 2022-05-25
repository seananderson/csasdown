test_that("Conversion of blank lines in Rmd works correctly", {

  chunk <- NULL
  tmp <- conv_blank_lines(chunk)
  blank_line_chunk <- tmp[[1]]
  the_rest <- tmp[[2]]
  expect_null(blank_line_chunk)
  expect_null(the_rest)

  chunk <- c("")
  tmp <- conv_blank_lines(chunk)
  blank_line_chunk <- tmp[[1]]
  the_rest <- tmp[[2]]
  expect_identical(blank_line_chunk, c("\\\\", "\\\\", ""))
  expect_null(the_rest)

  chunk <- c("asd", "", "", "")
  tmp <- conv_blank_lines(chunk)
  blank_line_chunk <- tmp[[1]]
  the_rest <- tmp[[2]]
  expect_null(blank_line_chunk)
  expect_identical(the_rest, chunk)

  chunk <- c("", "Some text..", "More Rmd text")
  tmp <- conv_blank_lines(chunk)
  blank_line_chunk <- tmp[[1]]
  the_rest <- tmp[[2]]
  expect_identical(blank_line_chunk, c("\\\\", ""))
  expect_identical(the_rest, c("Some text..", "More Rmd text"))

  chunk <- c("", "", "", "Some text..", "More Rmd text")
  tmp <- conv_blank_lines(chunk)
  blank_line_chunk <- tmp[[1]]
  the_rest <- tmp[[2]]
  expect_identical(blank_line_chunk, c("\\\\", "\\\\", "\\\\", ""))
  expect_identical(the_rest, c("Some text..", "More Rmd text"))
})
