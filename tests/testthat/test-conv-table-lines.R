test_that("Conversion of table lines in Rmd works correctly", {

  chunk <- NULL
  tmp <- conv_table_lines(chunk)
  expect_null(tmp[[1]])
  expect_null(tmp[[2]])

  chunk <- "----"
  tmp <- conv_table_lines(chunk)
  expect_null(tmp[[1]])
  expect_identical(tmp[[2]], chunk)

  chunk <- "-----"
  tmp <- conv_table_lines(chunk)
  expect_null(tmp[[1]])
  expect_identical(tmp[[2]], chunk)

  chunk <- c("-----", "", "-----")
  tmp <- conv_table_lines(chunk)
  expect_null(tmp[[1]])
  expect_identical(tmp[[2]], chunk)

  chunk <- c("-----", "asd", "-----")
  tmp <- conv_table_lines(chunk)
  expect_null(tmp[[1]])
  expect_identical(tmp[[2]], chunk)

})

test_that("Conversion of table lines in Rmd works correctly (type 1 table)", {

  chunk <- c("-----", "asd", "-----", "xyz", "-----")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], chunk)
  expect_null(tmp[[2]])

  chunk <- c("-----", "asd", "-----", "xyz", "", "", "" ,"-----")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("-----", "asd", "-----", "xyz", "-----"))
  expect_null(tmp[[2]])

  chunk <- c("-----", "asd", "-----", "", "", "", "xyz", "", "", "" ,"-----")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("-----", "asd", "-----", "xyz", "-----"))
  expect_null(tmp[[2]])

  chunk <- c("-----", "asd", "-----", "", "", "", "xyz", "", "", "" ,"-----",
             "Table: (\\#tab:text) Test label ")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("-----", "asd", "-----", "xyz", "-----",
                               "Table: (\\#tab:text) Test label "))
  expect_null(tmp[[2]])

  chunk <- c("-----", "asd", "-----", "", "", "", "xyz", "", "", "" ,"-----",
             "Table: (\\#tab:text) Test label.", "Two lines.")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("-----", "asd", "-----", "xyz", "-----",
                               "Table: (\\#tab:text) Test label.", "Two lines."))
  expect_null(tmp[[2]])

})

test_that("Conversion of table lines in Rmd works correctly (type 2 table)", {

  chunk <- c("asd", "-----", "xyz")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], chunk)
  expect_null(tmp[[2]])
})