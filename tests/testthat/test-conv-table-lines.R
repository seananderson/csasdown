test_that("Conversion of table lines in Rmd works correctly", {
  chunk <- NULL
  tmp <- conv_table_lines(chunk)
  expect_null(tmp[[1]])
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- "----"
  tmp <- conv_table_lines(chunk)
  expect_null(tmp[[1]])
  expect_identical(tmp[[2]], chunk)
  # ---------------------------------------------------------------------------
  chunk <- "-----"
  tmp <- conv_table_lines(chunk)
  expect_null(tmp[[1]])
  expect_identical(tmp[[2]], chunk)
  # ---------------------------------------------------------------------------
  chunk <- c("-----", "", "-----")
  tmp <- conv_table_lines(chunk)
  expect_null(tmp[[1]])
  expect_identical(tmp[[2]], chunk)
  # ---------------------------------------------------------------------------
  chunk <- c("-----", "asd", "-----")
  tmp <- conv_table_lines(chunk)
  expect_null(tmp[[1]])
  expect_identical(tmp[[2]], chunk)
})

test_that("Conversion of table lines in Rmd works correctly (type 1 table)", {
  chunk <- c("-----", "asd", "-----", "xyz", "-----")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c(chunk, "\\\\", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c("-----", "asd", "-----", "xyz", "-----", "", "",
             "Table: This is a table caption")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("-----", "asd", "-----", "xyz", "-----",
                               "Table: This is a table caption", "\\\\", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c("-----", "asd", "-----", "xyz", "", "", "" ,"-----")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("-----", "asd", "-----", "xyz", "-----", "\\\\",
                               ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c("-----", "asd", "-----", "", "", "", "xyz", "", "", "" ,"-----")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("-----", "asd", "-----", "xyz", "-----", "\\\\",
                               ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c("-----", "xyz", "-----", "abc", "", "efg", "-----", "", "",
             "Non-caption text")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("-----", "xyz", "-----", "abc", "", "efg",
                               "-----", "\\\\", ""))
  expect_identical(tmp[[2]], c("", "", "Non-caption text"))
  # ---------------------------------------------------------------------------
  chunk <- c("-----", "asd", "-----", "", "", "", "xyz", "", "", "" ,"-----",
             "Table: (\\#tab:text) Test label ")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("-----", "asd", "-----", "xyz", "-----",
                               "Table: (\\#tab:text) Test label ", "\\\\", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c("-----", "asd", "-----", "", "", "", "xyz", "", "", "" ,"-----",
             "Table: (\\#tab:text) Test label.", "Two lines.")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("-----", "asd", "-----", "xyz", "-----",
                               "Table: (\\#tab:text) Test label.", "Two lines.",
                               "\\\\", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c("-----", "asd", "-----", "", "", "", "xyz", "", "", "" ,"-----",
             "", "", "", "non-caption text")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("-----", "asd", "-----", "xyz", "-----", "\\\\",
                               ""))
  expect_identical(tmp[[2]], c("", "", "", "non-caption text"))
  # ---------------------------------------------------------------------------
  chunk <- c("----------- -----------",
             " fgdfgdgfd    assadsd  ",
             "----------- -----------",
             " gfbbggfbf    ffvvfvf  ",
             " hgnhhnhnf    oiuoiuo  ",
             "----------- -----------",
             "", "", "", "", "", "",
             "Table: (\\#tab:text) Test label.", "Two lines.")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("----------- -----------",
                               " fgdfgdgfd    assadsd  ",
                               "----------- -----------",
                               " gfbbggfbf    ffvvfvf  ",
                               "",
                               " hgnhhnhnf    oiuoiuo  ",
                               "----------- -----------",
                               "Table: (\\#tab:text) Test label.", "Two lines.",
                               "\\\\", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c("----------- -----------",
             " fgdfgdgfd    assadsd  ",
             "----------- -----------",
             "", "", "", "",
             " gfbbggfbf    ffvvfvf  ",
             " hgnhhnhnf    oiuoiuo  ",
             "", "", "", "",
             "----------- -----------",
             "", "", "", "", "", "",
             "Table: (\\#tab:text) Test label.", "", "", "Two lines.")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("----------- -----------",
                               " fgdfgdgfd    assadsd  ",
                               "----------- -----------",
                               " gfbbggfbf    ffvvfvf  ",
                               "",
                               " hgnhhnhnf    oiuoiuo  ",
                               "----------- -----------",
                               "Table: (\\#tab:text) Test label.",
                               "\\\\", ""))
  expect_identical(tmp[[2]], c("", "", "Two lines."))
})

test_that("Conversion of table lines in Rmd works correctly (type 2 table)", {
  chunk <- c("asd", "-----", "xyz")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("asd", "-----", "xyz", "\\\\", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "xyz", "abc")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("asd", "-----", "xyz", "abc", "\\\\", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "xyz", "abc", "", "", "", "Non-caption text")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("asd", "-----", "xyz", "abc", "\\\\", ""))
  expect_identical(tmp[[2]], c("", "", "", "Non-caption text"))
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "xyz", "abc", "", "", "", "Table: Test caption")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("asd", "-----", "xyz", "abc", "",
                               "Table: Test caption", "\\\\", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "xyz", "abc", "", "", "", "Table: Test caption",
             " - line 2")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("asd", "-----", "xyz", "abc", "",
                               "Table: Test caption", " - line 2", "\\\\", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "xyz", "abc", "efg", "Table: Test caption",
             " - line 2")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("asd", "-----", "xyz", "abc", "efg",
                               "Table: Test caption", " - line 2", "\\\\", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "xyz", "abc", "efg", "Table: Test caption",
             " - line 2")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("asd", "-----", "xyz", "abc", "efg",
                               "Table: Test caption", " - line 2", "\\\\", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "xyz", "abc", "efg", "Table: Test caption",
             " - line 2", "", "", "New paragraph")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("asd", "-----", "xyz", "abc", "efg",
                               "Table: Test caption", " - line 2", "\\\\", ""))
  expect_identical(tmp[[2]], c("", "", "New paragraph"))
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "xyz", "abc", "efg", "", "", "", "")
  tmp <- conv_table_lines(chunk)
  expect_identical(tmp[[1]], c("asd", "-----", "xyz", "abc", "efg", "\\\\",
                               ""))
  expect_identical(tmp[[2]], c( "", "", "", ""))

})