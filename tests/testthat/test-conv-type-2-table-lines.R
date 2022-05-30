test_that("Conversion of type 2 table lines in Rmd works correctly", {

  chunk <- NULL
  tmp <- conv_type_2_table_lines(chunk)
  expect_null(tmp[[1]])
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c("", "-----", "")
  expect_error(tmp <- conv_type_2_table_lines(chunk))
  # ---------------------------------------------------------------------------
  chunk <- c("-----", "asd", "-----")
  expect_error(tmp <- conv_type_2_table_lines(chunk))
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "xyz")
  tmp <- conv_type_2_table_lines(chunk)
  expect_identical(tmp[[1]], c(chunk, ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "xyz", "", "",
             "Table: This is a table caption")
  tmp <- conv_type_2_table_lines(chunk)
  expect_identical(tmp[[1]], c("asd", "-----", "xyz", "",
                               "Table: This is a table caption", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "xyz", "", "", "" )
  tmp <- conv_type_2_table_lines(chunk)
  expect_identical(tmp[[1]], c("asd", "-----", "xyz", ""))
  expect_identical(tmp[[2]], c("", ""))
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "dfg", "", "", "xyz", "", "", "" )
  tmp <- conv_type_2_table_lines(chunk)
  expect_identical(tmp[[1]], c("asd", "-----", "dfg", ""))
  expect_identical(tmp[[2]], c("", "xyz", "", "", ""))
  # ---------------------------------------------------------------------------
  chunk <- c("xyz", "-----", "abc", "efg", "", "Non-caption text", "",
             "Table:", "xxx")
  tmp <- conv_type_2_table_lines(chunk)
  expect_identical(tmp[[1]], c("xyz", "-----", "abc", "efg", ""))
  expect_identical(tmp[[2]], c("Non-caption text", "", "Table:", "xxx"))
  # ---------------------------------------------------------------------------
  chunk <- c("xyz", "-----", "abc", "", "efg", "-----", "", "",
             "Non-caption text")
  tmp <- conv_type_2_table_lines(chunk)
  expect_identical(tmp[[1]], c("xyz", "-----", "abc", ""))
  expect_identical(tmp[[2]], c("efg", "-----", "", "", "Non-caption text"))
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "xyz", "", "", "Table: (\\#tab:text) Test label ")
  tmp <- conv_type_2_table_lines(chunk)
  expect_identical(tmp[[1]], c("asd", "-----", "xyz", "",
                               "Table: (\\#tab:text) Test label ", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "xyz", "", "Table: (\\#tab:text) Test label.",
             "Two lines.")
  tmp <- conv_type_2_table_lines(chunk)
  expect_identical(tmp[[1]], c("asd", "-----", "xyz", "",
                               "Table: (\\#tab:text) Test label.",
                               "Two lines.", ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "xyz", "", "", "" ,
             "    Table: (\\#tab:text) Test label.", "Two lines.")
  expect_warning(tmp <- conv_type_2_table_lines(chunk))
  expect_identical(tmp[[1]], c("asd", "-----", "xyz", ""))
  expect_identical(tmp[[2]], c("", "", "",
                               "    Table: (\\#tab:text) Test label.",
                               "Two lines."))
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "xyz", "", "", "",
             "    Table:", "(\\#tab:text) Test label.")
  expect_warning(tmp <- conv_type_2_table_lines(chunk))
  expect_identical(tmp[[1]], c("asd", "-----", "xyz", ""))
  expect_identical(tmp[[2]], c("", "", "", "    Table:",
                               "(\\#tab:text) Test label."))
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "xyz", "", "", "",
             "  Table:", "          (\\#tab:text) Test label.")
  tmp <- conv_type_2_table_lines(chunk)
  expect_identical(tmp[[1]], c("asd", "-----", "xyz", "",
                               "  Table:",
                               "          (\\#tab:text) Test label.", ""))

  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "xyz", "", "", "",
             "", "", "", "non-caption text")
  tmp <- conv_type_2_table_lines(chunk)
  expect_identical(tmp[[1]], c("asd", "-----", "xyz", ""))
  expect_identical(tmp[[2]], c("", "", "", "", "", "non-caption text"))
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "xyz", "", "Table: 1st line ", "", "")
  tmp <- conv_type_2_table_lines(chunk)
  expect_identical(tmp[[1]], c("asd", "-----", "xyz", "", "Table: 1st line ",
                               "", ""))
  expect_identical(tmp[[2]], "")
  # ---------------------------------------------------------------------------
  chunk <- c("asd", "-----", "xyz", "", "Table:", "- 2nd line ",
             "         - third line", "", "")
  tmp <- conv_type_2_table_lines(chunk)
  expect_identical(tmp[[1]], c("asd", "-----", "xyz", "", "Table:",
                               "- 2nd line ", "         - third line", "", ""))
  expect_identical(tmp[[2]], "")
  # ---------------------------------------------------------------------------
  chunk <- c(" fgdfgdgfd    assadsd  ",
             "----------- -----------",
             " gfbbggfbf    ffvvfvf  ",
             " hgnhhnhnf    oiuoiuo  ",
             "", "", "", "", "", "",
             "Table: (\\#tab:text) Test label.", "Two lines.")
  tmp <- conv_type_2_table_lines(chunk)
  expect_identical(tmp[[1]], c(" fgdfgdgfd    assadsd  ",
                               "----------- -----------",
                               " gfbbggfbf    ffvvfvf  ",
                               " hgnhhnhnf    oiuoiuo  ",
                               "",
                               "Table: (\\#tab:text) Test label.", "Two lines.",
                               ""))
  expect_null(tmp[[2]])
  # ---------------------------------------------------------------------------
  chunk <- c(" fgdfgdgfd    assadsd  ",
             "----------- -----------",
             " gfbbggfbf    ffvvfvf  ",
             " hgnhhnhnf    oiuoiuo  ",
             "", "", "", "", "", "",
             "Table: (\\#tab:text) Test label.", "", "", "Two lines.")
  tmp <- conv_type_2_table_lines(chunk)
  expect_identical(tmp[[1]], c(" fgdfgdgfd    assadsd  ",
                               "----------- -----------",
                               " gfbbggfbf    ffvvfvf  ",
                               " hgnhhnhnf    oiuoiuo  ",
                               "",
                               "Table: (\\#tab:text) Test label.",
                               "", ""))
  expect_identical(tmp[[2]], c("", "Two lines."))

})
