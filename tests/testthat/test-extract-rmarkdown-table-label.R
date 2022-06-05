test_that("extract_rmd_table_label() works", {

  ret <- extract_rmd_table_label(NULL)
  expect_identical(ret, list(NULL, NULL))

  # ---------------------------------------------------------------------------
  j <- c("")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("# Header", "1. Item 1", "1. Item 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "# Header", "1. Item 1", "1. Item 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "", "# Header", "1. Item 1", "1. Item 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "", "", "# Header", "1. Item 1", "1. Item 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("1. Item 1", "1. Item 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "1. Item 1", "1. Item 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "", "1. Item 1", "1. Item 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "", "", "1. Item 1", "1. Item 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "", "")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Generic text")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list("Table: Caption", NULL))

  # ---------------------------------------------------------------------------
  j <- c("", "", "   Table: Caption")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list("   Table: Caption", NULL))

  # ---------------------------------------------------------------------------
  j <- c("", "", "    Table: Caption")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "1. Item 1")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list("Table: Caption", "1. Item 1"))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "1. Item 1", "2. Item 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list("Table: Caption", c("1. Item 1", "2. Item 2")))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "# Header 1")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list("Table: Caption", "# Header 1"))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "# Header 1", "# Header 2")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list("Table: Caption", c("# Header 1", "# Header 2")))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "-----", "Table header")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(c("Table: Caption", "-----",
                               "Table header"),
                             NULL))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "-----", "Table header", "-----")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list("Table: Caption",
                             c("-----", "Table header", "-----")))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "Table header", "-----", "Content")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list("Table: Caption",
                             c("Table header", "-----", "Content")))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list("Table: Caption", ""))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "Another caption line",
         "Yet another caption line")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(c("Table: Caption", "Another caption line",
                               "Yet another caption line"), NULL))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table: Caption", "Another caption line",
         "Yet another caption line", "", "Generic text")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(c("Table: Caption", "Another caption line",
                               "Yet another caption line"),
                             c("", "Generic text")))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table:", "")
  expect_warning(ret <- extract_rmd_table_label(j))
  expect_identical(ret, list(NULL, j))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table:", "Caption")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(c("Table:", "Caption"), NULL))

  # ---------------------------------------------------------------------------
  j <- c("", "", "   Table:", "Caption")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(c("   Table:", "Caption"), NULL))
  # ---------------------------------------------------------------------------

  j <- c("", "", "    Table:", "Caption")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(NULL, j))
  # ---------------------------------------------------------------------------

  j <- c("", "", "Table:", "Caption", "Another caption line",
         "Yet another caption line")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(c("Table:", "Caption", "Another caption line",
                               "Yet another caption line"), NULL))

  # ---------------------------------------------------------------------------
  j <- c("", "", "Table:", "Caption", "Another caption line",
         "Yet another caption line", "", "Generic text")
  ret <- extract_rmd_table_label(j)
  expect_identical(ret, list(c("Table:", "Caption", "Another caption line",
                               "Yet another caption line"),
                             c("", "Generic text")))

})
