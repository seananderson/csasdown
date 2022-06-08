test_that("is_rmarkdown_table_line() works", {

  k <- is_rmarkdown_table_line(NULL)
  expect_identical(k, NULL)
  # ---------------------------------------------------------------------------

  expect_error(is_rmarkdown_table_line(""))
  # ---------------------------------------------------------------------------

  expect_error(is_rmarkdown_table_line(c("", "")))
  # ---------------------------------------------------------------------------

  j <- c("", "", NA)
  k <- is_rmarkdown_table_line(j)
  expect_identical(k, "false")
  # ---------------------------------------------------------------------------

  j <- c("", "", "")
  k <- is_rmarkdown_table_line(j)
  expect_identical(k, "false")
  # ---------------------------------------------------------------------------

  j <- c("-----", "ABCDE", "-----")
  k <- is_rmarkdown_table_line(j)
  expect_identical(k, "type1")
  # ---------------------------------------------------------------------------

  j <- c("ABCDE", "-----", "EFGHI")
  k <- is_rmarkdown_table_line(j)
  expect_identical(k, "type2")
  # ---------------------------------------------------------------------------

  j <- list(c("-----", "ABCDE", "-----"),
            c("ABCDE", "-----", "EFGHI"))
  k <- is_rmarkdown_table_line(j)
  expect_identical(k, c("type1", "type2"))
  # ---------------------------------------------------------------------------

  j <- list(c("-----", "ABCDE", "-----"), NA)
  expect_error(is_rmarkdown_table_line(j))
  # ---------------------------------------------------------------------------

  j <- list(NA, NA)
  expect_error(is_rmarkdown_table_line(j))
  # ---------------------------------------------------------------------------

  j <- list(c("-----", "ABCDE", "-----"), NULL)
  expect_error(is_rmarkdown_table_line(j))
  # ---------------------------------------------------------------------------

  j <- list(NULL, NULL)
  expect_error(is_rmarkdown_table_line(j))

  # ---------------------------------------------------------------------------
  j <- c("-----", "ABCDE", "EFGHI")
  k <- is_rmarkdown_table_line(j)
  expect_identical(k, "false")
  # ---------------------------------------------------------------------------
})
