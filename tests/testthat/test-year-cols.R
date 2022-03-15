# No year columns
df1 <- data.frame(a = 1:50, b = seq(1000, 1001, length.out = 50))
# 1 year column
df2 <- data.frame(a = 2001:2050, b = seq(1000, 1001, length.out = 50))
# 2 year columns
df3 <- data.frame(a = 2001:2050, b = seq(1000, 1001, length.out = 50), c = 1901:1950)
# 2 year columns, additional column with large serial numbers
df4 <- data.frame(a = 2001:2050, b = seq(1000, 1001, length.out = 50), c = 1901:1950, d = 40001:40050)

test_that("year_col() function works", {
  i <- year_cols(df1)
  j <- year_cols(df2)
  k <- year_cols(df3)
  l <- year_cols(df4)
  expect_equal(i, NULL)
  expect_equal(j, "a")
  expect_equal(k, c("a", "c"))
  expect_equal(l, c("a", "c"))
})

test_that("Non-existent columns requested in cols_no_format for csas_table()", {
  expect_error(x <- csasdown::csas_table(df1, format = "latex", cols_no_format = c("x", "y")))
})

test_that("csas_table() detects 0 year columns correctly", {
  x <- csasdown::csas_table(df1, format = "latex")
  expect_true(length(grep("1,000.000", x)) == TRUE)
  expect_true(!length(grep("1000.000", x)))
  expect_true(length(grep("1,001.000", x)) == TRUE)
  expect_true(!length(grep("1001.000", x)))
})

test_that("csas_table() detects 1 year column correctly", {
  x <- csasdown::csas_table(df2, format = "latex")
  expect_true(length(grep("2001", x)) == TRUE)
  expect_true(!length(grep("2,001", x)))
  expect_true(length(grep("2050", x)) == TRUE)
  expect_true(!length(grep("2,050", x)))

  expect_true(length(grep("1,000.000", x)) == TRUE)
  expect_true(!length(grep("1000.000", x)))
  expect_true(length(grep("1,001.000", x)) == TRUE)
  expect_true(!length(grep("1001.000", x)))
})

test_that("csas_table() detects 2 year columns correctly", {
  x <- csasdown::csas_table(df3, format = "latex")
  expect_true(length(grep("1901", x)) == TRUE)
  expect_true(!length(grep("1,901", x)))
  expect_true(length(grep("1950", x)) == TRUE)
  expect_true(!length(grep("1,950", x)))

  expect_true(length(grep("2001", x)) == TRUE)
  expect_true(!length(grep("2,001", x)))
  expect_true(length(grep("2050", x)) == TRUE)
  expect_true(!length(grep("2,050", x)))

  expect_true(length(grep("1,000.000", x)) == TRUE)
  expect_true(!length(grep("1000.000", x)))
  expect_true(length(grep("1,001.000", x)) == TRUE)
  expect_true(!length(grep("1001.000", x)))
})

test_that("csas_table() detects 2 year columns correctly and makes exception of formatting for column supplied", {
  x <- csasdown::csas_table(df3, format = "latex", cols_no_format = "a")
  expect_true(length(grep("1901", x)) == TRUE)
  expect_true(!length(grep("1,901", x)))
  expect_true(length(grep("1950", x)) == TRUE)
  expect_true(!length(grep("1,950", x)))

  expect_true(length(grep("2001", x)) == TRUE)
  expect_true(!length(grep("2,001", x)))
  expect_true(length(grep("2050", x)) == TRUE)
  expect_true(!length(grep("2,050", x)))

  expect_true(length(grep("1,000.000", x)) == TRUE)
  expect_true(!length(grep("1000.000", x)))
  expect_true(length(grep("1,001.000", x)) == TRUE)
  expect_true(!length(grep("1001.000", x)))
})

test_that(paste0("csas_table() detects 2 year columns correctly and ignores ",
                 "one out of range (formats it correctly)"), {
  x <- csasdown::csas_table(df4, format = "latex")
  expect_true(length(grep("1901", x)) == TRUE)
  expect_true(!length(grep("1,901", x)))
  expect_true(length(grep("1950", x)) == TRUE)
  expect_true(!length(grep("1,950", x)))

  expect_true(length(grep("2001", x)) == TRUE)
  expect_true(!length(grep("2,001", x)))
  expect_true(length(grep("2050", x)) == TRUE)
  expect_true(!length(grep("2,050", x)))

  expect_true(length(grep("1,000.000", x)) == TRUE)
  expect_true(!length(grep("1000.000", x)))
  expect_true(length(grep("1,001.000", x)) == TRUE)
  expect_true(!length(grep("1001.000", x)))

  expect_true(length(grep("40,001", x)) == TRUE)
  expect_true(!length(grep("40001", x)))
  expect_true(length(grep("40,050", x)) == TRUE)
  expect_true(!length(grep("40050", x)))
})

test_that(paste0("csas_table() detects 2 year columns correctly makes exception ",
                 "of formatting for column supplied which is out of range for year"), {
  x <- csasdown::csas_table(df4, format = "latex", cols_no_format = "d")
  expect_true(length(grep("1901", x)) == TRUE)
  expect_true(!length(grep("1,901", x)))
  expect_true(length(grep("1950", x)) == TRUE)
  expect_true(!length(grep("1,950", x)))

  expect_true(length(grep("2001", x)) == TRUE)
  expect_true(!length(grep("2,001", x)))
  expect_true(length(grep("2050", x)) == TRUE)
  expect_true(!length(grep("2,050", x)))

  expect_true(length(grep("1,000.000", x)) == TRUE)
  expect_true(!length(grep("1000.000", x)))
  expect_true(length(grep("1,001.000", x)) == TRUE)
  expect_true(!length(grep("1001.000", x)))

  expect_true(length(grep("40001", x)) == TRUE)
  expect_true(!length(grep("40,001", x)))
  expect_true(length(grep("40050", x)) == TRUE)
  expect_true(!length(grep("40,050", x)))
})
