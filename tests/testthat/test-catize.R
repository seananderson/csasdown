str0 <- "Test without r code"
str1 <- "`r 1 + 1` Test with r code first only"
str2 <- "Test with r code last only `r 2 * 3`"
str3 <- "`r 1 + 1` Test with r code first and last only `r 2 * 3`"
str4 <- "`r only_r_code`"
str5 <- "complex `r map(x, ~{.x / (.x ^ 2) })` r code example from directory: `r here()`."
str6 <- "`r code_1``r code_2`"

test_that("csasdown::catize() function works", {
  i0 <- csasdown::catize(str0)
  i1 <- csasdown::catize(str1)
  i2 <- csasdown::catize(str2)
  i3 <- csasdown::catize(str3)
  i4 <- csasdown::catize(str4)
  i5 <- csasdown::catize(str5)
  i6 <- csasdown::catize(str6)
  i7 <- csasdown::catize("")

  expect_equal(i0, str0)
  expect_equal(as.character(i1), "\", 1 + 1, \" Test with r code first only")
  expect_equal(as.character(i2), "Test with r code last only \", 2 * 3, \"")
  expect_equal(as.character(i3), "\", 1 + 1, \" Test with r code first and last only \", 2 * 3, \"")
  expect_equal(as.character(i4), "only_r_code")
  expect_equal(as.character(i5), "complex \", map(x, ~{.x / (.x ^ 2) }), \" r code example from directory: \", here(), \".")
  expect_equal(as.character(i6), "\", code_1, \"\", code_2, \"")
  expect_equal(i7, "")

})

