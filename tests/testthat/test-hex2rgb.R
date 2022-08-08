test_that("hex2rgb() works", {

  expect_null(csasdown:::hex2rgb(NULL))
  expect_true(is.na(csasdown:::hex2rgb(NA)))

  expect_error(csasdown:::hex2rgb("#0"), "hex must be a 6- or 8-digit number")
  expect_error(csasdown:::hex2rgb("#0000"), "hex must be a 6- or 8-digit number")
  expect_error(csasdown:::hex2rgb("00000"), "hex must be a 6- or 8-digit number")
  expect_error(csasdown:::hex2rgb("0000000"), "hex must be a 6- or 8-digit number")
  expect_error(csasdown:::hex2rgb("#aaaaaafff"), "hex must be a 6- or 8-digit number")

  expect_identical(csasdown:::hex2rgb("#000000"), c(0L, 0L, 0L))
  expect_identical(csasdown:::hex2rgb("000000"), c(0L, 0L, 0L))

  expect_identical(csasdown:::hex2rgb("#000000", rel = TRUE), c(0, 0, 0))
  expect_identical(csasdown:::hex2rgb("#000000", ret_alpha = TRUE), c(0L, 0L, 0L, 255L))
  expect_identical(csasdown:::hex2rgb("#000000", rel = TRUE, ret_alpha = TRUE), c(0, 0, 0, 1))

  expect_identical(csasdown:::hex2rgb("FFFFFF", rel = TRUE), c(1, 1, 1))
  expect_identical(round(csasdown:::hex2rgb("FF32FF", rel = TRUE), 3), c(1, 0.196, 1))
  expect_identical(csasdown:::hex2rgb("FF3200"), c(255L, 50L, 0L))

  expect_error(csasdown:::hex2rgb("fffffg", rel = TRUE),
               "`hex` contains non-hexadecimal digits")

})
