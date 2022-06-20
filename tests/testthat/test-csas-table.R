df <- data.frame(a = 1:20, b = 1:20)

test_that("csasdown::csas_table works with an extra_header", {
  x <- csasdown::csas_table(
    df,
    caption = "Example of long table with header above column names",
    format = "latex",
    bold_header = FALSE,
    escape = FALSE,
    repeat_header = TRUE,
    font_size = 8,
    extra_header = toupper(letters[1:ncol(df)]),
    ex_line_sep = 0,
    ex_align = "r"
  )
  expect_true(grepl("endfirsthead", x))
})
