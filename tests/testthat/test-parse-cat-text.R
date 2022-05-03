test_that("cat parser works", {
  # Single quotes
  strs <- c("cat('This is an example",
            "of matching cat parens",
            "')")
  strs_mod <- c("'This is an example",
                "of matching cat parens",
                "'")
  out <- parse_cat_text(strs)
  expect_identical(out, strs_mod)

  # Double quotes
  strs <- c('cat("This is an example',
            'of matching cat parens',
            '")')
  strs_mod <- c('"This is an example',
                'of matching cat parens',
                '"')
  out <- parse_cat_text(strs)
  expect_identical(out, strs_mod)

  # Correct complex paren matching
  strs <- c('cat("This is (an) example',
            'of matching (cat) parens()',
            '")')
  strs_mod <- c('"This is (an) example',
                'of matching (cat) parens()',
                '"')
  out <- parse_cat_text(strs)
  expect_identical(out, strs_mod)

  # Correct complex paren matching
  strs <- c('cat("This is an example',
            'of matching cat(((()))) parens()',
            '()()")')
  strs_mod <- c('"This is an example',
                'of matching cat(((()))) parens()',
                '()()"')
  out <- parse_cat_text(strs)
  expect_identical(out, strs_mod)

  # Catch extra trailing characters after ')'
  strs <- c('cat("This is an example',
            'of matching cat parens',
            '") extra')
  expect_error(parse_cat_text(strs))

  # Incorrect paren matching
  strs <- c('cat("This is an )example',
            'of matching cat parens',
            '")')
  expect_error(parse_cat_text(strs))

  # Incorrect paren matching
  strs <- c('cat("(This is an example',
            'of matching cat parens',
            '")')
  expect_error(parse_cat_text(strs))

  # Incorrect paren matching
  strs <- c('cat("(This( is an example',
            'of matching cat))) parens',
            '")')
  expect_error(parse_cat_text(strs))

  # Incorrect paren matching
  strs <- c('cat("This is an example',
            'of ((())))matching cat parens',
            '")')
  expect_error(parse_cat_text(strs))

  # Incorrect paren matching
  strs <- c('cat("This is an example',
            'of matching cat parens',
            ')")')
  expect_error(parse_cat_text(strs))

  # Incorrect paren matching
  strs <- c('cat("(This is an example',
            'of (matching( cat) parens)))',
            '")')
  expect_error(parse_cat_text(strs))

  # Incorrect paren matching
  strs <- c('cat("(This) is an example',
            'of matching (cat) (parens',
            '")')
  expect_error(parse_cat_text(strs))

  # Longer than needed text, multiple lines (ok)
  strs <- c('cat("This is an example',
            'of longer than needed text',
            '")',
            'to find the correct text')
  strs_mod <- c('"This is an example',
                'of longer than needed text',
                '"')
  expect_identical(parse_cat_text(strs), strs_mod)

  # Longer than needed text, same lines
  strs <- c('cat("This is an example',
            'of longer than needed text',
            '") to find the correct text')
  expect_error(parse_cat_text(strs))

})
