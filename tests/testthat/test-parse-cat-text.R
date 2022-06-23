test_that("cat parser works", {

  # -----------------------------------------------------------------------------
  # Not starting with cat
  strs <- c("'This is an example",
            "of not starting with cat()")
  expect_error(csasdown:::parse_cat_text(strs),
               "The first line of `str_vec` must start with `cat(`",
               fixed = TRUE)

  # -----------------------------------------------------------------------------
  # Wrong quote type
  strs <- c("cat(`'This is an example",
            "of not starting with cat()`)")
  expect_error(csasdown:::parse_cat_text(strs),
               paste0("Quote type not allowed for `cat()` text. You must use ",
                      "either single or double quotes. You used `"),
               fixed = TRUE)

  # -----------------------------------------------------------------------------
  # `verbose`
  strs <- c("cat('This is an example",
            "of not starting with cat()')")
  x <- csasdown:::capture_log(csasdown:::parse_cat_text(strs, verbose = TRUE))
  j <- x(1)
  j <- j$logs
  mess <- purrr::map_chr(j, ~{.x$message})
  expect_identical(mess[2],
                   crayon::strip_style(
                     paste0("Pushed '(' to stack on line 2, char 25\nstack size ",
                            "is now 2\n\033[36mof not starting with ",
                            "cat\033[39m\033[31m(\033[39m\033[36m)')\033[39m\n\n")))
  expect_identical(mess[3],
                   crayon::strip_style(
                     paste0("Matched ')' on line 2, char 26\nstack size is now ",
                            "1\n\033[36mof not starting with ",
                            "cat(\033[39m\033[31m)\033[39m\033[36m')\033[39m\n\n")))
  expect_identical(mess[4],
                   crayon::strip_style(
                     paste0("Matched closing ')' for `cat()` on line 2, char ",
                            "28\nstack size is now 0\n\033[36mof not starting with ",
                            "cat()'\033[39m\033[31m)\033[39m\033[36m\033[39m\n\n")))

  # -----------------------------------------------------------------------------
  # `verbose` error
  strs <- c("cat('This is an )')")
  x <- csasdown:::capture_log(csasdown:::parse_cat_text(strs, verbose = TRUE))
  j <- x(1)
  j <- j$logs
  mess <- purrr::map_chr(j, ~{.x$message})
  expect_identical(mess[1],
                   "Pushed '(' from `cat()` to stack. Stack size is now 1\n\n")
  expect_identical(mess[2],
                   crayon::strip_style(
                     paste0("No matching '(' for this ')' on line  1, char ",
                            "13\n\033[36m'This is an \033[39m\033[31m)\033[39m\033[",
                            "36m')\033[39m\n")))

  x <- csasdown:::capture_log(csasdown:::parse_cat_text(strs, verbose = TRUE))
  j <- x(1)
  j <- j$logs
  mess <- purrr::map_chr(j, ~{.x$message})
  types <- purrr::map_chr(j, ~{.x$type})
  expect_identical(types[1], "message")
  expect_identical(mess[1],
                   paste0("Pushed '(' from `cat()` to stack. Stack size is ",
                          "now 1\n\n"))
  expect_identical(types[2], "error")
  expect_identical(mess[2],
                   crayon::strip_style(
                     paste0("No matching '(' for this ')' on line  1, char ",
                            "13\n\033[36m'This is an \033[39m\033[31m)\033[",
                            "39m\033[36m')\033[39m\n")))

  # -----------------------------------------------------------------------------
  # Single quotes
  strs <- c("cat('This is an example",
            "of matching cat parens",
            "')")
  strs_mod <- c("'This is an example",
                "of matching cat parens",
                "'")
  out <- csasdown:::parse_cat_text(strs)
  expect_identical(out, strs_mod)

  # -----------------------------------------------------------------------------
  # Double quotes
  strs <- c('cat("This is an example',
            'of matching cat parens',
            '")')
  strs_mod <- c('"This is an example',
                'of matching cat parens',
                '"')
  out <- csasdown:::parse_cat_text(strs)
  expect_identical(out, strs_mod)

  # -----------------------------------------------------------------------------
  # Correct complex paren matching
  strs <- c('cat("This is (an) example',
            'of matching (cat) parens()',
            '")')
  strs_mod <- c('"This is (an) example',
                'of matching (cat) parens()',
                '"')
  out <- csasdown:::parse_cat_text(strs)
  expect_identical(out, strs_mod)

  # -----------------------------------------------------------------------------
  # Correct complex paren matching
  strs <- c('cat("This is an example',
            'of matching cat(((()))) parens()',
            '()()")')
  strs_mod <- c('"This is an example',
                'of matching cat(((()))) parens()',
                '()()"')
  out <- csasdown:::parse_cat_text(strs)
  expect_identical(out, strs_mod)

  # -----------------------------------------------------------------------------
  # Catch extra trailing characters after ')'
  strs <- c('cat("This is an example',
            'of matching cat parens',
            '") extra')
  expect_error(csasdown:::parse_cat_text(strs))

  # -----------------------------------------------------------------------------
  # Incorrect paren matching
  strs <- c('cat("This is an )example',
            'of matching cat parens',
            '")')
  expect_error(csasdown:::parse_cat_text(strs))

  # -----------------------------------------------------------------------------
  # Incorrect paren matching
  strs <- c('cat("(This is an example',
            'of matching cat parens',
            '")')
  expect_error(csasdown:::parse_cat_text(strs))

  # -----------------------------------------------------------------------------
  # Incorrect paren matching
  strs <- c('cat("(This( is an example',
            'of matching cat))) parens',
            '")')
  expect_error(csasdown:::parse_cat_text(strs))

  # -----------------------------------------------------------------------------
  # Incorrect paren matching
  strs <- c('cat("This is an example',
            'of ((())))matching cat parens',
            '")')
  expect_error(csasdown:::parse_cat_text(strs))

  # -----------------------------------------------------------------------------
  # Incorrect paren matching
  strs <- c('cat("This is an example',
            'of matching cat parens',
            ')")')
  expect_error(csasdown:::parse_cat_text(strs))

  # -----------------------------------------------------------------------------
  # Incorrect paren matching
  strs <- c('cat("(This is an example',
            'of (matching( cat) parens)))',
            '")')
  expect_error(csasdown:::parse_cat_text(strs))

  # -----------------------------------------------------------------------------
  # Incorrect paren matching
  strs <- c('cat("(This) is an example',
            'of matching (cat) (parens',
            '")')
  expect_error(csasdown:::parse_cat_text(strs))

  # -----------------------------------------------------------------------------
  # Longer than needed text, multiple lines (ok)
  strs <- c('cat("This is an example',
            'of longer than needed text',
            '")',
            'to find the correct text')
  strs_mod <- c('"This is an example',
                'of longer than needed text',
                '"')
  expect_identical(csasdown:::parse_cat_text(strs), strs_mod)

  # -----------------------------------------------------------------------------
  # Longer than needed text, same lines
  strs <- c('cat("This is an example',
            'of longer than needed text',
            '") to find the correct text')
  expect_error(csasdown:::parse_cat_text(strs))

})
