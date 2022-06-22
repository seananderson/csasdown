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
               paste0("Non-existent quote type for `cat()` text. You must use ",
                      "either single or double quotes. You used `"),
               fixed = TRUE)

  # -----------------------------------------------------------------------------
  # `verbose`
  capture_log1 <- function(f) {
    # Capture all messages from a function
    function(...) {
      logs <- list()
      add_log <- function(type, message) {
        new_l <- logs
        new_log <- list(timestamp = format(Sys.time(), tz = 'UTC', format = '%Y-%m-%d %H:%M:%S'),
                        type = type,
                        message =  message)
        new_l[[length(new_l) + 1]]  <- new_log
        logs <<- new_l
      }
      res <- withCallingHandlers(
        tryCatch(f(...), error=function(e) {
          add_log("error", conditionMessage(e))
          NULL
        }), warning=function(w) {
          add_log("warning", conditionMessage(w))
          invokeRestart("muffleWarning")
        }, message = function(m) {
          add_log("message", conditionMessage(m))
          invokeRestart("muffleMessage")
        })
      list(res, logs = logs)
    }
  }
  strs <- c("cat('This is an example",
            "of not starting with cat()')")
  x <- capture_log1(csasdown:::parse_cat_text(strs, verbose = TRUE))
  j <- x(1)
  j <- j$logs
  mess <- map_chr(j, ~{.x$message})
  expect_identical(mess[1],
          "Pushed '(' from `cat()` to stack. Stack size is now 1\n\n")
  expect_identical(mess[2],
                 paste0("Pushed '(' to stack on line 2, char 25\nstack size ",
                 "is now 2\n\033[36mof not starting with ",
                 "cat\033[39m\033[31m(\033[39m\033[36m)')\033[39m\n\n"))
  expect_identical(mess[3],
                 paste0("Matched ')' on line 2, char 26\nstack size is now ",
                 "1\n\033[36mof not starting with ",
                 "cat(\033[39m\033[31m)\033[39m\033[36m')\033[39m\n\n"))
  expect_identical(mess[4],
                   paste0("Matched closing ')' for `cat()` on line 2, char ",
                   "28\nstack size is now 0\n\033[36mof not starting with ",
                   "cat()'\033[39m\033[31m)\033[39m\033[36m\033[39m\n\n"))

  # -----------------------------------------------------------------------------
  # `verbose` error
  strs <- c("cat('This is an )')")
  x <- capture_log1(csasdown:::parse_cat_text(strs, verbose = TRUE))
  j <- x(1)
  j <- j$logs
  mess <- map_chr(j, ~{.x$message})
  expect_identical(mess[1],
                   "Pushed '(' from `cat()` to stack. Stack size is now 1\n\n")
  expect_identical(mess[2],
                   paste0("Mismatched ')' found on line  1, char 13\n\033[36m'",
                   "This is an \033[39m\033[31m)\033[39m\033[36m')\033[39m\n"))
  suppressMessages(
    expect_error(csasdown:::parse_cat_text(strs, verbose = TRUE),
                 "Mismatched ')' found on line  1, char 13",
                 fixed = TRUE)
  )

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
