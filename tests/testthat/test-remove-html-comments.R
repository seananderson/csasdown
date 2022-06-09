test_that("remove_html_comments() works", {

  j <- NULL
  ret <- remove_html_comments(j)
  expect_null(ret)
  # ---------------------------------------------------------------------------

  j <- c("<!---->")
  ret <- remove_html_comments(j)
  expect_identical(ret, character(0))
  # ---------------------------------------------------------------------------

  j <- c("<!-- Anything  -->")
  ret <- remove_html_comments(j)
  expect_identical(ret, character(0))
  # ---------------------------------------------------------------------------

  j <- c("Hello <!-- Anything  -->world!")
  ret <- remove_html_comments(j)
  expect_identical(ret, "Hello world!")
  # ---------------------------------------------------------------------------

  j <- c("Hello <!-- Anything  -->world!<!-- csasdown is great       -->!")
  ret <- remove_html_comments(j)
  expect_identical(ret, "Hello world!!")
  # ---------------------------------------------------------------------------

  j <- c("Hi <!--ello--><!-- Anything  -->world!<!-- csasdown is great       -->!")
  ret <- remove_html_comments(j)
  expect_identical(ret, "Hi world!!")
  # ---------------------------------------------------------------------------

  j <- c("Multiple <!-- Anything  ", "Lines -->")
  ret <- remove_html_comments(j)
  expect_identical(ret, "Multiple ")
  # ---------------------------------------------------------------------------

  j <- c("<!-- Anything  ", "-->Lines", "<!--", "xxx", "--> Hello!")
  ret <- remove_html_comments(j)
  expect_identical(ret, c("Lines", " Hello!"))
  # ---------------------------------------------------------------------------

  j <- c("<!-- Anything  ", "-->Lines", "<!--", "xxx", "--> Hello!<!--",
         "XX", "--> World!")
  ret <- remove_html_comments(j)
  expect_identical(ret, c("Lines", " Hello!", " World!"))
  # ---------------------------------------------------------------------------

  j <- c("<!-- A -->",
         "<-- B ->",
         "<!-- C -->",
         "<!-- D -->",
         "E",
         "<!- F ->",
         "<!-- G -->")
  ret <- remove_html_comments(j)
  expect_identical(ret, c("<-- B ->", "E", "<!- F ->"))
  # ---------------------------------------------------------------------------

  j <- c("<!-- A ",
         "<-- B ->",
         " C ->",
         "<-- D >",
         "E",
         " F ->",
         "< G -->")
  ret <- remove_html_comments(j)
  expect_identical(ret, character(0))
  # ---------------------------------------------------------------------------
})
