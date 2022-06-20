test_that("csasdown:::remove_html_comments() works", {

  j <- NULL
  ret <- csasdown:::remove_html_comments(j)
  expect_null(ret)
  # ---------------------------------------------------------------------------

  j <- c("<!---->")
  ret <- csasdown:::remove_html_comments(j)
  expect_identical(ret, NA_character_)
  # ---------------------------------------------------------------------------

  j <- c("<!---------------------------->")
  ret <- csasdown:::remove_html_comments(j)
  expect_identical(ret, NA_character_)
  # ---------------------------------------------------------------------------

  j <- c("<!-- Anything  -->")
  ret <- csasdown:::remove_html_comments(j)
  expect_identical(ret, NA_character_)
  # ---------------------------------------------------------------------------

  j <- c("Hello <!-- Anything  -->world!")
  ret <- csasdown:::remove_html_comments(j)
  expect_identical(ret, "Hello world!")
  # ---------------------------------------------------------------------------

  j <- c("Hello <!-- Anything  -->world!<!-- csasdown is great       -->!")
  ret <- csasdown:::remove_html_comments(j)
  expect_identical(ret, "Hello world!!")
  # ---------------------------------------------------------------------------

  j <- c("Hi <!--ello--><!-- Anything  -->world!<!-- csasdown is great   -->!")
  ret <- csasdown:::remove_html_comments(j)
  expect_identical(ret, "Hi world!!")
  # ---------------------------------------------------------------------------

  j <- c("Multiple <!-- Anything  ", "Lines -->")
  ret <- csasdown:::remove_html_comments(j)
  expect_identical(ret, c("Multiple ", NA_character_))
  # ---------------------------------------------------------------------------

  j <- c("<!-- Anything  ", "-->Lines", "<!--", "xxx", "--> Hello!")
  ret <- csasdown:::remove_html_comments(j)
  expect_identical(ret, c(NA_character_, "Lines", NA_character_,
                          NA_character_, " Hello!"))
  # ---------------------------------------------------------------------------

  j <- c("<!-- Anything  ", "-->Lines", "<!--", "xxx", "--> Hello!<!--",
         "XX", "--> World!")
  ret <- csasdown:::remove_html_comments(j)
  expect_identical(ret, c(NA_character_, "Lines", NA_character_, NA_character_,
                          " Hello!", NA_character_, " World!"))
  # ---------------------------------------------------------------------------

  j <- c("<!-- A -->",
         "<-- B ->",
         "<!-- C -->",
         "<!-- D -->",
         "E",
         "<!- F ->",
         "<!-- G -->")
  ret <- csasdown:::remove_html_comments(j)
  expect_identical(ret, c(NA_character_, "<-- B ->", NA_character_,
                          NA_character_, "E", "<!- F ->", NA_character_))
  # ---------------------------------------------------------------------------

  j <- c("<!-- A ",
         "<-- B ->",
         " C ->",
         "<-- D >",
         "E",
         " F ->",
         "< G -->")
  ret <- csasdown:::remove_html_comments(j)
  expect_identical(ret, rep(NA_character_, 7))
  # ---------------------------------------------------------------------------

  j <- c("<!--Hello <!-- ", "Anything --> -->")
  ret <- csasdown:::remove_html_comments(j)
  expect_identical(ret, rep(NA_character_, 2))
  # ---------------------------------------------------------------------------

  j <- c("Hello <!-- Anything  ")
  expect_error(csasdown:::remove_html_comments(j))
  # ---------------------------------------------------------------------------

  j <- c("Hello --> Anything  ")
  expect_error(csasdown:::remove_html_comments(j))
  # ---------------------------------------------------------------------------

  j <- c("Hello --> Anything  <!--")
  expect_error(csasdown:::remove_html_comments(j))
  # ---------------------------------------------------------------------------

})
