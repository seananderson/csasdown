test_that("csasdown::convert_newlines_rmd works", {
  str <- c("")
  str_mod <- c("\\n", "")
  expect_identical(csasdown::convert_newlines_rmd(str), str_mod)


  str <- c("Hello world",
           "",
           "",
           "Welcome!")
  str_mod <- c("Hello world",
               "\\n",
               "\\\\",
               "",
               "Welcome!")
  expect_identical(csasdown::convert_newlines_rmd(str), str_mod)

  str <- c("#Section header",
           "",
           "Lorem ipsum dolor sit amet. Est magnam tenetur ut internos ",
           "internos aut similique vitae. In sapiente molestias ut aperiam ",
           "autem ut galisum dignissimos.",
           "",
           "",
           "",
           "",
           "#Section header 2")
  str_mod <- c("#Section header",
               "\\n",
               "",
               "Lorem ipsum dolor sit amet. Est magnam tenetur ut internos ",
               "internos aut similique vitae. In sapiente molestias ut aperiam ",
               "autem ut galisum dignissimos.",
               "\\n",
               "\\\\",
               "\\\\",
               "\\\\",
               "",
               "#Section header 2")
  expect_identical(csasdown::convert_newlines_rmd(str), str_mod)

  str <- c("",
           "",
           "",
           "Lorem ipsum dolor sit amet",
           "",
           "")
  str_mod <- c("\\n",
               "\\\\",
               "\\\\",
               "",
               "Lorem ipsum dolor sit amet",
               "\\n",
               "\\\\",
               "")
  expect_identical(csasdown::convert_newlines_rmd(str), str_mod)
})
