test_that("csasdown::convert_newlines_rmd works", {
  str <- c("")
  str_mod <- c("")
  expect_identical(csasdown::convert_newlines_rmd(str), str_mod)

  str <- c("Hello world",
           "",
           "",
           "Welcome!")
  str_mod <- c("Hello world",
               "\\\\",
               "\\\\",
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
               "\\\\",
               "Lorem ipsum dolor sit amet. Est magnam tenetur ut internos ",
               "\\\\",
               "",
               "internos aut similique vitae. In sapiente molestias ut aperiam ",
               "\\\\",
               "",
               "autem ut galisum dignissimos.",
               "\\\\",
               "\\\\",
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
  str_mod <- c("\\\\",
               "\\\\",
               "\\\\",
               "",
               "Lorem ipsum dolor sit amet",
               "\\\\",
               "\\\\",
               "\\\\",
               "",
               "")
  expect_identical(csasdown::convert_newlines_rmd(str), str_mod)

  str <- c("xx",
           "",
           "",
           "Lorem ipsum dolor sit amet",
           "",
           "",
           "yy")
  str_mod <- c("xx",
               "\\\\",
               "\\\\",
               "\\\\",
               "",
               "Lorem ipsum dolor sit amet",
               "\\\\",
               "\\\\",
               "\\\\",
               "",
               "yy")
  expect_identical(csasdown::convert_newlines_rmd(str), str_mod)

  str <- c("",
           "Creating a list",
           "1. Item 1",
           "2. Item 2",
           "   a. Item 2a",
           "   b. Item 2b",
           "      i. Item 2bi",
           "      ii. Item 2bii",
           "3. Item 3.",
           "",
           "End of list")
  str_mode <- c("Creating a list",
                "\\\\",
                "",
                "1. Item 1",
                "2. Item 2",
                "   a. Item 2a",
                "   b. Item 2b",
                "      i. Item 2bi"
                "\\\\",
                ""
                "      ii. Item 2bii",
                "\\\\",
                "",
                "3. Item 3.",
                "\\\\"
                "\\\\" )
})
