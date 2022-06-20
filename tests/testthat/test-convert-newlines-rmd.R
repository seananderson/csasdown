test_that("csasdown:::convert_newlines_rmd works", {

  str <- c("")
  str_mod <- c("", "\\\\ \\\\", "")
  ret <- csasdown:::convert_newlines_rmd(str)
  expect_identical(ret, str_mod)

  str <- c("Hello world",
           "",
           "",
           "Welcome!")
  str_mod <- c("Hello world",
               "",
               "\\\\",
               "",
               "Welcome!")
  ret <- csasdown:::convert_newlines_rmd(str)
  expect_identical(ret, str_mod)

  # ---------------------------------------------------------------------------
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
               "",
               "\\\\ \\\\",
               "",
               "Lorem ipsum dolor sit amet. Est magnam tenetur ut internos ",
               "\\\\","",
               "internos aut similique vitae. In sapiente molestias ut aperiam ",
               "\\\\","",
               "autem ut galisum dignissimos.",
               "",
               "\\\\",
               "\\\\",
               "\\\\",
               "",
               "#Section header 2")
  ret <- csasdown:::convert_newlines_rmd(str)
  expect_identical(ret, str_mod)

  # ---------------------------------------------------------------------------
  str <- c("",
           "",
           "",
           "Lorem ipsum dolor sit amet",
           "",
           "")
  str_mod <- c("",
               "\\\\",
               "\\\\",
               "",
               "Lorem ipsum dolor sit amet",
               "",
               "\\\\",
               "")
  ret <- csasdown:::convert_newlines_rmd(str)
  expect_identical(ret, str_mod)

  # ---------------------------------------------------------------------------
  str <- c("xx",
           "",
           "",
           "Lorem ipsum dolor sit amet",
           "",
           "",
           "yy")
  str_mod <- c("xx",
               "",
               "\\\\",
               "",
               "Lorem ipsum dolor sit amet",
               "",
               "\\\\",
               "",
               "yy")
  ret <- csasdown:::convert_newlines_rmd(str)
  expect_identical(ret, str_mod)

  # ---------------------------------------------------------------------------
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
  str_mod <- c("",
               "\\\\ \\\\",
               "",
               "Creating a list",
               "",
               "1. Item 1",
               "2. Item 2",
               "   a. Item 2a",
               "   b. Item 2b",
               "      i. Item 2bi",
               "      ii. Item 2bii",
               "",
               "3. Item 3.",
               "",
               "\\\\ \\\\",
               "",
               "End of list")
  ret <- csasdown:::convert_newlines_rmd(str)
  expect_identical(ret, str_mod)

  # ---------------------------------------------------------------------------
  str <- c("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod",
           "tempor incididunt ut labore et dolore magna aliqua. Bibendum ut",
           "",
           "",
           "   parameter           value",
           "---------------- -----------------",
           "       a                 5",
           "       b                 8",
           "       c                 9",
           "",
           "----------------------------------------------------------------------------------",
           "  Factors                    Correlation between Parents & Child      Inherited",
           "------------------------- ----------------------------------------- --------------",
           "  Education                                -0.49                         Yes",
           "",
           "",
           "",
           "Socio-Economic Status                     0.28                        Slight",
           "",
           "Income                                    0.08                          No",
           "",
           "Family Size                               0.18                        Slight",
           "",
           "Occupational Prestige                     0.21                        Slight",
           "------------------------- ----------------------------------------- --------------",
           "   Table: Correlation of Inheritance Factors for Parents and Child")

  str_mod <- c("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod",
               "\\\\",
               "",
               "tempor incididunt ut labore et dolore magna aliqua. Bibendum ut",
               "",
               "\\\\",
               "",
               "   parameter           value",
               "---------------- -----------------",
               "       a                 5",
               "       b                 8",
               "       c                 9",
               "",
               "\\\\ \\\\",
               "",
               "----------------------------------------------------------------------------------",
               "  Factors                    Correlation between Parents & Child      Inherited",
               "------------------------- ----------------------------------------- --------------",
               "  Education                                -0.49                         Yes",
               "",
               "Socio-Economic Status                     0.28                        Slight",
               "",
               "Income                                    0.08                          No",
               "",
               "Family Size                               0.18                        Slight",
               "",
               "Occupational Prestige                     0.21                        Slight",
               "------------------------- ----------------------------------------- --------------",
               "   Table: Correlation of Inheritance Factors for Parents and Child")

  ret <- csasdown:::convert_newlines_rmd(str)
  expect_identical(ret, str_mod)

  # ---------------------------------------------------------------------------
  str <- c("-----",
           "  a",
           "-----",
           "  b",
           "-----",
           "",
           "other text",
           "",
           "-----",
           "  x",
           "-----",
           "  y",
           "",
           "",
           "",
           "-----",
           "Table: 2nd Caption")

  str_mod <- c("-----",
               "  a",
               "-----",
               "  b",
               "-----",
               "",
               "\\\\ \\\\",
               "",
               "other text",
               "",
               "\\\\ \\\\",
               "",
               "-----",
               "  x",
               "-----",
               "  y",
               "-----",
               "Table: 2nd Caption")
  ret <- csasdown:::convert_newlines_rmd(str)
  expect_identical(ret, str_mod)

})