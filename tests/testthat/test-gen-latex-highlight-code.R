test_that("gen_latex_highlight_code() works", {

  expect_error(csasdown:::gen_latex_highlight_code("nope"),
               "'arg' should be one of ")

  expect_error(csasdown:::gen_latex_highlight_code("zenburn", "badpath"),
               "The `pandoc_path` 'badpath' does not exist")

  tmp_env_var <- Sys.getenv("RSTUDIO_PANDOC")
  Sys.unsetenv("RSTUDIO_PANDOC")
  expect_error(csasdown:::gen_latex_highlight_code("zenburn"),
               "Cannot find theme path on your system.")

  Sys.setenv(RSTUDIO_PANDOC = "wrongpath")
  expect_error(csasdown:::gen_latex_highlight_code("zenburn"),
               "The `pandoc_path` 'wrongpath' does not exist")

  # ---------------------------------------------------------------------------
  Sys.setenv(RSTUDIO_PANDOC = tmp_env_var)
  # j <- csasdown:::gen_latex_highlight_code("zenburn")
  # expect_identical(j[1], paste0("\\DefineVerbatimEnvironment{Highlighting}",
  #                               "{Verbatim}{commandchars=\\\\\\{\\},formatcom=",
  #                               "\\color[rgb]{0.80,0.80,0.80}}"))
  # expect_identical(j[3], paste0("\\definecolor{shadecolor}{RGB}{48,48,48}"))
  # expect_identical(j[10], paste0("\\newcommand{\\CharTok}[1]{\\textcolor[rgb]",
  #                                "{0.86,0.64,0.64}{#1}}"))
  # expect_identical(j[20], paste0("\\newcommand{\\FloatTok}[1]{\\textcolor[rgb]",
  #                                "{0.75,0.75,0.82}{#1}}"))
  # expect_identical(j[length(j)], paste0("\\newcommand{\\WarningTok}[1]{\\text",
  #                                       "color[rgb]{0.50,0.62,0.50}{\\textit",
  #                                       "{#1}}}"))

})
