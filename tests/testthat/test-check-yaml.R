test_that("check_yaml() works", {
  testing_path <- file.path(tempdir(), "sr-check-yaml")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(csasdown::draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  # ---------------------------------------------------------------------------
  expect_message(csasdown::check_yaml("sr"),
                 "Your \\S+ file contains all necessary YAML options")

  # ---------------------------------------------------------------------------
  file.copy("index.Rmd", "x.Rmd")
  rmd <- readLines("index.Rmd")
  ind <- grep("french_region:", rmd)
  rmd <- rmd[-ind]
  writeLines(rmd, "index.Rmd")
  suppressMessages(
    expect_error(csasdown::check_yaml("sr"),
                 "Your \\S+ file is missing the YAML tag")
  )

  # ---------------------------------------------------------------------------
  file.copy("x.Rmd", "index.Rmd", overwrite = TRUE)
  suppressMessages(
    expect_error(csasdown::check_yaml("resdoc"),
                 paste0("Your \\S+ file is missing the YAML tag\\(s\\)"))
  )

  # ---------------------------------------------------------------------------
  suppressMessages(
    expect_error(csasdown::check_yaml("techreport"),
                 paste0("Your \\S+ file is missing the YAML tag\\(s\\):"))
  )

})
