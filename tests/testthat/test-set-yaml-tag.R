test_that("set_yaml_tag() works", {
  testing_path <- file.path(tempdir(), "test-set-yaml-tags")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(csasdown::draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))

  # ---------------------------------------------------------------------------
  # Test NULL filename
  expect_error(csasdown::set_yaml_tag("french:", "true", NULL),
               "Filename \\(`fn`\\) cannot be `NULL`")

  # ---------------------------------------------------------------------------
  # Test NULL tag
  expect_error(csasdown::set_yaml_tag(NULL, "true"),
               "YAML tag \\(`tag`\\) cannot be `NULL`")

  # ---------------------------------------------------------------------------
  # Test NULL tag value
  expect_error(csasdown::set_yaml_tag("french:", NULL),
               "YAML tag value \\(`val`\\) cannot be `NULL`")

  # ---------------------------------------------------------------------------
  # Test Empty file
  rmd <- character(0)
  writeLines(rmd, "empty.Rmd")
  expect_error(csasdown::set_yaml_tag("french:", "tru", "empty.Rmd"),
               "File 'empty.Rmd' does not contain anything")

  # ---------------------------------------------------------------------------
  # Test unbalanced YAML file
  rmd <- readLines("index.Rmd")
  rmd <- c(rmd, "", "---", "tag1: junk", "")
  writeLines(rmd, "unbalanced.Rmd")
  expect_error(csasdown::set_yaml_tag("french:", "tru", "unbalanced.Rmd"),
               paste0("There are uneven sets of '---' lines meaning an ",
                      "unending YAML block in file 'unbalanced.Rmd'"))

  # ---------------------------------------------------------------------------
  # Test no YAML blocks
  rmd <- readLines("index.Rmd")
  inds <- grep("---", rmd)
  rmd <- rmd[-inds]
  writeLines(rmd, "index.Rmd")
  expect_warning(csasdown::set_yaml_tag("french:", "tru"),
                 paste0("There were no YAML blocks found in the file. ",
                        "Nothing was changed"))

  # ---------------------------------------------------------------------------
  # Test non existent
  testing_path <- file.path(tempdir(), "test-set-yaml-tags-2")
  unlink(testing_path, recursive = TRUE, force = TRUE)
  dir.create(testing_path, showWarnings = FALSE)
  setwd(testing_path)
  suppressMessages(csasdown::draft(
    system.file("rmarkdown", "templates", "sr", package = "csasdown"),
    create_dir = FALSE,
    edit = FALSE
  ))
  expect_error(csasdown::set_yaml_tag("prepub", "true", "no.Rmd"),
               "File 'no.Rmd' does not exist")

  # ---------------------------------------------------------------------------
  # Test trying to change csasdown::
  expect_error(csasdown::set_yaml_tag("csasdown::", "true"),
               paste0("The `output:` and `csasdown::<doc_type>` tags cannot ",
                      "be changed with this function."))

  # ---------------------------------------------------------------------------
  # Test trying to change knit:
  expect_error(csasdown::set_yaml_tag("knit", "true"),
               paste0("The `knit:` tag cannot be modified"))

  # ---------------------------------------------------------------------------
  # Test non existent
  expect_error(csasdown::set_yaml_tag("nonexistent", "true"),
               paste0("The YAML tag '\\^\\\\s\\*nonexistent' was not found in ",
                      "the file 'index.Rmd'"))

  # ---------------------------------------------------------------------------
  # Test trying tag names not in a YAML block
  rmd <- readLines("index.Rmd")
  rmd[64] <- "fake_tag: true"
  writeLines(rmd, "index.Rmd")
  expect_error(csasdown::set_yaml_tag("fake_chunk:", "false"),
               paste0("The YAML tag '\\^\\\\s\\*fake_chunk:' was not found ",
                      "in the file 'index.Rmd'"))

  # ---------------------------------------------------------------------------
  # Test tag names with and without leading spaces
  csasdown::set_yaml_tag("title: \"Insert", "New title")
  csasdown::set_yaml_tag("approver", c("Bob Ross", "Regional Director"))
  rmd <- readLines("index.Rmd")
  expect_identical(rmd[2], "title: New title")
  expect_identical(rmd[9:11], c("approver: |", "  Bob Ross\\ ", "  Regional Director"))

  # ---------------------------------------------------------------------------
  # Test tag name at end of YAML block
  csasdown::set_yaml_tag("lof", "true")

  # ---------------------------------------------------------------------------
  # Add more chunks and try
  rmd <- readLines("index.Rmd")
  rmd <- c(rmd, "", "", "---", "", "title: true",
           "another_newtag: Hello World!", "---", "",
           "---", "block3_tag1: Help", "block3_tag2: Help", "", "---", "", "")
  writeLines(rmd, "index.Rmd")
  csasdown::set_yaml_tag("block3_tag2", "It's all good")
  csasdown::set_yaml_tag("block3_tag1", "First tag of block")
  expect_error(csasdown::set_yaml_tag("title", "X"),
               paste0("The YAML tag '\\^\\\\s\\*title' was found more than ",
                      "once in the file 'index.Rmd'"))

})
