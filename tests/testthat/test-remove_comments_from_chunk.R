test_that("Removing all comments from knitr chunks works", {


  dr <- file.path(testthat::test_path(), "remove-comments-from-chunk")
  fn <- file.path(dr, "tests.Rmd")
  chunk <- c("```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('Be careful with your spacing in _Markdown_ documents.",
             "While whitespace largely is ignored, it does at",
             "times give _Markdown_ signals as to how to proceed.  ",
             "')",
             "",
             "# Test comment 3",
             "```")
  writeLines(chunk, fn)
  ret <- remove_comments_from_chunks(fn)
  actual <- readLines(fn)
  expected <- c("```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
                "cat('Be careful with your spacing in _Markdown_ documents.",
                "While whitespace largely is ignored, it does at",
                "times give _Markdown_ signals as to how to proceed.  ",
                "')",
                "",
                "```")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  chunk <- c("```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('Be careful with your spacing in _Markdown_ documents.",
             "While whitespace largely is ignored, it does at",
             "times give _Markdown_ signals as to how to proceed.  ",
             "')",
             "",
             "# Test comment 3",
             "```",
             "# BBBBBBBBBB",
             "",
             "BBBBBBBBBB",
             "")
  writeLines(chunk, fn)
  ret <- remove_comments_from_chunks(fn)
  actual <- readLines(fn)
  expected <- c("```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
                "cat('Be careful with your spacing in _Markdown_ documents.",
                "While whitespace largely is ignored, it does at",
                "times give _Markdown_ signals as to how to proceed.  ",
                "')",
                "",
                "```",
                "# BBBBBBBBBB",
                "",
                "BBBBBBBBBB",
                "")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  chunk <- c("# AAAAAAAAAA",
             "",
             "AAAAAAAAAA",
             "",
             "```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('Be careful with your spacing in _Markdown_ documents.",
             "While whitespace largely is ignored, it does at",
             "times give _Markdown_ signals as to how to proceed.  ",
             "')",
             "",
             "# Test comment 3",
             "```")
  writeLines(chunk, fn)
  ret <- remove_comments_from_chunks(fn)
  actual <- readLines(fn)
  expected <- c("# AAAAAAAAAA",
                "",
                "AAAAAAAAAA",
                "",
                "```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
                "cat('Be careful with your spacing in _Markdown_ documents.",
                "While whitespace largely is ignored, it does at",
                "times give _Markdown_ signals as to how to proceed.  ",
                "')",
                "",
                "```")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  chunk <- c("# AAAAAAAAAA",
             "",
             "AAAAAAAAAA",
             "",
             "```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('Be careful with your spacing in _Markdown_ documents.",
             "While whitespace largely is ignored, it does at",
             "times give _Markdown_ signals as to how to proceed.  ",
             "')",
             "",
             "# Test comment 3",
             "```",
             "# BBBBBBBBBB",
             "",
             "BBBBBBBBBB",
             "")
  writeLines(chunk, fn)
  ret <- remove_comments_from_chunks(fn)
  actual <- readLines(fn)
  expected <- c("# AAAAAAAAAA",
           "",
           "AAAAAAAAAA",
           "",
           "```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
           "cat('Be careful with your spacing in _Markdown_ documents.",
           "While whitespace largely is ignored, it does at",
           "times give _Markdown_ signals as to how to proceed.  ",
           "')",
           "",
           "```",
           "# BBBBBBBBBB",
           "",
           "BBBBBBBBBB",
           "")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  chunk <- c("```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('Be careful with your spacing in _Markdown_ documents.",
             "While whitespace largely is ignored, it does at",
             "times give _Markdown_ signals as to how to proceed.  ",
             "')",
             "",
             "# Test comment 3",
             "```",
             "# BBBBBBBBBB",
             "",
             "BBBBBBBBBB",
             "",
             "```{r chap01-para-7-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('More text!')",
             "",
             "# Test comment 3",
             "```")
  exp <- c("```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
           "cat('Be careful with your spacing in _Markdown_ documents.",
           "While whitespace largely is ignored, it does at",
           "times give _Markdown_ signals as to how to proceed.  ",
           "')",
           "",
           "```",
           "# BBBBBBBBBB",
           "",
           "BBBBBBBBBB",
           "",
           "```{r chap01-para-7-en, eval = !fr(), results = 'asis'}",
           "cat('More text!')",
           "",
           "```")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  chunk <- c("# AAAAAAAAAA",
             "",
             "AAAAAAAAAA",
             "",
             "```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('Be careful with your spacing in _Markdown_ documents.",
             "While whitespace largely is ignored, it does at",
             "times give _Markdown_ signals as to how to proceed.  ",
             "')",
             "",
             "# Test comment 3",
             "```",
             "# BBBBBBBBBB",
             "",
             "BBBBBBBBBB",
             "",
             "```{r chap01-para-7-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('More text!')",
             "",
             "# Test comment 3",
             "```")
  writeLines(chunk, fn)
  ret <- remove_comments_from_chunks(fn)
  actual <- readLines(fn)
  expected <- c("# AAAAAAAAAA",
           "",
           "AAAAAAAAAA",
           "",
           "```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
           "cat('Be careful with your spacing in _Markdown_ documents.",
           "While whitespace largely is ignored, it does at",
           "times give _Markdown_ signals as to how to proceed.  ",
           "')",
           "",
           "```",
           "# BBBBBBBBBB",
           "",
           "BBBBBBBBBB",
           "",
           "```{r chap01-para-7-en, eval = !fr(), results = 'asis'}",
           "cat('More text!')",
           "",
           "```")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  chunk <- c("# AAAAAAAAAA",
             "",
             "AAAAAAAAAA",
             "",
             "```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('Be careful with your spacing in _Markdown_ documents.",
             "While whitespace largely is ignored, it does at",
             "times give _Markdown_ signals as to how to proceed.  ",
             "')",
             "",
             "# Test comment 3",
             "```",
             "# BBBBBBBBBB",
             "",
             "BBBBBBBBBB",
             "",
             "```{r chap01-para-7-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('More text!')",
             "",
             "# Test comment 3",
             "```",
             "# CCCCCCCCCC",
             "",
             "CCCCCCCCCC",
             "")
  writeLines(chunk, fn)
  ret <- remove_comments_from_chunks(fn)
  actual <- readLines(fn)
  expected <- c("# AAAAAAAAAA",
           "",
           "AAAAAAAAAA",
           "",
           "```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
           "cat('Be careful with your spacing in _Markdown_ documents.",
           "While whitespace largely is ignored, it does at",
           "times give _Markdown_ signals as to how to proceed.  ",
           "')",
           "",
           "```",
           "# BBBBBBBBBB",
           "",
           "BBBBBBBBBB",
           "",
           "```{r chap01-para-7-en, eval = !fr(), results = 'asis'}",
           "cat('More text!')",
           "",
           "```",
           "# CCCCCCCCCC",
           "",
           "CCCCCCCCCC",
           "")
  expect_identical(actual, expected)

  # ---------------------------------------------------------------------------
  chunk <- c("```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('Be careful with your spacing in _Markdown_ documents.",
             "While whitespace largely is ignored, it does at",
             "times give _Markdown_ signals as to how to proceed.  ",
             "')",
             "",
             "# Test comment 3",
             "```",
             "# AAAAAAAAAA",
             "",
             "AAAAAAAAAA",
             "",
             "```{r chap01-para-7-en, eval = !fr(), results = 'asis'}",
             "# Test comment",
             "#Test Comment 2",
             "cat('More text!')",
             "",
             "# Test comment 3",
             "```")
  writeLines(chunk, fn)
  ret <- remove_comments_from_chunks(fn)
  actual <- readLines(fn)
  expected <- c("```{r chap01-para-6-en, eval = !fr(), results = 'asis'}",
           "cat('Be careful with your spacing in _Markdown_ documents.",
           "While whitespace largely is ignored, it does at",
           "times give _Markdown_ signals as to how to proceed.  ",
           "')",
           "",
           "```",
           "# AAAAAAAAAA",
           "",
           "AAAAAAAAAA",
           "",
           "```{r chap01-para-7-en, eval = !fr(), results = 'asis'}",
           "cat('More text!')",
           "",
           "```")
  expect_identical(actual, expected)

})

dr <- file.path(testthat::test_path(), "remove-comments-from-chunk")
fn <- file.path(dr, "tests.Rmd")
unlink(fn, recursive = TRUE)
