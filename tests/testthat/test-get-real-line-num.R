test_that("get_real_line_num works", {

  # ---------------------------------------------------------------------------
  expect_error(csasdown:::get_real_line_num(NULL),
               "header\\S+ cannot be \\S+NULL")

  # ---------------------------------------------------------------------------
  expect_error(csasdown:::get_real_line_num("header", NULL),
               "line_offsets\\S+ cannot be \\S+NULL")

  # ---------------------------------------------------------------------------
  expect_error(csasdown:::get_real_line_num("header", 1:4),
               "\\S+ must be a \\S+data.frame")

  # ---------------------------------------------------------------------------
  x <- tibble::tibble(fn = rep("test.Rmd", 4),
                      chunk_header = c("```{r test-1-en}",
                                       "```{r test-1-en}",
                                       "```{r test-2-en}",
                                       "```{r test-2-fr}"),
                      chunk_ind = c(1, 5, 9, 12),
                      pre_num = c(0, 3, 5, 2),
                      post_num = c(1, 1, 0, 4),
                      rmd_num = c(0, 6, 0, 17))
  expect_error(csasdown:::get_real_line_num("```{r test-0-en}", x),
               "There is no row that matches")

  # ---------------------------------------------------------------------------
  expect_error(csasdown:::get_real_line_num("```{r test-1-en}", x),
               "There is more than one row that matches")

  # ---------------------------------------------------------------------------
  actual <- csasdown:::get_real_line_num("```{r test-2-en}", x)
  expect_identical(actual, x[3, ]$chunk_ind + x[3, ]$pre_num + 1)

})
