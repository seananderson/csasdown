test_that("save_latex_theme_code() works", {

  expect_message(csasdown:::save_latex_theme_code("kate"),
                 "Created theme file ")

  expect_true(file.exists(here::here("inst/themes/kate.latex")))

  suppressMessages(csasdown:::save_latex_theme_code())
  expect_true(file.exists(here::here("inst/themes/breezedark.latex")))
  expect_true(file.exists(here::here("inst/themes/espresso.latex")))
  expect_true(file.exists(here::here("inst/themes/haddock.latex")))
  expect_true(file.exists(here::here("inst/themes/kate.latex")))
  expect_true(file.exists(here::here("inst/themes/monochrome.latex")))
  expect_true(file.exists(here::here("inst/themes/pygments.latex")))
  expect_true(file.exists(here::here("inst/themes/tango.latex")))
  expect_true(file.exists(here::here("inst/themes/zenburn.latex")))

  k <- file.info(here::here("inst/themes/zenburn.latex"))
  expect_identical(lubridate::day(lubridate::today()), lubridate::day(k$mtime))
  expect_identical(lubridate::hour(k$mtime), lubridate::hour(lubridate::now(tzone = "UTC")))


  TODO
})
