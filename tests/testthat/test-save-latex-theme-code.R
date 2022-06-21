test_that("save_latex_theme_code() works", {

  # On GitHub Actions..
  gha_dir <- Sys.getenv("RUNNER_TEMP")
  on_gha <-  gha_dir == ""
  if(on_gha){
    expect_message(csasdown:::save_latex_theme_code("kate", gha_dir),
                   "Created theme file ")

    expect_true(file.exists(file.path(gha_dir, "kate.latex")))

    suppressMessages(csasdown:::save_latex_theme_code(theme_path = gha_dir))
    expect_true(file.exists(file.path(gha_dir, "breezedark.latex")))
    expect_true(file.exists(file.path(gha_dir, "espresso.latex")))
    expect_true(file.exists(file.path(gha_dir, "haddock.latex")))
    expect_true(file.exists(file.path(gha_dir, "kate.latex")))
    expect_true(file.exists(file.path(gha_dir, "monochrome.latex")))
    expect_true(file.exists(file.path(gha_dir, "pygments.latex")))
    expect_true(file.exists(file.path(gha_dir, "tango.latex")))
    expect_true(file.exists(file.path(gha_dir, "zenburn.latex")))

  }else{
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
  }

})
