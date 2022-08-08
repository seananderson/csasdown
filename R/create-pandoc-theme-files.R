# nocov start

#' Creates package JSON and LaTeX files for pandoc highlight theme types
#'
#' @keywords internal
#' @family highlight-theme
#'
#' @description
#' Creates package JSON and LATEX files for pandoc highlight theme types.
#' Runs command line pandoc to extract the JSON code, then a modified function
#' to change the format to LaTeX
#'
#' @details
#' This is not testable on GitHub actions, so do not write tests for this
#' function
#'
#' @param pandoc_dir A directory name where pandoc command line is available.
#' If `NULL`, the environment variable `RSTUDIO_PANDOC` will be used
create_pandoc_theme_files <- function(pandoc_dir = NULL){

  # The valid highlight themes for Pandoc
  all_themes = c("pygments", "tango", "espresso", "zenburn",
                 "kate", "monochrome", "breezedark", "haddock")

  if(is.null(pandoc_dir)){
    pandoc_dir <- Sys.getenv("RSTUDIO_PANDOC")
    if(pandoc_dir == ""){
      bail("Cannot find pandoc on your system. The environment variable ",
           csas_color("RSTUDIO_PANDOC"), " is not set and you didn't ",
           "provide a directory name in the ", csas_color("pandoc_dir"),
           " argument.")
    }
  }
  # In case you're on Windows. Won't affect anything if you're not
  pandoc_dir <- gsub("Program Files", "Progra~1", pandoc_dir)
  if(!dir.exists(pandoc_dir)){
    bail("The directory ", fn_color(pandoc_dir), " does not exist")
  }
  json_lst <- map(all_themes, ~{
    cmd <- paste(file.path(pandoc_dir, "pandoc"),  "--print-highlight-style", .x)
    notify("Running pandoc system command:\n", csas_color(cmd), "\n")
    system(cmd, intern = TRUE)
  })

  latex_lst <- gen_latex_highlight_code(json_lst)

  # Save the files
  if(length(json_lst) != length(latex_lst)){
    bail("The length of the JSON list (", length(json_lst), ") and the ",
         "length of the latex list (", length(latex_lst), ") are not the ",
         "same. They should both be the same as the number of themes (",
         length(all_themes), ")")
  }
  theme_dir <- here::here("inst", "themes")
  imap(all_themes, ~{
    json_fn <- file.path(theme_dir, paste0(.x, ".json"))
    latex_fn <- file.path(theme_dir, paste0(.x, ".latex"))
    writeLines(latex_lst[[.y]], latex_fn)
    writeLines(json_lst[[.y]], json_fn)
    notify("Created JSON file ", fn_color(json_fn))
    notify("Created LaTeX file ", fn_color(latex_fn), "\n")
  })
  invisible()
}

# nocov end
