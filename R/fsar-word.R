#' Creates an Microsoft Word CSAS-formatted document
#'
#' @description
#' This is a function called in output in the YAML of the `index.Rmd` file
#' to specify the creation of an FSAR word document.
#'
#' @param ... Other arguments to [bookdown::word_document2()]
#' @rdname csas_docx
#' @return A Word Document in the `.docx` format based on the FSAR template.
#' @export
fsar_word <- function(...) {
  # file <- if (fr()) "RES2021-fra-content.docx" else "RES2021-eng-content.docx"
  file <- "fsar-template.docx"
  base <- word_document2(...,
    reference_docx = system.file("csas-docx",
      file,
      package = "csasdown"
    )
  )
  base$knitr$opts_chunk$comment <- NA
  base$knitr$opts_chunk$fig.align <- "center"
  base
}

#' Render a SAR
#'
#' @param ... Arguments to pass to [bookdown::render_book()].
#'
#' @export
render_sar <- function(...) {

  cat("\n")

  # Make sure all YAML entries are present in `index.Rmd`
  render_type <- "fsar_word"
  check_yaml(type = render_type, verbose = TRUE)

  # Find out what language is set to and set the option 'french' here
  # so that it works on the first compilation in a workspace
  # It sets `options(french)` to the value in the file
  # set_language_option(index_fn, verbose)

  # Set up the console Render message
  cat("\n")

  notify(
    "Rendering the ", csas_color("FSAR"), " as a ",
    csas_color("Word"), " document in ",
    # csas_color(`if`(fr(), "French", "English")),
    " ...\n\n"
  )

  # if (verbose) {
    notify("Knitting Rmd files and running Pandoc to build the document ...")
  # }

  # if (suppress_warnings) {
  #   suppressMessages(
  #     suppressWarnings(
  #       render_book(index_fn,
  #         config_file = tmp_yaml_fn,
  #         ...
  #       )
  #     )
  #   )
  # } else {
    # suppressMessages(
      render_book("index.Rmd",
        config_file = "_bookdown.yml",
        ...
      )
    # )
  # }
  # if (verbose) {
    # fn <- file.path(
    #   "_book",
    #   paste0(
    #     gsub("^(\\S+)_\\S+$", "\\1", render_type),
    #     ".",
    #     ifelse(doc_format == "pdf", "pdf", "docx")
    #   )
    # )
    fn <- "_book/fsar.docx"
    if (file.exists(fn)) {
      check_notify(
        "Knitting and Pandoc completed, document built ",
        "successfully\n"
      )
    } else {
      # nocov start
      bail(
        "The Knitting and Pandoc procedure did not produce ",
        fn_color(fn)
      )
      # nocov end
    }
  # }

  # Rename the output files to include 'english' or 'french' so that
  # rendering different language versions does not overwrite the other
  # language version.
  # rename_output_files(index_fn, verbose)
  # if (!verbose) {
  #   notify(
  #     "For help debugging your code, render your document like this:\n",
  #     csas_color("render(verbose = TRUE, keep_files = TRUE)"), "\n\n"
  #   )
  # }

  # Delete the temporary files
  # if (!keep_files) {
  #   map(fn_process, ~ {
  #     unlink(.x, force = TRUE)
  #   })
  #   unlink(tmp_yaml_fn)
  #   unlink(index_fn)
  #   unlink("*.log")
  #   unlink("*.upa")
  # }

  check_notify("Render completed")

  notify("Adding first and last pages.")

  x <- rmarkdown::yaml_front_matter("index.Rmd")

  file <- "fsar-first-page.docx"
  doc <- officer::read_docx(system.file("csas-docx", file, package = "csasdown"))

  doc <- officer::body_replace_all_text(doc, "<<context paragraph>>", x$context)

  date_title <- paste0(x$date, " '", x$title, "'")
  doc <- officer::body_replace_all_text(doc, "<<meeting date and title>>", date_title)

  print(doc, target = "TEMP-first-page.docx")

  file <- "fsar-last-page.docx"
  doc <- officer::read_docx(system.file("csas-docx", file, package = "csasdown"))

  doc <- officer::body_replace_all_text(doc, "<<region>>", x$region)
  doc <- officer::body_replace_all_text(doc, "<<phone>>", x$phone)
  print(doc, target = "TEMP-last-page.docx")

  d <- officer::read_docx("TEMP-first-page.docx")
  d <- officer::body_add_docx(d, "_book/fsar.docx")
  d <- officer::body_add_docx(d, "TEMP-last-page.docx")

  print(d, target = "_book/fsar.docx")

  unlink("TEMP-first-page.docx")
  unlink("TEMP-last-page.docx")

  invisible()
}
