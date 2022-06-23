#' @export
#' @rdname csas_pdf
sr_pdf <- function(latex_engine = "pdflatex",
                   prepub = FALSE,
                   copy_sty = TRUE,
                   line_nums = FALSE,
                   line_nums_mod = 1,
                   draft_watermark = FALSE,
                   highlight = "tango",
                   pandoc_args = c("--top-level-division=chapter",
                                   "--wrap=none",
                                   "--default-image-extension=png"),
                   ...) {

  themes <- c("pygments", "tango", "espresso",
              "zenburn", "kate", "monochrome",
              "breezedark", "haddock")

  if(is.null(highlight)){
    highlight = "monochrome"
  }

  if((!highlight %in% themes) && !file.exists(here(highlight))){
    stop("in YAML, `csasdown:sr_pdf: highlight` must be one of ",
         paste(themes, collapse = ", "),
         "\nor a filename for a custom latex theme file.",
         "\nSee pandoc documentation, --highlight-style argument.",
         call. = FALSE)
  }

  if (fr()) {
    file <- system.file("csas-tex", "sr-french.tex", package = "csasdown")
  } else {
    file <- system.file("csas-tex", "sr.tex", package = "csasdown")
  }

  base <- pdf_book(
    template = file,
    keep_tex = TRUE,
    pandoc_args = pandoc_args,
    latex_engine = latex_engine,
    ...
  )
  tmp_hl <- grep("--highlight-style", base$pandoc$args)
  base$pandoc$args <- base$pandoc$args[-c(tmp_hl[1], tmp_hl[1] + 1)]

  if (!class(line_nums_mod) %in% c("integer", "numeric")) {
    stop("line_nums_mod must be a numeric or integer value.",
         call. = FALSE)
  }

  update_csasstyle(
    copy = copy_sty,
    line_nums = line_nums,
    line_nums_mod = line_nums_mod,
    draft_watermark = draft_watermark,
    which_sty = ifelse(fr(), "sr-french.sty", "sr.sty")
  )

  base$knitr$opts_chunk$comment <- NA
  old_opt <- getOption("bookdown.post.latex")

  options(bookdown.post.latex = function(x) {
    fix_envs(
      x = x,
      prepub = prepub,
      highlight = highlight,
      include_abstract = FALSE
    )
  })

  on.exit(options(bookdown.post.late = old_opt))
  base
}
