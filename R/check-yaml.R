#' Check to make sure `index.Rmd` contains all current YAML options
#'
#' @description
#' As the csasdown package is updated, sometimes new mandatory YAML options are added
#' to the `index.Rmd` file. Running this function will compare your file to the
#' version built into the currently installed version of csasdown and issue
#' am error message telling you what doesn't match if needed.
#'
#' @param type Type of document
#' @param verbose Logical. If `TRUE`, print messages
#'
#' @importFrom rmarkdown yaml_front_matter
#' @export
check_yaml <- function(type = c("resdoc", "resdoc_pdf", "resdoc_word",
                                "sr", "sr_pdf", "sr_word",
                                "techreport", "techreport_pdf",
                                "techreport_word"),
                       verbose = FALSE) {

  if(verbose){
    notify("Checking that YAML options are all present for document type ",
           csas_color(type), " ...")
  }

  type <- match.arg(type)
  if(type %in% c("resdoc", "resdoc_pdf", "resdoc_word")){
    type <- "resdoc"
  }else if(type %in% c("sr", "sr_pdf", "sr_word")){
    type <- "sr"
  }else if(type %in% c("techreport", "techreport_pdf", "techreport_word")){
    type <- "techreport"
  }

  x_skeleton <- names(yaml_front_matter(
    system.file("rmarkdown", "templates", type, "skeleton", "skeleton.Rmd",
                package = "csasdown"
    )
  ))
  x_index <- names(yaml_front_matter("index.Rmd"))
  .diff <- setdiff(x_skeleton, x_index)
  if (length(.diff) > 0L) {
    bail("Your ", fn_color("index.Rmd"), " file is missing the YAML ",
         "tag(s):\n\n", tag_color(paste(.diff, collapse = "\n")))
  } else {
    if(verbose){
      check_notify("Your ", fn_color("index.Rmd"), " file contains all ",
                   "necessary YAML options\n")
    }
  }
}

