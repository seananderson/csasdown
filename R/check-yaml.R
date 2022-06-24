#' Check to make sure `index.Rmd` contains all current YAML options
#'
#' @description
#' As the csasdown package is updated, sometimes new mandatory YAML options are added
#' to the `index.Rmd` file. Running this function will compare your file to the
#' version built into the currently installed version of csasdown and issue
#' am error message telling you what doesn't match if needed.
#'
#' @param type Type of document
#'
#' @importFrom rmarkdown yaml_front_matter
#' @export
check_yaml <- function(type = c("resdoc", "resdoc_pdf", "resdoc_word",
                                "sr", "sr_pdf", "sr_word",
                                "techreport", "techreport_pdf",
                                "techreport_word")) {

  type <- match.arg(type)
  if(type %in% c("resdoc", "resdoc_pdf", "resdoc_word")){
    type <- "resdoc"
  }else if(type %in% c("sr", "sr_pdf", "sr_word")){
    type <- "sr"
  }else if(type %in% c("techreport", "techreport_pdf", "techreport_word")){
    type <- "techreport"
  }

  notify("Checking that YAML options are all present for document type '",
         type, "' ...")

  x_skeleton <- names(yaml_front_matter(
    system.file("rmarkdown", "templates", type, "skeleton", "skeleton.Rmd",
                package = "csasdown"
    )
  ))
  x_index <- names(yaml_front_matter("index.Rmd"))
  .diff <- setdiff(x_skeleton, x_index)
  if (length(.diff) > 0L) {
    bail("Your `index.Rmd` file is missing: ", paste(.diff, collapse = ", "))
  } else {
    check_notify("Your `index.Rmd` file contains all necessary YAML options")
  }
}

