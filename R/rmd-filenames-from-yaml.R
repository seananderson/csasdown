#' Fetch vector of the Rmd filenames found in a bookdown YAML file
#'
#' @description
#' Return a vector of the names of all Rmd files found in the file `yaml_fn`.
#' It is required that all filenames appear on their own line in the file.
#' Commented-out filenames will not be included.
#'
#' @details
#' The file must have an entry like this:
#'
#' `` rmd_files: ["index.Rmd", ``
#' ``             "01-chap1.Rmd"] ``
#' ``            #"02-chap2.Rmd", ``
#' ``            #"03-chap3.Rmd", ``
#' ``            #"04-references.Rmd", ``
#' ``            #"05-appendix.Rmd"] ``
#'
#' In this case, the vector returned will be:
#'
#' c("index.Rmd", "01-chap1.Rmd")
#'
#' @param yaml_fn The YAML file name
#'
#' @return A vector of the Rmd file names
rmd_filenames_from_yaml <- function(yaml_fn = "_bookdown.yml"){

  if(!file.exists(yaml_fn)){
    stop("The YAML file ", yaml_fn, " does not exist", call. = FALSE)
  }
  yaml <- readLines(yaml_fn)
  if(!length(yaml)){
    stop("The YAML file ", yaml_fn, " is empty", call. = FALSE)
  }
  # Remove surrounding whitespace
  yaml <- trimws(yaml)
  # Remove commented lines and extra surrounding quotes
  yaml <- noquote(yaml[!grepl("^#", yaml)])
  # Select only vector items containing 'Rmd'
  fns <- yaml[grepl("Rmd", yaml)]
  # Filter out the filenames
  gsub(".*?([a-zA-Z0-9_\\-]+\\.Rmd).*$", "\\1", fns)
}
