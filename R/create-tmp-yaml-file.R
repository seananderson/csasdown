#' Create a copy of a bookdown YAML file containing modified Rmd File names
#'
#' @description
#' Creates a copy of a bookdown YAML file with "tmp_" prepended to the name.
#' Inside the new file, all listed Rmd files except `book_fn` will have their
#' names prepended with "tmp_".
#' It is required that all filenames appear on their own line in the file.
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
#' This will appear in the new file as:
#'
#' `` rmd_files: ["index.Rmd", ``
#' ``             "tmp_01-chap1.Rmd"] ``
#'
#' The file must also have a line like this before the above list, which is
#' used as an index for where to insert the modified file list after:
#' book_filename: "resdoc"
#'
#' @param yaml_fn The YAML file name
#' @param book_fn The Rmd file containing the YAML code used for rendering.
#' This name will not be modified in the new YAML file
#' The default is 'index.Rmd' for [bookdown::render_book()]
#'
#' @return The name of the temporary YAML file created
create_tmp_yaml_file <- function(yaml_fn = "_bookdown.yml",
                                 book_fn = "index.Rmd"){
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
  # Filter out the filenames
  yaml <- gsub(".*?([a-zA-Z0-9_\\-]+\\.Rmd).*$", "\\1", yaml)
  # Remove book_fn as it is not modified
  yaml <- yaml[yaml != book_fn]
  rmd_inds <- grep("Rmd|rmd", yaml)
  if(length(rmd_inds)){
    yaml[rmd_inds] <- imap_chr(rmd_inds, ~{
      if(.y == length(rmd_inds)){
        paste0('"', "tmp_", yaml[.x], '"]')
      }else{
        paste0('"', "tmp_", yaml[.x], '",')
      }
    })
    yaml <- prepend(yaml, paste0("rmd_files: [", '"', "index.Rmd", '",'), before = rmd_inds[1])
  }else{
    book_fn_ind <- grep("book_filename", yaml)
    if(!length(book_fn_ind)){
      stop("No 'book_filename' field found in the YAML file ", yaml_fn,
           call. = FALSE)
    }
    yaml <- append(yaml, paste0("rmd_files: [", '"', "index.Rmd", '"]'), after = book_fn_ind)
  }
  tmp_fn <- paste0("tmp_", yaml_fn)
  writeLines(yaml, tmp_fn)
  tmp_fn
}
