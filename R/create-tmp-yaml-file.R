#' Create a copy of a bookdown YAML file containing modified Rmd File names
#'
#' @description
#' Creates a copy of a bookdown YAML file with "tmp_" prepended to the name.
#' Inside the new file, all listed Rmd files will have their names prepended
#' with "tmp_".
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
#' @param yaml_fn The [bookdown] YAML file name, by default is "_bookdown.yml"
#'
#' @return The name of the temporary YAML file created
create_tmp_yaml_file <- function(yaml_fn = "_bookdown.yml"){

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

  # Store indices where the open square bracket is
  brac_open_ind <- grep("^rmd_files: \\[", yaml)
  if(!length(brac_open_ind)){
    stop("`rmd_files: [` not found in ", yaml_fn, ". ",
         "It must appear at the beginning of a line",
         call. = FALSE)
  }
  if(length(brac_open_ind) > 1){
    stop("More than one `rmd_files: [` found in ", yaml_fn, call. = FALSE)
  }
  if(brac_open_ind > 1){
    pre_rmd_fns <- yaml[1:(brac_open_ind - 1)]
  }else{
    pre_rmd_fns <- NULL
  }

  # Store indices where the close square bracket is
  brac_close_ind <- grep("\\]$", yaml)
  if(!length(brac_close_ind)){
    stop("`]` not found in ", yaml_fn, ". ",
         "It must appear at the end of a line",
         call. = FALSE)
  }
  # Choose first `]` index. This will be the non-greedy match
  brac_close_ind <- brac_close_ind[1]
  if(brac_close_ind < length(yaml)){
    post_rmd_fns <- yaml[(brac_close_ind + 1):length(yaml)]
  }else{
    post_rmd_fns <- NULL
  }
  # Extract all Rmd filenames
  rmd_fns <- unlist(str_extract_all(yaml, "[a-zA-Z0-9_\\-]+\\.(R|r)md"))

  if(!length(rmd_fns)){
    stop("No .Rmd filenames found in ", yaml_fn,
         call. = FALSE)
  }
  rmd_fns <- imap_chr(rmd_fns, ~{
    if(.y == 1){
      paste0('rmd_files: ["', "tmp-", .x, '",')
    }else if(.y == length(rmd_fns)){
      paste0('            "', "tmp-", .x, '"]')
    }else{
      paste0('            "', "tmp-", .x, '",')
    }
  })

  yaml <- c(pre_rmd_fns, rmd_fns, post_rmd_fns)
  tmp_fn <- paste0("tmp", yaml_fn)
  writeLines(yaml, tmp_fn)
  tmp_fn
}
