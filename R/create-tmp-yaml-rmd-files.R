#' Create a copy of a bookdown YAML file containing modified Rmd File names
#' Create a copy of the RMd files listed, with the names listed in the new
#' YAML file
#'
#' @description
#' Creates a copy of a bookdown YAML file with "tmp_" prepended to the name.
#' Inside the new file, all listed Rmd files will have their names prepended
#' with "tmp_". Those files will also be created by copying the ones listed
#' in the original YAML file.
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
#' There are only two requirements to how the files are listed in the
#' `rmd_files: [` section: 1) they must begin right after the `[`
#' and 2) the closing `]` must be at the end of its line with no more code
#' following. They look nice as listed above but could be more than one to
#' a line, or all on one line.
#'
#' @keywords internal
#'
#' @param yaml_fn The [bookdown] YAML file name, by default is "_bookdown.yml"
#'
#' @return A list of two, 1) The name of the temporary YAML file created
#' 2) A vector of the temporary Rmd files created
create_tmp_yaml_rmd_files <- function(yaml_fn = "_bookdown.yml"){

  if(!file.exists(yaml_fn)){
    bail("The YAML file ", fn_color(yaml_fn), " does not exist")
  }
  yaml <- readLines(yaml_fn)
  if(!length(yaml)){
   bail("The YAML file ", fn_color(yaml_fn), " is empty")
  }
  # Remove surrounding whitespace
  yaml <- trimws(yaml)
  # Remove commented lines and extra surrounding quotes
  yaml <- noquote(yaml[!grepl("^#", yaml)])

  # Store indices where the open square bracket is
  brac_open_ind <- grep("^rmd_files: \\[", yaml)
  if(!length(brac_open_ind)){
    bail(tag_color("rmd_files: ["), " not found in ", fn_color(yaml_fn), ". ",
         "It must appear at the beginning of a line")
  }
  if(length(brac_open_ind) > 1){
    bail("More than one ", tag_color("rmd_files: ["), " found in ",
         fn_color(yaml_fn))
  }
  if(brac_open_ind > 1){
    pre_rmd_fns <- yaml[1:(brac_open_ind - 1)]
  }else{
    pre_rmd_fns <- NULL
  }

  # Store indices where the close square bracket is
  brac_close_ind <- grep("\\]$", yaml)
  if(!length(brac_close_ind)){
    bail(tag_color("]"), " not found in ", fn_color(yaml_fn), ". ",
         "It must appear at the end of a line")
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
    bail("No .Rmd filenames found in ", fn_color(yaml_fn))
  }

  # Copy the files into the new files
  orig_rmd_fns <- rmd_fns
  rmd_fns <- paste0("tmp-", rmd_fns)
  unlink(rmd_fns, force = TRUE)
  file.copy(orig_rmd_fns, rmd_fns)

  # Create the text listing the new Rmd files for inside the new YAML file
  rmd_fns_listing <- imap_chr(rmd_fns, ~{
    if(.y == 1){
      paste0('rmd_files: ["', .x, '",')
    }else if(.y == length(rmd_fns)){
      paste0('            "', .x, '"]')
    }else{
      paste0('            "', .x, '",')
    }
  })

  # Create the new YAML file
  yaml <- c(pre_rmd_fns, rmd_fns_listing, post_rmd_fns)
  tmp_fn <- paste0("tmp", yaml_fn)
  writeLines(yaml, tmp_fn)

  list(tmp_fn, rmd_fns)
}
