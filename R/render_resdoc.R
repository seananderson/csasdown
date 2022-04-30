#' Render a resdoc with inline R code inside [cat()] commands
#'
#' @description
#' Renders a resdoc using the [bookdown::render_book()] method but includes a
#' pre-processing step to convert anything inside [cat()] calls to cat-like
#' strings instead of rmarkdown strings. This means that any inline R code
#' included with backticks, eg: `` `r Sys.time()` `` will be replaced with
#' a quoted, comma separated string (see [catize()]). This allows the [cat()]
#' function inside a code chunk to contain backtick-quoted R expressions exactly
#' like what knitr processes inline.
#'
#' @details
#' This is a convenience function so users can place [cat()] around
#' already-written rmarkdown code containing inline R code chunks, and
#' place those [cat()] statements inside a large code chunk. It will be
#' invisible to users when using this function to render their document.
#'
#' Temporary files for all the Rmd files and the YAML file which contains these
#' filenames (typically _bookdown.yml) are created with modified code chunks.
#' Anywhere in the Rmd files containing [cat()] with knitr-style inline embedded
#' code chunks included are modified to make a string of text and R code together
#' which can be processed by core R. The official knitr regular expression is used
#' to extract these inline code chunks. The main Rmd file, typically index.Rmd
#' is not modified at all (it is not parsed).
#'
#' Any single-backslash escaped things in rmarkdown such as `` $\pi$ ``, or
#' `` $\alpha$ `` or similar must be double-backslashed inside the cat() command
#' or an error will be thrown.
#'
#' @param yaml_fn The YAML file name. The default is '_bookdown.yml' for
#' [bookdown::render_book()]
#' @param book_fn The Rmd file containing the YAML code used for rendering.
#' The default is 'index.Rmd' for [bookdown::render_book()]
#' @param keep_files If `TRUE`, keep the temporary files created (Rmd files
#' and YAML file)
#'
#' @return Nothing
#' @importFrom purrr prepend imap_chr
#' @export
render_resdoc <- function(yaml_fn = "_bookdown.yml",
                          book_fn = "index.Rmd",
                          keep_files = FALSE){

  tmp_yaml_fn <- create_tmp_yaml_file(yaml_fn, book_fn)

  book <- readLines(book_fn)
  fn_process <- files_to_process(yaml_fn)
  if(!length(fn_process)){
    stop("No uncommented Rmd files were found in the YAML file ", yaml_fn,
         call. = FALSE)
  }
  # Remove the book_fn from the vector if it is there
  fn_process <- fn_process[fn_process != book_fn]
  tmp_rmd_files <- map(fn_process, ~{
    if(!file.exists(.x)){
      stop("The file ", .x, " does not exist. Check the YAML file entry ", yaml_fn,
           call. = FALSE)
    }
    rmd <- readLines(.x)
    xxx <- rmd
    cat_inds <- grep("cat\\(.*", rmd)
    # Match ending parens
    map(cat_inds, ~{
      for(i in .x:length(rmd)){
        if(length(grep("\\)$", rmd[i]))){
          trmd <- rmd[.x:i]
          # Remove cat( from the first line and ) from the last and convert strings
          trmd[1] <- gsub("cat\\(", "", trmd[1])
          trmd[length(trmd)] <- gsub("\\)$", "", trmd[length(trmd)])
          trmd <- map_chr(trmd, ~{
            catize(.x)
          })
          trmd[1] <- paste0("cat(", trmd[1])
          trmd[length(trmd)] <- paste0(trmd[length(trmd)], ")")
          rmd[.x:i] <<- trmd
          break
        }
      }
    })
    tmp_fn <- paste0("tmp_", .x)
    writeLines(rmd, tmp_fn)
    tmp_fn
  })
  render_book(book_fn, config_file = tmp_yaml_fn)
  if(!keep_files){
    map(tmp_rmd_files, ~{
      unlink(.x, force = TRUE)
    })
    unlink(tmp_yaml_fn)
  }
}

#' Fetch vector of Rmd filenames from a YAML file
#'
#' @description
#' Assumes files each appear on their own line in the YAML file. Example:
#' rmd_files: ["index.Rmd",
#'             "01-chap1.Rmd"]
#'            #"02-chap2.Rmd",
#'            #"03-chap3.Rmd",
#'            #"04-references.Rmd",
#'            #"05-appendix.Rmd"]
#'
#' Any files that are commented out will not be included. In the above example,
#' the vector returned will be c("index.Rmd", "01-chap1.Rmd")
#'
#' @param yaml_fn The YAML file name
#'
#' @return A vector of the Rmd file names
files_to_process <- function(yaml_fn = "_bookdown.yml"){

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

#' Create a copy of the YAML file with temporary Rmd File names
#'
#' @param yaml_fn The YAML file name
#' @param book_fn The Rmd file containing the YAML code used for rendering.
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
  # Select only vector items containing 'Rmd'
  fns <- yaml[grepl("Rmd", yaml)]

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