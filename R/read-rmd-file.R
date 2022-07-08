#' Read in am Rmd file, place `cat()` around it, and return it
#'
#' @description
#' Read in am Rmd file and return it verbatim, with a newline added at the end.
#' Remove lines with only spaces in them, if they exist at the end of a file,
#' they cause an "incomplete final line" warning from Rmarkdown.
#'
#' @details
#' This allows the author to write pure Rmarkdown in a file, and have it read
#' into the [cat()] command in a csasdown resdoc Rmd file's code chunk. See
#' the bilingual resdoc example doc (`csasdown::draft("resdoc-b")`)
#'
#' @keywords internal
#'
#' @param fn The Rmd filename. An extension is not needed
#' @param src_fn The name of the Rmd filename which was used when trying to
#' import the `fn` file, i.e. the file in which `rmd_file("fn")` is called from
#'
#' @return Verbatim text representing the file
read_rmd_file <- function(fn, src_fn = "unknown"){

  # nocov start
  if(is.null(fn)){
    bail("The filename ", csas_color("fn"), " is ", csas_color("NULL"),
         " when trying to import from file ", fn_color(src_fn))
  }
  if(fn == ""){
    bail("The filename ", csas_color("fn"), " is an empty string ",
         "when trying to import from file ", fn_color(src_fn))
  }

  # nocov end
  if(!length(grep("\\.Rmd$", fn))){
    fn <- paste0(fn, ".Rmd")
  }
  if(!file.exists(fn)){
    bail("File ", fn_color(fn), " does not exist when trying to import from the ",
         "file ", fn_color(src_fn))
  }
  lines <- readLines(fn)
  if(!length(lines)){
    return("")
  }
  # Remove any double quotes
  lines <- gsub("\"", "'", lines)
  lines <- gsub("^\\s+$", "", lines)
  lines[1] <- paste0("cat(\"", lines[1])
  if(lines[length(lines)] == ""){
    lines[length(lines) + 1] <- "\")"
  }else{
    lines[length(lines)] <- paste0(lines[length(lines)], "\")")
  }

  lines
}