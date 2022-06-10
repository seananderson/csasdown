#' Validate the input file for required YAML entries for a
#' specific document type
#'
#' @details
#' Warnings will be given for missing items prior to returning `FALSE` for
#' missing YAML tags in the file `fn`
#'
#' @param fn The bookdown Rmd file which contains the YAML headers required
#' to run [bookdown::render_book()]
#' @param doc_type The type of document to validate. Can be one of 'resdoc',
#' 'sr', 'techreport' or any of those with appended underscore and more text,
#' for example 'resdoc_pdf' or 'sr_word'
#'
#' @return `TRUE` or `FALSE` for file validation
#' @export
validate_index_file <- function(fn = "index.Rmd", doc_type){

  if(!file.exists(fn)){
    stop("file ;'", fn, "' does not exist")
  }
  rmd <- readLines(fn)
  if(!length(rmd)){
    stop("File '", fn, "' contains no data")
  }

  rmd_trim <- trimws(rmd)
  v <- TRUE
  validate_tag <- function(name, extra_text = ""){
    pat <- paste0("^", name, ":(\\s*\\S+\\s*)+$")
    ind <- grep(pat, rmd_trim)
    ok <- as.logical(length(ind))
    if(!ok){
      warning("`", name, ":` tag not found in '", fn, "'\n",
              extra_text)
      return(FALSE)
    }
    return(TRUE)
  }
  # Remove the _pdf or _word or _anything
  if(length(grep("_", doc_type))){
    doc_type <- gsub("^(\\S+)_.*$", "\\1", doc_type)
  }

  if(doc_type == "resdoc"){
    v <- v && validate_tag("title")
    v <- v && validate_tag("french_title",
                           paste0("\nThis is a new addition as of May 2022, ",
                                  " and was previously called `title_other`"))
    v <- v && validate_tag("author")
    v <- v && validate_tag("author_list")
    v <- v && validate_tag("address")
    v <- v && validate_tag("french_address",
                           "\nThis is a new addition as of May 2022")
    v <- v && validate_tag("month")
    v <- v && validate_tag("french_month",
                           "\nThis is a new addition as of May 2022")
    v <- v && validate_tag("year")
    v <- v && validate_tag("report_number")
    v <- v && validate_tag("region")
    v <- v && validate_tag("french_region",
                           "\nThis is a new addition as of May 2022")
    v <- v && validate_tag("isbn")
    v <- v && validate_tag("cat_no")
    v <- v && validate_tag("citation_other_language")
    v <- v && validate_tag("abstract")
    v <- v && validate_tag("french_abstract",
                           "\nThis is a new addition as of May 2022")
    v <- v && validate_tag("header")
    v <- v && validate_tag("french")
    v <- v && validate_tag("copy_sty")
    v <- v && validate_tag("line_nums")
    v <- v && validate_tag("line_nums_mod")
    v <- v && validate_tag("lot_lof")
    v <- v && validate_tag("draft_watermark")
    v <- v && validate_tag("include_section_nums")
    v <- v && validate_tag("highlight")
    v <- v && validate_tag("knit")
    v <- v && validate_tag("link-citations")
    v <- v && validate_tag("bibliography")
  }
  if(doc_type == "sr"){
    v <- v && validate_tag("title")
    v <- v && validate_tag("title_short")
    v <- v && validate_tag("french_title",
                           paste0("\nThis is a new addition as of May 2022, ",
                                  " and was previously called `title_other`"))
    v <- v && validate_tag("year")
    v <- v && validate_tag("month")
    v <- v && validate_tag("french_month",
                           "\nThis is a new addition as of May 2022")
    v <- v && validate_tag("report_number")
    v <- v && validate_tag("approver")
    v <- v && validate_tag("approval_year")
    v <- v && validate_tag("approval_month")
    v <- v && validate_tag("approval_day")
    v <- v && validate_tag("work_done_year")
    v <- v && validate_tag("work_done_month")
    v <- v && validate_tag("branch")
    v <- v && validate_tag("region")
    v <- v && validate_tag("french_region",
                           "\nThis is a new addition as of May 2022")
    v <- v && validate_tag("isbn")
    v <- v && validate_tag("cat_no")
    v <- v && validate_tag("french")
    v <- v && validate_tag("prepub")
    v <- v && validate_tag("copy_sty")
    v <- v && validate_tag("line_nums")
    v <- v && validate_tag("line_nums_mod")
    v <- v && validate_tag("draft_watermark")
    v <- v && validate_tag("highlight")
    v <- v && validate_tag("knit")
    v <- v && validate_tag("site")
    v <- v && validate_tag("link-citations")
    v <- v && validate_tag("bibliography")
    v <- v && validate_tag("lot")
    v <- v && validate_tag("lof")
  }
  if(doc_type == "techreport"){
    v <- v && validate_tag("title")
    v <- v && validate_tag("french_title",
                           paste0("\nThis is a new addition as of May 2022, ",
                                  " and was previously called `title_other`"))
    v <- v && validate_tag("author")
    v <- v && validate_tag("author_list")
    v <- v && validate_tag("address")
    v <- v && validate_tag("french_address",
                           "\nThis is a new addition as of May 2022")
    v <- v && validate_tag("phone")
    v <- v && validate_tag("year")
    v <- v && validate_tag("report_number")
    v <- v && validate_tag("region")
    v <- v && validate_tag("french_region",
                           "\nThis is a new addition as of May 2022")
    v <- v && validate_tag("isbn")
    v <- v && validate_tag("abstract")
    v <- v && validate_tag("french_abstract",
                           paste0("\nThis is a new addition as of May 2022, ",
                                  " and was previously called `abstract_other`"))
    v <- v && validate_tag("author_footnote")
    v <- v && validate_tag("french")
    v <- v && validate_tag("copy_sty")
    v <- v && validate_tag("line_nums")
    v <- v && validate_tag("line_nums_mod")
    v <- v && validate_tag("lot_lof")
    v <- v && validate_tag("draft_watermark")
    v <- v && validate_tag("highlight")
    v <- v && validate_tag("knit")
    v <- v && validate_tag("link-citations")
    v <- v && validate_tag("bibliography")
  }

  v
}