#' Set the 'other' language citation up so that page 2 has the correct other
#' language based on the value of `fr()`
#'
#' @param fn The bookdown index filename, typically index.Rmd. This file
#' must have a YAML option called 'french' set to either 'true' or 'false'
#' @param verbose Logical. If `TRUE`, print messages
#'
#' @return
#' @export
set_citations <- function(fn = get_index_filename(
  system.file("rmarkdown",
              "templates",
              "resdoc", # All types have the same index filename
              "skeleton",
              "_bookdown.yml",
              package = "csasdown")),
  verbose = FALSE){

  if(verbose){
    notify("Checking file ", fn_color(fn), " for citations ...")
  }

  if(!file.exists(fn)){
    bail("File ", fn_color(fn), " does not exist")
  }

  rmd <- readLines(fn)
  ca_pat <- "^citation_english: *(.*)$"
  cf_pat <- "^citation_french: *(.*)$"
  ca_ind <- grep(ca_pat, rmd)
  cf_ind <- grep(cf_pat, rmd)

  if(fr()){
    if(!length(ca_ind)){
      warning("You are missing the `citation_english:` tag in your YAML file:\n",
              fn, ". This citation is required when you build in French as it ",
              "is set to the other language citation on page 2 of ",
              "the document")
      return(invisible())
    }
  }else{
    if(!length(cf_ind)){
      warning("You are missing the `citation_french:` tag in your YAML file:\n",
              fn, ". This citation is required when you build in English as it ",
              "is set to the other language citation on page 2 of ",
              "the document")
      return(invisible())
    }
  }
  ca <- rmd[ca_ind]
  ca <- gsub(ca_pat, "\\1", ca)
  cf <- rmd[cf_ind]
  cf <- gsub(cf_pat, "\\1", cf)

  cother_ind <- grep("^citation_other_language: *", rmd)
  cother <- paste0('citation_other_language: ',
                   ifelse(fr(),
                          ca,
                          cf))

  if(length(cother_ind)){
    rmd[cother_ind] <- cother
  }else{
    end_citations_ind <- max(ca_ind, cf_ind)
    prev <- rmd[1:end_citations_ind]
    last <- rmd[(end_citations_ind + 1):length(rmd)]
    rmd <- c(prev,
                  cother,
                  last)
  }

  writeLines(rmd, fn)
}