#' Validate that chunk headers contain only certain combinations of settings
#'
#' @description
#' Validate that chunk headers contain only certain combinations of settings.
#' Check that chunks with `eval = fr()` or `eval = !fr()`, have the correct
#' `needs_trans` setting and a correct chunk name format if `chunk_regex`
#' is `TRUE`
#'
#' @param rmd_files A vector of character strings representing the names
#' of Rmd files
#' @param en_chunk_regex A regular expression to match for the chunk
#' name for English chunks. Default is 'ends in -en'. The `$` means anchor
#' to the end, so '-en' must be at the end. `\\S+` means match one or more
#' any other non-whitespace characters
#' @param fr_chunk_regex A regular expression to match for the chunk
#' name for French chunks. Default is 'ends in -fr'
#' @param verbose Logical. If `TRUE`, print messages
#'
#' @return A vector of error messages, one item for each chunk problem
#' @keywords internal
validate_chunk_headers <- function(rmd_files,
                                   en_chunk_regex = "^\\S+-en$",
                                   fr_chunk_regex = "^\\S+-fr$",
                                   verbose = FALSE){

  if(verbose){
    notify("Validating knitr chunk headers ...")
  }

  if(!length(rmd_files)){
    return(invisible())
  }
  errs <- map(rmd_files, function(fn){

    if(!file.exists(fn)){
      bail("File ", fn_color(fn), " does not exist") # nocov
    }
    suppressWarnings(rmd <- readLines(fn))

    chunk_head_inds <- grep(all_patterns$md$chunk.begin, rmd)
    if(!length(chunk_head_inds)){
      return(NULL)
    }

    chunk_name_pat <- "^```\\s*\\{\\s*r\\s*(\\s*\\S+\\s*)+?\\s*,\\s*.*$"
    err_lst <- map(chunk_head_inds, function(chunk_head_ind){
      chunk_name <- gsub(chunk_name_pat, "\\1", trimws(rmd[chunk_head_ind]))
      if(verbose){
        notify(csas_color(trimws(rmd[chunk_head_ind])))
      }
      fr_opt <- grep("eval\\s*=\\s*fr\\(\\)\\s*[,|\\}]",
                     rmd[chunk_head_ind])
      en_opt <- grep("eval\\s*=\\s*!fr\\(\\)\\s*[,|\\}]",
                     rmd[chunk_head_ind])
      # Verify that the chunk name fulfills English format requirement
      err_mess <- NULL
      if(length(en_opt)){
        if(!is.null(en_chunk_regex)){
          if(!length(grep(en_chunk_regex, chunk_name))){
            err_mess <- paste0("Chunk name ",
                               csas_color(chunk_name),
                               " in file ", fn_color(fn),
                               "\n is not of correct format for English ",
                               "chunks ",
                               "(", csas_color(en_chunk_regex), ")")
          }
        }
        # Verify that the English chunk does not have `needs_trans = TRUE` or
        # `needs_trans = FALSE`
        if(length(grep("needs_trans\\s*=\\s*(TRUE|FALSE)\\s*[,|\\}]",
                       rmd[chunk_head_ind]))){
          err_mess <- c(err_mess,
                        paste0("Chunk ",
                               csas_color(chunk_name),
                               " in file ", fn_color(fn),
                               "\n must not include ",
                               csas_color("needs_trans = TRUE"), " or ",
                               csas_color("needs_trans = FALSE")))
        }
      }
      if(length(fr_opt)){
        # Verify that the chunk name fulfills French format requirement
        if(!is.null(fr_chunk_regex)){
          if(!length(grep(fr_chunk_regex, chunk_name))){
            err_mess <- paste0("Chunk name ",
                               csas_color(chunk_name),
                               " in file ", fn_color(fn),
                               "\n is not of correct format for French ",
                               "chunks ",
                               "(", csas_color(fr_chunk_regex), ")")
          }
        }
        # Verify that the French chunk has `needs_trans = TRUE` or `needs_trans = FALSE`
        if(!length(grep("needs_trans\\s*=\\s*(TRUE|FALSE)\\s*[,|\\}]",
                        rmd[chunk_head_ind]))){
          err_mess <- c(err_mess,
                        paste0("Chunk ",
                               csas_color(chunk_name),
                               " in file ", fn_color(fn),
                               "\n must include ",
                               csas_color("needs_trans = TRUE"), " or ",
                               csas_color("needs_trans = FALSE")))
        }
      }
      if(!length(en_opt) && !length(fr_opt)){
        # Ensure that neutral chunks do not have `need_trans` set
        if(length(grep("needs_trans\\s*=\\s*(TRUE|FALSE)\\s*[,|\\}]",
                       rmd[chunk_head_ind]))){
          err_mess <- c(err_mess,
                        paste0("Chunk ",
                               csas_color(chunk_name),
                               " in file ", fn_color(fn),
                               "\n must not include ",
                               csas_color("needs_trans = TRUE"), " or ",
                               csas_color("needs_trans = FALSE")))
        }
      }
      err_mess
    })
    err_lst[lengths(err_lst) > 0] |>
      unlist()
  }) |>
    unlist()

  if(verbose){
    check_notify("Knitr chunk headers are all good\n")
  }

  `if`(length(errs), errs, NULL)
}