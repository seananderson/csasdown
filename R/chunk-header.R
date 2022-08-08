#' Verify if a knitr chunk header contains a given option
#'
#' @details
#' Will return `TRUE` if you use the wrong type of quotes, for example
#' if you request `option`  as `"results = 'asis'"` and the header contains
#' `"results = \"asis\""`.
#' Spaces between equals sign are unimportant, the regular expression will
#' allow for any amount. User can provide any amount or none in `option`
#' and it will match any amount or none in the `header`.
#'
#' @keywords internal
#'
#' @family knitr-chunk-header
#'
#' @param header The knitr chunk header string
#' @param option The option to check for
#'
#' @return Logical. `TRUE` if `option` was found in `header`. `FALSE`
#' if `option` was not found in `header` or `option` was `NULL` or `NA`
chunk_header_contains <- function(header, option){

  if(is.null(header)){
    return(NULL)
  }
  if(is.na(header)){
    return(NA_character_)
  }
  if(is.null(option) || is.na(option)){
    return(FALSE)
  }

  new_header <- trimws(header)
  option <- trimws(option)
  # Need to escape special characters that may be in the option name
  option <- gsub("\\(", "\\\\(", option)
  option <- gsub("\\)", "\\\\)", option)


  # Convert single quotes to double quotes and vice versa and test for both
  # with the same option value
  option_is_key_val <- grepl("^(\\S+)\\s*=\\s*\\S+$", option)
  if(option_is_key_val){
    # option is of the format `key = value`, so have to check for both kinds
    # of quotes
    option_key <- gsub("^(\\S+)\\s*=\\s*\\S+$", "\\1", option)
    option_val <- gsub("^\\S+\\s*=\\s*(\\S+)$", "\\1", option)
    option_link <- paste0("\\s*=\\s*")
    option_val_single_q <- gsub("\"", "'", option_val)
    option_val_double_q <- gsub("'", "\"", option_val)
    option_single_q <- paste0(option_key, option_link, option_val_single_q)
    option_double_q <- paste0(option_key, option_link, option_val_double_q)
    # regex1 has no value, just the `option` text
    # regex2 has a value added to it (option = value)
    regex1_single_q <- paste0(",\\s*",
                              option_single_q, "\\s*(,|\\})")
    regex1_double_q <- paste0(",\\s*",
                              option_double_q, "\\s*(,|\\})")
    regex2_single_q <- paste0(",\\s*",
                              option_single_q, "\\s*=\\s*\\S+\\s*(,|\\})")
    regex2_double_q <- paste0(",\\s*",
                              option_double_q, "\\s*=\\s*\\S+\\s*(,|\\})")
    return(grepl(regex1_single_q, header) ||
             grepl(regex1_double_q, header) ||
             grepl(regex2_single_q, header) ||
             grepl(regex2_double_q, header))
  }

  # option is of the format `key`
  # `regex1` has no value, just the `option` text
  # `regex2` has a value added to it (option = value)
  regex1 <- paste0(",\\s*", option, "\\s*(,|\\})")
  regex2 <- paste0(",\\s*", option, "\\s*=\\s*\\S+\\s*(,|\\})")

  grepl(regex1, header) || grepl(regex2, header)
}

#' Add an option to a knitr chunk header
#'
#' @details
#' Here is an example of how the option is added. If `option` is
#' "needs_trans = TRUE" then the text in `option` will be added with a
#' preceeding comma and space, at the end of the list of options that is
#' already present:
#' ```{r example, eval = !fr()}`
#' would become
#' ```{r example, eval = !fr(), needs_trans = TRUE}`
#' If `header` is not matched by the regular expression used by knitr for
#' beginning chunks [knitr::all_patterns$md$chunk.begin] then a
#' warning will be issued and `header` will be returned.
#' If `option` already exists in `header` and `ovr` is `TRUE`, it will be
#' overwritten with the new `option` value
#'
#' @keywords internal
#'
#' @family knitr-chunk-header
#'
#' @param header The knitr chunk header string
#' @param option The option to add. This whole string will be added
#' @param ovr Logical. If `TRUE`, and `option` is already defined in
#' `header`, `option` will replace that definition completely
#'
#' @return The modified knitr chunk header string
chunk_header_add <- function(header = NULL, option = NULL, ovr = FALSE){

  if(is.null(header)){
    return(NULL)
  }
  if(is.na(header)){
    return(NA_character_)
  }
  if(is.null(option) || is.na(option)){
    return(header)
  }

  new_header <- trimws(header)
  option <- trimws(option)

  if(!grepl(all_patterns$md$chunk.begin, new_header)){
    alert("`header` = ", csas_color(new_header), " is not a valid knitr ",
          "chunk header\n",
          "See the regular expression that is used:\n",
          fn_color("knitr::all_patterns$md$chunk.begin"))
    return(new_header)
  }
  # Check to see that `option` is not already in the header. To do this,
  # need to separate the option from its value (if there is a value)
  option_key <- gsub("(\\S+)\\s*=\\s*\\S+", "\\1", option)
  if(grepl(option_key, header)){
    if(ovr){
      new_header <- chunk_header_remove(header, option_key)
    }else{
      alert("`option` = ", csas_color(option_key),
            " is already defined in the knitr chunk header ",
            "and was not added again")
      return(header)
    }
  }
  # Since we used `trimws()` above and `header` matches the regular expression
  # for beginning knitr chunks, the last character is guaranteed to be a `}`
  i <- substr(new_header, 1, nchar(new_header) - 1)
  paste0(i, ", ", option, "}")
}

#' Remove an option and its value from a knitr chunk header
#'
#' @details
#' Here is an example of how the option is added. If `option` is
#' "needs_trans" then the text in `option` will be removed along with
#' preceeding comma and space, up to the next comma or closing `}`
#' ```{r example, eval = !fr(), needs_trans = TRUE}`
#' with call: `chunk_header_remove(header, "needs_trans")`
#' ```{r example, eval = !fr()}`
#' If `header` is not matched by the regular expression used by knitr for
#' beginning chunks [knitr::all_patterns$md$chunk.begin] then a
#' warning will be issued and `header` will be returned.
#'
#' @keywords internal
#'
#' @family knitr-chunk-header
#'
#' @param header The knitr chunk header string
#' @param option The option to remove
#'
#' @return The modified knitr chunk header string
chunk_header_remove <- function(header = NULL, option = NULL){

  if(is.null(header)){
    return(NULL)
  }
  if(is.na(header)){
    return(NA_character_)
  }
  if(is.null(option) || is.na(option)){
    return(header)
  }

  new_header <- trimws(header)
  option <- trimws(option)

  if(!grepl(all_patterns$md$chunk.begin, new_header)){
    alert("`header` = ", csas_color(new_header), " is not a valid knitr ",
          "chunk header\n",
          "See the regular expression that is used:\n",
          fn_color("knitr::all_patterns$md$chunk.begin"))
    return(header)
  }
  # regex1 has no value, just the `option` text
  regex1 <- paste0(",\\s*", option, "\\s*(,|\\})")
  # regex2 has a value added to it (option = value)
  regex2 <- paste0(",\\s*", option, "\\s*=\\s*\\S+\\s*(,|\\})")
  if(grepl(regex1, header)){
    return(gsub(regex1, "\\1", new_header))
  }else if(grepl(regex2, header)){
    return(gsub(regex2, "\\1", new_header))
  }else{
    alert("The `option` = ", csas_color(option), " was not found in the ",
          "knitr chunk `header`:\n",
          csas_color(header))
    return(header)
  }
}
