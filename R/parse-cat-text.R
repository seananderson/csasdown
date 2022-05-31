#' Parse a string to ensure correct parentheses matching and find the end
#' of a [cat()] call
#'
#' @description
#' Simple character parser with stack implementation used to match parentheses
#' in the input string and to find the final closing paren for the [cat()] call
#' which must start off the string
#'
#' @details
#' A vector of strings is passed in starting with 'cat(' but is is unknown
#' where this call ends, i.e. where is the matching ')' for this call.
#' This function is a simple parser to parse the vector text one character
#' at a time, matching parentheses of arbitrary complexity as it goes,
#' trying to find the matching ')' to end the 'cat('. Ad a side effect,
#' the paren matching algorithm sill detect incorrectly nested parentheses
#' and throw an error.
#'
#' Either single or double quotes can be used to surround the text passed
#' to [cat()] in the string.
#'
#' @param str_vec A vector of strings which starts with a 'cat('
#' @param verbose Show details about what was matched and pushed to or
#' popped off the stack
#'
#' @return The text that is actually inside the 'cat(' call (as a vector
#' of strings, same as what was passed in). The 'cat(' and ')' are not
#' included in the return text
#'
#' @importFrom crayon cyan red
#' @export
parse_cat_text <- function(str_vec, verbose = FALSE){

  if(!length(grep("^cat\\(.*", str_vec[1]))){
    stop("The code chunks that contain `cat()` can only contain `cat()`. ",
         "This is because the `cat()` function is used only to contain ",
         "Rmarkdown text in this project",
         call. = FALSE)
  }
  str_vec[1] <- gsub("^cat\\(", "", str_vec[1])
  if(length(grep("^\"" , str_vec[1]))){
    # `cat()` text is surrounded by double quotes
    quote_type <- "\""
  }else if(length(grep("^'" , str_vec[1]))){
    # `cat()` text is surrounded by single quotes
    quote_type <- "'"
  }else{
    stop("Non-existent quote type for `cat()` text. You must use either ",
         "single or double quotes. You used ", substr(str_vec[1], 1, 1),
         call. = FALSE)
  }

  # Parse (possibly nested) parens in the text
  pstack <- NULL
  # Push the `cat(` '(' to the stack initially
  pstack <- stk_push(pstack, "(")
  if(verbose){
    message("Pushed '(' from `cat()` to stack. Stack size is now ",
            stk_size(pstack), "\n")
  }
  matched <- FALSE
  prev_char <- NULL
  for(.y in seq_along(str_vec)){
    for(char_pos in seq_len(nchar(str_vec[.y]))){
      char <- substr(str_vec[.y], char_pos, char_pos)
      if(char == "("){
        pstack <- stk_push(pstack, "(")
        if(verbose){
          message("Pushed '(' to stack on line ", .y, ", char ", char_pos, "\n",
                  "stack size is now ", stk_size(pstack), "\n",
                  cyan(substr(str_vec[.y], 1, char_pos - 1)),
                  red(substr(str_vec[.y], char_pos, char_pos)),
                  cyan(substr(str_vec[.y], char_pos + 1 , nchar(str_vec[.y]))),
                  "\n")
        }
      }
      if(char == ")"){
        # Close parens are never pushed to the stack, only matched with a '('
        # on top of the stack
        tmp <- stk_pop(pstack)
        pstack <- tmp$stack
        popval <- tmp$val
        if(is.null(popval)){
          stop("Mismatched ')' found on line  ", .y, ", char ", char_pos, "\n",
               cyan(substr(str_vec[.y], 1, char_pos - 1)),
               red(substr(str_vec[.y], char_pos, char_pos)),
               cyan(substr(str_vec[.y], char_pos + 1 , nchar(str_vec[.y]))),
               "\n",
               call. = FALSE)
        }
        # The check for prev_char not being NULL here is to stop this
        # running the first time through
        if(prev_char == quote_type){
          # Matched the end ')' of cat()
          if(stk_size(pstack)){
            stop("Mismatched ')' found on line  ", .y, ", char ", char_pos, "\n",
                 cyan(substr(str_vec[.y], 1, char_pos - 1)),
                 red(substr(str_vec[.y], char_pos, char_pos)),
                 cyan(substr(str_vec[.y], char_pos + 1 , nchar(str_vec[.y]))),
                 "\n",
                 call. = FALSE)
          }
          if(verbose){
            message("Matched closing ')' for `cat()` on line ", .y, ", char ", char_pos, "\n",
                    "stack size is now ", stk_size(pstack), "\n",
                    cyan(substr(str_vec[.y], 1, char_pos - 1)),
                    red(substr(str_vec[.y], char_pos, char_pos)),
                    cyan(substr(str_vec[.y], char_pos + 1 , nchar(str_vec[.y]))),
                    "\n")
          }
          if(char_pos != nchar(str_vec[.y])){
            stop("Extra characters follow the ')' that closes `cat()`.\n",
                 "Closing ')' for `cat()` must be at the end of the line \n",
                 "with nothing following it (check for trailing whitespace):\n\n",
                 cyan(substr(str_vec[.y], 1, char_pos - 1)),
                 red(substr(str_vec[.y], char_pos, char_pos)),
                 cyan(substr(str_vec[.y], char_pos + 1 , nchar(str_vec[.y]))),
                 "\n",
                 call. = FALSE)
          }
          # Remove ')' that closes `cat()`
          str_vec[.y] <- substr(str_vec[.y], 1, char_pos - 1)
          # This is the total return chunk
          out_chunk <- str_vec[1:.y]
          matched <- TRUE
          break
        }
        if(stk_size(pstack) == 0){
          stop("Mismatched ')' found on line  ", .y, ", char ", char_pos, "\n",
               cyan(substr(str_vec[.y], 1, char_pos - 1)),
               red(substr(str_vec[.y], char_pos, char_pos)),
               cyan(substr(str_vec[.y], char_pos + 1 , nchar(str_vec[.y]))),
               "\n",
               call. = FALSE)
        }
        if(verbose){
          message("Matched ')' on line ", .y, ", char ", char_pos, "\n",
                  "stack size is now ", stk_size(pstack), "\n",
                  cyan(substr(str_vec[.y], 1, char_pos - 1)),
                  red(substr(str_vec[.y], char_pos, char_pos)),
                  cyan(substr(str_vec[.y], char_pos + 1 , nchar(str_vec[.y]))),
                  "\n")
        }
      }
      prev_char <- char
    }
    if(matched){
      break
    }
  }
  out_chunk
}

