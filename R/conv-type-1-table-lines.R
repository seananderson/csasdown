conv_type_1_table_lines <- function(chunk){

  if(is.null(chunk)){
    return(list(NULL, NULL))
  }

  if(length(chunk) < 5){
    stop("A type 1 table must have at least 5 lines. Input table is:\n\n",
         paste(chunk, collapse = "\n"),
         "\n\n",
         call. = FALSE)
  }

  # `text_pat` matches any sequence of zero or more whitespace characters,
  # followed by 1 or more non-whitespace characters, followed by zero or
  # more whitespace characters all repeating
  # `dash_pat` matches any sequence of zero or more whitespace characters,
  # followed by 1 or more dashes, followed by zero or more whitespace
  # characters all repeating
  text_pat <- "^(\\s*\\S+\\s*)+$"
  dash_pat <- "^(\\s*-+\\s*)+$"
  t1 <- trimws(chunk[1])
  t2 <- trimws(chunk[2])
  t3 <- trimws(chunk[3])

  # Confirm type 1 table
  is_type_1 <- length(grep(dash_pat, t1)) &&
               length(grep(text_pat, t2)) &&
               length(grep(dash_pat, t3))

  if(!is_type_1){
    stop("The following table is not a type 1 table based on the first three ",
         "rows:\n\n", paste(chunk, collapse = "\n"),
         "\n\n",
         "They must start with:\n",
         "- a row of dashes\n",
         "- a row of text representing headers\n",
         "- a row of dashes.",
         call. = FALSE)
  }

  start_tbl_ind <- 1
  end_tbl_ind <- NULL
  start_lbl_ind <- NULL
  end_lbl_ind <- NULL
  # Add the first three rows as they have been checked already
  tbl_chunk <- chunk[1:3]
  i <- 4
  repeat{
    tn <- trimws(chunk[i])
    end_tbl <- length(grep(dash_pat, tn))
    if(end_tbl){
      end_tbl_ind <- i
      # Remove previous row's extra blank line while adding ending row
      # of dashes
      tbl_chunk <- c(tbl_chunk[-length(tbl_chunk)], chunk[i])
      break
    }

    if(chunk[i] != ""){
      tbl_chunk <- c(tbl_chunk, chunk[i], "")
    }
    if(i == length(chunk)){
      break
    }
    i <- i + 1
  }

  if(end_tbl){
    # Basic table without a table caption string included
    tbl_chunk <- c(tbl_chunk, "")
    if(end_tbl_ind == length(chunk)){
      ret_chunk <- NULL
      return(list(tbl_chunk, ret_chunk))
    }
  }else{
    stop("A table appears to have been started but not finished:\n\n",
         paste(chunk, collapse = "\n"),
         "\n\n",
         call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # At this point, the end of the table has been found and i is it's index.
  # - For a type 1 table, this is on the last "--------" line, which could be
  # at the end of the chunk. There are one or more lines past the end of the
  # table which need to be searched for a table caption label

  end_tbl_ind <- i
  # Find start of label if it exists and if table has a caption label
  # (Table: Caption here)
  i <- i + 1
  has_label <- FALSE
  start_label_ind <- NULL
  # `lbl_def_pat` matches any sequence of zero or more whitespace characters,
  # followed by 1 or more dashes, followed by zero or more whitespace characters,
  # preceded by "Table:" and stands for 'Label defined'
  # `lbl_undef_pat` matches any sequence of zero or more whitespace characters,
  # preceded by "Table:" and stands for 'Label undefined '
  lbl_def_pat <- "^Table:(\\s*\\S+\\s*)+$"
  lbl_undef_pat <- "^Table:\\s*$"
  repeat{
    # If the caption def looks like this:
    # Table: A caption is here.
    # More caption here.
    if(length(grep(lbl_def_pat , chunk[i]))){
      has_label <- TRUE
      start_label_ind <- i
      while(length(grep(text_pat , chunk[i]))){
        tbl_chunk <- c(tbl_chunk, chunk[i])
        end_lbl_ind <- i
        if(i == length(chunk)){
          break
        }
        i <-  i + 1
      }
      break
    }
    # If the caption def looks like this:
    # Table:
    # A caption is here.
    # More caption here.
    if(length(grep(lbl_undef_pat , chunk[i])) &&
       length(grep(text_pat, chunk[i + 1]))){
      has_label <- TRUE
      start_label_ind <- i
      tbl_chunk <- c(tbl_chunk, chunk[i])
      i <- i + 1
      while(length(grep(text_pat , chunk[i])) && i < length(chunk)){
        tbl_chunk <- c(tbl_chunk, chunk[i])
        end_lbl_ind <- i
        if(i == length(chunk)){
          break
        }
        i <-  i + 1
      }
      break
    }
    if(i == length(chunk)){
      break
    }
    i <- i + 1
  }

  if(has_label){
    if(end_lbl_ind == length(chunk)){
      ret_chunk <- NULL
    }else{
      ret_chunk <- chunk[(end_lbl_ind + 1):length(chunk)]
      # Add adjustment to fix number of blank lines after table caption
      # Take number of blanks before any text, replace eith \\\\ and add
      # another blank line before that
      out_chunk <- NULL
      i <- 1
      while(i <= length(ret_chunk)){
        if(ret_chunk[i] == ""){
          out_chunk[i] <- "\\\\"
        }else{
          out_chunk[i] <- ret_chunk[i]
        }
        i <- i + 1
      }
      tbl_chunk <- c(tbl_chunk, "", out_chunk)
      ret_chunk <- NULL
      return(list(tbl_chunk, ret_chunk))
    }
    tbl_chunk <- c(tbl_chunk, "")
  }else{
    if(end_tbl_ind == length(chunk)){
      ret_chunk <- NULL
    }else{
      ret_chunk <- chunk[(end_tbl_ind + 1):length(chunk)]
    }
  }
  browser()
  return(list(tbl_chunk, ret_chunk))
}