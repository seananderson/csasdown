#' Generate LaTeX code for pandoc highlight-style list
#'
#' @keywords internal
#' @family highlight-theme
#'
#' @param json_lst A list of pandoc highlight-style JSON codes, one for each
#' theme to make a latex version for
#'
#' @return A list of vectors of lines of latex code representing the highlight-style for pandoc
#' for each theme type
#' @importFrom grDevices col2rgb
gen_latex_highlight_code <- function(json_lst){

  if(is.null(json_lst)){
    stop("`json_lst` cannot be `NULL`",
         call. = FALSE)
  }

  imap(json_lst, function(json = .x, num = .y){
    if(!length(grep("^\\{\\s*$", json))){
      stop("`json_lst` element ", num, " does not start with an open curly brace",
           call. = FALSE)
    }
    style_list <- parse_pandoc_highlight_theme(json)

    if(is.na(style_list$`text-color`)){
      style_list$`text-color` <- "#000000"
    }
    if(is.na(style_list$`background-color`)){
      style_list$`background-color` <- "#ffffffff"
    }

    text_color <- paste(format(round(hex2rgb(style_list$`text-color`, rel = TRUE), 2), nsmall = 2), collapse = ",")
    background_color <- paste(hex2rgb(style_list$`background-color`), collapse = ",")

    # Add ',fontsize=\small' at the end of the DefineVerbatimEnvironment for more characters per line
    latex_head <- c(paste0("\\DefineVerbatimEnvironment{Highlighting}{Verbatim}{commandchars=\\\\\\{\\},formatcom=\\color[rgb]{",
                           text_color, "}}"),
                    "\\usepackage{framed}",
                    paste0("\\definecolor{shadecolor}{RGB}{", background_color, "}"),
                    "\\newenvironment{Shaded}{\\begin{snugshade}}{\\end{snugshade}}")

    style_list <- style_list$`sections-list`
    nms <- names(style_list)
    lst <- list()
    for(i in seq_along(style_list)){
      attr_list <- style_list[[i]]
      cmd <- nms[i]
      m <- paste0("\\newcommand{\\", cmd, "Tok}[1]")

      # There are 5 attributes for each command

      # Attribute 1, text color
      has_textcolor <- FALSE
      if(attr_list[[1]][2] != "null"){
        clrs <- as.vector(hex2rgb(attr_list[[1]][2], rel = TRUE))
        clrs <- paste0(format(round(clrs, digits = 2), nsmall = 2), collapse = ",")
        m <- paste0(m, "{\\textcolor[rgb]{", clrs, "}")
        has_textcolor <- TRUE
      }

      # Attribute 2, item-by-item background color not used since it is already set up
      # through the theme general value

      # Attributes 3, 4, and 5 (bold, italics, and underline) are intertwined in latex code so code chunk for all three here
      aa <- as.logical(attr_list[[3]][2]) # bold
      bb <- as.logical(attr_list[[4]][2]) # italics
      cc <- as.logical(attr_list[[5]][2]) # underline
      if(aa && bb && cc){
        m <- paste0(m, "{\\textbf{\\textit{\\underline{#1}}}}")
      }else if(aa & bb & !cc){
        m <- paste0(m, "{\\textbf{\\textit{#1}}}")
      }else if(aa & !bb & cc){
        m <- paste0(m, "{\\textbf{\\underline{#1}}}")
      }else if(!aa & bb & cc){
        m <- paste0(m, "{\\textit{\\underline{#1}}}")
      }else if(aa & !bb & !cc){
        m <- paste0(m, "{\\textit{#1}}")
      }else if(!aa & bb & !cc){
        m <- paste0(m, "{\\textbf{#1}}")
      }else if(!aa & !bb & cc){
        m <- paste0(m, "{\\underline{#1}}")
      }else{
        m <- paste0(m, "{#1}")
      }
      if(has_textcolor){
        m <- paste0(m, "}")
      }

      lst[[i]] <- m
    }
    # Add missing Toks if necessary
    names(lst) <- nms
    if(!length(grep("BuiltIn", names(lst)))){
      lst[[length(lst) + 1]] <- "\\newcommand{\\BuiltInTok}[1]{#1}"
      names(lst)[length(lst)] <- "BuiltIn"
    }
    if(!length(grep("Normal", names(lst)))){
      lst[[length(lst) + 1]] <- "\\newcommand{\\NormalTok}[1]{#1}"
      names(lst)[length(lst)] <- "Normal"
    }
    if(!length(grep("RegionMarker", names(lst)))){
      lst[[length(lst) + 1]] <- "\\newcommand{\\RegionMarkerTok}[1]{#1}"
      names(lst)[length(lst)] <- "RegionMarker"
    }
    if(!length(grep("Other", names(lst)))){
      lst[[length(lst) + 1]] <- "\\newcommand{\\OtherTok}[1]{#1}"
      names(lst)[length(lst)] <- "Other"
    }
    if(!length(grep("Function", names(lst)))){
      lst[[length(lst) + 1]] <- paste0("\\newcommand{\\FunctionTok}[1]{#1}")
      names(lst)[length(lst)] <- "Function"
    }
    if(!length(grep("String", names(lst)))){
      lst[[length(lst) + 1]] <- paste0("\\newcommand{\\StringTok}[1]{#1}")
      names(lst)[length(lst)] <- "String"
    }
    if(!length(grep("SpecialChar", names(lst)))){
      lst[[length(lst) + 1]] <- paste0("\\newcommand{\\SpecialCharTok}[1]{#1}")
      names(lst)[length(lst)] <- "SpecialChar"
    }
    if(!length(grep("Attribute", names(lst)))){
      lst[[length(lst) + 1]] <- paste0("\\newcommand{\\AttributeTok}[1]{#1}")
      names(lst)[length(lst)] <- "Attribute"
    }

    # Sort list by name of Tok
    nms <- names(lst)
    lst <- lst[order(nms)]
    out <- unlist(lst)
    names(out) <- NULL

    c(latex_head, out)
  })
}
