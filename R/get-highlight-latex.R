#' Read in pandoc theme file code as a vector of lines of code
#' and parse it to create a list of theme tokens
#'
#' @description
#' Assumed Grammar of the JSON theme file based on output of:
#' `pandoc --print-highlight-style breezedark > my_style.theme`
#' - `hexval` is a six-digit quoted string representing a hex
#'   color in this format: `"#ffffff"` or null if not included
#' - `logicalval` is true or false
#' `{4spc}` means 4 spaces
#'
#' Grammar:
#' ```
#' {
#' {4spc}"text-color": hexval,
#' {4spc}"background-color": hexval,
#' {4spc}"line-number-color": hexval
#' {4spc}"text-styles": {
#' {8spc}"Attribute1_Name": {
#' {12spc}"text-color": hexval,
#' {12spc}"background-color": hexval,
#' {12spc}"bold": logicalval,
#' {12spc}"italic": logicalval,
#' {12spc}"underline": logicalval
#' {8spc}},
#' ...
#' {4spc}"text-color": hexval,
#' {4spc}"background-color": hexval,
#' {4spc}"line-number-color": hexval
#' {4spc}"text-styles": {
#' {8spc}"AttributeN_Name": {
#' {12spc}"text-color": hexval,
#' {12spc}"background-color": hexval,
#' {12spc}"bold": logicalval,
#' {12spc}"italic": logicalval,
#' {12spc}"underline": logicalval
#' {8spc}}
#' {4spc}}
#' }
#' ```
#'
#' @param json JSON code as a vector of lines of code extracted using a system call like this:
#' pandoc --print-highlight-style breezedark
#' @param num_attrs The number of attributes for each style token in the file.
#' Style tokens are labeled in the file as text-color, italics, bold, etc.
#'
#' @return A list of 1 element each for the general colors (text and background colors) and 1 element
#' which is a list of lists of named key-value pairs, where each list represents a chunk of attributes
#' as defined in the grammar above
parse_pandoc_highlight_theme <- function(json = NULL,
                                         num_attrs = 5){

  stopifnot(!is.null(json))

  text_color_ind <- grep("\\\"text-color\\\"", json)[1]
  background_color_ind <- grep("\\\"background-color\\\"", json)[1]
  if(length(text_color_ind) && !length(grep("null", json[text_color_ind]))){
    tmp_tc <- gsub("\\\"", "", json[text_color_ind])
    tmp_tc <- gsub(" +text-color:", "", tmp_tc)
    tmp_tc <- gsub(" +", "", tmp_tc)
    text_color <- gsub(",", "", tmp_tc)
  }else{
    text_color <- NA
  }

  if(length(background_color_ind) && !length(grep("null", json[background_color_ind]))){
    tmp_bg <- gsub("\\\"", "", json[background_color_ind])
    tmp_bg <- gsub(" +background-color:", "", tmp_bg)
    tmp_bg <- gsub(" +", "", tmp_bg)
    background_color <- gsub(",", "", tmp_bg)
  }else{
    background_color <- NA
  }

  text_styles <- grep("\\\"\\w+\\\": \\{", json, value = T)
  styles_sections <- json[(grep("text-styles", json) + 1):length(json)]

  styles <- list()
  for(i in seq_along(text_styles)){
    attrs <- styles_sections[1:7]
    styles_sections <- styles_sections[-(1:7)]
    attrs <- gsub("\\\"", "", attrs)
    attrs <- gsub(": \\{", "", attrs)
    attrs <- gsub(" +", "", attrs)
    styles[[i]] <- attrs[-length(attrs)]
  }
  names(styles) <- sapply(styles, function(x){x[1]})
  sections_list <- lapply(styles, function(chunk){
    chunk <- chunk[-1]
    names(chunk) <- 1:num_attrs
    lapply(chunk, function(keyval){
      attr <- strsplit(keyval, ":")[[1]]
      attr[2] <- gsub(",", "", attr[2])
      attr
    })
  })
  out_list <- list(text_color, background_color, sections_list)
  names(out_list) <- c("text-color", "background-color", "sections-list")
  out_list
}

#' Generate LaTeX code for pandoc highlight-style list
#'
#' @param theme The pandoc highlight-style theme to make latex for
#' @param pandoc_path If provided, the path in which pandoc executable resides,
#' if `NULL`, `Sys.getenv("RSTUDIO_PANDOC")` will be used to attempt to provide the path
#'
#' @return A vector of lines of latex code representing the highlight-style for pandoc
#' @importFrom grDevices col2rgb
#' @export
gen_latex_highlight_code <- function(theme = c("pygments",
                                               "tango",
                                               "espresso",
                                               "zenburn",
                                               "kate",
                                               "monochrome",
                                               "breezedark",
                                               "haddock"),
                                     pandoc_path = NULL){

  theme <- match.arg(theme)

  if(is.null(pandoc_path)){
    # Get location of RStudio's pandoc
    pandoc_path <- Sys.getenv("RSTUDIO_PANDOC")
    if(!length(pandoc_path)){
      stop("Cannot find pandoc on your system. You need to have the environment ",
           "variable RSTUDIO_PANDOC set inside R or provide a path in the pandoc_path argument.",
           call. = FALSE)
    }
    pandoc_path <- gsub("Program Files", "Progra~1", pandoc_path)
  }
  if(!dir.exists(pandoc_path)){
    stop("The pandoc_path ", pandoc_path, " does not exist",
         call. = FALSE)
  }

  cmd <- paste(file.path(pandoc_path, "pandoc"),  "--print-highlight-style", theme)
  message("Running pandoc system command:\n", cmd)
  theme_code <- system(cmd, intern = TRUE)

  style_list <- parse_pandoc_highlight_theme(theme_code)

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
}

#' Create latex theme files for all the pandoc highlight-styles
#'
#' @param themes If NULL, run function for all themes. If not NULL, apply to
#' the theme(s) given.
#'
#' @details Source this file and run this function to create all the theme
#' files for the package. Once those have been created, the package has to
#' be re-installed, i.e. load_all("csasdown") will not access the new theme files.
save_latex_theme_code <- function(themes = NULL){

  theme_path <- here::here("inst", "themes")
  all_themes = c("pygments", "tango", "espresso", "zenburn",
                 "kate", "monochrome", "breezedark", "haddock")

  if(is.null(themes)){
    themes = all_themes
  }
  out <- lapply(themes, function(theme){
    file <- paste0(theme, ".latex")
    theme_file <- file.path(theme_path, file)
    theme_latex <- gen_latex_highlight_code(theme)
    writeLines(theme_latex, con = theme_file)
    message("Created theme file ", theme_file, "\n")
  })
}

#' Convert a hex string into a vector of three decimal values (RGB)
#'
#' @param hex The hex string of 6 or 8 digits (if alpha included). May of may not begin with  #
#' @param rel If `TRUE`, divide the RGB values by 255 for relative values
#' @param ret_alpha if `TRUE` alpha value will be included in the output vector as the last item, so it will be
#' length 4 instead of 3
#'
#' @return A vector of three RGB decimal values, or three relative values
#' @export
hex2rgb <- function(hex, rel = FALSE, ret_alpha = FALSE){

  hex <- gsub("#", "", hex)
  if(nchar(hex) != 6 && nchar(hex) != 8){
    stop("hex must be a 6- or 8-digit number", call. = FALSE)
  }
  if(ret_alpha && nchar(hex) != 8){
    hex <- paste0(hex, "ff")
  }
  if(ret_alpha){
    hex_vec <- substring(hex, seq(1, 7, 2), seq(2, 8, 2))
  }else{
    hex_vec <- substring(hex, seq(1, 5, 2), seq(2, 6, 2))
  }
  dec <- strtoi(hex_vec, 16)
  if(rel){
    dec <- dec / 255
  }
  dec
}