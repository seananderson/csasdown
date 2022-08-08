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
#' @keywords internal
#' @family highlight-theme
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
  if(length(json) == 1 && "list" %in% class(json)){
    json <- json[[1]]
  }

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

  text_styles <- grep("\\\"\\w+\\\": \\{", json, value = TRUE)

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
