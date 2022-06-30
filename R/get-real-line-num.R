#' Get the line number in the original source file for the first R code
#' line (non-comment line) in a chunk
#'
#' @details
#' If [remove_comments_from_chunks()] was run and had a `pre_num` (in
#' `line_offsets`) value for the chunk that matched the `header`,
#' that will be added to the original `chunk_ind` (in `line_offsets`)
#' value and any additions from [inject_rmd_files()] will be subtracted
#' (`rmd_num` in `line_offsets`).
#'
#' @keywords internal
#'
#' @param header A knitr chunk header that matches the regex
#' `all_patterns$md$chunk.begin`
#' @param line_offsets A [data.frame()] or [tibble::tibble()] containing the
#' line offsets to be added to line numbers in messages in order to give the
#' correct line numbers for the original files. Has columns `fn` (chr),
#' `chunk_header` (chr)  `chunk_ind` (int), `pre_num` (dbl), `post_num` (dbl),
#' and `rmd_num` (dbl). See `return` values for [inject_rmd_files()] and
#' [remove_comments_from_chunks()]
#'
#' @return The line number in the original file
get_real_line_num <- function(header, line_offsets){

  if(is.null(header)){
    bail(csas_color("header"), " cannot be ", csas_color("NULL"))
  }
  if(is.null(line_offsets)){
    bail(csas_color("line_offsets"), " cannot be ", csas_color("NULL"))
  }
  if(!"data.frame" %in% class(line_offsets)){
    bail(csas_color("line_offsets"), " must be a ", csas_color("data.frame"))
  }

  # Calculate cumulative additions/subtractions of lines prior to the
  # one we are interested in

  row_ind <- which(line_offsets$chunk_header == header)
  if(!length(row_ind)){
    bail("There is no row that matches the requested chunk header in the ",
         csas_color("line_offsets"), " data frame. Chunk header is: ",
         csas_color(header))
  }
  if(length(row_ind) > 1){
    bail("There is more than one row that matches the requested chunk ",
         "header in the ",
         csas_color("line_offsets"), " data frame. Chunk header is: ",
         csas_color(header))
  }
  row <- line_offsets[row_ind, ]
  ret_val <- row$chunk_ind + row$pre_num
  if(row_ind < nrow(line_offsets)){
    chunk_diff <- line_offsets[row_ind + 1, ]$chunk_ind -
      line_offsets[row_ind, ]$chunk_ind
    if(chunk_diff > 2){
      ret_val <- ret_val + 1
    }
  }
  ret_val
}