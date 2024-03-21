#' Checks whether or not the current version of Pandoc installed is below a
#' given version
#'
#' @param ver The version to compare the current version against. If `NULL`,
#' an error is thrown
#'
#' @return If the current version of Pandoc is prior to this dotted string
#' version, `TRUE`. If not `FALSE` is returned.
#' @export
pandoc_curr_ver_is_before <- function(ver = "3.1.8"){

  if(is.null(ver)){
    stop("Must supply a version string to compare (`ver`)")
  }
  ver_curr <- as.character(rmarkdown::pandoc_version())
  ver_curr_vec <- strsplit(ver_curr, "\\.")[[1]] |> as.numeric()
  ver_curr_len <- length(ver_curr_vec)

  ver_vec <- strsplit(ver, "\\.")[[1]] |> as.numeric()
  ver_vec_len <- length(ver_vec)
  # Add NAs to the end of the comparison version if the current pandoc
  # version has more dotted numbers than the comparison version
  length(ver_vec) <- length(ver_curr_vec)
  # Change the NAs to zeros if there are any
  # For example if ver originally was 3.1.8 and pandoc version (ver_curr) was
  # 3.1.12.1 then ver will be 3.1.8.0 to match in length the pandoc version
  # after this next call
  ver_vec[is.na(ver_vec)] <- 0

  # Compare each chunk of the dotted version number. Can return `TRUE` before
  # finishing all the compares if the current version chunk's value is less than
  # the comparison chunk's value or `FALSE` if the comparison chunk's value is
  # less than the current version chunk's value
  for(i in seq_along(ver_vec)){
    if(ver_curr_vec[i] != ver_vec[i]){
      if(ver_curr_vec[i] < ver_vec[i]){
        return(TRUE)
      }
      if(ver_vec[i] < ver_curr_vec[i]){
        return(FALSE)
      }
    }
  }

  FALSE
}
