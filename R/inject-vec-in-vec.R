#' Inject a vector of strings into another at given indices, replacing the
#' elements at those indices
#'
#' @description
#' Inject a vector of strings into another at given indices, replacing the
#' elements at those indices, creating a new, longer vector.
#'
#' @param main_vec The vector to insert into
#' @param sec_vec The vector to be inserted in one or more places
#' @param inds The indices at which to insert the vectors
#'
#' @keywords internal
#'
#' @return A vector of strings
#' @export
#'
#' @examples
#' library(csasdown)
#' a <- letters[1:10]
#' b <- LETTERS[25:26]
#' inject_vec_in_vec(a, b, c(2, 5, 8))
inject_vec_in_vec <- function(main_vec, sec_vec, inds){

  # Break up `main_vec` into chunks
  j <- imap(inds, ~{
    if(.y == 1){
      return(main_vec[1:(inds[.y] - 1)])
    }
    main_vec[(inds[.y - 1] + 1):(inds[.y] - 1)]
  })
  j <- append(j, list(main_vec[(inds[length(inds)] + 1):length(main_vec)]))

  # Insert the `sec_vec` between each list element
  out <- NULL
  map(seq_along(j), ~{
    if(.x == length(j)){
      out <<- c(out, j[[.x]])
    }else{
      out <<- c(out, j[[.x]], sec_vec)
    }
  })
  out
}