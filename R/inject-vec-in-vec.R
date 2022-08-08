#' Inject a vector of strings into another at given indices, replacing the
#' elements at those indices
#'
#' @description
#' Inject a vector of strings into another at given indices, replacing the
#' elements at those indices, creating a new, longer vector.
#'
#' @details
#' `inds` is sorted and made unique by this function prior to interleaving
#'
#' @param main_vec The vector to insert into
#' @param sec_vec The vector to be inserted in one or more places
#' @param inds The indices at which to insert the vectors
#'
#' @keywords internal
#'
#' @return A vector of strings
inject_vec_in_vec <- function(main_vec = NULL, sec_vec = NULL, inds = NULL){

  if(is.null(main_vec)){
    bail(csas_color("main_vec"), " cannot be ", csas_color("NULL"))
  }
  if(is.null(sec_vec)){
    bail(csas_color("sec_vec"), " cannot be ", csas_color("NULL"))
  }
  if(is.null(inds)){
    bail(csas_color("inds"), " cannot be ", csas_color("NULL"))
  }
  if(any(is.na(main_vec))){
    bail(csas_color("main_vec"), " cannot contain ", csas_color("NA"))
  }
  if(any(is.na(sec_vec))){
    bail(csas_color("sec_vec"), " cannot contain ", csas_color("NA"))
  }
  if(any(is.na(inds))){
    bail(csas_color("inds"), " cannot contain ", csas_color("NA"))
  }

  inds <- sort(unique(inds))
  if(!all(inds %in% seq_along(main_vec))){
    bail(csas_color("inds"), " extends outside the range of ",
         csas_color("main_vec"))
  }

  out <- NULL
  for(i in seq_along(main_vec)){
    if(i %in% inds){
      out <- c(out, sec_vec)
    }else{
      out <- c(out, main_vec[i])
    }
  }
  out
}