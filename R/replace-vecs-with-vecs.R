#' Inject vectors of strings into another vector replacing the elements
#' given the start and end indices in the main vector
#'
#' @details
#' Algorithm - Make a list of vector slices of the `main_vec` that are not
#' being replaced, than interleave the vectors in `repl_vecs` with those.
#' If the `start_inds` gives a value of 1 for the initial vector to replace,
#' that is a special case, and there is code to capture that and do it
#' correctly. The resulting list is flattened and returned.
#'
#' @param main_vec The vector to insert into
#' @param repl_vecs A list of vectors to be inserted in one or more places
#' @param start_inds A vector of indices at which to insert the vectors.
#' This argument will have `sort(unique())` run on it
#' @param end_inds A vector of indices to mark the end of insertion.
#' This argument will have `sort(unique())` run on it
#'
#' @keywords internal
#'
#' @return A vector of strings
replace_vecs_with_vecs <- function(main_vec = NULL,
                                   repl_vecs = NULL,
                                   start_inds = NULL,
                                   end_inds = NULL){

  if(is.null(main_vec)){
    bail(csas_color("main_vec"), " cannot be ", csas_color("NULL"))
  }
  if(is.null(repl_vecs)){
    bail(csas_color("repl_vecs"), " cannot be ", csas_color("NULL"))
  }
  if(is.null(start_inds)){
    bail(csas_color("start_inds"), " cannot be ", csas_color("NULL"))
  }
  if(is.null(end_inds)){
    bail(csas_color("end_inds"), " cannot be ", csas_color("NULL"))
  }
  if(any(is.na(main_vec))){
    bail(csas_color("main_vec"), " cannot contain ", csas_color("NA"))
  }
  if(any(is.na(repl_vecs))){
    bail(csas_color("repl_vecs"), " cannot contain ", csas_color("NA"))
  }
  if(any(is.na(start_inds))){
    bail(csas_color("start_inds"), " cannot contain ", csas_color("NA"))
  }
  if(any(is.na(end_inds))){
    bail(csas_color("end_inds"), " cannot contain ", csas_color("NA"))
  }

  start_inds <- sort(unique(start_inds))
  end_inds <- sort(unique(end_inds))

  # `keep_lst` contains the parts of the vector not spanned by the range of
  #  `start_inds` to `end_inds`
  keep_lst <- map(seq_along(start_inds), ~{
    if(.x == 1){
      if(start_inds[1] != 1){
        main_vec[1:(start_inds[1] - 1)]
      }else{
        NULL
      }
    }else{
      if((end_inds[.x - 1] + 1) <= (start_inds[.x] - 1)){
        main_vec[(end_inds[.x - 1] + 1):(start_inds[.x] - 1)]
      }else{
        NULL
      }
    }
  })
  if(end_inds[length(end_inds)] < length(main_vec)){
    keep_lst <- c(keep_lst,
                  list(main_vec[(end_inds[length(end_inds)] + 1):length(main_vec)]))
  }

  # Interleave the `repl_lst` vectors
  out_lst <- imap(keep_lst, ~{
    if(.y == 1 && start_inds[1] == 1){
      c(repl_vecs[[.y]], .x)
    }else{
      if(.y <= length(repl_vecs)){
        c(.x, repl_vecs[[.y]])
      }else{
        .x
      }
    }
  })
  out_lst <- out_lst[lengths(out_lst) > 0]
  unlist(out_lst)
}
