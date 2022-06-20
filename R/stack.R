#' Push a character to a stack
#'
#' @keywords internal
#'
#' @param stack A 'stack' (character vector). Set to `NULL` for an empty stack
#' @param val The value to push to the stack
#'
#' @return The stack with the new value on the top
stk_push <- function(stack, val){
  c(stack, val)
}

#' Pop a value from the top of a stack
#'
#' @keywords internal
#'
#' @param stack A 'stack' (character vector) or `NULL`
#'
#' @return A list of two elements, the first is the modified stack with the top
#' value removed, the second is the value that was popped off. If the stack is
#' empty, `NULL` is returned
stk_pop <- function(stack){
  if(is.null(stack)){
    return(NULL)
  }
  if(length(stack) == 1){
    return(list(stack = NULL, val = stack[1]))
  }
  list(stack = stack[1:(length(stack) - 1)], val = stack[length(stack)])
}

#' Return the size of the stack
#'
#' @keywords internal
#'
#' @param stack A 'stack' (character vector) or `NULL`
#'
#' @return The size of the stack (zero if the stack is empty)
stk_size <- function(stack){
  if(is.null(stack)){
    return(0)
  }
  length(stack)
}
