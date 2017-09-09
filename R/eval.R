#' Evaluate Expression After Recursively Substituting It
#'
#' Calls [recsub] on with the inputs, and then evaluates the result.
#'
#' @export
#' @param expr an object to be evaluated, see [eval].
#' @param envir
#' @param enclos
#' @return The result of evaluating the object: for an expression vector this is
#' the result of evaluating the last element.

eval_r <- function(
  expr, envir=parent.frame(),
  enclos=if(is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv()
) {
  eval(recsub(expr, envir, enclos), envir, enclos)
}
