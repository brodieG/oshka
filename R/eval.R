#' Evaluate Expression After Recursively Substituting It
#'
#' Calls [recsub] on the inputs, and then evaluates the result with [eval].
#' This allows us to implement programmable NSE in functions.
#'
#' @seealso [eval], [recsub], `vignette('recsub', 'recsub')`
#' @export
#' @inheritParams base::eval
#' @param expr an object to be evaluated, typically a quoted language object as
#'   returned by `quote` or `substitute`.  `expr` will be recursively
#'   substituted by `recsub`.
#' @return The result of evaluating the object: for an expression vector this is
#'   the result of evaluating the last element.
#' @examples
#' a <- quote(x > 5)
#' b <- quote(x < 10)
#' x <- 9:10
#'
#' eval_r(quote(x[a & b]))
#'
#' ## Add an interceding frame; here we want to ensure that the
#' ## variable used is the one in the data.frame, not the one we
#' ## just defined above.
#' DF$x    # use this one
#' x       # not this one
#' eval_r(quote(DF[a & b, ,drop=FALSE]))            # incorrect
#' eval_r(quote(DF[a & b, ,drop=FALSE]), envir=DF)  # correct
#'
#' ## Implement programmable NSE in a function; use `substitute` to capture
#' ## input unevaluated
#' subset2 <- function(x, subset) {
#'   sub.val <- eval_r(substitute(subset), envir=x, enclos=parent.frame())
#'   x[!is.na(sub.val) & sub.val, ]
#' }
#' subset2(DF, a & y < 'c')
#'
#' ## Use `recsub` to see the expanded expression:
#' recsub(quote(a & y < 'c'), envir=DF)

eval_r <- function(
  expr, envir=parent.frame(),
  enclos=if(is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv()
) {
  eval(recsub(expr, envir, enclos), envir, enclos)
}
