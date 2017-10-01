#' Evaluate Expression After Recursively Substituting It
#'
#' Calls [recsub] on the inputs, and then evaluates the result with [eval].
#' This allows us to implement programmable NSE in functions.  `evalqr`
#' substitutes the expression before calling [recsub] and evaluating it, similar
#' to [evalq].
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
#' xzw <- 8:10
#' aaa <- quote(xzw > 5)
#' bbb <- quote(xzw < 10)
#'
#' evalr(quote(xzw[aaa & bbb]))
#' evalqr(xzw[aaa & bbb])
#'
#' ## Add an interceding frame; here we want to ensure that the
#' ## variable used is the one in the data.frame, not the one we
#' ## just defined above.
#' DF <- data.frame(xzw=5:7, yac=letters[1:3], stringsAsFactors=FALSE)
#' DF$xzw    # use this one
#' xzw       # not this one
#' evalqr(DF[aaa & bbb, ,drop=FALSE])            # incorrect
#' evalqr(DF[aaa & bbb, ,drop=FALSE], envir=DF)  # correct
#'
#' ## Implement programmable NSE in a function; use `substitute` to capture
#' ## input unevaluated
#' subset2 <- function(x, subset) {
#'   sub.val <- evalr(substitute(subset), envir=x, enclos=parent.frame())
#'   x[!is.na(sub.val) & sub.val, ]
#' }
#' subset2(DF, aaa & yac < 'c')
#'
#' ## Use `recsub` to see the expanded expression:
#' recsub(quote(aaa & yac < 'c'), envir=DF)

evalr <- function(
  expr, envir=parent.frame(),
  enclos=if(is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv()
) {
  envir.proc <- env_resolve(envir, enclos, internal=TRUE)
  eval(recsub(expr, envir=envir.proc), envir=envir.proc)
}

#' @export
#' @rdname evalr

evalqr <- function(
  expr, envir=parent.frame(),
  enclos=if(is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv()
) {
  envir.proc <- env_resolve(envir, enclos, internal=TRUE)
  eval(recsub(substitute(expr), envir=envir.proc), envir=envir.proc)
}
