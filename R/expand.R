## Helper function for recsub
##
## @symbols character vector of symbols encountered so far

#' @importFrom utils tail

recsub_int <- function(lang, envir, symbols=NULL, mode) {
  if(is.symbol(lang)) {
    symb.as.chr <- as.character(lang)
    if(symb.as.chr %in% symbols)
      stop(
        "Potential infinite recursion detected substituting symbol `",
        symb.as.chr, "`"
      )

    lang.sub <- get_with_env(symb.as.chr, envir=envir, mode=mode)

    if(!is.null(lang.sub) && is.language(lang.sub$obj)) {
      # track all symbols detected at this env level so we can detect an
      # infinite recursion
      symbols <- if(identical(envir, lang.sub$envir)) c(symbols, symb.as.chr)
      recsub_int(
        lang.sub$obj, envir=lang.sub$envir, symbols=symbols, mode="any"
      )
    }
    else lang
  } else if (is.language(lang)) {
    lang.el.seq <- seq_along(lang)
    # special function symbol if in call
    loop.over <- if(is.expression(lang)) lang.el.seq else tail(lang.el.seq, -1L)
    for(i in seq_along(lang)) {
      mode <- if(i == 1L && !is.expression(lang)) "function" else "any"
      lang[[i]] <-
        recsub_int(lang[[i]], envir=envir, symbols=symbols, mode=mode)
    }
    lang
  } else lang
}
#' Recursively Expands Symbols in Quoted Language
#'
#' Recursively Expands symbols in quoted language (i.e. `typeof(x)` one of
#' "expression", "language", "symbol") that point to quoted language objects.
#' The resulting substituted language object can then be evaluated.  See
#' examples.
#'
#' The recursive expansion of quoted language via allows the implementation of
#' programmable Non-Standard Evaluation (NSE hereafter).  Users can create
#' complex quoted language expressions from simple ones by combining them as
#' they would tokens in standard R expressions.  Then, a programmable NSE aware
#' function can use `recsub` to expand the quoted language into usable form.
#'
#' During the recursive expansion, symbols are looked up through the search
#' path in the same way as standard R evaluation looks up symbols.  One subtlety
#' is that if symbol A expands to a language object B, the symbols in
#' language object B are looked for starting from the environment that A is
#' bound to, not the initial evaluation environment.
#'
#' Symbols at the first position in calls (e.g. `fun` in `fun(x, y)`) are
#' expanded slightly differently: they will continue to be expanded until an
#' object of mode "function" is found.  This is to follow the semantics of
#' symbol searches in R where a symbol pointing to a non-function object will
#' not mask a symbol pointing to a function object when it is used as the name
#' of the function in a call.
#'
#' See examples and `browseVignettes('recsub')` for more details.
#'
#' @export
#' @inheritParams base::eval
#' @return If the input is a language object, that object with all symbols
#'   recursively substituted, otherwise the input unchanged.
#' @examples
#' xzw <- uvt <- NULL  # make sure not lang objects
#' aaa <- quote(xzw > 3)
#' bbb <- quote(xzw < 10)
#' ccc <- quote(aaa & bbb)
#' recsub(ccc)
#'
#' ## You can place list like objects in the search path
#' l <- list(bbb=quote(uvt < 9999))
#' recsub(ccc, l)
#'
#' ## But notice what happens if we use `quote(ccc)` instead of
#' ## just `ccc`.  This is because in this case `recsub` must
#' ## look for the `ccc` symbol in the search path, and once
#' ## it finds it it looks for `aaa` and `bbb` starting from the
#' ## environment `ccc` is bound to, so the `bbb` defined
#' ## inside `l` is skipped.
#' recsub(quote(ccc), l)
#'
#' ## Implementing an NSE fun (see vignettes for detailed
#' ## examples)
#' subset2 <- function(x, subset) {
#'   subset <- recsub(substitute(subset), x, parent.frame())
#'   eval(bquote(base::subset(.(x), .(subset))), parent.frame())
#' }
#' subset2(iris, Sepal.Width > 4.3)
#' iris.sub <- quote(Sepal.Width > 4.3)
#' subset2(iris, iris.sub)

recsub <- function(
  expr, envir=parent.frame(),
  enclos=if(is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv(),
  what="all"
) {
  if(!is.language(expr)) {
    expr
  } else {
    envir.proc <- env_resolve(envir, enclos, internal=TRUE)
    recsub_int(expr, envir=envir.proc, symbols=character(), mode="any")
  }
}
