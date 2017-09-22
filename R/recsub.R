## Helper function for recsub
##
## @symbols character vector of symbols encountered so far

#' @importFrom utils tail

recsub_int <- function(lang, envir, symbols=NULL) {
  if(is.symbol(lang)) {
    symb.as.chr <- as.character(lang)
    if(nzchar(symb.as.chr)) {
      if(symb.as.chr %in% symbols)
        stop(
          "Potential infinite recursion detected substituting symbol `",
          symb.as.chr, "`"
        )

      lang.sub <- get_with_env(symb.as.chr, envir=envir)

      if(!is.null(lang.sub) && is.language(lang.sub$obj)) {
        # track all symbols detected at this env level so we can detect an
        # infinite recursion
        symbols <- if(identical(envir, lang.sub$envir)) c(symbols, symb.as.chr)
        recsub_int(lang.sub$obj, envir=lang.sub$envir, symbols=symbols)
      }
      else lang
    } else lang
  } else if (is.language(lang)) {
    lang.el.seq <- seq_along(lang)
    # Skip function symbol if in call
    loop.over <- if(is.expression(lang)) lang.el.seq else tail(lang.el.seq, -1L)
    for(i in loop.over)
      lang[[i]] <- recsub_int(lang[[i]], envir=envir, symbols=symbols)
    lang
  } else lang
}
#' Find Symbol and Environment it is Bound in
#'
#' Like `get`, except that it returns the symbol value and the environment it
#' was found in.
#'
#' @export
#' @param symb.chr a character(1L) representation of symbol name
#' @param mode "any" or "function"
#' @return a list with the object and environment it was found in if there was
#'   one, NULL otherwise

get_with_env <- function(symb.chr, envir, mode="any") {
  stopifnot(mode %in% c("any", "function"), is.environment(envir))
  if(!identical(envir, emptyenv())) {
    # checking for NULL alone is not sufficient
    ex.try <- try(exists(symb.chr, envir=envir, inherits=FALSE))
    if(inherits(ex.try, "try-error")) {
      # nocov start
      stop("Internal error: exists failed, envir type: ", typeof(envir))
      # nocov end
    }
    if(ex.try) {
      obj.val <- tryCatch(
        envir[[symb.chr]],
        error=function(e) stop(
          "Error evaluating promise for symbol `", symb.chr, "` in ",
          "environment ", envir
        )
      )
      if(mode == "function" && mode(obj.val) != function) {
        get_with_env(symb.chr, envir=parent.env(envir))
      } else list(obj=obj.val, envir=envir)
    } else get_with_env(symb.chr, envir=parent.env(envir))
  }
}
#' Recursively Substitute Symbols in Quoted Language
#'
#' Recursively substitutes symbols in quoted language (i.e. `typeof(x) %in%
#' c("expression", "language", "symbol")`) that point to quoted language objects
#' until the resulting language object only contains symbols that point to
#' non-language objects.  The examples are easier to understand than the prior
#' sentence, so you are encouraged to look at those.
#'
#' The expansion of quoted language via recursive substitution allows the
#' implementation of programmable Non-Standard Evaluation (NSE hereafter).
#' Users can create complex quoted language expressions from simple ones by
#' combining them as they would tokens in standard R expressions.  Then, a
#' programmable NSE aware function can use `recsub` or [evalr] to expand the
#' quoted language into usable form.
#'
#' During the recursive substitution, symbols are looked up through the search
#' path in the same way as standard R evaluation looks up symbols.  One subtlety
#' is that if symbol A expands to a language object B, the symbols in
#' language object B are looked for starting from the environment that A is
#' bound to, not the topmost environment.  This is because presumably the
#' process that resulted in language object B cannot be aware of what the child
#' environments of A and B's environment will be at runtime.
#'
#' Symbols at the first position in function calls are not substituted (i.e. in
#' `fun(x, y)` the `fun` is not eligible for substitution).
#'
#' @export
#' @seealso [evalr]
#' @inheritParams evalr
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
#' ## But notice what happens if we use `quote(c)` instead of
#' ## just `c`.  This is because in this case `recsub` must
#' ## look for the `c` symbol in the search path, and once
#' ## it finds it it looks for `a` and `b` starting from the
#' ## environment `c` is bound to.
#'
#' recsub(quote(ccc), l)

recsub <- function(
  expr, envir=parent.frame(),
  enclos=if(is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv()
) {
  if(!is.language(expr)) {
    expr
  } else {
    envir.proc <- env_resolve(envir, enclos, internal=TRUE)
    recsub_int(expr, envir=envir.proc, symbols=character())
  }
}
