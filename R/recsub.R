## Helper function for recsub
##
## @symbols character vector of symbols encountered so far

recsub_int <- function(lang, envir, symbols=NULL) {
  if(is.symbol(lang)) {
    symb.as.chr <- as.character(lang)
    if(nzchar(symb.as.chr)) {
      if(symb.as.chr %in% symbols)
        stop(
          "Potential infinite recursion detected substituting symbol `",
          symb.as.chr, "`"
        )

      lang.sub <- gets(symb.as.chr, envir=envir)
      symbols <- if(identical(envir, lang.sub$envir)) c(symbols, symb.as.chr)

      # NOTE: need to think about the exact structure of symbols, the main thing
      # we need to worry about is if a symbol resolves to itself within a single
      # environment.

      if(!is.null(lang.sub) && is.language(lang.sub$obj))
        recsub_int(lang.sub$obj, envir=lang.sub$envir, symbols=symbols)
      else lang
    } else lang
  } else if (is.language(lang)) {
    if(length(lang > 1L)) {
      for(i in tail(seq_along(lang), -1L))
        lang[[i]] <- recsub_int(lang[[i]], envir=envir, symbols=symbols)
    }
    lang
  } else lang
}
## Find a symbol binding in environments
##
## Like `get`, except that it returns the symbol value and the environment it
## was found in.
##
## @param symb.chr a character(1L) representation of symbol name

gets <- function(symb.chr, envir) {
  if(!identical(envir, emptyenv())) {
    # checking for NULL alone is not sufficient
    if(exists(symb.chr, envir=envir, inherit=FALSE)) {
      list(obj=envir[[symb.chr]], envir=envir)
    } else {
      gets(symb.chr, envir=parent.env(envir))
    }
  }
}
#' Recursively Substitute Language
#'
#' Takes R language objects and recursively substitutes symbols therein that
#' point to other symbols with the other symbols.  Substitution stops when
#' either a symbol does not point to anything, or points to something that is
#' not a language object (i.e. symbol, quoted language, or expression object).
#'
#' Symbols are looked up first in `envir` and then through the parent
#' environments chain.  Each symbol lookup is always done from `envir`, even if
#' we are in the middle of a recursive symbol substitution and the previously
#' expanded symbol is several steps down the search path (this is likely to
#' change).
#'
#' Symbols at the first position in function calls are not substituted (i.e. in
#' `fun(x, y)` the `fun` is not eligible for substitution).
#'
#' @export
#' @seealso [eval_r]
#' @inheritParams eval_r
#' @return If the input is a language object, that object with all symbols
#'   recursively substituted, otherwise the input unchanged.
#' @examples
#' a <- quote(x > 3)
#' b <- quote(x < 10)
#' c <- quote(a & b)
#' recsub(c)
#'
#' ## You can place list like objects in the search path
#' l <- list(b=quote(x < 1e4), d=quote(b))
#' recsub(c, l)
#'
#' ## Notice how the symbol search always starts with `l`,
#' ## i.e. after we find and expand `d`, we look for `b`
#' ## in `l` first, not in `enclos` where `b` is `x < 10`
#' recsub(quote(d), l)

recsub <- function(
  expr, envir=parent.frame(),
  enclos=if(is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv()
) {
  if(!is.language(expr)) {
    expr
  } else {
    # construct the evaluation chain depending on whether `envir` is an
    # environment or a list

    if(!is.environment(enclos))
      stop("Argument `enclos` must be an environment.")

    env.proc <- if(!is.environment(envir)) {
      if(!is.list(envir) && !is.pairlist(envir)) {
        stop("Argument `envir` must be `environment`, `list`, or `pairlist`")
      } else {
        # In theory this should not copy any of the contents of the list or pair
        # lists so should be a cheap operation
        env.list <- if(is.pairlist(envir)) as.list(envir) else envir
        if(!all(names(nzchar(env.list))))
          stop(
            "Argument `envir` may not have \"\" as a name for any elements ",
            "when it is a list or a pairlist."
          )
        list2env(env.list, parent=enclos)
      }
    } else envir
    # Do the substitution as needed

    recsub_int(expr, env.proc, symbols=character())
  }
}
#' Recursively Substitute Language and Evaluate
#'
