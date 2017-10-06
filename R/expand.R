## Helper function for expand
##
## @symbols character vector of symbols encountered so far

#' @importFrom utils tail

expand_int <- function(lang, envir, symbols=NULL, mode, shield) {
  if(
    !is.object(lang) ||
    (is.logical(shield) && !shield) ||
    (is.character(shield) && !any(class(lang) %in% shield))
  ) {
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
        lang <- expand_int(
          lang.sub$obj, envir=lang.sub$envir, symbols=symbols, mode="any",
          shield=shield
        )
      }
    } else if (is.language(lang)) {
      lang.el.seq <- seq_along(lang)
      # special function symbol if in call
      loop.over <- if(is.expression(lang)) lang.el.seq else tail(lang.el.seq, -1L)
      for(i in seq_along(lang)) {
        mode <- if(i == 1L && !is.expression(lang)) "function" else "any"
        lang[[i]] <- expand_int(
          lang[[i]], envir=envir, symbols=symbols, mode=mode, shield=shield
        )
  } } }
  lang
}
#' Recursively Expand Symbols in Quoted Language
#'
#' Finds symbols in quoted R language objects and recursively replaces them with
#' any language objects that those symbols point to.  This leads to an expanded
#' language object that can be evaluated.  Language objects are objects of type
#' "symbol", "language", or "expression", though only unclassed language
#' is expanded by default.
#'
#' @section Programmable NSE:
#'
#' The expansion can be used to implement programmable Non-Standard Evaluation
#' (NSE hereafter).  Users can create complex quoted language expressions from
#' simple ones by combining them as they would tokens in standard R expressions.
#' Then, a programmable NSE aware function can use `expand` to turn the quoted
#' language into usable form.  See examples.
#'
#' @section Expansion mechanics:
#'
#' During the recursive expansion, symbols are looked up through the search
#' path in the same way as standard R evaluation looks up symbols.  One subtlety
#' is that if symbol A expands to a language object B, the symbols in
#' language object B are looked for starting from the environment that A is
#' bound to, not the initial evaluation environment.  Expansion stops at symbols
#' that point to non-language objects.
#'
#' Symbols at the first position in calls (e.g. `fun` in `fun(x, y)`) are
#' expanded slightly differently: they will continue to be expanded until an
#' object of mode "function" is found.  This is to follow the semantics of
#' symbol searches in R where a symbol pointing to a non-function object will
#' not mask a symbol pointing to a function object when it is used as the name
#' of the function in a call.
#'
#' You can prevent expansion of portions of language by giving that language a
#' class since the default behavior is to keep classed language unexpanded.
#' This is why formulas are not expanded by default.  Be careful though that you
#' do not give a symbol a class as that is bad practice and will become an R
#' runtime error in the future.  See the `shield` parameter and examples.
#'
#' See examples and `browseVignettes('oshka')` for more details.
#'
#' @export
#' @inheritParams base::eval
#' @param shield TRUE, FALSE, or character, determines what portions of quoted
#'   language are shielded from expansion.  TRUE, the default, means that any
#'   any classed language (e.g. formula) will be left unexpanded.  If FALSE all
#'   language will be expanded, irrespective of class.  If character, then any
#'   classed objects with classes in the vector will be left unexpanded, and all
#'   others will be expanded.
#' @return If the input is a language object, that object with all symbols
#'   recursively expanded, otherwise the input unchanged.
#' @examples
#' xzw <- uvt <- NULL  # make sure not lang objects
#' aaa <- quote(xzw > 3)
#' bbb <- quote(xzw < 10)
#' ccc <- quote(aaa & bbb)
#' expand(ccc)
#'
#' ## You can place list like objects in the search path
#' l <- list(bbb=quote(uvt < 9999))
#' expand(ccc, l)
#'
#' ## But notice what happens if we use `quote(ccc)` instead of
#' ## just `ccc`.  This is because in this case `expand` must
#' ## look for the `ccc` symbol in the search path, and once
#' ## it finds it it looks for `aaa` and `bbb` starting from the
#' ## environment `ccc` is bound to, so the `bbb` defined
#' ## inside `l` is skipped.
#' expand(quote(ccc), l)
#'
#' ## Implementing an NSE fun (see vignettes for detailed
#' ## examples)
#' subset2 <- function(x, subset) {
#'   subset <- expand(substitute(subset), x, parent.frame())
#'   eval(bquote(base::subset(.(x), .(subset))), parent.frame())
#' }
#' subset2(iris, Sepal.Width > 4.3)
#' iris.sub <- quote(Sepal.Width > 4.3)
#' subset2(iris, iris.sub)
#'
#' ## Shielding
#' expand(I(ccc))  # add the `AsIs` class to `ccc` with `I`
#' expand(ccc)
#'
#' ## Notice extra set of parentheses we use around
#' ## `quote((bbb))` as otherwise we would attach attributes
#' ## to a symbol:
#' ccd <- bquote(aaa & .(I(quote((bbb)))))
#' expand(ccd)
#'
#' ## Equivalently
#' cce <- ccc
#' cce[[3]] <- I(quote((bbb)))
#' expand(cce)
#'
#' ## Formulas not expanded by default, but can be forced
#' ## to expand by setting `shield` to FALSE
#' expand(aaa ~ bbb)
#' expand(aaa ~ bbb, shield=FALSE)

expand <- function(
  expr, envir=parent.frame(),
  enclos=if(is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv(),
  shield=TRUE
) {
  if(
    !is.character(shield) &&
    (!is.logical(shield) || !isTRUE(shield %in% c(TRUE, FALSE)))
  )
    stop("Argument `shield` must be TRUE, FALSE, or character")
  envir.proc <- env_resolve(envir, enclos, internal=TRUE)
  expand_int(
    expr, envir=envir.proc, symbols=character(), mode="any", shield=shield
  )
}
