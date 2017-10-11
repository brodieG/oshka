## Helper function for expand
##
## @symbols character vector of symbols encountered so far

#' @importFrom utils tail

expand_int <- function(lang, envir, symbols=NULL, mode, s.c, s.s) {
  if(
    !is.object(lang) ||
    (is.logical(s.c) && !s.c) ||
    (is.character(s.c) && !any(class(lang) %in% s.c))
  ) {
    if(is.symbol(lang)) {
      symb.as.chr <- as.character(lang)
      if(!symb.as.chr %in% s.s) {
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
            s.c=s.c
          )
      } }
    } else if (is.language(lang)) {
      # Proess only non-protected symbols in function call
      if(length(lang) && !(is.symbol(lang[[1]]) && lang[[1]] %in% s.s))
        for(i in seq_along(lang)) {
          mode <- if(i == 1L && !is.expression(lang)) "function" else "any"
          lang[[i]] <- expand_int(
            lang[[i]], envir=envir, symbols=symbols, mode=mode, s.c=s.c
          )
  } } } }
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
#' For more general documentation `browseVignettes('oshka')`.
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
#' You can prevent expansion on portions of language via shielding (see next
#' section).
#'
#' @section Shielding:
#'
#' There are two mechanisms for shielding language from expansion.  The first
#' one is to give language a class.  This is why formulas are not expanded by
#' default.  Be careful though that you do not give a symbol a class as that is
#' bad practice and will become an R runtime error in the future.
#'
#' The second mechanism is to specify symbol names that should not be expanded.
#' This is easier to specify than the class based mechanism, but it is less
#' precise as it applies to all instances of that name.  By default the symbols
#' "::" and ":::" are not expanded.  If a function call has shielded symbol for
#' function name the *entire* call will be shielded.
#'
#' See the `shield` parameter and examples.
#'
#' @export
#' @inheritParams base::eval
#' @param shield a name list with two elements:
#'   * "class": TRUE, FALSE, or character, determines what portions of quoted
#'     language are shielded from expansion.  TRUE, the default, means that any
#'     any classed language (e.g. formula) will be left unexpanded.
#'     If FALSE all language will be expanded, irrespective of class.  If
#'     character, then any classed objects with classes in the vector will be
#'     left unexpanded, and all others will be expanded.
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
  shield=getOption('oskha.shield')
) {
  if(
    !is.list(shield) || length(shield) != 2 ||
    !all(names(shield) %in% c("classes", "symbols"))
  ) {
    stop(
      "Argument `shield` must be a list with named elements ",
      "'class' and 'symbol'."
    )
  }
  s.c <- shield$class
  s.s <- shield$symbol
  if(
    !is.character(s.c) &&
    (!is.logical(s.c) || !isTRUE(s.c %in% c(TRUE, FALSE)))
  )
    stop("`shield$class` must be TRUE, FALSE, or character")

  if(!is.character(s.s) || anyNA(s.s))
    stop("`shield$symbol` must be character without NAs")

  envir.proc <- env_resolve(envir, enclos, internal=TRUE)
  expand_int(
    expr, envir=envir.proc, symbols=character(), mode="any",
    s.c=s.c, s.s=s.s
  )
}
