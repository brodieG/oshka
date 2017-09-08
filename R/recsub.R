## Helper function for recsub
##
## @symbols character vector of symbols encountered so far

recsub_int <- function(lang, envir, symbols) {
  if(is.symbol(lang)) {
    symb.as.chr <- as.character(lang)

    if(symb.as.chr %in% symbols)
      stop(
        "Potential infinite recursion detected substituting symbol `",
        symb.as.chr, "`"
      )

    lang.sub <- tryCatch(get(symb.as.chr, envir=envir), error=function(e) NULL)

    if(is.language(lang.sub))
      recsub_int(lang.sub, envir, symbols=c(symbols, symb.as.chr))
    else lang
  } else if (is.language(lang)) {
    if(length(lang > 1L)) {
      for(i in tail(seq_along(lang), -1L))
        lang[[i]] <- recsub_int(lang[[i]], envir=envir, symbols=symbols)
    }
    lang
  } else lang
}
#' Recursively Substitute Language
#'
#' Used to implement programmable Non-Standard Evaluation
#'
#' @export
#' @examples
#' a <- quote(x > 3)
#' b <- quote(x < 10)
#' c <- quote(a & b)
#' recsub(c)

recsub <- function(
  expr, envir=parent.frame(),
  enclos=if(is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv()
) {
  x.sub <- substitute(expr)
  if(!is.language(x.sub)) {
    x.sub
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
        list2env(env.list, parent=enclos())
      }
    } else envir
    # Do the substitution as needed

    recsub_int(x.sub, env.proc, symbols=character())
  }
}
