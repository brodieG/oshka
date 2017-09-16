## Resolve Non-Environments into Environments
##
## Attempts to follow `eval` semantics
##
## @param internal add one to stack because this has been called within another
##   function

env_resolve <- function(envir, enclos, internal=FALSE) {
  if(!is.environment(enclos))
    stop("Argument `enclos` must be an environment.", call.=sys.call(-2))

  env.proc <- if(!is.environment(envir)) {
    if(!is.list(envir) && !is.pairlist(envir) && !is.numeric(envir)) {
      stop(
        "Argument `envir` must be `environment`, `list`, `pairlist`, ",
        "or `numeric`",
        call=sys.call(-2)
      )
    }
    # In theory this should not copy any of the contents of the list or pair
    # lists so should be a cheap operation

    if(is.numeric(envir)) {
      # In internal mode we need to subtract one from negative numbers because
      # we are calling this fun from within `evalr` or `recsub`.  We only need
      # to do these for strictly neg numbers b/c all others are absolute
      # starting from root of calls

      num.neg <- internal && length(envir) >= 1 && isTRUE(envir[1] < 0)
      if(num.neg) envir[1] <- envir[1] - 1

      env.try <- try(sys.frame(envir))

      if(inherits(env.try, "try-error"))
        stop(
          "Unable to resolve frame with integer `envir`, see prior error.",
          call.=sys.call(-2)
        )
      env.try
    } else {
      env.list <- if(is.pairlist(envir)) as.list(envir) else envir
      if(!all(names(nzchar(env.list))))
        stop(
          "Argument `envir` may not have \"\" as a name for any elements ",
          "when it is a list or a pairlist.",
          call.=sys.call(-2)
        )
      list2env(env.list, parent=enclos)
    }
  } else envir

  env.proc
}

