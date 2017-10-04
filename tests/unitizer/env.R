library(recsub)

unitizer_sect("Env levels", {
  ## Key thing here is that we get the same answer from eval and evalr, we use
  ## complicated var name in the hopes it doesn't exist in global env since some
  ## of our looks here can cause lookups in globalenv

  aHrXaasBHJEJKdf <- "top-level"
  fun <- function(n) {
    aHrXaasBHJEJKdf <- "mid-level"
    local({
      aHrXaasBHJEJKdf <- "inner-level"
      c(
        eval=eval(quote(aHrXaasBHJEJKdf), n),
        evalr=eval(recsub(quote(aHrXaasBHJEJKdf), n), n)
      )
    })
  }
  fun(-1)
  fun(-2)
  fun(-3)
  fun(-8)
  fun(-9)
})
unitizer_sect("Functions", {
  ## some stuff gets funky here with `all.equal` in the environment since the
  ## environment being compared is this one, so any objects that we add
  ## automatically breaks the tests
  imafun <- imafun2 <- function() NULL
  local({
    imafun <- TRUE
    recsub:::get_with_env("imafun", envir=environment())
  })
  local({
    imafun <- TRUE
    recsub:::get_with_env("imafun", envir=environment(), mode="function")
  })
  local({
    imafun2 <- quote(a + b)
    recsub:::get_with_env("imafun2", envir=environment(), mode="function")
  })
})
unitizer_sect("Errors", {
  recsub:::env_resolve(letters, new.env())
})
