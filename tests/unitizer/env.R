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
