library(recsub)

unitizer_sect("basic eval", {
  a <- quote(x > 5)
  b <- quote(x < 10)
  x <- 9:10

  evalr(quote(x[a & b]))

  ## Add an interceding frame; here we want to ensure that the
  ## variable used is the one in the data.frame, not the one we
  ## just defined above.
  DF <- data.frame(x=5:7, y=letters[1:3], stringsAsFactors=FALSE)
  DF$x    # use this one
  x       # not this one
  evalr(quote(DF[a & b, ,drop=FALSE]))            # incorrect
  evalr(quote(DF[a & b, ,drop=FALSE]), envir=DF)  # correct
})
unitizer_sect("subset2", {
  ## Implement programmable NSE in a function; use `substitute` to capture
  ## input unevaluated
  subset2 <- function(x, subset) {
    sub.val <- evalr(substitute(subset), envir=x, enclos=parent.frame())
    x[!is.na(sub.val) & sub.val, ]
  }
  subset2(DF, a & y < 'c')
})
