library(recsub)

unitizer_sect("simple tests", {
  a <- quote(x > 3)
  b <- quote(x < 10)

  recsub(quote(a & b))

  c <- quote(a & b)

  recsub(c)

  l <- list(b=quote(x < 1e4), d=quote(b))

  recsub(c, l)
})
unitizer_sect("catch inf rec", {
  a1 <- quote(b1 > 3)
  b1 <- quote(b1 < 10)
  c1 <- quote(a1 & b1)

  recsub(c1)
})
