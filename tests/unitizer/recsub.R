library(subversive)
unitizer_sect("simple tests", {
  a <- quote(x > 3)
  b <- quote(x < 10)
  c <- quote(a & b)

  recsub(c)
})
unitizer_sect("catch inf rec", {
  a1 <- quote(b1 > 3)
  b1 <- quote(b1 < 10)
  c1 <- quote(a1 & b1)

  recsub(c1)
})
