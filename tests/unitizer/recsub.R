library(recsub)

unitizer_sect("simple tests", {
  a <- quote(x > 3)
  b <- quote(x < 10)

  recsub(quote(a & b))

  c <- quote(a & b)

  recsub(c)

  l <- list(b=quote(x < 1e4), d=quote(b))

  recsub(c, l)

  ## In this case we find b from the same level

  recsub(quote(d), l)

  # But let's make the env chain more interesting, in this case we find `e` one
  # level past `l`, so then `b` resolves to the next environment instead of in
  # `l`

  env1 <- list2env(list(e=quote(b)))
  recsub(quote(e), l, enclos=env1)
})
unitizer_sect("expressions", {
  sub.exp <- as.expression(l)
  recsub(expression(c, a && l, sub.exp))
})
unitizer_sect("catch inf rec", {
  a1 <- quote(b1 > 3)
  b1 <- quote(b1 < 10)
  c1 <- quote(a1 & b1)

  recsub(c1)

  # Infinite recursion does not happen if symbols are offset by levels, in this
  # case we end up with `a2` unsubstituted at the end because once we find `b2`
  # the `a2` symbol is no longer defined.

  l2 <- list(a2=quote(b2 > 3), c2=quote(a2 & b2))
  b2 <- quote(a2 < 10)

  recsub(quote(c2), l2)
})
