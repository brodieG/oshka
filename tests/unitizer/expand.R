library(oshka)

unitizer_sect("simple tests", {
  a <- quote(x > 3)
  b <- quote(x < 10)

  expand(quote(a & b))

  c <- quote(a & b)

  expand(c)

  l <- list(b=quote(x < 1e4), d=quote(b))

  expand(c, l)

  ## In this case we find b from the same level

  expand(quote(d), l)

  # But let's make the env chain more interesting, in this case we find `e` one
  # level past `l`, so then `b` resolves to the next environment instead of in
  # `l`

  env1 <- list2env(list(e=quote(b)))
  expand(quote(e), l, enclos=env1)
})
unitizer_sect("expressions", {
  sub.exp <- as.expression(l)
  expand(expression(c, a && l, sub.exp))
})
unitizer_sect("catch inf rec", {
  a1 <- quote(b1 > 3)
  b1 <- quote(b1 < 10)
  c1 <- quote(a1 & b1)

  expand(c1)

  # Infinite recursion does not happen if symbols are offset by levels, in this
  # case we end up with `a2` unsubstituted at the end because once we find `b2`
  # the `a2` symbol is no longer defined.

  l2 <- list(a2=quote(b2 > 3), c2=quote(a2 & b2))
  b2 <- quote(a2 < 10)

  expand(quote(c2), l2)
})
unitizer_sect("function", {
  fun <- function(x) NULL
  x <- quote(fun)
  z <- quote(b3 + c3)
  local({
    x <- "hello"
    z <- quote(a3 + b3)
    expand(quote(x(z)))
  })
  local({
    x <- quote(fun23adsf)
    z <- quote(a3 + b3)
    expand(quote(x(z)))
  })
  expand(quote(x(z)))
})
unitizer_sect("class shield", {
  a4 <- quote(1 + 1)
  b4 <- quote(2 + 1)

  expand(a4 ~ b4)
  expand(a4 ~ b4, class.shield="AsIs")     # formulas not shielded
  expand(a4 ~ b4, class.shield="formula")
  expand(I(a4 ~ b4), class.shield="AsIs")
  expand(I(a4 ~ b4), class.shield=c("hello", "AsIs"))
  expand(I(a4 ~ b4), class.shield=c("hello", NA_character_))

  expand(bquote(a4 & .(I(quote((b4))))))
  expand(bquote(a4 & .(I(quote(a4 & b4)))))

  xzw <- uvt <- NULL  # make sure not lang objects
  aaa <- quote(xzw > 3)
  bbb <- quote(xzw < 10)
  ccc <- quote(aaa & bbb)

  expand(I(ccc))  # add the `AsIs` class to `ccc` with `I`

  ccd <- bquote(aaa & .(I(quote((bbb)))))
  expand(ccd)

  cce <- ccc
  cce[[3]] <- I(quote((bbb)))
  expand(cce)

  expand(aaa ~ bbb)
  expand(aaa ~ bbb, class.shield=FALSE)

  ## errors

  expand(aaa ~ bbb, class.shield=1:3)
  expand(aaa ~ bbb, class.shield=NA)
})
unitizer_sect("name shield", {

  c5 <- quote(a4 * b4)

  expand(c5, name.shield=c("b4"))
  expand(c5, name.shield=c("b4", "a4"))

  c6 <- quote(base::c5)
  expand(c6)
  expand(c6, name.shield=character())

  c7 <- quote(base:::c5)
  expand(c7)
  expand(c7, name.shield=character())
})

# this next block should probably be last given we can't really `rm` the masked
# `expression` we created

unitizer_sect("expressions", {
  exp.a <- quote(1 + 1)
  exp.b <- quote(2 + 2)

  expand(expression(exp.a, exp.b))
  expand(quote(expression(exp.a, exp.b)))

  my_fun <- function(...) NULL
  expression <- quote(my_fun)

  expand(expression(exp.b, exp.a))
  expand(quote(expression(exp.b, exp.a)))

  expression <- base::expression  # can't `rm`
})

unitizer_sect("tryCatch", {
  a5 <- quote(1 + 8)
  tryCatch(expand(a5))

  local({
    a5 <- quote(1 - 3)
    tryCatch(expand(a5))
  })
})
