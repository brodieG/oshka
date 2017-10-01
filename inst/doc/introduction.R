## ----global_options, echo=FALSE------------------------------------------
knitr::opts_chunk$set(error=TRUE, comment=NA)
library(recsub)

## ------------------------------------------------------------------------
subset(iris, Sepal.Width > 4.1)

## ------------------------------------------------------------------------
my.exp.a <- quote(Sepal.Width > 4.1)
subset(iris, my.exp.a)

## ------------------------------------------------------------------------
subset2 <- function(x, subset) {
  sub.exp <- recsub(substitute(subset), x, parent.frame())
  sub.val <- eval(sub.exp, x, parent.frame())
  x[!is.na(sub.val) & sub.val, ]
}
subset2(iris, my.exp.a)

## ----rec_ex_1------------------------------------------------------------
my.exp.b <- quote(Species == 'virginica')
my.exp.c <- quote(Sepal.Width > 3.6)
my.exp.d <- quote(my.exp.b & my.exp.c)

subset2(iris, my.exp.d)

## ------------------------------------------------------------------------
subset3 <- function(x, subset, select, drop=FALSE) {
  frm <- parent.frame()  # as per note in ?parent.frame, better to call here
  sub.q <- recsub(substitute(subset), x, frm)
  sel.q <- recsub(substitute(select), x, frm)
  eval(bquote(base::subset(.(x), .(sub.q), .(sel.q), drop=.(drop))), frm)
}

## ------------------------------------------------------------------------
col <- quote(Sepal.Length)
sub <- quote(Species == 'setosa')

subset3(iris, sub & col > 5.5, col:Petal.Length)

## ------------------------------------------------------------------------
col.a <- quote(I_dont_exist)
col.b <- quote(Sepal.Length)
sub.a <- quote(stop("all hell broke loose"))
sub.b <- quote(stop("all hell broke loose"))
threshold <- 3.35

local({
  col.a <- quote(Sepal.Width)
  sub.a <- quote(Species == 'virginica')
  subs <- list(sub.a, quote(Species == 'versicolor'))

  lapply(
    subs,
    function(x) subset3(iris, x & col.a > threshold, col.b:Petal.Length)
  )
})

## ---- eval=FALSE---------------------------------------------------------
#  my_fun_inner <- function(x) {
#    # ... bunch of code
#    stop("end")
#  }
#  my_fun_outer <- function(x) {
#    eval(bquote(.(my_fun)(.(x))), parent.frame())
#  }
#  my_fun_outer(mtcars)
#  traceback()

## ---- eval=FALSE---------------------------------------------------------
#  sapply(.traceback(), head, 1)
#  sapply(sys.calls(), head, 1)  # sys.calls is similarly affected

## ----eval=FALSE----------------------------------------------------------
#  rlang.b <- quo(Species == 'virginica')
#  rlang.c <- quo(Sepal.Width > 3.6)
#  rlang.d <- quo(!!rlang.b & !!rlang.c)
#  
#  dplyr::filter(iris, !!rlang.d)

## ----rec_ex_1, eval=FALSE------------------------------------------------
#  my.exp.b <- quote(Species == 'virginica')
#  my.exp.c <- quote(Sepal.Width > 3.6)
#  my.exp.d <- quote(my.exp.b & my.exp.c)
#  
#  subset2(iris, my.exp.d)

## ----eval=FALSE----------------------------------------------------------
#  rlang_virginica <- function(subset) {
#    subset <- enquo(subset)
#    dplyr::filter(iris, Species == 'virginica' & !!subset)
#  }

## ----eval=FALSE----------------------------------------------------------
#  recub_virginica <- function(subset) {
#    subset <- bquote(Species == 'virginica' & .(substitute(subset)))
#    eval(bquote(.(subset2)(iris, .(subset))), parent.frame())
#  }

