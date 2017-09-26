<!-- README.md is generated from README.Rmd. Please edit that file -->



# recsub - Programmable Non-Standard Evaluation

[![](https://travis-ci.org/brodieG/recsub.svg?branch=master)](https://travis-ci.org/brodieG/recsub)
[![](https://codecov.io/github/brodieG/recsub/coverage.svg?branch=master)](https://codecov.io/github/brodieG/recsub?branch=master)
[![](http://www.r-pkg.org/badges/version/recsub)](https://cran.r-project.org/package=recsub)
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)


## Basic Programmable NSE

Non-Standard Evaluation (NSE hereafter) occurs when R expressions are
captured and evaluated in a manner different than if they had been executed
without intervention.  `subset` is a canonical example:


```r
subset(iris, Sepal.Width > 4.1)
##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 16          5.7         4.4          1.5         0.4  setosa
## 34          5.5         4.2          1.4         0.2  setosa
```

`Sepal.Width` does not exist in the global environment, yet this works because
`subset` captures the expression and evaluates it within `iris`.

A limitation of NSE is that it is difficult to use programmatically:


```r
my.exp.a <- quote(Sepal.Width > 4.1)
subset(iris, my.exp.a)
## Error in subset.data.frame(iris, my.exp.a): 'subset' must be logical
```

`recsub` facilitates programmable NSE, as with this simplified version of
`subset`:


```r
subset2 <- function(x, subset) {
  sub.exp <- recsub(substitute(subset), x, parent.frame())
  sub.val <- eval(sub.exp, x, parent.frame())
  x[!is.na(sub.val) & sub.val, ]
}
subset2(iris, my.exp.a)
##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 16          5.7         4.4          1.5         0.4  setosa
## 34          5.5         4.2          1.4         0.2  setosa
```

`recsub` is recursive:


```r
my.exp.b <- quote(Species == 'virginica')
my.exp.c <- quote(Sepal.Width > 3.6)
my.exp.d <- quote(my.exp.b & my.exp.c)

subset2(iris, my.exp.d)
##     Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
## 118          7.7         3.8          6.7         2.2 virginica
## 132          7.9         3.8          6.4         2.0 virginica
```

We abide by R semantics so that programmable NSE functions are almost
identical to normal NSE functions.  Using it them is the same, with
programmability as a bonus.  Recursive language substitution follows the
same semantics as normal object substitution.

## Forwarding NSE Arguments to NSE Functions

If you wish to write a function that uses a programmable NSE function and
forwards its NSE arguments to it, you must ensure the NSE expressions are
evaluated in the correct environment, typically the `parent.frame()`.  This is
no different than with normal NSE functions.  An example:


```r
subset3 <- function(x, subset, select, drop=FALSE) {
  frm <- parent.frame()  # as per note in ?parent.frame, better to call here
  sub.q <- recsub(substitute(subset), x, frm)
  sel.q <- recsub(substitute(select), x, frm)
  eval(bquote(base::subset(.(x), .(sub.q), .(sel.q), drop=.(drop))), frm)
}
```

We use `bquote` to assemble our substituted call and `eval` to evaluate it in
the correct frame.  The parts of the call that should evaluate in `subset3` are
escaped with `.()`.  This requires some work from the programmer, but the user
reaps the benefits:


```r
col <- quote(Sepal.Length)
sub <- quote(Species == 'setosa')

subset3(iris, sub & col > 5.5, col:Petal.Length)
##    Sepal.Length Sepal.Width Petal.Length
## 15          5.8         4.0          1.2
## 16          5.7         4.4          1.5
## 19          5.7         3.8          1.7
```

The forwarding is robust to unusual evaluation:


```r
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
## [[1]]
##     Sepal.Length Sepal.Width Petal.Length
## 110          7.2         3.6          6.1
## 118          7.7         3.8          6.7
## 132          7.9         3.8          6.4
## 137          6.3         3.4          5.6
## 149          6.2         3.4          5.4
## 
## [[2]]
##    Sepal.Length Sepal.Width Petal.Length
## 86            6         3.4          4.5
```

## Related Packages

[rlang](https://cran.r-project.org/package=rlang) by Lionel Henry and Hadley
Wickham implements a more comprehensive version of programmable NSE.  In
particular it captures NSE expression environments, which among other
thing simplifies the NSE expression forwarding problem.  On the other hand, it
is substantially more complex.

## Development Status

This package is proof-of-concept.  Feedback is welcome, particularly if you are
aware of some NSE pitfalls we may be ignoring.

Currently this package is only available on github:


```r
devtools::install_github('brodieg/recsub')
```

## Acknowledgements

* R Core for developing and maintaining such a wonderful language.
* CRAN maintainers, for patiently shepherding packages onto CRAN and maintaining
  the repository, and Uwe Ligges in particular for maintaining
  [Winbuilder](http://win-builder.r-project.org/).
* [Jim Hester](https://github.com/jimhester) because
  [covr](https://cran.r-project.org/package=covr) rocks.
* [Dirk Eddelbuettel](https://github.com/eddelbuettel) and [Carl
  Boettiger](https://github.com/cboettig) for the
  [rocker](https://github.com/rocker-org/rocker) project, and [Gábor
  Csárdi](https://github.com/gaborcsardi) and the
  [R-consortium](https://www.r-consortium.org/) for
  [Rhub](https://github.com/r-hub), without which testing bugs on R-devel and
  other platforms would be a nightmare.
* Hadley Wickham for [devtools](https://cran.r-project.org/package=devtools) and
  [roxygen2](https://cran.r-project.org/package=roxygen2).
* [Yihui Xie](https://github.com/yihui) for
  [knitr](https://cran.r-project.org/package=knitr) and  [J.J.
  Allaire](https://github.com/jjallaire) etal for
  [rmarkdown](https://cran.r-project.org/package=rmarkdown), and by extension
  John MacFarlane for [pandoc](http://pandoc.org/).

