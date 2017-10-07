<!-- README.md is generated from README.Rmd. Please edit that file -->



# oshka - Simple Programmable NSE

[![](https://travis-ci.org/brodieG/oshka.svg?branch=master)](https://travis-ci.org/brodieG/oshka)
[![](https://codecov.io/github/brodieG/oshka/coverage.svg?branch=master)](https://codecov.io/github/brodieG/oshka?branch=master)
[![](http://www.r-pkg.org/badges/version/oshka)](https://cran.r-project.org/package=oshka)
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)


## Programmable Non-Standard Evaluation

Non-Standard Evaluation (NSE hereafter) occurs when R expressions are
captured and evaluated in a manner different than if they had been executed
without intervention.  `subset` is a canonical example, which we use here with
the built-in `iris` data set:


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
exp.a <- quote(Sepal.Width > 4.1)
subset(iris, exp.a)
## Error in subset.data.frame(iris, exp.a): 'subset' must be logical
```

`oshka::expand` facilitates programmable NSE, as with this simplified
version of `subset`:


```r
subset2 <- function(x, subset) {
  sub.exp <- expand(substitute(subset), x, parent.frame())
  sub.val <- eval(sub.exp, x, parent.frame())
  x[!is.na(sub.val) & sub.val, ]
}
subset2(iris, exp.a)
##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 16          5.7         4.4          1.5         0.4  setosa
## 34          5.5         4.2          1.4         0.2  setosa
```

`expand` is recursive:


```r
exp.b <- quote(Species == 'virginica')
exp.c <- quote(Sepal.Width > 3.6)
exp.d <- quote(exp.b & exp.c)

subset2(iris, exp.d)
##     Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
## 118          7.7         3.8          6.7         2.2 virginica
## 132          7.9         3.8          6.4         2.0 virginica
```

We abide by R semantics so that programmable NSE functions are almost
identical to normal NSE functions, with programmability as a bonus.


## Documentation

* [Intro
  vignette](http://htmlpreview.github.io/?https://github.com/brodieG/oshka/blob/master/inst/doc/introduction.html)
  for a more in depth introduction to `oshka`, including a brief comparison to
  `rlang`.
* [NSE Functions with
  `oshka`](http://htmlpreview.github.io/?https://github.com/brodieG/oshka/blob/master/inst/doc/nse-fun.html)
  in which we recreate simplified versions of `dplyr` and `data.table` that
  implement programmable NSE with `oshka::expand`.

## Installation

This package is currently only available on Github, although we intend to
release it to CRAN shortly.


```r
devtools::instal_github('brodieg/oshka')
```

This package is proof-of-concept.  Feedback is welcome, particularly if you are
aware of some NSE pitfalls we may be ignoring.

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

## About

Brodie Gaslam is a hobbyist programmer based on the US East Coast.

The name of this package is derived from "matryoshka", the Russian nesting
dolls.

