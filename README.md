<!-- README.md is generated from README.Rmd. Please edit that file -->



# recsub - Programmable Non-Standard Evaluation

[![](https://travis-ci.org/brodieG/recsub.svg?branch=master)](https://travis-ci.org/brodieG/recsub)
[![](https://codecov.io/github/brodieG/recsub/coverage.svg?branch=master)](https://codecov.io/github/brodieG/recsub?branch=master)
[![](http://www.r-pkg.org/badges/version/recsub)](https://cran.r-project.org/package=recsub)
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)


## Programmable NSE

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
identical to normal NSE functions, with programmability as a bonus.


## Additional Documentation

* [Intro
  vignette](http://htmlpreview.github.io/?https://github.com/brodieG/recsub/blob/master/inst/doc/introduction.html)
  for a more in depth introduction to `recsub`, including a brief comparison to
  `rlang`.
* [NSE Functions with
  `recsub`](http://htmlpreview.github.io/?https://github.com/brodieG/recsub/blob/master/inst/doc/nse-fun.html)
  in which we recreate simplified versions of `dplyr` and `data.table` that
  implement programmable NSE with `recsub`.

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

