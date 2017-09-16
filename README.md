<!-- README.md is generated from README.Rmd. Please edit that file -->



# recsub - Programmable Non-Standard Evaluation

[![](https://travis-ci.org/brodieG/recsub.svg?branch=master)](https://travis-ci.org/brodieG/recsub)
[![](https://codecov.io/github/brodieG/recsub/coverage.svg?branch=master)](https://codecov.io/github/brodieG/recsub?branch=master)
[![](http://www.r-pkg.org/badges/version/recsub)](https://cran.r-project.org/package=recsub)
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

## Overview

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
my.exp <- quote(Sepal.Width > 4.1)
subset(iris, my.exp)
## Error in subset.data.frame(iris, my.exp): 'subset' must be logical
```

Instead, we would have to resort to the following contortion:


```r
eval(bquote(subset(iris, .(my.exp))))
##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 16          5.7         4.4          1.5         0.4  setosa
## 34          5.5         4.2          1.4         0.2  setosa
```

`recsub` provides tools to write functions that support programmable NSE, such
as this simplified version of `subset`:


```r
subset2 <- function(x, subset) {
  sub.val <- evalr(substitute(subset), envir=x, enclos=parent.frame())
  x[!is.na(sub.val) & sub.val, ]
}
subset2(iris, my.exp)
##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 16          5.7         4.4          1.5         0.4  setosa
## 34          5.5         4.2          1.4         0.2  setosa
```

While this may not seem like much of an improvement to the poor sod writing
`subset2`, it does make it easy to program with.  Additionally, because `evalr`
substitutes language recursively, the following is possible:


```r
my.exp.1 <- quote(Species == 'virginica')
my.exp.2 <- quote(Sepal.Width > 3.6)
my.exp.3 <- quote(my.exp.1 & my.exp.2)

subset2(iris, my.exp.3)
##     Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
## 118          7.7         3.8          6.7         2.2 virginica
## 132          7.9         3.8          6.4         2.0 virginica
```

`recsub` sacrifices power for simplicity.  For example, the developer must
manage the evaluation environment, but in exchange they get semantics that are
identical to those of `eval`.  Users of functions that implement programmable
NSE with `recsub` must learn to use `quote` and nothing else, and then only if
they want to program with NSE.  The semantics of recursive substitution follow
those of standard evaluation of non-language R objects.

## Development Status

This package is proof-of-concept.  Whether it leaves that state will depend on
how much interest it elicits.

Currently this package is only available on github:


```r
devtools::install_github('brodieg/recsub')
```

## Related Packages

[rlang](https://cran.r-project.org/package=rlang) by Lionel Henry and Hadley
Wickham implements a more comprehensive, but far more complicated version of
programmable NSE.

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

