<!-- README.md is generated from README.Rmd. Please edit that file -->



# recsub

[![](https://travis-ci.org/brodieG/recsub.svg?branch=master)](https://travis-ci.org/brodieG/recsub)
[![](https://codecov.io/github/brodieG/recsub/coverage.svg?branch=master)](https://codecov.io/github/brodieG/recsub?branch=master)
[![](http://www.r-pkg.org/badges/version/recsub)](https://cran.r-project.org/package=recsub)
[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)

## Programmable Non-Standard Evaluation

Non-Standard Evaluation (NSE hereafter) occurs when R expressions are
captured and evaluated in a manner different than if they had been executed
without intervention.  `subset` is a canonical example:


```r
subset(iris, Sepal.Width > 4.1)
##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 16          5.7         4.4          1.5         0.4  setosa
## 34          5.5         4.2          1.4         0.2  setosa
```

The expression `Sepal.Width > 4.1` would normally produce
an error because neither `Species` nor `Sepal.Width` exist in the global
environment.  `subset` captures the expression and evaluates it within `iris`
instead of directly in the global environment so we get a meaningful result.

A limitation of standard NSE is that it is difficult to use programmatically.
For example, if we wanted to store a `subset` expression in a variable it would
not work:


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

This package provide tools to write functions that support programmable NSE.  We
can rewrite a simplified version of `subset` as follows:


```r
subset2 <- function(x, subset) {
  sub.val <- eval_r(substitute(subset), envir=x, enclos=parent.frame())
  x[!is.na(sub.val) & sub.val, ]
}
subset2(iris, my.exp)
##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 16          5.7         4.4          1.5         0.4  setosa
## 34          5.5         4.2          1.4         0.2  setosa
```

While this may not seem like much of an improvement to the poor sod writing
`subset2`, the user will benefit from the new feature.  Additionally, because
`eval_r` substitutes language recursively the following is possible:


```r
my.exp.1 <- quote(Species == 'virginica')
my.exp.2 <- quote(Sepal.Width > 3.6)
subset2(iris, my.exp.1 & my.exp.2)
##     Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
## 118          7.7         3.8          6.7         2.2 virginica
## 132          7.9         3.8          6.4         2.0 virginica

my.exp.3 <- quote(sub.exp.1 & sub.exp.2)
subset2(iris, sub.exp.3)
##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 15          5.8         4.0          1.2         0.2  setosa
## 16          5.7         4.4          1.5         0.4  setosa
## 19          5.7         3.8          1.7         0.3  setosa
```

