---
title: "NSE Functions with `recsub`"
author: "Brodie Gaslam"
output:
    rmarkdown::html_vignette:
        toc: true
        css: styles.css

vignette: >
  %\VignetteIndexEntry{NSE Functions with Recsub}
  %\VignetteEngine{knitr::rmarkdown}
  %\usepackage[utf8]{inputenc}
---

```{r global_options, echo=FALSE}
knitr::opts_chunk$set(error=TRUE, comment=NA)
library(recsub)
```

## Overview

We will implement simplified versions of `dplyr` and `data.table` to illustrate
how to write programmable NSE functions with `recsub`.  The implementations are
intentionally limited in functionality, robustness, and speed for the sake of
simplicity.

Our test data is derived from the `state` dataset that comes pre-loaded with R:

```{r}
head(state.data, 2)
```

## An Ersatz `dplyr`

### Interface

The interface is as follows:

```{r, eval=FALSE}
group_r <- function(x, ...) {...}     # similar to dplyr::group_by
filter_r <- function(x, subset) {...} # similar to dplyr::filter
summarize_r <- function(x, ...) {...} # similar to dplyr::summarise
`%$%` <- function(x, y) {...}         # similar to the magrittr pipe
```
```{r dplyr_extra_0, echo=FALSE}
summarize_r <- function(x, ...)
  eval(bquote(.(summarize_r_l)(.(x), .(substitute(list(...))))), parent.frame())
```
```{r dplyr_extra_1, echo=FALSE}
summarize_r_l <- function(x, els) {
  frm <- parent.frame()
  exps.sub <- recsub(substitute(els), x, frm)
  if(is.null(exps.sub)) x else {
    # compute groups and splits
    grps <- make_grps(x)        # see appendix
    splits <- lapply(grps, eval, x, frm)
    dat.split <- split(x, splits, drop=TRUE)
    grp.split <- if(!is.null(grps)) lapply(splits, split, splits, drop=TRUE)

    # aggregate
    res.list <- lapply(
      dot_list(exps.sub),       # see appendix
      function(exp) lapply(dat.split, eval, expr=exp, enclos=frm)
    )
    list_to_df(res.list, grp.split, splits)   # see appendix
  }
}
```
```{r dplyr_extra_2, echo=FALSE}

# -- Grouping ------------------------------------------------------------------

group_r <- function(x, ...)
  eval(bquote(.(group_r_l)(.(x), .(substitute(list(...))))), parent.frame())
group_r_l <- function(x, els) {
  exps.sub <- recsub(substitute(els), x, parent.frame())
  if(is.null(exps.sub)) x else {
    if(!is.call(exps.sub) || exps.sub[[1L]] != quote(list))
      exps.sub <- call("list", exps.sub)
    structure(x, .GRP=dot_list(exps.sub, "G"))
} }
# -- Filtering -----------------------------------------------------------------

filter_r <- function(x, subset) {
  sub.exp <- substitute(subset)
  sub.val <- evalr(sub.exp, envir=x, enclos=parent.frame())
  as.data.frame(
    if(!is.null(sub.val)) {
      as.data.frame(x)[
        if(is.numeric(sub.val)) sub.val else !is.na(sub.val) & sub.val,
      ]
    } else x
  )
}
# -- Pipe ----------------------------------------------------------------------

`%$%` <- function(x, y) {
  x.sub <- recsub(substitute(x), parent.frame())
  y.sub <- recsub(substitute(y), parent.frame())
  y.list <- if(!is.call(y.sub)) list(y.sub) else as.list(y.sub)
  eval(sub_dat(y.sub, x), parent.frame())
}
# -- Helper Funs ---------------------------------------------------------------

# Takes result of `substitute(list(...))` and returns a list of quoted language
# object with nice names.

dot_list <- function(x, pre="V") {
  if(!is.call(x) || x[[1L]] != quote(list)) x <- call("list", x)
  dots <- tail(as.list(x), -1L)

  if(is.null(names(dots))) names(dots) <- character(length(dots))
  for(i in seq_along(dots)[!nzchar(names(dots))])
    names(dots)[i] <- if(
      is.language(dots[[i]]) && nchar(deparse(dots[[i]])[[1]]) < 20
    ) deparse(dots[[i]])[[1]] else sprintf("%s%d", pre, i)
  dots
}
# Used by the `%$%` pipe operator to find the correct point in the RHS to
# substitute the forwarded argument in

sub_dat <- function(z, dat) {
  if(is.call(z)) {
    if(z[[1]] == as.name('%$%')) z[[2]] <- sub_dat(z[[2]], dat)
    else {
      z.list <- as.list(z)
      z <- as.call(c(z.list[1], list(dat), tail(z.list, -1)))
  } }
  z
}
# convert the ".GRP" attribute into usable form

make_grps <- function(x)
  if(is.null(attr(x, ".GRP")) || !length(attr(x, ".GRP")))
    list(rep_len(1, nrow(x))) else attr(x, ".GRP")

# Takes result list and makes into a data.frame by recycling elements so they
# are the same length a longest, and also adds in cols for the group vars

list_to_df <- function(dat, grp, splits) {
  lens <- do.call(pmax, lapply(dat, lengths, integer(length(splits))))
  as.data.frame(
    lapply(c(grp, dat), function(x) unname(unlist(Map(rep_len, x, lens))))
  )
}
```
Our functions mimic the corresponding `dplyr` ones:

```{r}
state.data %$%
  filter_r(Region %in% c('Northeast', 'South')) %$%
  group_r(Region) %$%
  summarize_r(weighted.mean(Income, Population))
```

### Implementation

Most of the implementation is not directly related to `recsub` NSE, but we will
go over `summarize_r` to highlight how those parts integrate with the rest.
`summarize_r` is just a forwarding function:

```{r dplyr_extra_0, eval=FALSE}
```
We use the `eval`/`bquote` pattern to forward `NSE` arguments.  We
retrieve `summarize_r_l` from the current function frame with `.()`, because
there is no guarantee we would find it on the search path starting from the
parent frame.  In this case it happens to be available, but it would not be if
these functions were in a package.

We present `recsub` in full for reference, but feel free to skip as we highlight
the interesting bits next:

```{r dplyr_extra_1, eval=FALSE}
```

The only `recsub` specific line is the second one:

```{r eval=FALSE}
  exps.sub <- recsub(substitute(els), x, frm)
```

`els` is the language captured and forwarded by `summarize_r`.  We run `recub`
on that language with our data `x` as the environment and the parent frame as
the enclosure.  We then compute the groups:

```{r eval=FALSE}
    grps <- make_grps(x)        # see appendix
    splits <- lapply(grps, eval, x, frm)
```

`make_grps` extracts the grouping expressions generating by `group_r`.  These
have already been substituted so we evaluate each one with `x` as the
environment and the parent frame as the enclosure.  We use this to split our
data into groups:

```{r eval=FALSE}
    dat.split <- split(x, splits, drop=TRUE)
```

Finally we can evaluate our `recsub`ed expressions within each of the groups:

```{r eval=FALSE}
    # aggregate
    res.list <- lapply(
      dot_list(exps.sub),       # see appendix
      function(exp) lapply(dat.split, eval, expr=exp, enclos=frm)
    )
    list_to_df(res.list, grp.split, splits)   # see appendix
```

`dot.list` turns `exps.sub` into a list of expressions.  Each expression is then
evaluated with each data chunk as the environment and the parent frame as the
enclosure.  Finally `list_to_df` turns our lists of vectors into a data frame.

You can see the rest of the implementation in the [appendix](#appendix).

### Examples

That single `rescub` line enables a programmable NSE:

```{r}
f.exp <- quote(Region %in% c('Northeast', 'South'))
s.exp <- quote(weighted.mean(Income, Population))

state.data %$%
  filter_r(f.exp & Population > 1000) %$%
  group_r(Region) %$%
  summarize_r(round(s.exp))
```

Because `%$%` uses `recsub` too you can even do the following:

```{r}
flt <- quote(filter_r(f.exp & Population > 1000))
grp.and.sum <- quote(group_r(Region) %$% summarize_r(round(s.exp)))

state.data %$% flt %$% grp.and.sum
```

## An Ersatz `data.table`

### Implementation

We wish to re-use our ersatz `dplyr` functions to create a `data.table`-like
interface:

```{r}
as.super_df <- function(x) {
  class(x) <- c("super_df", class(x))
  x
}
"[.super_df" <- function(x, i=NULL, j=NULL, by=NULL) {
  frm <- parent.frame() # as per docs, safer to do this here
  x <- as.data.frame(x)
  x <- eval(bquote(.(filter_r)(     .(x),  .(substitute(i)))), frm)
  x <- eval(bquote(.(group_r_l)(    .(x), .(substitute(by)))), frm)
  x <- eval(bquote(.(summarize_r_l)(.(x),  .(substitute(j)))), frm)
  as.super_df(x)
}
```

Again, we use the `eval`/`bquote` pattern to forward the NSE arguments to our
NSE functions `filter_r_l`, `group_r_l`, and `summarize_r_l`.  The pattern
is not trivial, but it only took six lines of code to transmogrify our
faux-`dplyr` into a faux-`data.table`.

### Examples

After we add the `super_df` class to our data we can start using it with
`data.table` semantics, but with programmable NSE:

```{r}
sd <- as.super_df(state.data)
sd[f.exp, s.exp, by=Region]

exp.a <- quote(max(Illiteracy))
exp.b <- quote(min(Illiteracy))

sd[f.exp, list(exp.a, exp.b), by=list(Region, HasNfl)][1:3,]

exp.c <- quote(list(exp.a, exp.b))
exp.d <- quote(list(Region, HasNfl))

sd[f.exp, exp.c, by=exp.d][1:3,]

```

Despite the forwarding layers the symbols resolve as expected in complex
circumstances:

```{r}
exps <- quote(list(stop("boo"), stop("ya")))  # don't use this
g.exp <- quote(State)                         # nor this

local({
  summarize_r_l <- function(x, y) stop("boom")  # nor this
  max.inc <- quote(max(Income))                 # use this
  min.inc <- quote(min(Income))                 # and this
  exps <- list(max.inc, min.inc)

  g.exp <- quote(Region)                        # and this

  lapply(exps, function(y) sd[f.exp, y, by=g.exp])
})
```

And we can even nest our `dplyr` and `data.table` for an unholy abomination:

```{r}

exp <- quote(data.frame(pop=Population) %$% summarize_r(new.pop=pop * 1.2))

local({
  exps <- list(quote(sum(exp$new.pop)), quote(sum(Population)))
  g.exp <- quote(Region)
  lapply(exps, function(y) sd[f.exp, y, by=g.exp])
})

```

## Appendix

Ersatz `dplyr` implementation.

```{r dplyr_extra_0, eval=FALSE}
```
```{r dplyr_extra_1, eval=FALSE}
```
```{r dplyr_extra_2, eval=FALSE}
```

## References

