## ----global_options, echo=FALSE------------------------------------------
knitr::opts_chunk$set(error=TRUE, comment=NA)
library(oshka)
knitr::read_chunk('../tests/helper/ersatz.R')

## ---- eval=FALSE---------------------------------------------------------
#  group_r <- function(x, ...) {...}     # similar to dplyr::group_by
#  filter_r <- function(x, subset) {...} # similar to dplyr::filter
#  summarize_r <- function(x, ...) {...} # similar to dplyr::summarise
#  `%$%` <- function(x, y) {...}         # similar to the magrittr pipe

## ---- echo=FALSE---------------------------------------------------------
summarize_r <- function(x, ...)
  eval(bquote(.(summarize_r_l)(.(x), .(substitute(list(...))))), parent.frame())
summarize_r_l <- function(x, els) {
  frm <- parent.frame()
  exps.sub <- expand(substitute(els), x, frm)
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
## - Grouping ------------------------------------------------------------------ 

group_r <- function(x, ...)
  eval(bquote(.(group_r_l)(.(x), .(substitute(list(...))))), parent.frame())
group_r_l <- function(x, els) {
  exps.sub <- expand(substitute(els), x, parent.frame())
  if(is.null(exps.sub)) x else {
    if(!is.call(exps.sub) || exps.sub[[1L]] != quote(list))
      exps.sub <- call("list", exps.sub)
    structure(x, .GRP=dot_list(exps.sub, "G"))
} }
## - Filtering -----------------------------------------------------------------

filter_r <- function(x, subset) {
  sub.exp <- expand(substitute(subset), x, parent.frame())
  sub.val <- eval(sub.exp, x, parent.frame())
  as.data.frame(
    if(!is.null(sub.val)) {
      as.data.frame(x)[
        if(is.numeric(sub.val)) sub.val else !is.na(sub.val) & sub.val,
      ]
    } else x
  )
}
## - Pipe ----------------------------------------------------------------------

`%$%` <- function(x, y) {
  x.sub <- expand(substitute(x), parent.frame())
  y.sub <- expand(substitute(y), parent.frame())
  y.list <- if(!is.call(y.sub)) list(y.sub) else as.list(y.sub)
  eval(sub_dat(y.sub, x), parent.frame())
}
## - Helper Funs ---------------------------------------------------------------

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

## ------------------------------------------------------------------------
CO2 %$%                              # built-in dataset
  filter_r(grepl("[12]", Plant)) %$%
  group_r(Type, Treatment) %$%
  summarize_r(mean(conc), mean(uptake))

## ----eval=FALSE----------------------------------------------------------
#  summarize_r <- function(x, ...)
#    eval(bquote(.(summarize_r_l)(.(x), .(substitute(list(...))))), parent.frame())

## ----eval=FALSE----------------------------------------------------------
#  summarize_r_l <- function(x, els) {
#    frm <- parent.frame()
#    exps.sub <- expand(substitute(els), x, frm)
#    if(is.null(exps.sub)) x else {
#      # compute groups and splits
#      grps <- make_grps(x)        # see appendix
#      splits <- lapply(grps, eval, x, frm)
#      dat.split <- split(x, splits, drop=TRUE)
#      grp.split <- if(!is.null(grps)) lapply(splits, split, splits, drop=TRUE)
#  
#      # aggregate
#      res.list <- lapply(
#        dot_list(exps.sub),       # see appendix
#        function(exp) lapply(dat.split, eval, expr=exp, enclos=frm)
#      )
#      list_to_df(res.list, grp.split, splits)   # see appendix
#    }
#  }

## ----eval=FALSE----------------------------------------------------------
#    exps.sub <- expand(substitute(els), x, frm)

## ----eval=FALSE----------------------------------------------------------
#      grps <- make_grps(x)        # see appendix
#      splits <- lapply(grps, eval, x, frm)

## ----eval=FALSE----------------------------------------------------------
#      dat.split <- split(x, splits, drop=TRUE)

## ----eval=FALSE----------------------------------------------------------
#      # aggregate
#      res.list <- lapply(
#        dot_list(exps.sub),       # see appendix
#        function(exp) lapply(dat.split, eval, expr=exp, enclos=frm)
#      )
#      list_to_df(res.list, grp.split, splits)   # see appendix

## ------------------------------------------------------------------------
f.exp <- quote(grepl("[12]", Plant))
s.exp <- quote(mean(uptake))

CO2 %$%
  filter_r(f.exp & conc > 500) %$%
  group_r(Type, Treatment) %$%
  summarize_r(round(s.exp))

## ------------------------------------------------------------------------
f.exp.b <- quote(filter_r(grepl("[12]", Plant) & conc > 500))
g.exp.b <- quote(group_r(Type, Treatment))
s.exp.b <- quote(summarize_r(mean(conc), mean(uptake)))
exp <- quote(f.exp.b %$% g.exp.b %$% s.exp.b)

CO2 %$% exp

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
co2 <- as.super_df(CO2)
co2[f.exp, s.exp, by=Type]

exp.a <- quote(max(conc))
exp.b <- quote(min(conc))

co2[f.exp, list(exp.a, exp.b), by=list(Type, Treatment)][1:3,]

exp.c <- quote(list(exp.a, exp.b))
exp.d <- quote(list(Type, Treatment))

co2[f.exp, exp.c, by=exp.d][1:3,]


## ------------------------------------------------------------------------
exps <- quote(list(stop("boo"), stop("ya")))  # don't use this
g.exp <- quote(Whatever)                         # nor this

local({
  summarize_r_l <- function(x, y) stop("boom")  # nor this
  max.upt <- quote(max(uptake))                 # use this
  min.upt <- quote(min(uptake))                 # and this
  exps <- list(max.upt, min.upt)

  g.exp <- quote(Treatment)                        # and this

  lapply(exps, function(y) co2[f.exp, y, by=g.exp])
})

## ------------------------------------------------------------------------

exp <- quote(data.frame(upt=uptake) %$% summarize_r(new.upt=upt * 1.2))

local({
  exps <- list(quote(sum(exp$new.upt)), quote(sum(uptake)))
  g.exp <- quote(Treatment)
  lapply(exps, function(y) co2[f.exp, y, by=g.exp])
})


## ----eval=FALSE----------------------------------------------------------
#  ## - Summarize -----------------------------------------------------------------
#  
#  summarize_r <- function(x, ...)
#    eval(bquote(.(summarize_r_l)(.(x), .(substitute(list(...))))), parent.frame())
#  summarize_r_l <- function(x, els) {
#    frm <- parent.frame()
#    exps.sub <- expand(substitute(els), x, frm)
#    if(is.null(exps.sub)) x else {
#      # compute groups and splits
#      grps <- make_grps(x)        # see appendix
#      splits <- lapply(grps, eval, x, frm)
#      dat.split <- split(x, splits, drop=TRUE)
#      grp.split <- if(!is.null(grps)) lapply(splits, split, splits, drop=TRUE)
#  
#      # aggregate
#      res.list <- lapply(
#        dot_list(exps.sub),       # see appendix
#        function(exp) lapply(dat.split, eval, expr=exp, enclos=frm)
#      )
#      list_to_df(res.list, grp.split, splits)   # see appendix
#    }
#  }
#  ## - Grouping ------------------------------------------------------------------ 
#  
#  group_r <- function(x, ...)
#    eval(bquote(.(group_r_l)(.(x), .(substitute(list(...))))), parent.frame())
#  group_r_l <- function(x, els) {
#    exps.sub <- expand(substitute(els), x, parent.frame())
#    if(is.null(exps.sub)) x else {
#      if(!is.call(exps.sub) || exps.sub[[1L]] != quote(list))
#        exps.sub <- call("list", exps.sub)
#      structure(x, .GRP=dot_list(exps.sub, "G"))
#  } }
#  ## - Filtering -----------------------------------------------------------------
#  
#  filter_r <- function(x, subset) {
#    sub.exp <- expand(substitute(subset), x, parent.frame())
#    sub.val <- eval(sub.exp, x, parent.frame())
#    as.data.frame(
#      if(!is.null(sub.val)) {
#        as.data.frame(x)[
#          if(is.numeric(sub.val)) sub.val else !is.na(sub.val) & sub.val,
#        ]
#      } else x
#    )
#  }
#  ## - Pipe ----------------------------------------------------------------------
#  
#  `%$%` <- function(x, y) {
#    x.sub <- expand(substitute(x), parent.frame())
#    y.sub <- expand(substitute(y), parent.frame())
#    y.list <- if(!is.call(y.sub)) list(y.sub) else as.list(y.sub)
#    eval(sub_dat(y.sub, x), parent.frame())
#  }
#  ## - Helper Funs ---------------------------------------------------------------
#  
#  # Takes result of `substitute(list(...))` and returns a list of quoted language
#  # object with nice names.
#  
#  dot_list <- function(x, pre="V") {
#    if(!is.call(x) || x[[1L]] != quote(list)) x <- call("list", x)
#    dots <- tail(as.list(x), -1L)
#  
#    if(is.null(names(dots))) names(dots) <- character(length(dots))
#    for(i in seq_along(dots)[!nzchar(names(dots))])
#      names(dots)[i] <- if(
#        is.language(dots[[i]]) && nchar(deparse(dots[[i]])[[1]]) < 20
#      ) deparse(dots[[i]])[[1]] else sprintf("%s%d", pre, i)
#    dots
#  }
#  # Used by the `%$%` pipe operator to find the correct point in the RHS to
#  # substitute the forwarded argument in
#  
#  sub_dat <- function(z, dat) {
#    if(is.call(z)) {
#      if(z[[1]] == as.name('%$%')) z[[2]] <- sub_dat(z[[2]], dat)
#      else {
#        z.list <- as.list(z)
#        z <- as.call(c(z.list[1], list(dat), tail(z.list, -1)))
#    } }
#    z
#  }
#  # convert the ".GRP" attribute into usable form
#  
#  make_grps <- function(x)
#    if(is.null(attr(x, ".GRP")) || !length(attr(x, ".GRP")))
#      list(rep_len(1, nrow(x))) else attr(x, ".GRP")
#  
#  # Takes result list and makes into a data.frame by recycling elements so they
#  # are the same length a longest, and also adds in cols for the group vars
#  
#  list_to_df <- function(dat, grp, splits) {
#    lens <- do.call(pmax, lapply(dat, lengths, integer(length(splits))))
#    as.data.frame(
#      lapply(c(grp, dat), function(x) unname(unlist(Map(rep_len, x, lens))))
#    )
#  }

