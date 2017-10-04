## ---- summarize_r ----

summarize_r <- function(x, ...)
  eval(bquote(.(summarize_r_l)(.(x), .(substitute(list(...))))), parent.frame())

## ---- summarize_r_l ----

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
## ---- fo_dplyr_extra ----

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
## ---- super_df ----

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

