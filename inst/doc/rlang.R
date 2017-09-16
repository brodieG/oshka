## ----global_options, echo=FALSE------------------------------------------
knitr::opts_chunk$set(error=TRUE, comment=NA)
library(dplyr)
library(recsub)

## ------------------------------------------------------------------------
summarise_by <- function(df, group, var) {
  group <- enquo(group)
  var <- enquo(var)
  df %>%
    group_by(!! group) %>%
    summarise(avg = mean(!! var))
}
summarise_by(starwars, species, height) %>% head(2)

## ------------------------------------------------------------------------
summarise_by_r <- function(df, group, var) {
  group <- substitute(group)
  var <- substitute(var)
  expr <- recsub(
    bquote(
      .(df) %>%
      group_by(.(group)) %>%
      summarise(avg = mean(.(var)))
    ),
    df, parent.frame()
  )
  eval(expr, parent.frame())
}
summarise_by_r(starwars, species, height) %>% head(2)

## ------------------------------------------------------------------------
my.var <- quote(species)
summarise_by_r(starwars, my.var, height) %>% head(2)

## ------------------------------------------------------------------------
group_by_r <- function(.data, ..., add=FALSE) {
  group.call <- recsub(sys.call(), .data, parent.frame())
  group.call[[1]] <- quote(group_by)
  eval(group.call, parent.frame())
}
summarise_r <- function(.data, ..., add=FALSE) {
  summarise.call <- recsub(sys.call(), .data, parent.frame())
  summarise.call[[1]] <- quote(summarise)
  eval(summarise.call, parent.frame())
}
summarise_by_r2 <- function(df, group, var)
  df %>% group_by_r(group) %>% summarise_r(avg=mean(var))

summarise_by_r2(starwars, species, height)

