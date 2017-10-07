library(oshka)

# these tests are from the nse-fun vignette

source('helper/ersatz.R', local=TRUE)

unitizer_sect("ersatz dplyr", {
  f.exp <- quote(grepl("[12]", Plant))
  s.exp <- quote(mean(uptake))

  CO2 %$%
    filter_r(f.exp & conc > 500) %$%
    group_r(Type, Treatment) %$%
    summarize_r(round(s.exp))

  f.exp.b <- quote(filter_r(grepl("[12]", Plant) & conc > 500))
  g.exp.b <- quote(group_r(Type, Treatment))
  s.exp.b <- quote(summarize_r(mean(conc), mean(uptake)))
  exp <- quote(f.exp.b %$% g.exp.b %$% s.exp.b)

  CO2 %$% exp
})
unitizer_sect("ersatz data.table", {
  f.exp <- quote(grepl("[12]", Plant))
  s.exp <- quote(mean(uptake))

  co2 <- as.super_df(CO2)
  co2[f.exp, s.exp, by=Type]

  exp.a <- quote(max(conc))
  exp.b <- quote(min(conc))

  co2[f.exp, list(exp.a, exp.b), by=list(Type, Treatment)][1:3,]

  exp.c <- quote(list(exp.a, exp.b))
  exp.d <- quote(list(Type, Treatment))

  co2[f.exp, exp.c, by=exp.d][1:3,]

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
  exp <- quote(data.frame(upt=uptake) %$% summarize_r(new.upt=upt * 1.2))

  local({
    exps <- list(quote(sum(exp$new.upt)), quote(sum(uptake)))
    g.exp <- quote(Treatment)
    lapply(exps, function(y) co2[f.exp, y, by=g.exp])
  })
})
