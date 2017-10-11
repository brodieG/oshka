
# nocov start
.onLoad <- function(libname, pkgname) {
  # Scheme defaults are fairly complex...

  if(is.null(getOption('oshka.shield')))
    options(oshka.shield=list(class=TRUE, symbol=c("::", ":::")))
}
