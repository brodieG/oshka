
# nocov start
.onLoad <- function(libname, pkgname) {
  # default values specified via options

  if(is.null(getOption('oshka.class.shield')))
    options(oshka.class.shield=TRUE)
  if(is.null(getOption('oshka.name.shield')))
    options(oshka.name.shield=c("::", ":::"))
}
# nocov end
