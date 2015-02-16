.onLoad <- function(libname, pkgname) {
  assign("globals", new.env(), envir=parent.env(environment()))
}
