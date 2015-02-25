.onLoad <- function(libname, pkgname) {
  assign("globals", new.env(), envir=parent.env(environment()))
}

custom_warning <- function(subclass, message, call = sys.call(-1), ...) {
  w <- simpleWarning(message, call = call, ...)
  class(w) %<>% append(subclass)
  warning(w)
}
