#' Sets the cut-off point of expired records in cache
#'
#' Utility function \code{cache_cut_off} is used by other functions from the
#' package for deciding expired records in cache.
#'
#'
#'
#' @importFrom lubridate duration
#' @export
cache_cut_off <- function(x, default = duration(1, "day")) {
  UseMethod("cache_cut_off")
}

message_default <- function(x, default) {
  message("- Argument to cache_cut_off is ", capture.output(str(x)),
          ", returning default: Sys.time() - ", default, ".")
  Sys.time() - default
}

#' @describeIn cache_cut_off
#' @export
cache_cut_off.NULL <- function(x, default = duration(1, "day")) {
  message_default(NULL, default)
}

#' @describeIn cache_cut_off
#' @importFrom lubridate duration
#' @export
cache_cut_off.numeric <- function(x, default = duration(1, "day")) {
  if (length(x) != 1 || is.na(x) || is.infinite(x)) {
    ret <- message_default(x, default)
  } else {
    ret <- Sys.time() - duration(x, "days")
  }
  ret
}

#' @describeIn cache_cut_off
#' @export
cache_cut_off.logical <- function(x, default = duration(1, "day")) {
  if (length(x) != 1 || is.na(x) || !x) {
    ret <- message_default(x, default)
  } else {
    message("- Argument to cache_cut_off is ", capture.output(str(x)),
            ", returning Sys.time().")
    ret <- Sys.time()
  }
  ret
}

#' @describeIn cache_cut_off
#' @export
cache_cut_off.POSIXct <- function(x, default = duration(1, "day")) {
  if (length(x) != 1 || is.na(x)) {
    ret <- message_default(x, default)
  } else {
    ret <- x
  }
  ret
}

#' @describeIn cache_cut_off
#' @export
cache_cut_off.POSIXlt <- function(x, default = duration(1, "day")) {
  if (length(x) != 1 || is.na(x)) {
    ret <- message_default(x, default)
  } else {
    ret <- as.POSIXct(x)
  }
  ret
}

#' @describeIn cache_cut_off
#' @export
cache_cut_off.Date <- function(x, default = duration(1, "day")) {
  if (length(x) != 1 || is.na(x)) {
    ret <- message_default(x, default)
  } else {
    ret <- as.POSIXct(x)
  }
  ret
}

#' @describeIn cache_cut_off
#' @importFrom lubridate duration
#' @export
cache_cut_off.Duration <- function(x, default = duration(1, "day")) {
  if (length(x) != 1 || is.na(x) || is.infinite(x)) {
    ret <- message_default(x, default)
  } else {
    ret <- Sys.time() - x
  }
  ret
}

#' @describeIn cache_cut_off
#' @importFrom lubridate duration
#' @export
cache_cut_off.character <- function(x, default = duration(1, "day")) {
  if (length(x) != 1 || is.na(x)) {
    ret <- message_default(x, default)
  } else {
    ret <- tryCatch({
      cache_cut_off.numeric(as.numeric(x), default)
    },
    warning = function(w) {
      tryCatch({
        as.POSIXct(x)
      },
      error = function(e) {
        l <- as.numeric(gsub("(^[0-9]*[.]?[0-9]*)([[:space:]]*)(.*)",
                             "\\1", x))
        u <- gsub("(^[0-9]*[.]?[0-9]*)([[:space:]]*)(.*)", "\\L\\3",
                  x, perl = TRUE)
        tryCatch({
          Sys.time() - duration(l, u)
        },
        error = function(e) {
          message("- Your specification of cache cut-off: \"", x,
                  "\" could not be converted to numeric, POSIXct, nor duration (",
                  e$message, "), returning default cache cut-off: ",
                  "Sys.time() - ", default, ".")
          Sys.time() - default
        })
      })
    })
  }
  ret
}

#' @describeIn cache_cut_off
#' @export
cache_cut_off.default <- function(x, default = duration(1, "day")) {
  message("- Argument to cache_cut_off is ", capture.output(str(x)),
          " - don't know how to handle this, returning default: Sys.time() - ",
          default, ".")
  Sys.time() - default
}
