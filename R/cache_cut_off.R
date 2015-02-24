#' Sets the cut-off point of expired records in cache
#'
#' Utility function \code{cache_cut_off} is used by other functions from the
#' package for deciding expired records in cache.
#'
#' @param x A \code{numeric}, \code{logical}, \code{character} scalar or
#'   \code{\link[base]{POSIXct}}, \code{\link[base]{POSIXlt}},
#'   \code{\link[base]{Date}}, or \code{\link[lubridate]{duration}} giving the
#'   period back in time or date.
#' @param default Default period back in time
#'   (\code{\link[lubridate]{duration}}) used when \code{x} is \code{NULL},
#'   \code{NA}, \code{Inf}, or otherwise invalid.
#'
#' @return A \code{\link[base]{POSIXct}} object giving the precise point in time
#'   (cut-off).
#'
#' @examples
#' cache_cut_off(1)
#' cache_cut_off(FALSE)
#' cache_cut_off(TRUE)
#' cache_cut_off("1 day")
#' cache_cut_off("2015-01-01")
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
          if (is.na(l)) stop("invalid format")
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

#' @export
cache_cut_off.default <- function(x, default = duration(1, "day")) {
  message("- Argument to cache_cut_off is ", capture.output(str(x)),
          " - don't know how to handle this, returning default: Sys.time() - ",
          default, ".")
  Sys.time() - default
}
