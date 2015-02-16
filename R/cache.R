#' Gets information on the specified (cache) file access
#'
#' Utility function \code{cache_access} returns a list ...
#'
#' @param cache_path A character scalar.
#' @return A list.
#' @examples
#' \dontrun{cache_access("cache.rds")}
#' @export
cache_access <- function(cache_path) {
  list(E = file.exists(cache_path),
       R = unname(file.access(cache_path, 4) == 0),
       W = unname(file.access(cache_path, 2) == 0),
       C = unname(file.access(dirname(cache_path), 2) == 0),
       L = normalizePath(dirname(cache_path)) == normalizePath("."),
       LE = file.exists(basename(cache_path)),
       LR = unname(file.access(basename(cache_path), 4) == 0),
       LW = unname(file.access(basename(cache_path), 2) == 0),
       LC = unname(file.access(".", 2) == 0))
}

#' Deserializes an R object from the specified (cache) file (in RDS format)
#'
#' Utility function \code{read_cache} returns the R object serialized in the
#' file specified. If the file does not exist, is not readable, or is not
#' a RDS file, \code{NULL} is returned and a message printed to console.
#'
#' @param cache_path A character scalar.
#' @return An R object deserialized from the RDS file or \code{NULL}.
#' @examples
#' \dontrun{read_cache("cache.rds")}
#' @export
read_cache <- function(cache_path) {
  a <- cache_access(cache_path)
  if (all(a$E, a$R)) {
    cache <- tryCatch(readRDS(cache_path),
             error = function(e) {
               message("- Error when reading cache file (RDS) \"", cache_path,
                       "\": ", e$message, "; ignoring it.")
               return(NULL)
             })
  } else {
    message("- Cache file \"", cache_path, "\" is not ",
            if (!a$E) "present." else "readable.")
    cache <- NULL
  }
  cache
}

#' Serializes the specified object into the specified (cache) file (RDS format)
#'
#' Utility function \code{write_cache} writes the specified object in serialized
#' (RDS) format to the specified file. If the file cannot be written or created,
#' tries to write to a file with the same name in the working directory. If
#' successful, returns the path to the file written, otherwise returns
#' \code{NULL}.
#'
#' @param x The R object to be written.
#' @param cache_path The file path.
#' @return The file path written (a character scalar) or \code{NULL}.
#' @examples
#' \dontrun{write_cache(x, "cache.rds")}
#' @export
write_cache <- function(x, cache_path) {
  a <- cache_access(cache_path)
  if (!a$E && a$C) {
    saveRDS(x, cache_path)
    message("- Cache created.")
    return(cache_path)
  }
  if (a$E && a$W) {
    saveRDS(x, cache_path)
    message("- Cache overwritten.")
    return(cache_path)
  }
  if ((!a$E && !a$C || a$E && !a$W) && !a$L && !a$LE && a$LC) {
    saveRDS(x, basename(cache_path))
    message("- Cache created in working directory.")
    return(basename(cache_path))
  }
  if ((!a$E && !a$C || a$E && !a$W) && !a$L && a$LE && a$LW) {
    saveRDS(x, basename(cache_path))
    message("- Cache overwritten in working directory.")
    return(basename(cache_path))
  }
  if ((!a$E && !a$C && (a$L || !a$L && (!a$LE && !a$LC || a$LE && !a$LW)))
      ||
      (a$E && !a$W && (a$L || !a$L && (!a$LE && !a$LC || a$LE && !a$LW)))) {
    message("- Cache could not be created or written, even not in working directory.")
    return(NULL)
  }
}
