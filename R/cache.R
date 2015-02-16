cache_access <- function(cache_path) {
  list(E = file.exists(cache_path),
       R = file.access(cache_path, 4) == 0,
       W = file.access(cache_path, 2) == 0,
       C = file.access(dirname(cache_path), 2) == 0,
       L = normalizePath(dirname(cache_path)) == normalizePath("."),
       LE = file.exists(basename(cache_path)),
       LR = file.access(basename(cache_path), 4) == 0,
       LW = file.access(basename(cache_path), 2) == 0,
       LC = file.access(basename(cache_path), 2) == 0)
}

read_cache <- function(cache_path) {
  a <- cache_access(cache_path)
  if (all(a$E, a$R)) {
    cache <- readRDS(cache_path)
  } else {
    message("- Cache file \"", cache_path, "\" is not ",
            if (!a$E) "present." else "readable.")
    cache <- NULL
  }
  cache
}

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
    message("Cache could not be created or written, even not in working directory.")
    return(NULL)
  }
}
