update_sinf_data <- function(file = "sinf_cache.rds", new_download = FALSE,
                             suppress_messages = TRUE) {
  plyr::a_ply(diseaseform_values$values_labels$disease_id_hidden, 1, get_sinf,
        file = file, new_download = new_download,
        suppress_messages = suppress_messages)
  SINF <- read_cache(file)
  if (is.null(SINF)) {
    return(NULL)
  } else {
    save(SINF, file = "data/SINF.RData")
    devtools::document(roclets=c('rd', 'collate', 'namespace'))
    system("Rcmd.exe INSTALL --no-multiarch --with-keep.source .")
  }
}

#' @importFrom magrittr %<>%
custom_warning <- function(subclass, message, call = sys.call(-1), ...) {
  w <- simpleWarning(message, call = call, ...)
  class(w) %<>% append(subclass)
  warning(w)
}
