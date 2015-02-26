#' @export
get_sinf <- function(x,
                     years,
                     countries,
                     coutries_match = "all",
                     new_download = FALSE,
                     file = "sinf_cache.rds",
                     cache_interval = "1 day") {
  UseMethod("get_sinf")
}

#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @export
get_sinf.numeric <- function(x,
                             years,
                             countries,
                             countries_match = "all",
                             new_download = FALSE,
                             file = "sinf_cache.rds",
                             cache_interval = "1 day") {
  if (length(x) != 1 || is.na(x)) {
    message("- Incorrect specification of disease_id: ",
            capture.output(str(x)), ".")
    return(NULL)
  }
  form_values <- get_diseaseform_values() %>%
    filter(disease_id_hidden == x)
  if (nrow(form_values) == 0) {
    message("- Non-existent disease_id: ", x, ".")
    return(NULL)
  }
  get_sinf.diseaseform(form_values,
                       years,
                       countries,
                       countries_match,
                       new_download = FALSE,
                       file = "sinf_cache.rds",
                       cache_interval = "1 day",
                       check_x = FALSE)
}

#' @export
get_sinf.character <- function(x,
                               years,
                               countries,
                               countries_match = "all",
                               new_download = FALSE,
                               file = "sinf_cache.rds",
                               cache_interval = "1 day") {
  if (length(x) != 1 || is.na(x)) {
    message("- Incorrect specification of the search pattern: ",
            capture.output(str(x)), ".")
    return(NULL)
  }
  form_values <- get_diseaseform_values(x)
  if (is.null(form_values)) return(NULL)
  get_sinf.diseaseform(form_values,
                       years,
                       countries,
                       countries_match,
                       new_download,
                       file,
                       cache_interval,
                       check_x = FALSE)
}

#' @importFrom dplyr inner_join
#' @importFrom dplyr data_frame
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr anti_join
#' @importFrom dplyr as_data_frame
#' @importFrom dplyr arrange
#' @importFrom dplyr distinct_
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
get_sinf.diseaseform <- function(x,
                                 years,
                                 countries,
                                 countries_match = "all",
                                 new_download = FALSE,
                                 file = "sinf_cache.rds",
                                 cache_interval = "1 day",
                                 check_x = TRUE) {
  message("Getting summaries of immediate notifications and follow-ups:")
  if (!exists("web_not_changed", where = globals, inherits = FALSE)) {
    check_web()
  }
  if (!exists("values_labels", where = globals, inherits = FALSE)
      || !exists("years_available", where = globals, inherits = FALSE)) {
    get_diseaseform_values(set_global_only = TRUE)
  }
  if(!exists("countries_available", where = globals, inherits = FALSE)) {
    get_countries(set_global_only = TRUE)
  }
  if (check_x) {
    if (missing(x)) {
      message("- Disease not specified.")
      return(NULL)
    }
    if (nrow(x) != 1) {
      message("- You specified more than one disease:\n",
              capture.output(str(x)))
      return(NULL)
    }
    tryCatch({
      test <- inner_join(x,
                         globals$values_labels,
                         by = c("disease_id_hidden",
                                "disease_type_hidden"))
    },
    error = function() {
      message("- Wrong format of x: ", capture.output(str(x)), ".")
      return(NULL)
    })
    if (nrow(test) == 0) {
      message("- The entered combination of disease_id_hidden and disease_type_hidden: ",
              capture.output(str(x)), " doesn't exist.")
      return(NULL)
    }
  }
  if (!missing(years)) {
    yy <- match_years(years)
  } else {
    message("- Year(s) not specified, using the whole range available: ",
            deparse(globals$years_available), ".")
    yy <- globals$years_available
  }
  if (!missing(countries)) {
    cc <- match_countries(countries, fields = countries_match, select = "all")
    if (is.null(cc)) return(NULL)
  } else {
    cc <- globals$countries_available
  }
  entered <- data_frame(disease_id_hidden = x$disease_id_hidden, year = yy)
  di <- select(x, label)
  message("- Disease: ", di,
          "\n- Country(ies): ", paste0(cc$ISO3, collapse = " "),
          "\n- Year(s): ", deparse(yy))
  cco <- cache_cut_off(cache_interval)
  cache <- read_cache(file)
  if (is.null(cache)) {
    message("- Using data supplied with the package.")
    cache <- sinf()
  } else {
    message("- Using cache file saved on ",
            file.info(file)$mtime,
            ", not the original data supplied with the package.")
    # if (!all.equal(sinf(), cache[0, ], ignore_col_order = FALSE)) {
    if (!identical(sinf(), cache[0, ])) {
      message("- Cache file ignored because wrong format, using data ",
              "supplied with the package.")
      cache <- sinf()
    }
  }
  if (new_download) message("- Dropping cached SINFS for the disease ",
                            "and years entered.")
  message("- Total records in cache (all diseases): ", nrow(cache), ".")
  suppressMessages(cached <- inner_join(cache, entered))
  message("- Cached records for current request: ", nrow(cached),".")
  if(new_download) {
    to_download <- yy
  } else {
    not_cached <- yy[!yy %in% cached[["year"]]]
    expired <- cached %>%
      filter(status == "Continuing"
             | is.na(status) & year == year(Sys.time())) %>%
      group_by(year) %>%
      summarise(oldest = min(SINF_retrieved)) %>%
      filter(oldest < cco) %>%
      .[["year"]]
    message("- Expired records for current request (older than ",
            cco,
            "): ", nrow(filter(cached, year %in% expired)), ".")
    to_download <- c(not_cached, expired) %>% sort
  }
  if(!new_download) {
    suppressMessages(back_to_cache <- entered %>%
                       anti_join(cache, .) %>%
                       rbind(filter(cached, !year %in% expired)))
  } else {
    suppressMessages(back_to_cache <- anti_join(cache,
                                                entered))
  }
  message("- Back to cache records (all diseases): ", nrow(back_to_cache), ".")
  if(length(to_download) > 0) {
    message("- Downloading yearly summaries (", length(to_download), "):")
    globals$D1counter <- 0
    summaries <- sapply(to_download,
                        function(y) {
                          download_sinf(year = y,
                                       id = x$disease_id_hidden,
                                       type = x$disease_type_hidden)
                        },
                        simplify = FALSE)
    message("- Parsing yearly summaries (", length(summaries), "):")
    globals$P1counter <- 0
    summaries %<>% lapply(parse_sinf)
    summaries <- do.call("rbind", summaries)
    summaries <- as_data_frame(c(list(disease = rep(as.character(di),
                                                    nrow(summaries))),
                                 as.list(summaries)))
    message("- Downloaded records to cache: ", nrow(summaries), ".")
  } else {
    summaries <- sinf()
  }
  write_cache <- rbind(back_to_cache,
                       summaries) %>%
    arrange(disease_id_hidden, year, country, reportid)
  class(write_cache) %<>% append("sinf", after = 0)
  if(!identical(cache, write_cache)) {
    message("- Writing cache.")
    write_cache(write_cache, file)
  } else {
    message("- Cache not written because identical.")
  }
  if(!new_download) {
    ret <- rbind(filter(cached, !year %in% expired), summaries) %>%
      arrange(year, country, reportid)
  } else {
    ret <- summaries
  }
  ret %<>% filter(summary_country %in% cc$ISO3)
  ret %<>% distinct_("reportid")
  class(ret) %<>% append("sinf", after = 0)
  message("- Deduplicated records retrieved: ", nrow(ret), ".")
  return(ret)
}

#' @export
get_sinf.default <- function(x, ...) {
  message("- Function get_sinf: no method for ", capture.output(str(x)), ".")
  return(NULL)
}

#' @export
sinf <- function(...) UseMethod("sinf")

#' @export
sinf.default <- function(x, ...) {
  message("- Function sinf(): no method for ", capture.output(str(x)), ".")
  return(NULL)
}

#' @importFrom dplyr data_frame
#' @importFrom magrittr %<>%
#' @export
sinf.NULL <- function() {
  x <- data_frame(disease = character(0),
                  year = integer(0),
                  disease_id_hidden = integer(0),
                  disease_type_hidden = integer(0),
                  SINF_retrieved = as.POSIXct(character(0),
                                              format = "%Y-%m-%dT%H:%M:%S%z"),
                  country = character(0),
                  status = character(0),
                  date = as.Date(character(0),
                                 format = "%Y-%m-%d"),
                  summary_country = character(0),
                  reportid = integer(0))
                  # event_summary_link = character(0),
                  # full_report_link = character(0))
  class(x) %<>% append("sinf", after = 0)
  x
}

#' @importFrom magrittr %<>%
#' @export
sinf.tbl_df <- function(x, ...) {
  templ <- sinf.NULL()
  trimmed <- x[0, ]
  if (!identical(templ, trimmed)) {
    message("- Supplied tbl_df has wrong format: ", capture.output(str(x)), ".")
    return(NULL)
  }
  class(x) %<>% append("sinf", after = 0)
  x
}

#' @export
match_years <- function(years) UseMethod("match_years")

#' @export
match_years.default <- function(years) {
  if (!exists("years_available", where = globals, inherits = FALSE)) {
    get_diseaseform_values(set_global_only = TRUE)
  }
  tryCatch({
    yy <- as.integer(years)
    if (length(yy) == 0) custom_warning("empty", "are empty")
    if (anyNA(yy)) custom_warning("na", "are NA")
    if(!all(yy %in% globals$years_available)) {
      if(any(yy %in% globals$years_available)) {
        custom_warning("some",
                       paste0("- Some of the years entered: ",
                              deparse(years),
                              " are outside the range of WAHIS reports, ",
                              "using only years in the range: ",
                              deparse(yy[yy %in% globals$years_available]),
                              "."))
      } else {
        custom_warning("no", "are outside the range of WAHIS reports")
      }
    }
    yy
  },
  some = function(w) {
    message(w$message)
    yy[yy %in% globals$years_available]
  },
  warning = function(w) {
    message("- Years entered: ", deparse(years), " ",
            w$message,
            ", returning NULL.")
    NULL
  })
}
