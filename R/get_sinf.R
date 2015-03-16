#' Downloads and processes the Summaries of Immediate notifications and
#' Follow-ups.
#'
#' High-level generic function \code{get_sinf} fetches and processes the
#' Summaries of immediate notifications and Follow-ups for specified disease,
#' years, and countries from the OIE WAHID website
#' (\href{http://www.oie.int/wahis_2/public/wahid.php/Diseaseinformation/Immsummary}{WAHID
#' --> Disease information --> Immediate notifications and Follow-ups}).
#'
#' The function utilises data supplied with the package (\code{data(SINF)}) and
#' also a local cache to minimise OIE website traffic. Information on the
#' website is organised by disease and year. Records for a given year older than
#' \code{cache_interval} are considered expired if any of the records has status
#' "Continuing" or if that year is the current year or the last year (if run in
#' the first quarter).
#'
#' @section Side effects: This function sets variables \code{web_not_changed},
#'   \code{values_labels}, \code{years_available}, and
#'   \code{countries_available} (if not set previously) in the \code{globals}
#'   environment that are utilized by other functions from the package. As
#'   mentioned above, it also writes a cache file at the location specified by
#'   the user.
#'
#' @param x Specifies the disease of interest. See methods.
#' @param years An integer vector specifying the years of interest. Defaults to
#'   the full range available (2005 - ) if not specified.
#' @param countries A character vector specifying the countries or areas of
#'   interest (see \code{\link{match_countries}}). Defaults to whole world if
#'   not specified.
#' @param countries_match A character vector containing any combination of
#'   column names to match \code{countries} against in
#'   \code{countries_available} (see \code{\link{get_countries}}), or
#'   \code{"all"} (the default). Ignored if \code{countries} not specified.
#' @param new_download A logical scalar. If \code{TRUE}, the cached data (either
#'   supplied with the package or in the \code{file} argument) are ignored and
#'   the data are fetched from the web.
#' @param file A character scalar. Path to the cache file (see
#'   \code{\link{read_cache}}, \code{\link{write_cache}}).
#' @param cache_interval A specification of a duration (default \code{"1 day"})
#'   or a date (see \code{\link{cache_cut_off}}). Older records of uncomplete
#'   years (see details) are downloaded, records of uncomplete years within the
#'   cache interval and record of complete years are taken from the \code{file}
#'   or the package-supplied data.
#' @param suppress_messages A logical scalar. If \code{TRUE}, all informative
#'   messages from the function are suppressed.
#'
#' @return An object of class \code{c("sinf", "tbl", "tbl_df", "data.frame")}
#'   containing summaries of immediate notifications and follow-ups from the OIE
#'   WAHID website for the disease, years, and countries given.
#' @return \code{NULL} if anything specified incorrectly or not matched.
#'
#' @examples
#' \dontrun{get_sinf("african swine fever")}
#'
#' @export
get_sinf <- function(x,
                     years,
                     countries,
                     coutries_match = "all",
                     new_download = FALSE,
                     file = "sinf_cache.rds",
                     cache_interval = "1 day",
                     suppress_messages = FALSE,
                     ...) {
  UseMethod("get_sinf")
}

#' @describeIn get_sinf
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @export
get_sinf.numeric <- function(x,
                             years,
                             countries,
                             countries_match = "all",
                             new_download = FALSE,
                             file = "sinf_cache.rds",
                             cache_interval = "1 day",
                             suppress_messages = FALSE) {
  if (suppress_messages) {
    return(suppressMessages(get_sinf.numeric(x, years, countries,
                                             countries_match = countries_match,
                                             new_download = new_download,
                                             file = file,
                                             cache_interval = cache_interval,
                                             suppress_messages = FALSE)))
  }
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
                       new_download,
                       file,
                       cache_interval,
                       check_x = FALSE)
}

#' @describeIn get_sinf
#' @export
get_sinf.character <- function(x,
                               years,
                               countries,
                               countries_match = "all",
                               new_download = FALSE,
                               file = "sinf_cache.rds",
                               cache_interval = "1 day",
                               suppress_messages = FALSE) {
  if (suppress_messages) {
    return(suppressMessages(get_sinf.character(x, years, countries,
                                               countries_match = countries_match,
                                               new_download = new_download,
                                               file = file,
                                               cache_interval = cache_interval,
                                               suppress_messages = FALSE)))
  }
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

#' @describeIn get_sinf
#'
#' @param check_x A logical scalar specifying whether the disease given as
#'   \code{diseaseform} should be checked. Set to \code{FALSE} when
#'   \code{get_sinf.diseaseform} called from other methods.
#'
#' @importFrom dplyr inner_join
#' @importFrom dplyr data_frame
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr anti_join
#' @importFrom dplyr as_data_frame
#' @importFrom dplyr arrange
#' @importFrom dplyr distinct_
#' @importFrom lubridate month
#' @importFrom lubridate year
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom plyr ldply
#' @importFrom plyr llply
#' @export
get_sinf.diseaseform <- function(x,
                                 years,
                                 countries,
                                 countries_match = "all",
                                 new_download = FALSE,
                                 file = "sinf_cache.rds",
                                 cache_interval = "1 day",
                                 check_x = TRUE,
                                 suppress_messages = FALSE) {
  if (suppress_messages) {
    return(suppressMessages(get_sinf.diseaseform(x, years, countries,
                                                 countries_match = countries_match,
                                                 new_download = new_download,
                                                 file = file,
                                                 cache_interval = cache_interval,
                                                 check_x = check_x,
                                                 suppress_messages = FALSE)))
  }
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
  di <- select(x, label) %>% as.character
  message("- Disease: ", di,
          "\n- Country(ies): ", paste0(cc$ISO3, collapse = " "),
          "\n- Year(s): ", deparse(yy))
  cco <- cache_cut_off(cache_interval)
  cache <- read_cache(file)
  if (is.null(cache)) {
    message("- Using data supplied with the package.")
    cache <- sinf()
  } else {
    if (!isTRUE(all.equal(sinf(), cache[0, ], ignore_col_order = FALSE))) {
      message("- Cache file ignored because wrong format, using data ",
              "supplied with the package.")
      cache <- sinf()
    } else {
      message("- Using cache file saved on ",
              file.info(file)$mtime, ".")
    }
  }
  message("- Total records in cache (all diseases): ", nrow(cache), ".")
  suppressMessages(cached <- inner_join(cache, entered))
  message("- Cached records for current request: ", nrow(cached),".")
  message("- Total package-supplied records (all diseases): ", nrow(SINF), ".")
  suppressMessages(supplied <- inner_join(SINF, entered))
  message("- Package-supplied records for current request: ",
          nrow(supplied), ".")
  if (nrow(cached) == 0 && nrow(supplied) == 0) {
    available <- sinf()
  } else {
    suppressMessages(available <- rbind(cached, supplied) %>%
                       group_by(year, country, reportid) %>%
                       summarise(SINF_retrieved = max(SINF_retrieved)) %>%
                       ungroup %>%
                       semi_join(rbind(cached, supplied), .))
    class(available) %<>% append("sinf", after = 0)
  }
  message("- Total available records for current request: ",
          nrow(available), ".")
  if(new_download) {
    message("- Dropping cached SINFS for the disease ",
            "and years entered.")
    to_download <- yy
  } else {
    not_available <- yy[!yy %in% available[["year"]]]
    year <- year(Sys.time())
    month <- month(Sys.time())
    current <- if (month > 3) year else c(year - 1, year)
    expired <- available %>%
      filter(status == "Continuing"
             | year %in% current) %>%
      group_by(year) %>%
      summarise(oldest = min(SINF_retrieved)) %>%
      filter(oldest < cco) %>%
      .[["year"]]
    message("- Expired records for current request (older than ",
            cco,
            "): ", nrow(filter(available, year %in% expired)), ".")
    to_download <- c(not_available, expired) %>% sort
  }
  if(!new_download) {
    suppressMessages(back_to_cache <- entered %>%
                       anti_join(cache, .) %>%
                       rbind(filter(available, !year %in% expired)))
  } else {
    suppressMessages(back_to_cache <- anti_join(cache,
                                                entered))
  }
  message("- Back to cache records (all diseases): ", nrow(back_to_cache), ".")
  if(length(to_download) > 0) {
    message("- Downloading yearly summaries (", length(to_download), "):",
            "\n  ", appendLF = FALSE)
    summaries <- llply(to_download,
                       print_progress(download_sinf),
                       id = x$disease_id_hidden,
                       type = x$disease_type_hidden)
    message("\n- Parsing yearly summaries (", length(summaries), "):",
            "\n  ", appendLF = FALSE)
    summaries %<>% ldply(print_progress(parse_sinf)) %>%
      cbind(disease = di, .) %>%
      as_data_frame
    message("\n- Downloaded records to cache: ", nrow(summaries), ".")
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
    ret <- rbind(filter(available, !year %in% expired), summaries) %>%
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
  if (!isTRUE(all.equal(templ, trimmed, ignore_col_order = FALSE))) {
    message("- Supplied tbl_df has wrong format:\n\n  ",
            gsub("\\$", "\n  $", capture.output(str(x))), ".")
    return(NULL)
  }
  class(x) %<>% append("sinf", after = 0)
  x
}

#' @export
sinf.sinf <- function(x, ...) {
  templ <- sinf.NULL()
  trimmed <- x[0, ]
  if (!isTRUE(all.equal(templ, trimmed, ignore_col_order = FALSE))) {
    message("- Supplied sinf has wrong format:\n\n  ",
            gsub("\\$", "\n  $", capture.output(str(x))), ".")
    return(NULL)
  }
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
