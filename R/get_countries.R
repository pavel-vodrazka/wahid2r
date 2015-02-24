#' Fetches the countries available for submitting the search form
#'
#' Utility function \code{get_countries} fetches the countries available for
#' getting the summaries and outbreak reports at
#' \href{http://www.oie.int/wahis_2/public/wahid.php/Diseaseinformation/Immsummary}{WAHID
#' --> Disease information --> Immediate notifications and Follow-ups}.
#'
#' The function utilizes a cache that is as a default supplied as a dataset
#' \code{data(countries_available)}, but optionally a path to a cache file can
#' be supplied. The fields obtained from the OIE website (\code{ISO3},
#' \code{OIE_name}, \code{OIE_region}) are supplemented with \code{ADMIN},
#' \code{REGION}, \code{continent}, \code{GEO3major}, and \code{GEO3} from the
#' dataset \code{countryRegions} from \code{rvest} package (matched by
#' \code{ISO3}) to facilitate text-based search and different regionalization.
#'
#' @section Side effects: This function sets the variable
#'   \code{countries_available} in the \code{globals} environment that are
#'   utilized by other functions from the package. As mentioned above, it also
#'   optionally writes a cache file at the location specified by the user.
#'
#' @param print_only A logical scalar. When \code{TRUE}, the result is printed
#'   to the console and \code{NULL} returned invisibly.
#' @param set_global_only A logical scalar. When \code{TRUE}, \code{NULL} is
#'   returned invisibly.
#' @param new_download A logical scalar. If \code{TRUE}, the cached data (either
#'   supplied with the package or in the \code{file} argument) are ignored and
#'   the data are fetched from the web.
#' @param file A character scalar. If the file specified exists, it is used
#'   instead of the dataset supplied with the package. If \code{new_download} is
#'   \code{TRUE}, the cache is written to the file specified.
#'
#' @return An object of class \code{c("tbl", "tbl_df", "data.frame",
#'   "countries")} containing records for all countries available on the OIE
#'   WAHID website.
#' @return \code{NULL} (invisibly) when \code{set_global_only} is set to
#'   \code{TRUE} or (invisibly) when \code{print_only} is set to \code{TRUE}.
#'
#' @seealso \code{\link{read_cache}}, \code{\link{write_cache}},
#'   \code{\link[rworldmap]{countryRegions}}
#'
#' @examples
#' \dontrun{get_countries()}
#'
#' @importFrom httr GET
#' @importFrom httr stop_for_status
#' @importFrom rvest html
#' @importFrom magrittr %>%
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom dplyr data_frame
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @export
get_countries <- function(print_only = FALSE,
                          set_global_only = FALSE,
                          new_download = FALSE,
                          file = "countries_available.rds") {
  if (!exists("web_not_changed", where = globals)) {
    assign("web_not_changed", check_web(), globals)
  }
  message("Getting available countries:")
  if (!new_download) {
    cache <- read_cache(file)
    if (is.null(cache)) {
      message("- Using data supplied with the package.")
      cache <- countries_available
    } else {
      message("- Using cache file saved on ",
              file.info(file)$mtime,
              ", not the original data supplied with the package.")
      if (!all(c("ISO3", "OIE_name", "OIE_region", "ADMIN", "REGION",
                 "continent", "GEO3major", "GEO3") %in% names(cache))
          || !"countries" %in% class(cache)) {
        message("- Cache file ignored because wrong format, using data
                supplied with the package.")
        cache <- countries_available
      }
    }
    globals$countries_available <- cache
  } else {
    message("- Downloading.")
    url <- "http://www.oie.int/wahis_2/public/wahid.php/Countryinformation/Countryhome"
    resp <- GET(url)
    stop_for_status(resp)
    doc <- html(resp, encoding = "UTF-8")
    continent_code <- doc %>%
      html_nodes("select#locn1 option") %>%
      html_attr("value")
    continent_name <- doc %>%
      html_nodes("select#locn1 option") %>%
      html_attr("label")
    continents <- data_frame(continent_name,
                             continent_code) %>%
      filter(continent_code != "0",
             continent_name != "Entire World")
    data("countryRegions", package = "rworldmap", envir = environment())
    countries <- apply(continents, 1, function(x) {
      ISO3 <- doc %>%
        html_nodes(paste0("select#country",
                          x[2],
                          " option")) %>%
        html_attr("value")
      OIE_name <- doc %>%
        html_nodes(paste0("select#country",
                          x[2],
                          " option")) %>%
        html_attr("label")
      return(data_frame(ISO3,
                        OIE_name,
                        OIE_region = x[1]))
    }) %>%
      do.call(rbind, .) %>%
      left_join(., select(countryRegions, ISO3:GEO3))
    class(countries) %<>% append("countries")
    globals$countries_available <- countries
    write_cache(globals$countries_available, file)
  }
  if(set_global_only) return(invisible(NULL))
  if(print_only) {
    message("\nCountry codes and names available on the OIE WAHID website:\n")
    message(paste0(names(countries), collapse = ", "))
    message(paste(apply(countries, 1, paste, collapse = ", "),
                  collapse = "\n"))
    return(invisible(NULL))
  }
  return(countries)
}
