getCountries <- function(printOnly = FALSE,
                         setGlobalOnly = FALSE,
                         newDownload = FALSE,
                         file = "countriesAvailable.rds") {
        message("Getting available countries:")
        cache_exists <- file.exists(file)
        cache_readable <- file.access(file, 4) == 0
        cache_writable <- file.access(file, 2) == 0
        cache_creatable <- file.access(dirname(file), 2) == 0
        cache_in_local <- dirname(file) == "."
        if(!exists("web_not_changed"))
                web_not_changed <- checkIfwebNotChanged()
        if(all(cache_exists,
               cache_readable,
               !newDownload)) {
                countries_available <<- readRDS(file)
                if(!"OIE searchform countries" %in% class(countries_available))
                        newDownload <- TRUE
        }
        if(any(!cache_exists,
               !cache_readable,
               newDownload)) {
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
                data(countryRegions, package = "rworldmap")
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
                        left_join(select(countryRegions,
                                         ISO3:GEO3))
                class(countries) <- c(class(countries), "OIE searchform countries")
                countries_available <<- countries
                if(any(all(cache_exists,
                           cache_writable),
                       cache_creatable)) {
                        message("- Writing cache.")
                        saveRDS(countries_available, file)
                } else {
                        if(!cache_in_local) {
                                message("- Writing cache file in current directory.")
                                saveRDS(countries_available, basename(file))
                        } else {
                                message("- Cache not written.")
                        }
                }
        } else {
                message("- Using cached data.")
        }
        if(setGlobalOnly)
                return(invisible(NULL))
        if(printOnly) {
                message("\nCountry codes and names available on the OIE WAHID website:\n")
                message(paste0(names(countries_available), collapse = ", "))
                message(paste(apply(countries_available, 1, paste, collapse = ", "),
                              collapse = "\n"))
                return(invisible(NULL))
        }
        return(countries_available)
}
