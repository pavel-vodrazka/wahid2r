getCountries <- function(printOnly = FALSE,
                         setGlobalOnly = FALSE,
                         newDownload = FALSE,
                         file = "countriesAvailable.rds") {
        message("Getting available countries")
        if(!exists("web_not_changed"))
                web_not_changed <- checkIfwebNotChanged()
        #         if(!web_not_changed)
        #                 warning("The OIE WAHID website has changed, the following may not work.",
        #                         immediate. = TRUE)
        if(file.exists(file) & !newDownload) {
                countries_available <<- readRDS(file)
                if(!"OIE searchform countries" %in% class(countries_available))
                        newDownload <- TRUE
        }
        if(!file.exists(file) | newDownload) {
                message("... downloading")
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
                saveRDS(countries_available, file)
        } else {
                message("... using cached data")
        }
        if(setGlobalOnly)
                return(invisible(NULL))
        if(printOnly) {
                message("Country codes and names available on the OIE WAHID website:\n")
                message(paste0(names(countries), collapse = ", "))
                message(paste(apply(countries, 1, paste, collapse = ", "),
                              collapse = "\n"))
                return(invisible(NULL))
        }
        return(countries)
}
