getSINF <- function(formValues,
                    years,
                    ISO3Codes,
                    countryNames,
                    OIERegions,
                    regions,
                    continents,
                    GEO3Majors,
                    GEO3s,
                    diseaseID,
                    newDownload = FALSE,
                    file = "SINFcache.rds",
                    cacheInterval = "1 day") {
        if(missing(formValues) & missing(diseaseID))
                stop("Disease not specified.")
        loc_arg_spec <- sum(c(!missing(ISO3Codes),
                              !missing(countryNames),
                              !missing(OIERegions),
                              !missing(regions),
                              !missing(continents),
                              !missing(GEO3Majors),
                              !missing(GEO3s)))
        if(loc_arg_spec > 1)
                stop("Multiple specification of countries.")
        if(!exists("web_not_changed"))
                web_not_changed <- checkIfwebNotChanged()
        if(!exists("values_labels") | !exists("years_available"))
                getDiseaseFormValues(setGlobalOnly = TRUE)
        if(!missing(formValues)) {
                if(!"OIE diseaseform values_labels" %in% class(formValues))
                        stop("The argument \"formValues\" is not of class ",
                             "\"OIE diseaseform values_labels\".")
                if(nrow(formValues) > 1)
                        stop("You specified more than one disease:\n",
                             paste(formValues[["label"]], collapse = ", "))
                diseaseID <- formValues[["disease_id_hidden"]]
                diseaseType <- formValues[["disease_type_hidden"]]
                id_type <- paste(diseaseID, diseaseType, sep = "-")
                ids_types <- apply(select(values_labels, -label),
                                          1,
                                          paste0,
                                          collapse = "-")
                if(!id_type %in% ids_types)
                        stop("The entered combination of diseaseID and diseaseType (\"",
                             id_type,
                             "\") doesn't exist.")
        } else {
                if(missing(diseaseID)) {
                        stop("Disease not specified.")
                }
                diseaseType <- filter(values_labels,
                                      disease_id_hidden == diseaseID) %>%
                        select(disease_type_hidden) %>%
                        as.integer
                if(length(diseaseType) == 0)
                        stop("The entered diseaseID doesn't exist.")
        }
        disease <- select(filter(values_labels,
                                 disease_id_hidden == diseaseID),
                          label)
        if(!missing(years)) {
                years <- as.integer(years)
                if(!all(years %in% years_available)) {
                        if(any(years %in% years_available)) {
                                warning("Some of the years entered (",
                                        paste0(years[!years %in%
                                                             years_available],
                                               collapse = " "),
                                        ") are outside the range of WAHIS reports, ",
                                        "using only years in the range.",
                                        immediate. = TRUE)
                                years <- years[years %in% years_available]
                        } else {
                                stop("Year(s) entered (",
                                     paste0(years, collapse = " "),
                                     ") are outside the range of WAHIS reports.")
                        }
                }
        } else {
                message("- Year(s) not specified, using the whole range available (",
                        paste0(years_available, collapse = " "),
                        ").")
                years <- years_available
        }
        if(!exists("countries_available"))
                getCountries(setGlobalOnly = TRUE)
        if(loc_arg_spec == 1) {
                if(!missing(countryNames)) {
                        not_matched <- countryNames[sapply(countryNames,
                                                           function(y) all(apply(countries_available,
                                                                                 1,
                                                                                 function(x) length(grep(y, x, invert = TRUE)) == 8)))]
                        if(length(not_matched) == length(countryNames)) {
                                stop("Countries or regions entered (countryNames = ",
                                     deparse(countryNames),
                                     ") were not matched in the OIE list of countries.")
                        }
                        if(length(not_matched) > 0) {
                                warning("Some of the countries or regions entered (countryNames = ",
                                        deparse(not_matched),
                                        ") were not matched in the OIE list of countries, ",
                                        "using only countries that matched.",
                                        immediate. = TRUE)
                        }
                        countries <- lapply(countryNames,
                                            function(y) countries_available[apply(
                                                    countries_available,
                                                    1,
                                                    function(x) sum(grep(y, x)) > 0), ])
                        message("- Countries that matched to your search terms:")
                        names(countries) <- countryNames
                        lapply(names(countries), function(x) {
                                message(" \"", x, "\": ",
                                        paste0(countries[[x]][["OIE_name"]],
                                               collapse = ", "))})
                        countries %<>%
                                do.call("rbind", .)
                } else {
                        if(!missing(ISO3Codes)) {
                                sel <- ISO3Codes
                                col <- "ISO3"
                        }

                        if(!missing(OIERegions)) {
                                sel <- OIERegions
                                col <- "OIE_region"
                        }
                        if(!missing(regions)) {
                                sel <- regions
                                col <- "REGION"
                        }
                        if(!missing(continents)) {
                                sel <- continents
                                col <- "continent"
                        }
                        if(!missing(GEO3Majors)) {
                                sel <- GEO3Majors
                                col <- "GEO3major"
                        }
                        if(!missing(GEO3s)) {
                                sel <- GEO3s
                                col <- "GEO3"
                        }
                        if(!all(sel %in% unlist(countries_available[, col]))) {
                                matched <- sel[sel %in% unlist(countries_available[, col])]
                                if(length(matched) == 0) {
                                        stop("Countries or regions entered (",
                                             col,
                                             " = ",
                                             deparse(sel),
                                             ") were not matched in the OIE list of countries.")
                                } else {
                                        warning("Some of the countries or regions entered (",
                                                col,
                                                " = ",
                                                deparse(sel[!sel %in%
                                                                    unlist(countries_available[, col])]),
                                                ") were not matched in the OIE list of countries, ",
                                                "using only countries that matched.",
                                                immediate. = TRUE)
                                }
                        }
                        countries <- countries_available[unlist(countries_available[, col]) %in% sel, ]
                }
        } else {
                countries <- countries_available
        }
        if(is.null(cacheInterval) | is.na(cacheInterval))
                cache_interval <- duration(0, "days")
        if(is.numeric(cacheInterval))
                cache_interval <- duration(cacheInterval, "days")
        if(is.character(cacheInterval))
                cache_interval <- tryCatch({
                        duration(as.numeric(gsub("(^[0-9]*[.]?[0-9]*)([[:space:]]*)(.*)",
                                                 "\\1", cacheInterval)),
                                 gsub("(^[0-9]*[.]?[0-9]*)([[:space:]]*)(.*)", "\\L\\3",
                                      cacheInterval, perl = TRUE))},
                        error = function() {
                                return(duration(1, "day"))
                        })
        templ <- data_frame(disease = character(0),
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
#                             event_summary_link = character(0),
#                             full_report_link = character(0))
        entered <- data_frame(disease_id_hidden = diseaseID, year = years)
        message("Getting summaries of immediate notifications and follow-ups of ",
                select(filter(values_labels, disease_id_hidden == diseaseID), label),
                " for country(ies): ",
                paste0(countries$ISO3, collapse = " "),
                " and year(s): ",
                paste0(years, collapse = " "),
                ".")
        if(file.exists(file)) {
                message("- Using cache file",
                        if(newDownload) {paste0(", dropping cached SINFs for ",
                                                disease,
                                                "and year(s) ",
                                                paste0(years, collapse = " "))
                        },
                        ".")
                cached_sinfs <- readRDS(file)
                if(!"summary of immediate notifications and followups"
                   %in% class(cached_sinfs)) {
                        warning("Cached SINFs file ignored because it doesn't ",
                                "have right format, building a new one.",
                                immediate. = TRUE)
                        cached_sinfs <- templ
                }
        } else {
                cached_sinfs <- templ
        }
        message("- Total records in cache (all diseases): ", nrow(cached_sinfs), ".")
        suppressMessages(cached <- inner_join(cached_sinfs, entered))
        message("- Cached records for current request: ", nrow(cached),".")
        if(newDownload) {
                to_download <- years
        } else {
                not_cached <- years[!years %in% cached[["year"]]]
                expired <- cached %>%
                        filter(status == "Continuing"
                               | is.na(status) & year == year(Sys.time())) %>%
                        group_by(year) %>%
                        summarise(oldest = min(SINF_retrieved)) %>%
                        filter(oldest < Sys.time() - cache_interval) %>%
                        .[["year"]]
                message("- Expired records for current request (older than ",
                        cache_interval,
                        "): ", nrow(filter(cached, year %in% expired)), ".")
                to_download <- c(not_cached, expired) %>% sort
        }
        if(!newDownload) {
                suppressMessages(back_to_cache <- entered %>%
                                         anti_join(cached_sinfs, .) %>%
                                         rbind(filter(cached, !year %in% expired)))
        } else {
                suppressMessages(back_to_cache <- anti_join(cached_sinfs,
                                                            entered))
        }
        message("- Back to cache records (all diseases): ", nrow(back_to_cache), ".")
        if(length(to_download) > 0) {
                message("- Downloading yearly summaries (", length(to_download), "):")
                D1counter <<- 0
                summaries <- sapply(to_download,
                                    function(x) {
                                            downloadSINF(year = x,
                                                         disease_id_hidden = diseaseID,
                                                         disease_type_hidden = diseaseType)
                                    },
                                    simplify = FALSE)
                message("- Parsing yearly summaries (", length(summaries), "):")
                P1counter <<- 0
                summaries %<>% lapply(parseSINF)
                summaries <- do.call("rbind", summaries)
                summaries <- as_data_frame(c(list(disease = rep(as.character(disease),
                                                                nrow(summaries))),
                                             as.list(summaries)))
                message("- Downloaded records to cache: ", nrow(summaries), ".")
        } else {
                summaries <- templ
        }
        write_cache <- rbind(back_to_cache,
                             summaries) %>%
                arrange(disease_id_hidden, year, country, reportid)
        class(write_cache) %<>% c(., "summary of immediate notifications and followups")
        if(!identical(cached_sinfs, write_cache)) {
                message("- Writing cache.")
                saveRDS(write_cache, file)
        } else {
                message("- Cache not written because identical.")
        }
        if(!newDownload) {
                ret <- rbind(filter(cached, !year %in% expired), summaries) %>%
                        arrange(year, country, reportid)
        } else {
                ret <- summaries
        }
        if(!missing(countries))
                ret %<>% filter(summary_country %in% countries[["ISO3"]])
        ret %<>% distinct_("reportid")
        class(ret) %<>% c(., "summary of immediate notifications and followups")
        message("- Deduplicated records retrieved: ", nrow(ret), ".")
        return(ret)
}
