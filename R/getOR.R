getOR <- function(SO,
                  newDownload = FALSE,
                  file = "ORcache.rds",
                  cacheInterval = "1 day") {
        if(!"summary of outbreaks" %in% class(SO))
                stop("The argument \"SO\" is not of class \"summary of outbreaks\".")
        if(!exists("web_not_changed"))
                web_not_changed <- checkIfwebNotChanged()
        if(is.null(cacheInterval) | is.na(cacheInterval))
                cache_interval <- duration(0, "days")
        if(is.numeric(cacheInterval))
                cache_interval <- duration(cacheInterval, "days")
        if(is.character(cacheInterval))
                cache_interval <- tryCatch({
                        duration(as.numeric(gsub("(^[0-9]*[.]?[0-9]*)([[:space:]]*)(.*)",
                                                 "\\1", cacheInterval)),
                                 gsub("(^[0-9]*[.]?[0-9]*)([[:space:]]*)(.*)",
                                      "\\L\\3", cacheInterval, perl = TRUE))},
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
                            reportid = integer(0),
#                             event_summary_link = character(0),
#                             full_report_link = character(0),
                            SO_retrieved = as.POSIXct(character(0),
                                                      format = "%Y-%m-%dT%H:%M:%S%z"),
                            outbreak_country = character(0),
                            outbreak_report = integer(0),
                            loc1 = character(0),
                            loc2 = character(0),
                            loc3 = character(0),
                            epi_unit = character(0),
                            start_date = as.Date(character(0),
                                                 format = "%Y-%m-%d"),
                            outbreak_status = character(0),
                            OR_retrieved = as.POSIXct(character(0),
                                                      format = "%Y-%m-%dT%H:%M:%S%z"),
                            resolution_date = as.Date(character(0),
                                                      format = "%Y-%m-%d"),
                            loc4 = character(0),
                            lat = numeric(0),
                            lon = numeric(0),
                            description = character(0),
                            multi_species = logical(0),
                            species_affected = character(0),
                            at_risk_total = integer(0),
                            cases_total = integer(0),
                            deaths_total = integer(0),
                            destroyed_total = integer(0),
                            species = character(0),
                            at_risk = character(0),
                            cases = character(0),
                            deaths = character(0),
                            destroyed = character(0))
        entered <- SO %>%
                filter(!is.na(outbreak_report)) %>%
                select(outbreak_report)
        message("Getting outbreak reports (", nrow(SO), ") for outbreak summaries: ",
                paste0(entered$outbreak_report, collapse = " "),
                ".")
        if(file.exists(file)) {
                message("- Using cache file",
                        if(newDownload) ", dropping cached ORs",
                        ".")
                cached_ors <- readRDS(file)
                if(!"outbreak reports" %in% class(cached_ors)) {
                        warning("Cached ORs file ignored because it doesn't ",
                                "have right format, building a new one.",
                                immediate. = TRUE)
                        cached_ors <- templ
                }
        } else {
                cached_ors <- templ
        }
        message("- Total records in cache (all diseases): ", nrow(cached_ors), ".")
        suppressMessages({
                cached <- inner_join(cached_ors, entered)})
        message("- Cached records for current request: ", nrow(cached), ".")
        if(newDownload) {
                to_download <- SO
        } else {
                not_cached <- suppressMessages({
                        entered %>%
                                anti_join(cached_ors)})
                expired <- cached %>%
                        filter(status == "Continuing"
                               | outbreak_status == "Continuing") %>%
                        group_by(outbreak_report) %>%
                        summarise(oldest = min(OR_retrieved)) %>%
                        filter(oldest < Sys.time() - cache_interval) %>%
                        .[["outbreak_report"]]
                message("- Expired records for current request (older than ",
                        cache_interval,
                        "): ", length(expired), ".")
                to_download <- suppressMessages({
                        SO %>%
                                inner_join(data_frame(outbreak_report = c(unlist(not_cached),
                                                                   expired)))})
        }
        if(!newDownload) {
                suppressMessages({
                        back_to_cache <- cached_ors %>%
                                anti_join(entered) %>%
                                rbind(filter(cached, !outbreak_report %in% expired))})
        } else {
                suppressMessages({
                        back_to_cache <- cached_ors %>%
                                anti_join(entered)})
        }
        message("- Back to cache records (all diseases): ", nrow(back_to_cache), ".")
        if(nrow(to_download) > 0) {
                message("- Downloading outbreak reports (", nrow(to_download), "):")
                D3counter <<- 0
                summaries <- list()
                for(i in 1:nrow(to_download)) {
                        summaries[[i]] <- to_download %>%
                                slice(i) %>%
                                downloadOR
                }
                message("- Parsing outbreak reports (", length(summaries), "):")
                P3counter <<- 0
                summaries %<>% lapply(parseOR)
                summaries <- do.call("rbind", summaries)
                message("- Downloaded records to cache: ", nrow(summaries), ".")
        } else {
                summaries <- templ
        }
        write_cache <- rbind(back_to_cache,
                             summaries) %>%
                arrange(disease_id_hidden, year, country, reportid, outbreak_report)
        class(write_cache) %<>% c(., "outbreak reports")
        if(!identical(cached_ors, write_cache)) {
                message("- Writing cache.")
                saveRDS(write_cache, file)
        } else {
                message("- Cache not written because identical.")
        }
        if(!newDownload) {
                ret <- rbind(filter(cached, !reportid %in% expired), summaries) %>%
                        arrange(year, country, reportid, outbreak_report)
        } else {
                ret <- summaries
        }
        class(ret) %<>% c(., "outbreak reports")
        message("- Records retrieved: ", nrow(ret), ".")
        return(ret)
}
