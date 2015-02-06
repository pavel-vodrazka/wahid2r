getSO <- function(SINF,
                  newDownload = FALSE,
                  file = "SOcache.rds",
                  cacheInterval = "1 day") {
        if(!"summary of immediate notifications and followups" %in%
                   class(SINF))
                stop("The argument \"diseaseSummaries\" is not of class \"summary of immediate notifications and followups\".")
        if(!exists("web_not_changed"))
                web_not_changed <- checkIfwebNotChanged()
        if(is.null(cacheInterval) | is.na(cacheInterval)) cache_interval <- duration(0, "days")
        if(is.numeric(cacheInterval)) cache_interval <- duration(cacheInterval, "days")
        if(is.character(cacheInterval)) cache_interval <- tryCatch({
                duration(as.numeric(gsub("(^[0-9]*[.]?[0-9]*)([[:space:]]*)(.*)", "\\1", cacheInterval)),
                         gsub("(^[0-9]*[.]?[0-9]*)([[:space:]]*)(.*)", "\\L\\3", cacheInterval, perl = TRUE))},
                error = function() {
                        return(duration(1, "day"))
                })
        template_sos <- data_frame(disease = character(0),
                                   year = character(0),
                                   disease_id_hidden = character(0),
                                   disease_type_hidden = character(0),
                                   SINF_retrieved = as.POSIXct(character(0),
                                                               format = "%Y-%m-%dT%H:%M:%S%z"),
                                   country = character(0),
                                   status = character(0),
                                   date = as.Date(character(0),
                                                  format = "%Y-%m-%d"),
                                   summary_country = character(0),
                                   reportid = character(0),
                                   event_summary_link = character(0),
                                   full_report_link = character(0),
                                   SO_retrieved = as.POSIXct(character(0),
                                                               format = "%Y-%m-%dT%H:%M:%S%z"),
                                   outbreak_country = character(0),
                                   outbreak_report = character(0),
                                   loc1 = character(0),
                                   loc2 = character(0),
                                   loc3 = character(0),
                                   epi_unit = character(0),
                                   start_date = as.Date(character(0),
                                                        format = "%Y-%m-%d"),
                                   outbreak_status = character(0))
        entered <- select(SINF, reportid)
        message("Getting summaries of outbreaks (", nrow(SINF), ") for reports: ",
                paste0(entered$reportid, collapse = " "),
                ".")
        if(file.exists(file)) {
                message("- Using cache file",
                        if(newDownload) ", dropping cached SOs",
                        ".")
                cached_sos <- readRDS(file)
                if(!"summary of outbreaks" %in% class(cached_sos)) {
                        warning("Cached SOs file ignored because it doesn't ",
                                "have right format, building a new one.",
                                immediate. = TRUE)
                        cached_sos <- template_sos
                }
        } else {
                cached_sos <- template_sos
        }
        message("- Total records in cache (all diseases): ", nrow(cached_sos), ".")
        suppressMessages(cached <- inner_join(cached_sos, entered))
        message("- Cached records for current request: ", nrow(cached), ".")
        if(newDownload) {
                to_download <- SINF
        } else {
                not_cached <- suppressMessages(entered %>%
                                                       anti_join(cached_sos))
                expired <- cached %>%
                        filter(status == "Continuing" | outbreak_status == "Continuing") %>%
                        group_by(reportid) %>%
                        summarise(oldest = min(SO_retrieved)) %>%
                        filter(oldest < Sys.time() - cache_interval) %>%
                        .[["reportid"]]
                message("- Expired records for current request (older than ",
                        cache_interval,
                        "): ", length(expired), ".")
                to_download <- suppressMessages(SINF %>%
                                                        inner_join(data_frame(reportid = c(unlist(not_cached), expired))))
        }
        if(!newDownload) {
                suppressMessages(back_to_cache <- cached_sos %>%
                                         anti_join(entered) %>%
                                         rbind(filter(cached, !reportid %in% expired)))
        } else {
                suppressMessages(back_to_cache <- cached_sos %>%
                                         anti_join(entered))
        }
        message("- Back to cache records (all diseases): ", nrow(back_to_cache), ".")
        if(nrow(to_download) > 0) {
                message("- Downloading summaries of outbreaks (", nrow(to_download), "):")
                D2counter <<- 0
#                 summaries <- apply(to_download, 1, downloadSO)
                summaries <- list()
                for(i in 1:nrow(to_download)) {
                        summaries[[i]] <- to_download %>%
                                slice(i) %>%
                                downloadSO
                }
                message("- Parsing summaries of outbreaks (", length(summaries), "):")
                P2counter <<- 0
                summaries %<>% lapply(parseSO)
                summaries <- do.call("rbind", summaries)
                message("- Downloaded records to cache: ", nrow(summaries), ".")
        } else {
                summaries <- template_sos
        }
        write_cache <- rbind(back_to_cache,
                             summaries) %>%
                arrange(disease_id_hidden, year, country, reportid, outbreak_report)
        class(write_cache) %<>% c(., "summary of outbreaks")
        if(!identical(cached_sos, write_cache)) {
                message("- Writing cache.")
                saveRDS(write_cache, file)
        } else {
                message("- Cache not written because identical.")
        }
        if(!newDownload) {
                summaries_to_return <- rbind(filter(cached, !reportid %in% expired), summaries) %>%
                        arrange(year, country, reportid, outbreak_report)
        } else {
                summaries_to_return <- summaries
        }

        class(summaries_to_return) %<>% c(., "summary of outbreaks")
        message("- Records retrieved: ", nrow(summaries_to_return), ".")
        return(summaries_to_return)
}
