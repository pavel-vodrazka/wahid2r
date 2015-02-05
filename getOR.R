getOR <- function(outbreakSummaries, writeCSV = FALSE) {
        if(!"summary of outbreaks" %in% class(outbreakSummaries))
                stop("The argument \"outbreakSummaries\" is not of class \"summary of outbreaks\".")
        if(!exists("web_not_changed"))
                web_not_changed <- checkIfwebNotChanged()
        if(!web_not_changed)
                warning("The OIE WAHID website has changed, the following may not work.",
                        immediate. = TRUE)
        new_cache <- !file.exists("wahid_cache.sqlite3")
        wahid_cache <- src_sqlite("wahid_cache.sqlite3", create = new_cache)
        if(!new_cache) {
                cached_reports <- tbl(wahid_cache, "reports")
        }
        message(paste0("Downloading outbreak reports (",
                       nrow(outbreakSummaries),
                       "):"))
        reports <- apply(outbreakSummaries,
                         1,
                         function(x) downloadOR(x[["year"]],
                                                x[["outbreak_report"]],
                                                x[["outbreak_country"]],
                                                gsub("http://www.oie.int/wahis_2/public/wahid.php/Reviewreport/Review\\?page_refer=MapFullEventReport&reportid=",
                                                     "",
                                                     x[["full_report_link"]])))
        message(paste0("Parsing outbreak reports (", length(reports), "):"))
        reports %<>%
                lapply(parseOR) %>%
                do.call("rbind", .)
        attr(reports, "disease") <- attr(outbreakSummaries, "disease")
        attr(reports, "countries") <- attr(outbreakSummaries, "countries")
        attr(reports, "years") <- attr(outbreakSummaries, "years")
        class(reports) %<>% c(., "outbreak reports")
        if(writeCSV) {
                write.table(reports,
                            file = paste0("outbreaks of ",
                                          disease,
                                          ", year ",
                                          paste0(unique(range(years)), collapse = "-"),
                                          ", countries",
                                          paste0(countries, collapse = ", "),
                                          ".txt"),
                            na = "",
                            fileEncoding = "UTF-8")
                return(NULL)
        } else {
                return(reports)
        }
}
