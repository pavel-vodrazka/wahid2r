downloadOR <- function(x) {
        if(!all(c("reportid", "summary_country") %in% names(summaryRecord)))
                stop("Not all required arguments specified.")
        if(any(missing(year),
               missing(outbreakReportID),
               missing(outbreakCountry),
               missing(fullReportID)))
                stop("Not all required arguments specified.")
        url <- "http://www.oie.int/wahis_2/public/wahid.php/Diseaseinformation/Immsummary/outbreakreport"
        if(!exists("web_not_changed"))
                web_not_changed <- checkIfwebNotChanged()
        if(!web_not_changed)
                warning("The OIE WAHID website has changed, the following may not work.",
                        immediate. = TRUE)
        resp <- POST(url = url,
                     body = paste0("reportid=",
                                   outbreakReportID,
                                   "&summary_country=",
                                   outbreakCountry,
                                   "&backreportid=",
                                   fullReportID
                     ),
                     content_type("application/x-www-form-urlencoded"),
                     add_headers(Referer = "http://www.oie.int/wahis_2/public/wahid.php/Diseaseinformation/Immsummary/listoutbreak")
        )
        stop_for_status(resp)
        if(!exists("D3counter")) D3counter <<- 0
        D3counter <<- D3counter + 1
        if(D3counter %% 50 == 0) {
                cat(D3counter, "\n")
        } else {
                if(D3counter %% 10 == 0) cat("|") else cat(".")
        }
        return(list(year = year,
                    reportid = outbreakReportID,
                    summary_country = outbreakCountry,
                    resp = resp))
}
