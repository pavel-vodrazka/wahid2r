downloadOR <- function(SOrow) {
        if(!all(c("outbreak_report", "summary_country", "reportid")
                %in% names(SOrow)))
                stop("Not all required arguments specified.")
        url <- paste0("http://www.oie.int/wahis_2/public/wahid.php/",
                      "Diseaseinformation/Immsummary/outbreakreport")
        if(!exists("web_not_changed"))
                web_not_changed <- checkIfwebNotChanged()
        resp <- POST(url = url,
                     body = list(reportid = SOrow[["outbreak_report"]],
                                 summary_country = SOrow[["summary_country"]],
                                 backreportid = SOrow[["reportid"]]),
                     encoding = "form",
                     add_headers(Referer = paste0("http://www.oie.int/wahis_2/",
                                                  "public/wahid.php/Diseaseinformation/",
                                                  "Immsummary/listoutbreak")))
        stop_for_status(resp)
        if(!exists("D3counter")) D3counter <<- 0
        D3counter <<- D3counter + 1
        if(D3counter %% 50 == 0) {
                cat(D3counter, "\n")
        } else {
                if(D3counter %% 10 == 0) cat("|") else cat(".")
        }
        return(c(SOrow,
                 list(resp = resp,
                      OR_retrieved = as.POSIXct(Sys.time(),
                                                "%Y-%m-%dT%H:%M:%S%z"))))
}
