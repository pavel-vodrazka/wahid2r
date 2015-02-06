downloadSO <- function(SINFrow) {
        if(!all(c("reportid", "summary_country") %in% names(SINFrow)))
                stop("Not all required arguments specified.")
        url <- "http://www.oie.int/wahis_2/public/wahid.php/Diseaseinformation/Immsummary/listoutbreak"
        if(!exists("web_not_changed"))
                web_not_changed <- checkIfwebNotChanged()
        #         if(!web_not_changed)
        #                 warning("The OIE WAHID website has changed, the following may not work.",
        #                         immediate. = TRUE)
        resp <- POST(url = url,
                     body = list(reportid = SINFrow[["reportid"]],
                                 summary_country = SINFrow[["summary_country"]]),
                     encoding = "form",
                     add_headers(Referer = "http://www.oie.int/wahis_2/public/wahid.php/Diseaseinformation/Immsummary?reportid="))
        stop_for_status(resp)
        if(!exists("D2counter")) D2counter <<- 0
        D2counter <<- D2counter + 1
        if(D2counter %% 50 == 0) {
                cat(D2counter, "\n")
        } else {
                if(D2counter %% 10 == 0) cat("|") else cat(".")
        }
        return(c(SINFrow,
                 list(resp = resp,
                      SO_retrieved = as.POSIXct(Sys.time(),
                                                "%Y-%m-%dT%H:%M:%S%z"))))
}
