parseSINF <- function(nonParsedSINF) {
        if(!all(c("year",
                  "disease_id_hidden",
                  "disease_type_hidden",
                  "resp") %in% names(nonParsedSINF)) |
                   !"response" %in% class(nonParsedSINF[["resp"]]))
                stop ("The argument specified is not a list containing a response object.")
        response <- nonParsedSINF[["resp"]]
        url <- "http://www.oie.int/wahis_2/public/wahid.php/Diseaseinformation/Immsummary"
        postfields <- rawToChar(response[["request"]][["body"]][["config"]][["postfields"]])
        if(!all(response["url"] == url,
                grep("disease_id_hidden=", postfields),
                grep("disease_type_hidden=", postfields),
                grep("year=", postfields)
        )) stop ("The argument specified is not a summary ",
                 "of immediate notifications and follow-ups ",
                 url)
        year <- nonParsedSINF[["year"]]
        disease_id_hidden <- nonParsedSINF[["disease_id_hidden"]]
        disease_type_hidden <- nonParsedSINF[["disease_type_hidden"]]
        SINF_retrieved <- nonParsedSINF[["SINF_retrieved"]]
        doc <- html(response, encoding = "UTF-8")
        country <- doc %>%
                html_nodes(".outbreakdetails .outbreak_country") %>%
                html_text(trim = TRUE) %>%
                gsub("[^A-Za-z ']", "", .)
        while(length((empty_countries <- which(nchar(country) == 0))) > 0) {
                country[empty_countries] <- country[empty_countries - 1]
        }
        status <- doc %>%
                html_nodes(".outbreakdetails :nth-child(2)") %>%
                html_text(trim = TRUE) %>%
                gsub("[0-9/]", "", .)
        date <- doc %>%
                html_nodes(".outbreakdetails :nth-child(2)") %>%
                html_text(trim = TRUE) %>%
                gsub("[A-Za-z ]", "", .) %>%
                as.Date("%d/%m/%Y")
        summary_country <- doc %>%
                html_nodes("#diseaseform :nth-child(3) a") %>%
                html_attr("onclick") %>%
                gsub("outbreaklist\\('|',[0-9]*);", "", .)
        reportid <- doc %>%
                html_nodes("#diseaseform :nth-child(3) a") %>%
                html_attr("onclick") %>%
                gsub("outbreaklist\\('[A-Z]*',|);", "", .)
        event_summary_link <- doc %>%
                html_nodes(".vacborder:nth-child(4) a") %>%
                html_attr("href") %>%
                paste0("http://www.oie.int", .)
        full_report_link <- doc %>%
                html_nodes(".vacborder:nth-child(5) a") %>%
                html_attr("href") %>%
                gsub("javascript: open_report\\(\"", "http://www.oie.int", .) %>%
                gsub("\",", "reportid=", .) %>%
                gsub("\\);", "", .)
        if(!exists("P1counter")) P1counter <<- 0
        P1counter <<- P1counter + 1
        if(P1counter %% 50 == 0) {
                cat(P1counter, "\n")
        } else {
                if(P1counter %% 10 == 0) cat("|") else cat(".")
        }
        return(data_frame(year,
                          disease_id_hidden,
                          disease_type_hidden,
                          country = if(length(full_report_link) > 0) country else NA,
                          status = if(length(full_report_link) > 0) status else "Resolved",
                          date = if(length(full_report_link) > 0) date else NA,
                          summary_country = if(length(full_report_link) > 0) summary_country else NA,
                          reportid = if(length(full_report_link) > 0) reportid else NA,
                          event_summary_link = if(length(full_report_link) > 0) event_summary_link else NA,
                          full_report_link = if(length(full_report_link) > 0) full_report_link else NA,
                          SINF_retrieved))
}
