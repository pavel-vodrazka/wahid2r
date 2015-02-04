parseOutbreakReport <- function(nonParsedOutbreakReport) {
        if(!all(c("year",
                  "summary_country",
                  "reportid",
                  "resp") %in% names(nonParsedOutbreakReport)) |
                   !"response" %in% class(nonParsedOutbreakReport[["resp"]]))
                stop ("The argument specified is not a list containing a response object.")
        url <- "http://www.oie.int/wahis_2/public/wahid.php/Diseaseinformation/Immsummary/outbreakreport"
        postfields <- rawToChar(nonParsedOutbreakReport[["resp"]]
                                [["request"]][["body"]][["config"]][["postfields"]])
        if(!all(nonParsedOutbreakReport[["resp"]][["url"]] == url,
                grep("reportid=", postfields),
                grep("summary_country=", postfields),
                grep("backreportid=", postfields)))
                stop (paste0("The argument specified is not a non parsed outbreak report ", url))
        year <- nonParsedOutbreakReport[["year"]]
        outbreak_country <- nonParsedOutbreakReport[["summary_country"]]
        reportid <- nonParsedOutbreakReport[["reportid"]]
        doc <- html(nonParsedOutbreakReport[["resp"]], encoding = "UTF-8")
        data <- doc %>%
                html_nodes(".vacborder") %>%
                html_text
        start_date <- data[1] %>% as.Date("%d/%m/%Y")
        status <- data[2]
        resolution_date <- data[3] %>% as.Date("%d/%m/%Y")
        loc1 <- data[4]
        loc2 <- data[5]
        loc3 <- data[6]
        loc4 <- data[8]
        epi_unit <- data[7]
        lat <- data[9] %>% as.numeric
        lon <- data[10] %>% as.numeric
        description <- data[11]
        at_risk_total <- data[length(data) - 3] %>% as.numeric
        cases_total <- data[length(data) - 2] %>% as.numeric
        deaths_total <- data[length(data) - 1] %>% as.numeric
        destroyed_total <- data[length(data)] %>% as.numeric
        multi_species <- length(data) > 20
        species <- paste0(data[seq(12, length(data) - 4, 5)], collapse = ", ") # ERROR HERE
        species_breakdown <- data[12:(length(data) - 4)] %>%
                matrix(ncol = 5,
                       byrow = TRUE,
                       dimnames = list(NULL,
                                       c("species",
                                         "at_risk",
                                         "cases",
                                         "deaths",
                                         "destroyed"))) %>%
                data_frame
        species_breakdown[, 2:5] <- as.numeric(
                as.character(unlist(species_breakdown[, 2:5])))
        ret <- data_frame(year,
                          reportid,
                          outbreak_country,
                          start_date,
                          status,
                          resolution_date,
                          loc1,
                          loc2,
                          loc3,
                          loc4,
                          epi_unit,
                          lat,
                          lon,
                          description,
                          at_risk_total,
                          cases_total,
                          deaths_total,
                          destroyed_total,
                          multi_species,
                          species,
                          species_breakdown)
        if(!exists("P3counter")) P3counter <<- 0
        P3counter <<- P3counter + 1
        if(P3counter %% 50 == 0) {
                cat(P3counter, "\n")
        } else {
                if(P3counter %% 10 == 0) cat("|") else cat(".")
        }
        return(ret)
}
