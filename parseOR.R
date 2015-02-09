parseOR <- function(nonParsedOR) {
        x <- nonParsedOR
        if(!all(c("year",
                  "summary_country",
                  "reportid",
                  "resp") %in% names(x)) |
                   !"response" %in% class(x[["resp"]]))
                stop ("The argument specified is not a list containing",
                      "a response object.")
        url <- paste0("http://www.oie.int/wahis_2/public/wahid.php/",
                      "Diseaseinformation/Immsummary/outbreakreport")
        postfields <- names(x[[c("resp", "request", "body", "body")]])
        if(!all(x[[c("resp", "url")]] == url,
                c("reportid",
                  "summary_country",
                  "backreportid") %in% postfields))
                stop (paste0("The argument specified is not",
                             " a non parsed outbreak report ", url))
        doc <- html(x[["resp"]], encoding = "UTF-8")
        data <- doc %>%
                html_nodes(".vacborder") %>%
                html_text
        len <- length(data)
        state <- if((len - 20) %% 5 == 0) {
                "OK"
        } else {
                if(len == 19
                   & doc %>%
                           html_nodes("tr:nth-child(6) .ta_left") %>%
                           html_text == "Epidemiological Unit Type") {
                        "MISSING_LOC_2_3" # e.g. AHS-SWZ-2005
                } else {
                        if(len == 15) {
                                "MISSING_OR" # e.g. CSF-ZAF-2005
                        }
                }
        }
        if(is.null(state)) stop("Don't know how to parse this OR: ",
                                x[["disease"]], " - ",
                                x[["year"]], " - ",
                                x[["country"]], " - ",
                                x[["outbreak_report"]], ".")
        x[["resp"]] <- NULL
        if(state == "OK") {
                x[["resolution_date"]] <- data[3] %>% as.Date("%d/%m/%Y")
                x[["loc4"]] <-  data[8]
                x[["lat"]] <- data[9] %>% as.numeric
                x[["lon"]] <- data[10] %>% as.numeric
                x[["description"]] <- data[11]
                x[["multi_species"]] <- len > 20
                breakdown <- data[12:(len - 4)]
                n <- length(breakdown) / 5
                x[["species_affected"]] <- paste0(breakdown[seq(by = 5,
                                                                length.out = n)],
                                                  collapse = ", ")
                x[["at_risk_total"]] <- data[len - 3] %>% as.integer
                x[["cases_total"]] <- data[len - 2] %>% as.integer
                x[["deaths_total"]] <- data[len - 1] %>% as.integer
                x[["destroyed_total"]] <- data[len] %>% as.integer
                x %<>% lapply(function(y) rep(y, times = n))
                x[["species"]] <- breakdown[seq(by = 5, length.out = n)]
                x[["at_risk"]] <- breakdown[seq(2, by = 5, length.out = n)] %>%
                        as.integer
                x[["cases"]] <- breakdown[seq(3, by = 5, length.out = n)] %>%
                        as.integer
                x[["deaths"]] <- breakdown[seq(4, by = 5, length.out = n)] %>%
                        as.integer
                x[["destroyed"]] <- breakdown[seq(5, by = 5, length.out = n)] %>%
                        as.integer
        }
        if(state == "MISSING_LOC_2_3") {
                x[["resolution_date"]] <- data[3] %>% as.Date("%d/%m/%Y")
                x[["loc4"]] <-  data[7]
                x[["lat"]] <- data[8] %>% as.numeric
                x[["lon"]] <- data[9] %>% as.numeric
                x[["description"]] <- data[10]
                x[["multi_species"]] <- len > 19
                breakdown <- data[11:(len - 4)]
                n <- length(breakdown) / 5
                x[["species_affected"]] <- paste0(breakdown[seq(by = 5,
                                                                length.out = n)],
                                                  collapse = ", ")
                x[["at_risk_total"]] <- data[len - 3] %>% as.integer
                x[["cases_total"]] <- data[len - 2] %>% as.integer
                x[["deaths_total"]] <- data[len - 1] %>% as.integer
                x[["destroyed_total"]] <- data[len] %>% as.integer
                x %<>% lapply(function(y) rep(y, times = n))
                x[["species"]] <- breakdown[seq(by = 5, length.out = n)]
                x[["at_risk"]] <- breakdown[seq(2, by = 5, length.out = n)] %>%
                        as.integer
                x[["cases"]] <- breakdown[seq(3, by = 5, length.out = n)] %>%
                        as.integer
                x[["deaths"]] <- breakdown[seq(4, by = 5, length.out = n)] %>%
                        as.integer
                x[["destroyed"]] <- breakdown[seq(5, by = 5, length.out = n)] %>%
                        as.integer
        }
        if(state == "MISSING_OR") {
                x[["resolution_date"]] <- as.Date(NA_character_,
                                                  format = "%Y-%m-%d")
                x[["loc4"]] <-  NA_character_
                x[["lat"]] <- NA_real_
                x[["lon"]] <- NA_real_
                x[["description"]] <- NA_character_
                x[["multi_species"]] <- NA
                x[["species_affected"]] <- NA_character_
                x[["at_risk_total"]] <- NA_integer_
                x[["cases_total"]] <- NA_integer_
                x[["deaths_total"]] <- NA_integer_
                x[["destroyed_total"]] <- NA_integer_
                x[["species"]] <- NA_character_
                x[["at_risk"]] <- NA_integer_
                x[["cases"]] <- NA_integer_
                x[["deaths"]] <- NA_integer_
                x[["destroyed"]] <- NA_integer_
        }

        if(!exists("P3counter")) P3counter <<- 0
        P3counter <<- P3counter + 1
        if(P3counter %% 50 == 0) {
                cat(P3counter, "\n")
        } else {
                if(P3counter %% 10 == 0) cat("|") else cat(".")
        }
        return(as_data_frame(data.frame(x, stringsAsFactors = FALSE)))
}
