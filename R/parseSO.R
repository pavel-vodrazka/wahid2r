parseSO <- function(nonParsedSO) {
        x <- nonParsedSO
        if(!all(c("resp") %in% names(x)) |
                   !"response" %in% class(x[["resp"]]))
                stop ("The argument specified is not a list containing a response object.")
        url <- "http://www.oie.int/wahis_2/public/wahid.php/Diseaseinformation/Immsummary/listoutbreak"
        postfields <- names(x[[c("resp", "request", "body", "body")]])
        if(!all(x[[c("resp", "url")]] == url,
                c("reportid",
                  "summary_country") %in% postfields))
                stop ("The argument specified is not a non parsed summary of outbreaks ", url)
        doc <- html(x[["resp"]], encoding = "UTF-8")
        len <- doc %>%
                html_nodes(".vacborder") %>%
                length
        state <- if(len > 0) "OK" else "MISSING_SO"
        x[["resp"]] <- NULL
        if(state == "OK") {
                x[["outbreak_country"]] <- doc %>%
                        html_nodes(".vacborder:nth-child(1) a") %>%
                        html_attr("href") %>%
                        gsub("javascript: outbreak_report\\(\\\"|\\\",[0-9]*\\);", "", .)
                x[["outbreak_report"]] <- doc %>%
                        html_nodes(".vacborder:nth-child(1) a") %>%
                        html_attr("href") %>%
                        gsub("javascript: outbreak_report\\(\\\"[A-Z]*\\\",|\\);", "", .) %>%
                        as.integer
                #         x[["full_report_link"]] <- doc %>%
                #                 html_nodes(".vacborder:nth-child(2) a") %>%
                #                 html_attr("href") %>%
                #                 gsub("javascript: open_report\\(\"", "http://www.oie.int", .) %>%
                #                 gsub("\",", "reportid=", .) %>%
                #                 gsub("\\);", "", .)
                x[["loc1"]] <- doc %>%
                        html_nodes(".vacborder:nth-child(3)") %>%
                        html_text
                x[["loc2"]] <- doc %>%
                        html_nodes(".vacborder:nth-child(4)") %>%
                        html_text
                x[["loc3"]] <- doc %>%
                        html_nodes(".vacborder:nth-child(5)") %>%
                        html_text
                x[["epi_unit"]] <- doc %>%
                        html_nodes(".vacborder:nth-child(6)") %>%
                        html_text
                x[["start_date"]] <- doc %>%
                        html_nodes(".vacborder:nth-child(7)") %>%
                        html_text %>%
                        as.Date("%d/%m/%Y")
                x[["outbreak_status"]] <- doc %>%
                        html_nodes(".vacborder.last") %>%
                        html_text
        }
        if(state == "MISSING_SO") {
                x[["outbreak_country"]] <- NA_character_
                x[["outbreak_report"]] <- NA_integer_
#                 x[["full_report_link"]] <- NA_character_
                x[["loc1"]] <- NA_character_
                x[["loc2"]] <- NA_character_
                x[["loc3"]] <- NA_character_
                x[["epi_unit"]] <- NA_character_
                x[["start_date"]] <- as.Date(NA_character_,
                                             format = "%Y-%m-%d")
                x[["outbreak_status"]] <- NA_character_
        }
        if(!exists("P2counter")) P2counter <<- 0
        P2counter <<- P2counter + 1
        if(P2counter %% 50 == 0) {
                cat(P2counter, "\n")
        } else {
                if(P2counter %% 10 == 0) cat("|") else cat(".")
        }
        return(as_data_frame(data.frame(x, stringsAsFactors = FALSE)))
}
