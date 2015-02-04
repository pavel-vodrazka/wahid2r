downloadSINF <- function(disease_id_hidden,
                         disease_type_hidden,
                         year
) {
        if(any(missing(disease_id_hidden),
               missing(disease_type_hidden),
               missing(year)))
                stop("Not all required arguments specified.")
        if(!exists("web_not_changed"))
                web_not_changed <- checkIfwebNotChanged()
        #         if(!web_not_changed)
        #                 warning("The OIE WAHID website has changed, the following may not work.",
        #                         immediate. = TRUE)
        if(!exists("values_labels") | !exists("years_available"))
                getDiseaseFormValues(setGlobalOnly = TRUE)
        diseaseID_Type <- paste(disease_id_hidden, disease_type_hidden, sep = "-")
        diseaseIDs_Types <- apply(values_labels[, c("disease_id_hidden",
                                                    "disease_type_hidden")],
                                  1,
                                  paste0,
                                  collapse = "-")
        if(!diseaseID_Type %in% diseaseIDs_Types)
                stop("The entered combination of disease_id_hidden ",
                     "and disease_type_hidden (\"",
                     diseaseID_Type,
                     "\") doesn't exist.")
        if(!as.character(year) %in% years_available)
                stop("The year entered (\"",
                     year,
                     "\") is outside the range of WAHIS reports.")
        url <- "http://www.oie.int/wahis_2/public/wahid.php/Diseaseinformation/Immsummary"
        resp <- POST(url = url,
                     config = list(ssl.verifypeer = FALSE),
                     body = list(disease_id_hidden = disease_id_hidden,
                                 disease_type_hidden = disease_type_hidden,
                                 year = year),
                     encode = "form")
        stop_for_status(resp)
        if(!exists("D1counter")) D1counter <<- 0
        D1counter <<- D1counter + 1
        if(D1counter %% 50 == 0) {
                cat(D1counter, "\n")
        } else {
                if(D1counter %% 10 == 0) cat("|") else cat(".")
        }
        return(list(year = year,
                    disease_id_hidden = disease_id_hidden,
                    disease_type_hidden = disease_type_hidden,
                    resp = resp,
                    SINF_retrieved = as.POSIXct(Sys.time(),
                                          "%Y-%m-%dT%H:%M:%S%z")))
}
