getDiseaseFormValues <- function(disease = character(),
                                 printOnly = FALSE,
                                 setGlobalOnly = FALSE,
                                 newDownload = FALSE,
                                 file = "diseaseFormValues.rds") {
        message("Getting the form values for disease(s) entered:")
        cache_exists <- file.exists(file)
        cache_readable <- file.access(file, 4) == 0
        cache_writable <- file.access(file, 2) == 0
        cache_creatable <- file.access(dirname(file), 2) == 0
        cache_in_local <- dirname(file) == "."
        if(!exists("web_not_changed"))
                web_not_changed <- checkIfwebNotChanged()
        if(all(cache_exists,
               cache_readable,
               !newDownload)) {
                current_year <- as.integer(format(Sys.Date(), "%Y"))
                cached <- readRDS(file)
                values_labels <<- cached[["values_labels"]]
                if(!"OIE diseaseform values_labels" %in% class(values_labels))
                        newDownload <- TRUE
                years_available <<- cached[["years_available"]]
                if(!all(range(years_available) == c(2005, current_year)))
                        newDownload <- TRUE
        }
        if(any(!cache_exists,
               !cache_readable,
               newDownload)) {
                message("- Downloading.")
                url <- "http://www.oie.int/wahis_2/public/wahid.php/Diseaseinformation/Immsummary"
                resp <- GET(url)
                stop_for_status(resp)
                doc <- html(resp, encoding = "UTF-8")
                years_available <<- doc %>%
                        html_nodes("#diseaseform #year option") %>%
                        html_attr("value") %>%
                        as.integer
                terrestrial_values <- doc %>%
                        html_nodes("#diseaseform #disease_id_terrestrial option") %>%
                        html_attr("value") %>%
                        as.integer
                terrestrial_labels <- doc %>%
                        html_nodes("#diseaseform #disease_id_terrestrial option") %>%
                        html_attr("label")
                aquatic_values <- doc %>%
                        html_nodes("#diseaseform #disease_id_aquatic option") %>%
                        html_attr("value") %>%
                        as.integer
                aquatic_labels <- doc %>%
                        html_nodes("#diseaseform #disease_id_aquatic option") %>%
                        html_attr("label")
                values_labels <<- data_frame(label = c(terrestrial_labels, aquatic_labels),
                                             disease_id_hidden = c(terrestrial_values,
                                                                   aquatic_values),
                                             disease_type_hidden = c(rep.int(0L,
                                                                             length(terrestrial_values)),
                                                                     rep.int(1L,
                                                                             length(aquatic_values))
                                             )) %>%
                        filter(disease_id_hidden != -999L) %>%
                        arrange(label)
                class(values_labels) <- c(class(values_labels), "OIE diseaseform values_labels")
                if(any(all(cache_exists,
                           cache_writable),
                       cache_creatable)) {
                        message("- Writing cache.")
                        saveRDS(list(values_labels = values_labels,
                                     years_available = years_available),
                                file)
                } else {
                        if(!cache_in_local) {
                                message("- Writing cache file in current directory.")
                                saveRDS(list(values_labels = values_labels,
                                             years_available = years_available),
                                        basename(file))
                        } else {
                                message("- Cache not written.")
                        }

                }
        } else {
                message("- Using cached data.")
        }
        if(setGlobalOnly)
                return(invisible(NULL))
        if(length(disease) > 0) {
                values_labels_filtered <- values_labels[grep(disease,
                                                             values_labels$label,
                                                             ignore.case = TRUE)
                                                        ,]
                class(values_labels_filtered) <- c(class(values_labels_filtered),
                                                   "OIE diseaseform values_labels")
        }
        if(printOnly) {
                message("\nValues for submitting the \"diseaseform\" on the OIE WAHID ",
                        "\"Summary of Immediate notifications and Follow-ups\" ",
                        "webpage\n[http://www.oie.int/wahis_2/public/wahid.php/",
                        "Diseaseinformation/Immsummary)]:\n")
                if(length(disease) > 0) {
                        message("Entered search for disease: \"", disease, "\"\n")
                        if(nrow(values_labels_filtered) == 0) {
                                message("No disease found. Diseases available: \n")
                                message(paste0(names(values_labels), collapse = "\t"))
                                message(paste(apply(values_labels, 1, paste, collapse = "\t"),
                                              collapse = "\n"))
                        } else {
                                message(paste0(names(values_labels_filtered), collapse = "\t"))
                                message(paste(apply(values_labels_filtered, 1, paste, collapse = "\t"),
                                              collapse = "\n"))
                        }
                } else {
                        message(paste0(names(values_labels), collapse = "\t"))
                        message(paste(apply(values_labels, 1, paste, collapse = "\t"),
                                      collapse = "\n"))
                }
                message("\nYears available:\n")
                message(paste0(range(years_available), collapse = " - "))
                return(invisible(NULL))
        } else {
                if(length(disease > 0)) {
                        if(nrow(values_labels_filtered) > 1)
                                warning("Your search for pattern \"",
                                        disease,
                                        "\" returned more than one disease.",
                                        immediate. = TRUE)
                        if(nrow(values_labels_filtered) == 0) {
                                warning("Your search for pattern \"",
                                        disease,
                                        "\" returned no disease.",
                                        immediate. = TRUE)
                                return(NULL)
                        }
                        return(values_labels_filtered)
                } else {
                        return(values_labels)
                }
        }
}
