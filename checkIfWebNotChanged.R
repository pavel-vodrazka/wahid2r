checkIfwebNotChanged <- function(hashFile = "hashes.rds") {
        # do file opening with error catching
        message("Checking whether the OIE WAHID website structure didn't change")
        url <- "http://www.oie.int/wahis_2/public/wahid.php/Diseaseinformation/Immsummary"
        val <- list()
        resp1 <- GET(url)
        stop_for_status(resp1)
        doc1 <- html(resp1, encoding = "UTF-8")
        val[["disease form attributes"]] <- doc1 %>%
                html_nodes("#diseaseform") %>%
                html_attrs %>%
                digest(algo = "md5")
        val[["diseaseform scripts"]] <- doc1 %>%
                html_nodes("#diseaseform script") %>%
                digest(algo = "md5")
        val[["diseaseform inputs"]] <- doc1 %>%
                html_nodes("#diseaseform input") %>%
                digest(algo = "md5")
        val[["diseaseform selects' attributes"]] <- doc1 %>%
                html_nodes("select") %>%
                html_attrs %>%
                digest(algo = "md5")
        resp2 <- GET(paste0(url, "/listoutbreak"))
        stop_for_status(resp2)
        doc2 <- html(resp2, encoding = "UTF-8")
        val[["outbreakreport form attributes"]] <- doc2 %>%
                html_nodes("form[name='outbreakreport']") %>%
                html_attrs %>%
                digest(algo = "md5")
        val[["outbreakreport form inputs"]] <- doc2 %>%
                html_nodes("form[name='outbreakreport'] input") %>%
                digest(algo = "md5")

        if(file.exists(hashFile)) {
                saved <- readRDS(hashFile)
                comparison <- mapply(identical, val, saved)
                if(!all(comparison)) {
                        not_equal <- names(comparison[!comparison])
                        warning("There are changes on the OIE WAHID website ",
                                "that can affect the functionality of this package.",
                                "\nThings that have changed are:\n- ",
                                paste0(not_equal, collapse = "\n- "),
                                immediate. = TRUE)
                        web_not_changed <<- FALSE
                        return(FALSE)
                } else {
                        web_not_changed <<- TRUE
                        message("... OK")
                        return(TRUE)
                }
        } else {
                warning("Impossible to check whether the OIE WAHID website ",
                        "has not changed (hash file not present, creating ",
                        "a new one from the current state of the site).",
                        immediate. = TRUE)
                saveRDS(val, hashFile)
                web_not_changed <<- TRUE
                return(TRUE)
        }
}
