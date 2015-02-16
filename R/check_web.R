check_web <- function(hash_file = "hashes.rds",
                      write_cache_if_different = FALSE) {
        message("Checking whether the OIE WAHID website structure didn't change:")
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
        cache <- read_cache(hash_file)
        if (is.null(cache)) {
          data("hashes", envir = globals)
          cache <- get("hashes", envir = globals)
        } else {
          message("- Using cache file saved on ",
                  file.info(hash_file)$mtime,
                  ", not the original hashes supplied with the package.")
        }
        comparison <- mapply(identical, val, cache)
        if (all(comparison)) {
          assign("web_not_changed", TRUE, globals)
          message("- OK.")
          return(TRUE)
        } else {
          not_equal <- names(comparison[!comparison])
          warning("There are changes on the OIE WAHID website ",
                  "that can affect the functionality of this package.",
                  "\nThings that have changed are:\n- ",
                  paste0(not_equal, collapse = "\n- "),
                  immediate. = TRUE)
          assign("web_not_changed", FALSE, globals)
          if (write_cache_if_different) {
            write_cache(val, hash_file)
          }
          return(FALSE)
        }
}