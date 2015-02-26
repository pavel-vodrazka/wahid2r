#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom rvest html
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom rvest html_text
#' @export
parse_sinf <- function(x) {
  if(!all(c("year",
            "disease_id_hidden",
            "disease_type_hidden",
            "resp") %in% names(x)) |
     !"response" %in% class(x[["resp"]]))
    stop ("The argument specified is not a list ",
          "containing a response object.")
  url <- paste0("http://www.oie.int/wahis_2/public/wahid.php/",
                "Diseaseinformation/Immsummary")
  postfields <- rawToChar(x[[c("resp", "request", "body",
                               "config", "postfields")]])
  if(!all(x[[c("resp", "url")]] == url,
          grep("disease_id_hidden=", postfields),
          grep("disease_type_hidden=", postfields),
          grep("year=", postfields)))
    stop ("The argument specified is not a summary ",
          "of immediate notifications and follow-ups ",
          url)
  doc <- html(x[["resp"]], encoding = "UTF-8")
  x[["resp"]] <- NULL
  empty <- doc %>%
    html_nodes("#diseaseform :nth-child(3) a") %>%
    html_attr("onclick") %>%
    gsub("outbreaklist\\('[A-Z]*',|);", "", .) %>%
    as.integer %>%
    length == 0
  if(!empty) {
    country <- doc %>%
      html_nodes(".outbreakdetails .outbreak_country") %>%
      html_text(trim = TRUE) %>%
      gsub("[^A-Za-z ']", "", .)
    while(length((empty_countries <- which(nchar(country) == 0))) > 0) {
      country[empty_countries] <- country[empty_countries - 1]
    }
    x[["country"]] <- country
    x[["status"]] <- doc %>%
      html_nodes(".outbreakdetails :nth-child(2)") %>%
      html_text(trim = TRUE) %>%
      gsub("[0-9/]", "", .)
    x[["date"]] <- doc %>%
      html_nodes(".outbreakdetails :nth-child(2)") %>%
      html_text(trim = TRUE) %>%
      gsub("[A-Za-z ]", "", .) %>%
      as.Date("%d/%m/%Y")
    x[["summary_country"]] <- doc %>%
      html_nodes("#diseaseform :nth-child(3) a") %>%
      html_attr("onclick") %>%
      gsub("outbreaklist\\('|',[0-9]*);", "", .)
    x[["reportid"]] <- doc %>%
      html_nodes("#diseaseform :nth-child(3) a") %>%
      html_attr("onclick") %>%
      gsub("outbreaklist\\('[A-Z]*',|);", "", .) %>%
      as.integer
    #         event_summary_link <- doc %>%
    #                 html_nodes(".vacborder:nth-child(4) a") %>%
    #                 html_attr("href") %>%
    #                 paste0("http://www.oie.int", .)
    #         full_report_link <- doc %>%
    #                 html_nodes(".vacborder:nth-child(5) a") %>%
    #                 html_attr("href") %>%
    #                 gsub("javascript: open_report\\(\"", "http://www.oie.int", .) %>%
    #                 gsub("\",", "reportid=", .) %>%
    #                 gsub("\\);", "", .)
  } else {
    x[["country"]] <- NA_character_
    x[["status"]] <- NA_character_
    x[["date"]] <- as.Date(NA_character_, format = "%Y-%m-%d")
    x[["summary_country"]] <- NA_character_
    x[["reportid"]] <- NA_integer_
    #         event_summary_link <- NA_character_
    #         full_report_link <- NA_character_
  }
  if(!exists("P1counter", where = globals, inherits = FALSE)) {
    globals$P1counter <- 0
  }
  globals$P1counter %<>% add(1)
  if(globals$P1counter %% 50 == 0) {
    cat(globals$P1counter, "\n")
  } else {
    if(globals$P1counter %% 10 == 0) cat("|") else cat(".")
  }
  return(as_data_frame(data.frame(x, stringsAsFactors = FALSE)))
}
