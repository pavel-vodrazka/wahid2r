#' @importFrom httr POST
#' @importFrom httr stop_for_status
#' @export
download_sinf <- function(id, type, year) {
  if(any(missing(id),
         missing(type),
         missing(year)))
    stop("Not all required arguments specified.")
  if(!exists("web_not_changed", where = globals, inherits = FALSE))
    check_web()
  if (!exists("values_labels", where = globals, inherits = FALSE)
      || !exists("years_available", where = globals, inherits = FALSE)) {
    get_diseaseform_values(set_global_only = TRUE)
  }
  diseaseID_Type <- paste(id, type, sep = "-")
  diseaseIDs_Types <- apply(globals$values_labels[, c("disease_id_hidden",
                                                      "disease_type_hidden")],
                            1,
                            paste0,
                            collapse = "-")
  if(!diseaseID_Type %in% diseaseIDs_Types)
    stop("The entered combination of disease_id_hidden ",
         "and disease_type_hidden (\"",
         diseaseID_Type,
         "\") doesn't exist.")
  if(!year %in% globals$years_available)
    stop("The year entered: ",
         year,
         ") is outside the range of WAHIS reports.")
  url <- "http://www.oie.int/wahis_2/public/wahid.php/Diseaseinformation/Immsummary"
  resp <- POST(url = url,
               config = list(ssl.verifypeer = FALSE),
               body = list(disease_id_hidden = id,
                           disease_type_hidden = type,
                           year = year),
               encode = "form")
  stop_for_status(resp)
  if(!exists("D1counter", where = globals, inherits = FALSE)) {
    globals$D1counter <- 0
  }
  globals$D1counter <- globals$D1counter + 1
  if(globals$D1counter %% 50 == 0) {
    cat(globals$D1counter, "\n")
  } else {
    if(globals$D1counter %% 10 == 0) cat("|") else cat(".")
  }
  return(list(year = year,
              disease_id_hidden = id,
              disease_type_hidden = type,
              resp = resp,
              SINF_retrieved = as.POSIXct(Sys.time(),
                                          "%Y-%m-%dT%H:%M:%S%z")))
}
