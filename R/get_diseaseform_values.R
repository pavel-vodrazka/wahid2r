#' Fetches the values used to submit the search form
#'
#' Utility function \code{get_diseaseform_values} fetches the values needed for
#' submitting the initial search form at
#' \href{http://www.oie.int/wahis_2/public/wahid.php/Diseaseinformation/Immsummary}{WAHID
#' --> Disease information --> Immediate notifications and Follow-ups} -- the
#' \emph{disease_id_hidden} and \emph{disease_type_hidden} for a name of the
#' disease entered. It also fetches the range of years available.
#'
#' The function utilizes a cache that is as a default supplied as a dataset
#' \code{data(diseaseform_values)}, but optionally a path to a cache file can be
#' supplied.
#'
#' @section Side effects: This function sets the variables \code{values_labels}
#'   containing the form value for all diseases available and
#'   \code{years_available} in the \code{globals} environment that are utilized
#'   by other functions from the package. As mentioned above, it also optionally
#'   writes a cache file at the location specified by the user.
#'
#' @param disease A character scalar. The disease entered is matched against the
#'   form labels by the \code{grep} function. It is case insensitive and can be
#'   entered partially.
#' @param print_only A logical scalar. When \code{TRUE}, the result is printed
#'   to the console and \code{NULL} returned invisibly.
#' @param set_global_only A logical scalar. When \code{TRUE}, \code{NULL} is
#'   returned invisibly.
#' @param new_download A logical scalar. If \code{TRUE}, the cached data (either
#'   supplied with the package or in the \code{file} argument) are ignored and
#'   the data are fetched from the web. It is set to \code{TRUE} inside the
#'   function if \code{years_available} in the cached data do not extend to
#'   current year.
#' @param file A character scalar. If the file specified exists, it is used
#'   instead of the dataset supplied with the package. If \code{new_download} is
#'   \code{TRUE}, the cache is written to the file specified.
#' @return An object of class \code{c(tbl, tbl.df, data.frame, diseaseform)}
#'   containing values for all diseases available if \code{disease} was not
#'   specified, or containing exactly one row when \code{disease} was specified
#'   and matched.
#' @return \code{NULL} (invisibly) when \code{set_global_only} is set to
#'   \code{TRUE} or (invisibly) when \code{print_only} is set to \code{TRUE} or
#'   (with a warning) when \code{disease} does not give exactly one match.
#' @seealso \code{\link{read_cache}}, \code{\link{write_cache}}
#' @examples
#' \dontrun{get_diseaseform_values(disease = "african swine fever")}
#' @importFrom lubridate year
#' @importFrom httr GET
#' @importFrom httr stop_for_status
#' @importFrom rvest html
#' @importFrom magrittr %>%
#' @importFrom rvest html_nodes
#' @importFrom rvest html_attr
#' @importFrom dplyr data_frame
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom magrittr %<>%
#' @export
get_diseaseform_values <- function(disease = character(),
                                   print_only = FALSE,
                                   set_global_only = FALSE,
                                   new_download = FALSE,
                                   file = "diseaseform_values.rds") {
  if (!exists("web_not_changed", where = globals)) {
    assign("web_not_changed", check_web(), globals)
  }
  message("Getting the form values for disease(s) entered:")
  if (!new_download) {
    cache <- read_cache(file)
    if (is.null(cache)) {
      message("- Using data supplied with the package.")
      cache <- diseaseform_values
    } else {
      message("- Using cache file saved on ",
              file.info(file)$mtime,
              ", not the original data supplied with the package.")
      if (!all(c("values_labels", "years_available") %in% names(cache))
          || !"diseaseform" %in% class(cache$values_labels)) {
        message("- Cache file ignored because wrong format, using data
                supplied with the package.")
        cache <- diseaseform_values
      }
    }
    if (!all(range(cache$years_available) == c(2005, year(Sys.Date())))) {
      message("- Cached data ignored because old.")
      new_download <- TRUE
    } else {
      globals$values_labels <- cache$values_labels
      globals$years_available <- cache$years_available
    }
  }
  if (new_download) {
    message("- Downloading.")
    url <- "http://www.oie.int/wahis_2/public/wahid.php/Diseaseinformation/Immsummary"
    resp <- GET(url)
    stop_for_status(resp)
    doc <- html(resp, encoding = "UTF-8")
    globals$years_available <- doc %>%
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
    lab <- c(terrestrial_labels, aquatic_labels)
    dih <- c(terrestrial_values, aquatic_values)
    dth <- c(rep.int(0L, length(terrestrial_values)),
             rep.int(1L, length(aquatic_values)))
    globals$values_labels <- data_frame(label = lab,
                                        disease_id_hidden = dih,
                                        disease_type_hidden = dth) %>%
      filter(disease_id_hidden != -999L) %>%
      arrange(label)
    class(globals$values_labels) %<>% append("diseaseform")
    write_cache(list(values_labels = globals$values_labels,
                     years_available = globals$years_available),
                file)
  }
  if(set_global_only) return(invisible(NULL))
  if(length(disease) > 0) {
    values_labels_filtered <- globals$values_labels[grep(disease,
                                                         globals$values_labels$label,
                                                         ignore.case = TRUE),]
    class(values_labels_filtered) %<>% append("diseaseform")
  }
  if(print_only) {
    message("\nValues for submitting the \"diseaseform\" on the OIE WAHID ",
            "\"Summary of Immediate notifications and Follow-ups\" ",
            "webpage [http://www.oie.int/wahis_2/public/wahid.php/",
            "Diseaseinformation/Immsummary)]:\n")
    if(length(disease) > 0) {
      message("Entered search for disease: \"", disease, "\"\n")
      if(nrow(values_labels_filtered) == 0) {
        message("No disease found. Diseases available: \n")
        message(paste0(names(globals$values_labels), collapse = "\t"))
        message(paste(apply(globals$values_labels, 1, paste, collapse = "\t"),
                      collapse = "\n"))
      } else {
        message(paste0(names(values_labels_filtered), collapse = "\t"))
        message(paste(apply(values_labels_filtered, 1, paste, collapse = "\t"),
                      collapse = "\n"))
      }
    } else {
      message(paste0(names(globals$values_labels), collapse = "\t"))
      message(paste(apply(globals$values_labels, 1, paste, collapse = "\t"),
                    collapse = "\n"))
    }
    message("\nYears available: ",
            paste0(range(globals$years_available), collapse = " - "),
            "\n")
    return(invisible(NULL))
  } else {
    if(length(disease > 0)) {
      if(nrow(values_labels_filtered) > 1) {
        warning("Your search for pattern \"",
                disease,
                "\" returned more than one disease:\n  - ",
                paste(values_labels_filtered$label, collapse = "\n  - "),
                immediate. = TRUE)
        return(NULL)
      }
      if(nrow(values_labels_filtered) == 0) {
        warning("Your search for pattern \"",
                disease,
                "\" returned no disease.",
                immediate. = TRUE)
        return(NULL)
      }
      return(values_labels_filtered)
    } else {
      return(globals$values_labels)
    }
  }
}
