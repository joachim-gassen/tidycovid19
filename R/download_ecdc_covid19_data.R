#' Download ECDC Data on Covid-19 cases
#'
#' Downloads case data provided by the European Centre for Disease Prevention
#' and Control
#' (\url{https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide}).
#' The data is updated daily and contains the latest available public data
#' on the number of new Covid-19 cases reported per day and per country.
#'
#' @param silent Whether you want the function to send some status messages to
#'     the console. Might be informative as downloading will take some time
#'     and thus defaults to \code{TRUE}.
#' @param cached Whether you want to download the cached version of the data
#'     from the {tidycovid19} Github repository instead of retrieving the
#'     data from the authorative source. Downloading the cached version is
#'     faster and the cache is updated daily. Defaults to \code{FALSE}.
#'
#' @return A data frame containing the data.
#'
#' @examples
#' df <- download_ecdc_covid19_data(silent = TRUE, cached = TRUE)
#'
#' df %>%
#'   dplyr::filter(iso3c == "ITA", !is.na(cases)) %>%
#'   ggplot2::ggplot(ggplot2::aes(x = date, y = cases)) +
#'   ggplot2::geom_bar(stat = "identity", fill = "lightblue")
#'
#' @export
download_ecdc_covid19_data <- function(silent = FALSE, cached = FALSE) {
  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' needs to be a single logical value"
  )
  if (!silent) message("Start downloading ECDC Covid-19 case data\n")
  if (length(cached) > 1 || !is.logical(cached)) stop(
    "'cached' needs to be a single logical value"
  )

  if(cached) {
    if (!silent) message("Downloading cached version of ECDC Covid-19 case data...", appendLF = FALSE)
    df <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/ecdc_covid19.RDS")))
    if (!silent) message(sprintf("done. Timestamp is %s", df$timestamp[1]))
    return(df)
  }

  data_raw <- readr::read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", col_types = readr::cols())

  ecdc_data <- data_raw %>%
    dplyr::rename(
      iso3c = .data$countryterritoryCode,
      country_territory = .data$countriesAndTerritories
    ) %>%
    dplyr::mutate(date = lubridate::dmy(.data$dateRep)) %>%
    dplyr::select(.data$iso3c, .data$country_territory, .data$date,
                  .data$cases, .data$deaths) %>%
    dplyr::mutate(timestamp = Sys.time()) %>%
    dplyr::arrange(.data$iso3c, .data$date)

  if (!silent) {
    message("Done downloading ECDC Covid-19 case data\n")
    data_info("ecdc_covid19")
  }

  ecdc_data
}
