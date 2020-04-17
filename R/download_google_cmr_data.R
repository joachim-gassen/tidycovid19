#' Download Google Community Mobility Reports Data
#'
#' Downloads Google Community Mobility Report data
#' (\url{https://www.google.com/covid19/mobility/}).
#' As stated on this webpage, the "reports chart movement trends over time
#' by geography, across different categories of places such as retail and
#' recreation, groceries and pharmacies, parks, transit stations,
#' workplaces, and residential". Google prepares these reports to help
#' interested parties to assess responses to social distancing guidance
#' related to Covid-19.
#'
#' @param silent Whether you want the function to send some status messages to
#'     the console. Might be informative as downloading will take some time
#'     and thus defaults to \code{TRUE}.
#' @param cached Whether you want to download the cached version of the data
#'     from the {tidycovid19} Github repository instead of retrieving the
#'     data from the authorative source. Downloading the cached version is
#'     faster and the cache is updated daily. Defaults to \code{FALSE}.
#'
#' @return The data, tidied at the country level
#'
#' @examples
#' df <- download_google_cmr_data(silent = TRUE, cached = TRUE)
#' df %>%
#'   dplyr::group_by(date) %>%
#'   dplyr::summarize(
#'     retail_recreation = mean(retail_recreation, na.rm = TRUE)
#'   ) %>%
#'   ggplot2::ggplot(ggplot2::aes(x = date, y = retail_recreation)) +
#'   ggplot2::geom_line()
#'
#' @export
download_google_cmr_data <- function(silent = FALSE, cached = FALSE) {
  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' needs to be a single logical value"
  )
  if (length(cached) > 1 || !is.logical(cached)) stop(
    "'cached' needs to be a single logical value"
  )

  if(cached) {
    if (!silent) message("Downloading cached version of Google's Community Mobility Reports data...", appendLF = FALSE)
    df <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/google_cmr.RDS")))
    if (!silent) message(sprintf("done. Timestamp is %s", df$timestamp[1]))
    return(df)
  }

  if (!silent) message("Start downloading Google CMR PDFs\n")

  cmr_url <- "https://www.google.com/covid19/mobility/"

  url <- xml2::read_html(cmr_url) %>%
    rvest::html_nodes(xpath = "/html/body/section/section[3]/div[2]/div/div[1]/p[2]/a[1]") %>%
    rvest::html_attr('href')

  if(!silent) message(sprintf("Downloading '%s'.\n", url))
  raw_data <- readr::read_csv(url, col_types = readr::cols(), guess_max = 224147)

  df <- raw_data %>%
    dplyr::filter(is.na(.data$sub_region_1), is.na(.data$sub_region_2)) %>%
    dplyr::select(
      -.data$sub_region_1,
      -.data$sub_region_2,
      -.data$country_region
    ) %>%
    dplyr::rename(
      retail_recreation = .data$retail_and_recreation_percent_change_from_baseline,
      grocery_pharmacy = .data$grocery_and_pharmacy_percent_change_from_baseline,
      parks = .data$parks_percent_change_from_baseline,
      transit_stations = .data$transit_stations_percent_change_from_baseline,
      workplaces = .data$workplaces_percent_change_from_baseline,
      residential = .data$residential_percent_change_from_baseline
    ) %>%
    dplyr::mutate_at(dplyr::vars(3:8), ~ round(./100, 2)) %>%
    dplyr::mutate(
      iso3c = countrycode::countrycode(.data$country_region_code,
                                       origin = "iso2c",
                                       destination = "iso3c"),
      date = lubridate::ymd(date),
      timestamp = Sys.time()
    ) %>%
    dplyr::select(.data$iso3c, .data$date,
                  dplyr::everything(), -.data$country_region_code)

  if (!silent) message("Done downloading Google Community Mobility Reports data\n")
  df
}
