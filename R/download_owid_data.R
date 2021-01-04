#' Download Our World in Data data
#'
#' Downloads data on testing, hospital occupation and vaccinations
#' as collected by the Our World in Data team from official reports.
#' You can find the source information for every country at
#' (\url{https://github.com/owid/covid-19-data/tree/master/public/data}).
#' The data accumulates tests over time. The definition of what constitutes
#' a test and the frequency of data collection vary across countries. The
#' vaccination data is currently only available based on ad hoc disclosures
#' by a small set of countries.
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
#' df <- download_owid_data(silent = TRUE, cached = TRUE)
#' df %>%
#'   dplyr::group_by(iso3c) %>%
#'   tidyr::fill(total_tests) %>%
#'   dplyr::group_by(date) %>%
#'   dplyr::summarise(tests = sum(total_tests, na.rm = TRUE)) %>%
#'   ggplot2::ggplot(ggplot2::aes(x = date, y = tests)) +
#'   ggplot2::geom_line()
#'
#' @export
download_owid_data <- function(silent = FALSE, cached = FALSE) {
  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' needs to be a single logical value"
  )
  if (!silent) message("Start downloading Our World in Data data\n")
  if (length(cached) > 1 || !is.logical(cached)) stop(
    "'cached' needs to be a single logical value"
  )

  if(cached) {
    if (!silent) message("Downloading cached version of Our World in Data  data...", appendLF = FALSE)
    df <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/owid_data.RDS")))
    if (!silent) message(sprintf("done. Timestamp is %s", df$timestamp[1]))
    return(df)
  }

  data_raw <- readr::read_csv(
    "https://covid.ourworldindata.org/data/owid-covid-data.csv",
    col_types = readr::cols(), guess_max = 50000
  )

  owid_data <- data_raw %>%
    dplyr::rename(iso3c = .data$iso_code) %>%
    dplyr::select(
      .data$iso3c, .data$date,
      .data$total_tests, .data$tests_units, .data$positive_rate,
      .data$hosp_patients, .data$icu_patients,
      .data$total_vaccinations
    ) %>%
    dplyr::filter(
      !is.na(.data$total_tests) | !is.na(.data$icu_patients) |
        !is.na(.data$hosp_patients) | !is.na(.data$total_vaccinations),
      .data$iso3c != "OWID_WRL"
    ) %>%
    dplyr::mutate(timestamp = Sys.time())

  if (!silent) {
    message("Done downloading Our World in Data data\n")
    data_info("owid_data")
  }

  owid_data
}
