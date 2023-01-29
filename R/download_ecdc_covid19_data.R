#' Download ECDC Data on Covid-19 cases
#'
#' Downloads case data provided by the European Centre for Disease Prevention
#' and Control
#' (\url{https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide}).
#' The data is updated weekly and contains the latest available public data
#' on the number of new Covid-19 cases reported per week and per country.
#'
#' @param silent Whether you want the function to send some status messages to
#'     the console. Might be informative as downloading will take some time
#'     and thus defaults to \code{TRUE}.
#' @param cached Whether you want to download the cached version of the data
#'     from the {tidycovid19} Github repository instead of retrieving the
#'     data from the authorative source. Downloading the cached version is
#'     faster and the cache is updated daily. The cached version includes the
#'     discontinued daily data (see below). Defaults to \code{FALSE}.
#' @param use_daily On December 14th, 2020 the ECDC switched from daily to
#'     weekly reporting. If \code{TRUE} (the default), the code will use daily
#'     data up to 2020-12-14 and then switch to the weekly data that currently
#'     is being reported by the ECDC.
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
download_ecdc_covid19_data <- function(
  silent = FALSE, cached = FALSE, use_daily = TRUE
) {
  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' needs to be a single logical value"
  )
  if (!silent) message("Start downloading ECDC Covid-19 case data\n")
  if (length(cached) > 1 || !is.logical(cached)) stop(
    "'cached' needs to be a single logical value"
  )

  if (use_daily) {
    if (!silent) message("Downloading cached version of discontinued ECDC Covid-19 daily case data...", appendLF = FALSE)
    ecdc_daily <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/ecdc_covid19_daily.RDS")))
    if (!silent) message("done.")
  }

  if(cached) {
    if (!silent) message("Downloading cached version of ECDC Covid-19 case data...", appendLF = FALSE)
    df <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/ecdc_covid19.RDS")))
    if (!silent) message(sprintf("done. Timestamp is %s", df$timestamp[1]))
    return(df)
  }

  # 2021-02-18: The ECDC has transformed its data structure to long format
  # and now also offeres the old daily data and subnational data for download.
  # I stick to our cached daily data and ignore the subnational data for the
  # time being

  data_raw <- readr::read_csv(
    "https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/csv", 
    col_types = readr::cols(note = "c")
  )

  ecdc_wk_to_date <- function(str) {
    # I callbirated this to the old daily ECDC data for China and found that
    # 2020-03 == 2020-01-20 (third Monday of the year)
    # They role over to 2021 from 2020-53 to 2021-01
    # As 2020-53 == 2021-01-04 = first Monday of 2021,
    # I assume that 2021-01 == 2021-01-11.
    # That is weird but fits the data

    yrpart <- as.integer(substr(str, 1, 4))
    wkpart <- as.integer(substr(str, 6, 7))
    if (max(yrpart) > 2023) stop(paste(
      "Is it really 2024? ECDC week day conversion needs adjustment.",
      "Please file an issue on Github quoting this error message."
    ))
    as.Date(
      dplyr::case_when(
        yrpart == 2020 ~ as.Date("2019-12-30"),
        yrpart == 2021 ~ as.Date("2021-01-04"),
        yrpart == 2022 ~ as.Date("2022-01-03"),
        yrpart == 2023 ~ as.Date("2023-01-02") 
        # 2023-01-28: educated guess for 2024: first Monday of the year
        # that would be yrpart == 2024 ~ as.Date("2023-01-01")
      ) +
      7*wkpart, origin = "1970-01-01"
    )
  }

  ecdc_weekly <- data_raw %>%
    dplyr::rename(
      iso3c = .data$country_code,
      country_territory = .data$country,
      variable = .data$indicator,
      value = .data$weekly_count
    ) %>%
    dplyr::mutate(date = ecdc_wk_to_date(.data$year_week)) %>%
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(c("iso3c", "country_territory", "date")),
      names_from = .data$variable,
      values_from = .data$value
    ) %>%
    dplyr::mutate(timestamp = Sys.time()) %>%
    dplyr::arrange(.data$iso3c, .data$date)

  if (use_daily) ecdc_data <- rbind(
    ecdc_daily,
    ecdc_weekly %>% dplyr::filter(.data$date > "2020-12-14")
  )
  else ecdc_data <- ecdc_weekly

  if (!silent) {
    if (use_daily) message(
      sprintf(
        "Combining %d daily and %d weekly observations",
        nrow(ecdc_daily),
        nrow(ecdc_weekly)
      )
    )
    message("Done downloading ECDC Covid-19 case data\n")
    data_info("ecdc_covid19")
  }

  ecdc_data
}
