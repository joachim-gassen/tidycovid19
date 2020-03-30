#' Download a merged country-day data frame containing Covid-19 related data
#'
#' Merges data from the Johns Hopkins University CSSSE team on the spread of the
#' SARS-CoV-2 virus and the Covid-19 pandemic
#' (\url{https://github.com/CSSEGISandData/COVID-19}), the ACAPS governmental
#' measures database
#' (\url{https://www.acaps.org/covid19-government-measures-dataset}),
#' Google Trends Covid-19 related search volume
#' (\url{https://trends.google.com/trends/}), and the World Bank
#' (\url{https://data.worldbank.org}) intro a country-day data frame.
#'
#' @param wbank_vars Specify the World Bank data items that you want to retrieve.
#' @param wbank_labels Give somewhat more informative World Bank variable names
#'     for the output data frame. Has to match the length of \code{wbank_vars}
#'     and needs to contain valid variable names.
#' @param search_term Google Trends serch term. Defaults to "coronavirus".
#' @param silent Whether you want the function to send some status messages to
#'     the console. Might be informative as downloading will take some time
#'     and thus defaults to \code{TRUE}.
#' @param cached Whether you want to download the cached version of the data
#'     from the {tidycovid19} Github repository instead of retrieving the
#'     data from the authorative source. Downloading the cached version is
#'     faster and the cache is updated daily. Defaults to \code{FALSE}.
#'
#' @return A data frame containing the data, organized by country and date. It
#'     includes a \code{timestamp} variable indicating the time of data
#'     retrieval.
#'
#' @details See the documentation of the separate download functions of the
#'    package for more detail.
#'
#'
#' @examples
#' df <- download_merged_data(silent = TRUE, cached = TRUE)
#' df %>%
#'   dplyr::group_by(iso3c) %>%
#'   dplyr::summarise(
#'     confirmed_cases = max(confirmed, na.rm = TRUE),
#'     soc_dist_measures = max(soc_dist)
#'   ) %>%
#'   dplyr::filter(confirmed_cases >= 1000) %>%
#'   ggplot2::ggplot(df, mapping = ggplot2::aes(x = confirmed_cases,
#'                                              y = soc_dist_measures)) +
#'   ggplot2::geom_point() +
#'   ggrepel::geom_label_repel(ggplot2::aes(label = iso3c)) +
#'   ggplot2::scale_x_continuous(trans='log10', labels = scales::comma)

#'
#' @export
download_merged_data <- function(wbank_vars = c("SP.POP.TOTL", "AG.LND.TOTL.K2",
                                                "EN.POP.DNST", "EN.URB.LCTY",
                                                "SP.DYN.LE00.IN", "NY.GDP.PCAP.KD"),
                                 wbank_labels = c("population", "land_area_skm",
                                                  "pop_density", "pop_largest_city",
                                                  "life_expectancy", "gdp_capita"),
                                 search_term = "coronavirus",
                                 silent = FALSE,
                                 cached = FALSE) {

  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' needs to be a single logical value"
  )
  if (length(cached) > 1 || !is.logical(cached)) stop(
    "'silent' needs to be a single logical value"
  )

  if(cached) {
    if (!silent) message("Downloading cached version of merged data...", appendLF = FALSE)
    df <-readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/merged.RDS")))
    if (!silent) message(sprintf("done. Timestamp is %s", df$timestamp[1]))
    return(df)
  }
  cases <- download_jhu_csse_covid19_data(silent) %>%
    dplyr::select(-.data$timestamp)
  npis <- download_acaps_npi_data(silent) %>%
    dplyr::mutate(npi_date = lubridate::ymd(.data$date_implemented)) %>%
    dplyr::rename(npi_type = .data$category) %>%
    dplyr::select(.data$iso3c, .data$npi_date, .data$npi_type)
  gtrends_list <- download_google_trends_data(search_term,
                                              c("country_day", "country"),
                                              silent)
  gtrends_cd <- gtrends_list[[1]] %>%
    dplyr::select(-.data$timestamp)

  gtrends_c <- gtrends_list[[2]] %>%
    dplyr::rename(gtrends_country_score = .data$gtrends_score) %>%
    dplyr::select(-.data$timestamp)

  wbank <- download_wbank_data(wbank_vars, wbank_labels, silent) %>%
    dplyr::select(-.data$country, -.data$timestamp)

  if (!silent) message("Merging data ...", appendLF = FALSE)

  calc_npi_measure <-function(type, var_name) {
    my_npi <- npis %>% dplyr::filter(.data$npi_type == type)
    cases %>%
      dplyr::left_join(
        my_npi %>%
          dplyr::rename(date = .data$npi_date) %>%
          dplyr::mutate(npi = TRUE) %>%
          dplyr::select(.data$iso3c, .data$date, .data$npi) %>%
          dplyr::group_by(.data$iso3c, .data$date) %>%
          dplyr::summarise(npi = sum(.data$npi)),
        by = c("iso3c", "date")
      ) %>%
      dplyr::group_by(.data$iso3c) %>%
      dplyr::mutate(
        npi = ifelse(is.na(.data$npi), 0, .data$npi),
        sum_npi = cumsum(.data$npi)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$iso3c, .data$date, .data$sum_npi) -> df

    names(df)[3] <- var_name
    df
  }

  df <- cases %>%
    dplyr::left_join(
      calc_npi_measure("Social distancing", "soc_dist"),
      by = c("iso3c", "date")
    ) %>%
    dplyr::left_join(
      calc_npi_measure("Movement restrictions", "mov_rest"),
      by = c("iso3c", "date")
    ) %>%
    dplyr::left_join(
      calc_npi_measure("Public health measures", "pub_health"),
      by = c("iso3c", "date")
    ) %>%
    dplyr::left_join(
      calc_npi_measure("Social and economic measures", "soc_econ"),
      by = c("iso3c", "date")
    ) %>%
    dplyr::left_join(
      calc_npi_measure("Lockdown", "lockdown"),
      by = c("iso3c", "date")
    ) %>%
    dplyr::left_join(gtrends_cd, by = c("iso3c", "date")
    ) %>%
    dplyr::left_join(gtrends_c, by = "iso3c") %>%
    dplyr::left_join(wbank, by = "iso3c") %>%
    dplyr::mutate(timestamp = Sys.time())

  if (!silent) message("done!")
  df
}
