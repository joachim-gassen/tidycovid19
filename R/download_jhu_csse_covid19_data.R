download_jhu_csse_covid19_data <- function(silent = FALSE, cached = FALSE) {
  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' needs to be a single logical value"
  )
  if (!silent) message("Start downloading JHU CSSE Covid-19 data\n")
  if (length(cached) > 1 || !is.logical(cached)) stop(
    "'silent' needs to be a single logical value"
  )

  if(cached) {
    if (!silent) message("Downloading cached version of JHU CSSE Covid 19 data...", appendLF = FALSE)
    df <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/jhu_csse_covid19.RDS")))
    if (!silent) message("done. Timestamp is %s", df$timestamp[1])
    return(df)
  }

  clean_jhd_to_long <- function(df) {
    df_str <- deparse(substitute(df))
    var_str <- substr(df_str, 1, stringr::str_length(df_str) - 4)

    df %>%
      dplyr::select(-.data$`Province/State`, -.data$Lat, -.data$Long) %>%
      dplyr::rename(country = .data$`Country/Region`) %>%
      dplyr::mutate(iso3c = countrycode::countrycode(.data$country,
                                                     origin = "country.name",
                                                     destination = "iso3c")) %>%
      dplyr::select(-.data$country) %>%
      dplyr::filter(!is.na(.data$iso3c)) %>%
      dplyr::group_by(.data$iso3c) %>%
      dplyr::summarise_at(dplyr::vars(-dplyr::group_cols()), sum) %>%
      tidyr::pivot_longer(
        -.data$iso3c,
        names_to = "date_str",
        values_to = var_str
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(date = lubridate::mdy(.data$date_str)) %>%
      dplyr::select(.data$iso3c, .data$date, !! rlang::sym(var_str))
  }

  confirmed_raw <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", col_types = readr::cols())
  deaths_raw <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", col_types = readr::cols())
  recovered_raw <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", col_types = readr::cols())

  jh_covid19_data <- clean_jhd_to_long(confirmed_raw) %>%
    dplyr::full_join(clean_jhd_to_long(deaths_raw), by = c("iso3c", "date")) %>%
    dplyr::full_join(clean_jhd_to_long(recovered_raw), by = c("iso3c", "date"))

  jhd_countries <- dplyr::tibble(
    country = unique(confirmed_raw$`Country/Region`),
    iso3c = countrycode::countrycode(.data$country,
                                     origin = "country.name",
                                     destination = "iso3c")
  ) %>% dplyr::filter(!is.na(.data$iso3c))

  df <- jh_covid19_data %>%
    dplyr::left_join(jhd_countries, by = "iso3c") %>%
    dplyr::select(.data$country, .data$iso3c,.data$ date, .data$confirmed,
                  .data$deaths, .data$recovered) %>%
    dplyr::mutate(timestamp = Sys.time())
  if (!silent) message("Done downloading JHU CSSE data\n")
  df

}
