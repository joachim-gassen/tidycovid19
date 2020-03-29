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
    if (!silent) message("done. Timestamp is %s", df$timestamp[1])
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
    dplyr::left_join(wbank, by = "iso3c")

  if (!silent) message("done!")
  df
}
