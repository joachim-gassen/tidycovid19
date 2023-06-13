#' Download Johns Hopkins University CSSE data on Covid-19
#'
#' Downloads Johns Hopkins University CSSE data on the spread of the
#' SARS-CoV-2 virus and the Covid-19 pandemic
#' (\url{https://github.com/CSSEGISandData/COVID-19}).
#' The data for confirmed cases, reported deaths and recoveries are merged into
#' one data frame, converted to long format and
#' joined with ISO3c (ISO 3166-1 alpha-3) country codes based on the
#' \link{countrycode} package. Please note: JHU stopped updating the data 
#' on March 10, 2023.
#'
#' @param type The type of data that you want to retrieve. Can be any subset of
#' \itemize{
#'  \item{"country": }{Data at the country level (the default).}
#'  \item{"country_region": }{Data at the country region level (only available for Australia, Canada, China and some oversea areas).}
#'  \item{"us_county": }{Data at the U.S. county level.}
#' }
#' @param silent Whether you want the function to send some status messages to
#'     the console. Might be informative as downloading will take some time
#'     and thus defaults to \code{TRUE}.
#' @param cached Whether you want to download the cached version of the data
#'     from the {tidycovid19} Github repository instead of retrieving the
#'     data from the authorative source. Downloading the cached version is
#'     faster and the cache is updated daily. Defaults to \code{FALSE}.
#'
#' @return If only one \code{type} was selected, a data frame containing the
#'     data. Otherwise, a list containing the desired data frames ordered as
#'     in \code{type}.
#'
#' @examples
#' df <- download_jhu_csse_covid19_data(silent = TRUE, cached = TRUE)
#' df %>%
#'   dplyr::group_by(country) %>%
#'   dplyr::summarise(confirmed_cases = max(confirmed, na.rm = TRUE)) %>%
#'   dplyr::arrange(-confirmed_cases) %>%
#'   dplyr::top_n(10)
#'
#' df <- download_jhu_csse_covid19_data(
#'   type = "us_county", silent = TRUE, cached = TRUE
#' )
#' df %>%
#'   dplyr::filter(!is.na(state)) %>%
#'   dplyr::group_by(state) %>%
#'   dplyr::summarise(deaths = max(deaths, na.rm = TRUE)) %>%
#'   dplyr::arrange(-deaths) %>%
#'   dplyr::top_n(10)
#'
#' @export
download_jhu_csse_covid19_data <- function(type = "country", silent = FALSE, cached = FALSE) {
  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' needs to be a single logical value"
  )
  if (!silent) message("Start downloading JHU CSSE Covid-19 data\n")
  if (length(cached) > 1 || !is.logical(cached)) stop(
    "'cached' needs to be a single logical value"
  )
  if (!all(type %in% c('country', 'country_region', 'us_county')))
    stop(
      "'type' needs to be a vector containing any set of 'country', 'country_region', and 'us_county'."
    )

  if(cached) {
    if (!silent) message("Downloading cached version of JHU CSSE Covid 19 data...", appendLF = FALSE)
    lst <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/jhu_csse_covid19.RDS")))
    lst <- lst[match(type, c('country', 'country_region', 'us_county'))]
    if (!silent) message(sprintf("done. Timestamp is %s", lst[[1]]$timestamp[1]))
  } else {
    jhd_to_long <- function(df) {
       df %>%
        dplyr::group_by(.data$iso3c, .data$region, .data$sub_region) %>%
        dplyr::summarise_at(dplyr::vars(-dplyr::group_cols()), sum) %>%
        tidyr::pivot_longer(
          c(-.data$iso3c, -.data$region, -.data$sub_region),
          names_to = "date_str",
          values_to = "values"
        ) %>%
        dplyr::ungroup()
    }

    prep_country <- function(df) {
      df_str <- deparse(substitute(df))
      var_str <- substr(df_str, 1, stringr::str_length(df_str) - 4)
      if (substr(var_str, 1, 3) == "us_")
        var_str <- substr(var_str, 4, stringr::str_length(var_str))

      df %>%
        dplyr::select(-.data$`Province/State`, -.data$Lat, -.data$Long) %>%
        dplyr::mutate(
          region = NA,
          sub_region = NA,
          iso3c = countrycode::countrycode(
            .data$`Country/Region`,
            origin = "country.name",
            destination = "iso3c"
          )
        ) %>%
        dplyr::select(-.data$`Country/Region`) %>%
        dplyr::filter(!is.na(.data$iso3c)) %>%
        jhd_to_long() %>%
        dplyr::rename(!! rlang::sym(var_str) := .data$values) %>%
        dplyr::mutate(date = lubridate::mdy(.data$date_str)) %>%
        dplyr::select(.data$iso3c, .data$date, !! rlang::sym(var_str))
    }

    prep_country_region <- function(df) {
      df_str <- deparse(substitute(df))
      var_str <- substr(df_str, 1, stringr::str_length(df_str) - 4)
      if (substr(var_str, 1, 3) == "us_")
        var_str <- substr(var_str, 4, stringr::str_length(var_str))

      df %>%
        dplyr::filter(!is.na(.data$`Province/State`)) %>%
        dplyr::select(-.data$Lat, -.data$Long) %>%
        dplyr::rename(region = .data$`Province/State`) %>%
        dplyr::mutate(
          sub_region = NA,
          iso3c = countrycode::countrycode(
            .data$`Country/Region`,
            origin = "country.name",
            destination = "iso3c"
          )
        ) %>%
        dplyr::select(-.data$`Country/Region`) %>%
        dplyr::filter(!is.na(.data$iso3c)) %>%
        jhd_to_long() %>%
        dplyr::rename(!! rlang::sym(var_str) := .data$values) %>%
        dplyr::mutate(date = lubridate::mdy(.data$date_str)) %>%
        dplyr::select(.data$iso3c, .data$region, .data$date, !! rlang::sym(var_str))
    }

    prep_county_us <- function(df) {
      df_str <- deparse(substitute(df))
      var_str <- substr(df_str, 1, stringr::str_length(df_str) - 4)
      if (substr(var_str, 1, 3) == "us_")
        var_str <- substr(var_str, 4, stringr::str_length(var_str))

      df %>%
        dplyr::filter(!is.na(.data$Province_State)) %>%
        dplyr::rename(
          iso3c = .data$iso3,
          region = .data$Province_State,
          sub_region = .data$Admin2
        ) %>%
        dplyr::select(.data$iso3c, .data$region, .data$sub_region,
                      dplyr::matches("\\d+/\\d+/\\d+")) %>%
        dplyr::filter(!is.na(.data$iso3c)) %>%
        jhd_to_long() %>%
        dplyr::rename(!! rlang::sym(var_str) := .data$values) %>%
        dplyr::mutate(date = lubridate::mdy(.data$date_str)) %>%
        dplyr::select(.data$iso3c, .data$region, .data$sub_region,
                      .data$date, !! rlang::sym(var_str))
    }

    confirmed_raw <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", col_types = readr::cols())
    deaths_raw <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", col_types = readr::cols())
    recovered_raw <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", col_types = readr::cols())

    us_confirmed_raw <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv", col_types = readr::cols())
    us_deaths_raw <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv", col_types = readr::cols())

    jh_covid19_data <- prep_country(confirmed_raw) %>%
      dplyr::full_join(prep_country(deaths_raw), by = c("iso3c", "date")) %>%
      dplyr::full_join(prep_country(recovered_raw), by = c("iso3c", "date"))

    jhd_countries <- dplyr::tibble(
      country = unique(confirmed_raw$`Country/Region`),
      iso3c = countrycode::countrycode(.data$country,
                                       origin = "country.name",
                                       destination = "iso3c")
    ) %>% dplyr::filter(!is.na(.data$iso3c))

    country <- jh_covid19_data %>%
      dplyr::left_join(jhd_countries, by = "iso3c") %>%
      dplyr::select(.data$country, .data$iso3c,.data$ date, .data$confirmed,
                    .data$deaths, .data$recovered) %>%
      dplyr::mutate(timestamp = Sys.time())

    country_region <- prep_country_region(confirmed_raw) %>%
      dplyr::full_join(prep_country_region(deaths_raw),
                       by = c("iso3c", "region", "date")) %>%
      dplyr::full_join(prep_country_region(recovered_raw),
                       by = c("iso3c", "region", "date"))

    us_county <- prep_county_us(us_confirmed_raw) %>%
      dplyr::full_join(prep_county_us(us_deaths_raw),
                       by = c("iso3c", "region", "sub_region", "date")) %>%
      dplyr::rename(
        state = .data$region,
        county = .data$sub_region
      )

    lst <- list(
      country = country,
      country_region = country_region,
      us_county = us_county
    )

    lst <- lst[match(type, c('country', 'country_region', 'us_county'))]

    if (!silent) {
      message("Done downloading JHU CSSE data\n")
    }
  }

  if (!silent) {
    data_info("jhu_csse")
  }

  if (length(type) == 1) lst[[1]] else lst
}
