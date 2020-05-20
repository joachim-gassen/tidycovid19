#' Download Apple Mobility Trend Reports Data
#'
#' Downloads Mobility Trends Reports provided by Apple related to Covid-19
#' (\url{https://www.apple.com/covid19/mobility}).
#'
#' @param url The URL of the Apple Mobility Trend Reports is burried in
#'     Java Script, making it non-trivial to parse. The Github repository
#'     of this package contains an R script that uses the 'RSelenium'
#'     package and headless browsing to scrape the URL. The scraped URL
#'     is included in the daily updated data contained in this repository.
#'     By default, the function uses this cached URL to retrieve the data.
#'     You can, however, pass your own URL by setting this variable.
#'     Defaults to \code{NULL} to trigger loading the cached URL.
#' @param type The type of data that you want to retrieve. Can be any subset of
#' \itemize{
#'  \item{"country": }{Mobility trends by country.}
#'  \item{"country_region": }{Mobility trends by country regions as classified by Apple (only available for some countries).}
#'  \item{"country_city": }{Mobility trends by city as classified by Apple (only available for some countries).}
#' }
#' @param silent Whether you want the function to send some status messages to
#'     the console. Might be informative as downloading will take some time
#'     and thus defaults to \code{TRUE}.
#' @param cached Whether you want to download the cached version of the data
#'     from the {tidycovid19} Github repository instead of retrieving the
#'     data from the authorative source. Downloading the cached version is
#'     faster and the cache is updated daily. Defaults to \code{FALSE}.
#'
#' @return A data frame containing the data. It includes a \code{timestamp}
#'     variable indicating the time of data retrieval.
#'
#' @examples
#' df <- download_apple_mtr_data(silent = TRUE, cached = TRUE)
#' df %>%
#'   dplyr::group_by(date) %>%
#'   dplyr::summarize(
#'     walking = mean(walking, na.rm = TRUE)
#'   ) %>%
#'   ggplot2::ggplot(ggplot2::aes(x = date, y = walking)) +
#'   ggplot2::geom_line()
#'
#' df <- download_apple_mtr_data(type = "country_city", silent = TRUE, cached = TRUE)
#' df %>%
#'   dplyr::filter(iso3c == "DEU") %>%
#'   ggplot2::ggplot(ggplot2::aes(x = date, y = driving, color = city)) +
#'   ggplot2::geom_line()
#'
#' @export
download_apple_mtr_data <- function(type = "country", url = NULL, silent = FALSE, cached = FALSE) {
  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' needs to be a single logical value"
  )
  if (length(cached) > 1 || !is.logical(cached)) stop(
    "'cached' needs to be a single logical value"
  )
  if (!all(type %in% c('country', 'country_region', 'country_city')))
    stop(
      "'type' needs to be a vector containing any set of 'country', 'country_region', and 'country_city'."
    )

  if(cached) {
    if (!silent) message("Downloading cached version of Apple's Mobility Trend Reports data...", appendLF = FALSE)
    lst <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/apple_mtr.RDS")))
    lst <- lst[match(type, c('country', 'country_region', 'country_city'))]
    if (!silent)  message(sprintf("done. Timestamp is %s", lst[[1]]$timestamp[1]))
  } else {
    if (!silent) message("Start downloading Apple's Mobility Trend Reports data\n")

    if (is.null(url)) {
      df_url <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/apple_mtr_url.RDS")))

      if(!silent) message(sprintf(
        "Obtained URL: %s, timestamp of URL is %s.", df_url$url[1], df_url$timestamp[1]
      ))
      url <- df_url$url[1]
    }

    if(!silent) message(sprintf("Downloading '%s'.\n", url))
    raw_data <- readr::read_csv(url, col_types = readr::cols()) %>%
      rename(sub_region = `sub-region`)

    raw_data$iso3c <- NA

    raw_data$iso3c[raw_data$geo_type == "country/region"] <-
      countrycode::countrycode(raw_data$region[raw_data$geo_type == "country/region"],
                               origin = "country.name",
                               destination = "iso3c")

    raw_data$iso3c[raw_data$geo_type != "country/region"] <-
      countrycode::countrycode(raw_data$country[raw_data$geo_type != "country/region"],
                               origin = "country.name",
                               destination = "iso3c")

    clean_mtr_data <- function(df) {
      city <-  "sub_region" %in% names(df)
      df <- df %>%
        dplyr::select(-.data$geo_type, -.data$alternative_name, -.data$country) %>%
        dplyr::group_by(.data$iso3c, .data$region, .data$transportation_type)

      if (city) {
        df <- df %>% tidyr::pivot_longer(
            cols = -c(.data$iso3c, .data$region, .data$sub_region,
                      .data$transportation_type),
            names_to = "date",
            values_to = "values"
          )
      } else {
        df <- df %>%         tidyr::pivot_longer(
          cols = -c(.data$iso3c, .data$region, .data$transportation_type),
          names_to = "date",
          values_to = "values"
        )
      }
      df <- df %>% dplyr::mutate(date = lubridate::ymd(date)) %>%
        tidyr::pivot_wider(
          names_from = .data$transportation_type,
          values_from = .data$values
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(timestamp = Sys.time())

      if (city) df %>% dplyr::arrange(.data$iso3c, .data$region,
                                      .data$sub_region, .data$date)
      else df %>% dplyr::arrange(.data$iso3c, .data$region, .data$date)
    }

    country <- raw_data %>%
      dplyr::filter(.data$geo_type == "country/region") %>%
      dplyr::select(-.data$sub_region) %>%
      clean_mtr_data() %>%
      dplyr::select(-.data$region)

    country_region <- raw_data %>%
      dplyr::select(-.data$sub_region) %>%
      dplyr::filter(.data$geo_type == "sub-region") %>%
      clean_mtr_data()

    country_city <- raw_data %>%
      dplyr::filter(.data$geo_type == "city") %>%
      clean_mtr_data() %>%
      dplyr::rename(city = .data$region)

    lst <- list(
      country = country,
      country_region = country_region,
      country_city = country_city
    )

    lst <- lst[match(type, c('country', 'country_region', 'country_city'))]

    if (!silent) {
      message("Done downloading Apple Mobility Trend Reports data\n")
    }
  }

  if (!silent) {
    data_info("apple_mtr")
  }

  if (length(type) == 1) lst[[1]] else lst
}
