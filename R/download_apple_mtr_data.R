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
#' @export
download_apple_mtr_data <- function(url = NULL, silent = FALSE, cached = FALSE) {
  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' needs to be a single logical value"
  )
  if (length(cached) > 1 || !is.logical(cached)) stop(
    "'cached' needs to be a single logical value"
  )

  if(cached) {
    if (!silent) message("Downloading cached version of Apple's Mobility Trend Reports data...", appendLF = FALSE)
    df <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/apple_mtr.RDS")))
    if (!silent) message(sprintf("done. Timestamp is %s", df$timestamp[1]))
    return(df)
  }


  if (!silent) message("Start downloading Apple's Mobility Trend Reports data\n")

  if (is.null(url)) {
    df_url <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/apple_mtr_url.RDS")))

    if(!silent) message(sprintf(
      "Obtained URL: %s, timestamp of URL is %s.", df_url$url[1], df_url$timestamp[1]
    ))
    url <- df_url$url[1]
  }

  if(!silent) message(sprintf("Downloading '%s'.\n", url))
  raw_data <- readr::read_csv(url, col_types = readr::cols())

  df <- raw_data %>%
    dplyr::filter(.data$geo_type == "country/region") %>%
    dplyr::select(-.data$geo_type) %>%
    dplyr::group_by(.data$region, .data$transportation_type) %>%
    tidyr::pivot_longer(
      cols = -c(.data$region, .data$transportation_type),
      names_to = "date",
      values_to = "values"
    ) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    tidyr::pivot_wider(
      names_from = .data$transportation_type,
      values_from = .data$values
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      iso3c = countrycode::countrycode(.data$region,
                                       origin = "country.name",
                                       destination = "iso3c")
    ) %>%
    dplyr::select(
      .data$iso3c, dplyr::everything(), -.data$region
    ) %>%
    dplyr::mutate(timestamp = Sys.time())

  if (!silent) message("Done downloading Apple Mobility Trend Reports data\n")
  df
}
