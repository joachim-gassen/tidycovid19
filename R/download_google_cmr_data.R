#' Download Google Community Mobility Reports Data
#'
#' Downloads Google Community Mobility Report data
#' (\url{https://www.google.com/covid19/mobility/}).
#' As stated on this webpage, the "reports chart movement trends over time
#' by geography, across different categories of places such as retail and
#' recreation, groceries and pharmacies, parks, transit stations,
#' workplaces, and residential". Google prepares these reports to help
#' interested parties to assess responses to social distancing guidance
#' related to Covid-19. As Google is no longer updating this data since 
#' October 15, 2022 static historic is downloaded and calling the function 
#' with \code{cache = FALSE} yields a warning.
#'
#' @param type The type of data that you want to retrieve. Can be any subset of
#' \itemize{
#'  \item{"country": }{Movement trends by country.}
#'  \item{"country_region": }{Movement trends by country regions as classified by Google (only available for some countries).}
#'  \item{"country_sub_region": }{Movement trends by country sub-regions as classified by Google (only available for some countries).}
#'  \item{"us_county": }{Movement trends at the U.S. county level.}
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
#' df <- download_google_cmr_data(silent = TRUE, cached = TRUE)
#' df %>%
#'   dplyr::group_by(date) %>%
#'   dplyr::summarize(
#'     retail_recreation = mean(retail_recreation, na.rm = TRUE)
#'   ) %>%
#'   ggplot2::ggplot(ggplot2::aes(x = date, y = retail_recreation)) +
#'   ggplot2::geom_line()
#'
#' df <- download_google_cmr_data(type = "country_region", silent = TRUE, cached = TRUE)
#' df %>%
#'   dplyr::filter(iso3c == "USA") %>%
#'   dplyr::select(-iso3c) %>%
#'   dplyr::group_by(region) %>%
#'   dplyr::summarise(`Retail and Recreation Effect` =
#'      max(retail_recreation, na.rm = TRUE) -
#'      min(retail_recreation, na.rm = TRUE)) %>%
#'   dplyr::rename(`U.S. State` = region) %>%
#'   dplyr::arrange(-`Retail and Recreation Effect`)
#'
#' @export
download_google_cmr_data <- function(type = "country", silent = FALSE,
                                     cached = FALSE) {
  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' needs to be a single logical value"
  )
  if (length(cached) > 1 || !is.logical(cached)) stop(
    "'cached' needs to be a single logical value"
  )
  if (!all(type %in% c('country', 'country_region', 'country_sub_region', 'us_county')))
    stop(
      "'type' needs to be a vector containing any set of 'country', 'country_region', and 'us_county'."
    )

  if(cached) {
    if (!silent) message("Downloading cached version of Google's Community Mobility Reports data...", appendLF = FALSE)
    lst <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/google_cmr.RDS")))
    lst <- lst[match(type, c('country', 'country_region', 'country_sub_region', 'us_county'))]
    if (!silent) message(sprintf("done. Timestamp is %s", lst[[1]]$timestamp[1]))
  } else {
    warning("Google stopped providing mobility data as of 2022-10-15. Downloading historic data.")

    # 2022-10-17: Google stopped providing mobility data as of 2022-10-15
    # the URL should be static

    url <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"

    if(!silent) message(sprintf("Downloading '%s'.\n", url))
    raw_data <- readr::read_csv(url, col_types = "cccccccccnnnnnn")
    raw_data$date <- lubridate::ymd(raw_data$date)

    # 2021-02-16: Added a Google place_id (pos 8 , alphanumeric char string)
    # https://developers.google.com/places/web-service/place-id
    
    clean_cmr_data <- function(df) {
      df %>% dplyr::rename(
        retail_recreation = .data$retail_and_recreation_percent_change_from_baseline,
        grocery_pharmacy = .data$grocery_and_pharmacy_percent_change_from_baseline,
        parks = .data$parks_percent_change_from_baseline,
        transit_stations = .data$transit_stations_percent_change_from_baseline,
        workplaces = .data$workplaces_percent_change_from_baseline,
        residential = .data$residential_percent_change_from_baseline
      ) %>%
        dplyr::mutate(
          iso3c = countrycode::countrycode(.data$country_region_code,
                                           origin = "iso2c",
                                           destination = "iso3c"),
          date = lubridate::ymd(.data$date),
          timestamp = Sys.time()
        ) %>%
        dplyr::select(.data$iso3c, dplyr::starts_with("sub_region"), .data$date,
                      dplyr::everything(), -.data$country_region_code)
    }

    country <- raw_data %>%
      dplyr::filter(
        is.na(.data$sub_region_1),
        is.na(.data$sub_region_2),
        is.na(.data$metro_area)
      ) %>%
      dplyr::select(
        -.data$sub_region_1,
        -.data$sub_region_2,
        -.data$metro_area,
        -.data$iso_3166_2_code,
        -.data$census_fips_code,
        -.data$country_region
      ) %>% clean_cmr_data()

    country_region <- raw_data %>%
      dplyr::filter(!is.na(.data$sub_region_1), is.na(.data$sub_region_2)) %>%
      dplyr::select(
        -.data$sub_region_2,
        -.data$country_region,
        -.data$metro_area,
        -.data$iso_3166_2_code,
        -.data$census_fips_code
      ) %>% clean_cmr_data() %>%
      dplyr::rename(
        region = .data$sub_region_1
      )

    country_sub_region <- raw_data %>%
      dplyr::filter(!is.na(.data$sub_region_2)) %>%
      dplyr::select(
        -.data$country_region,
        -.data$metro_area,
        -.data$iso_3166_2_code,
        -.data$census_fips_code
      ) %>% clean_cmr_data()

    us_county <- country_sub_region %>%
      dplyr::filter(.data$iso3c == "USA") %>%
      dplyr::rename(
        state = .data$sub_region_1,
        county = .data$sub_region_2
      ) %>%
      dplyr::select(-.data$iso3c)

    country_sub_region <- country_sub_region %>%
      dplyr::filter(.data$iso3c != "USA")

    lst <- list(
      country = country,
      country_region = country_region,
      country_sub_region = country_sub_region,
      us_county = us_county
    )

    lst <- lst[match(type, c('country', 'country_region', 'country_sub_region', 'us_county'))]

    if (!silent) {
      message("Done downloading Google Community Mobility Reports data\n")
    }
  }

  if (!silent) {
    data_info("google_cmr")
  }

  if (length(type) == 1) lst[[1]] else lst
}
