#' Download Apple Mobility Trend Reports Data
#'
#' Provides Mobility Trends Reports related to Covid-19 as provided by Apple 
#' (\url{https://www.apple.com/covid19/mobility}). Since apple is no longer 
#' providing this data since April 14, 2022 the cached data will be returned
#' and calling the function with \code{cache = FALSE} will yield an error.
#' 
#' @param url The URL of the now defunct Apple Mobility Trend Reports. Ignored.
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
#'     from the {tidycovid19} Github repository. As Apple is no longer provides
#'     the data, this needs to be set to \code{TRUE} and this is what it
#'     defaults to. Will return an error if set to \code{FALSE}.
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
download_apple_mtr_data <- function(type = "country", url = NULL, silent = FALSE, cached = TRUE) {
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

  if(!cached) {
    stop(paste(
      "Apple stopped providing Mobility Trend Reports on April 14, 2022.",
      "Use 'cache = TRUE' (the new default) to retrieve the most recent cached",
      "data."
    ))
  }
  
  if (!silent) message("Downloading cached version of Apple's Mobility Trend Reports data...", appendLF = FALSE)
  lst <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/apple_mtr.RDS")))
  lst <- lst[match(type, c('country', 'country_region', 'country_city'))]
  if (!silent)  {
    message(sprintf("done. Timestamp is %s", lst[[1]]$timestamp[1]))
    data_info("apple_mtr")
  }

  if (length(type) == 1) lst[[1]] else lst
}
