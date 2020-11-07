#' Download ACAPS non-pharmaceutical interventions data
#'
#' Downloads non-pharmaceutical interventions (NPI) data related to Covid-19
#' from the ACAPS governmental measures database
#' (\url{https://www.acaps.org/covid19-government-measures-dataset}).
#'
#' @param silent Whether you want the function to send some status messages to
#'     the console. Might be informative as downloading will take some time
#'     and thus defaults to \code{TRUE}.
#' @param cached Whether you want to download the cached version of the data
#'     from the {tidycovid19} Github repository instead of retrieving the
#'     data from the authorative source. Downloading the cached version is
#'     faster and the cache is updated daily. Defaults to \code{FALSE}.
#'
#' @return A data frame containing the data, organized by intervention. It
#'     includes a \code{timestamp} variable indicating the time of data
#'     retrieval.
#'
#' @examples
#' df <- download_acaps_npi_data(silent = TRUE, cached = TRUE)
#' df %>%
#'   dplyr::group_by(country) %>%
#'   dplyr::summarise(number_of_interventions = dplyr::n()) %>%
#'   dplyr::arrange(-number_of_interventions)
#'
#' @export
download_acaps_npi_data <- function(silent = FALSE, cached = FALSE) {
  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' needs to be a single logical value"
  )
  if (length(cached) > 1 || !is.logical(cached)) stop(
    "'cached' needs to be a single logical value"
  )

  if(cached) {
    if (!silent) message("Downloading cached version of ACAPS NPI data...", appendLF = FALSE)
    df <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/acaps_npi.RDS")))
    if (!silent) {
      message(sprintf("done. Timestamp is %s", df$timestamp[1]))
      data_info("acaps_npi")
    }
    return(df)
  }

  if (!silent) message("Start downloading ACAPS NPI data\n")

    url <- "https://data.humdata.org/dataset/acaps-covid19-government-measures-dataset"

  selector_path <- paste0(
    "#data-resources-0 > div > ul > li:nth-child(2) > ",
    "div.hdx-btn-group.hdx-btn-group-fixed > ",
    "a.btn.btn-empty.btn-empty-blue.hdx-btn.resource-url-analytics.ga-download"
  )

  dta_url <- xml2::read_html(url) %>%
    rvest::html_node(css = selector_path) %>% rvest::html_attr('href')

  tmp_file <- tempfile(".xlsx")
  utils::download.file(paste0("https://data.humdata.org", dta_url), tmp_file,
                       quiet = silent, mode = "wb")

  # raw_dta <- readxl::read_excel(tmp_file, sheet = "Database")
  # 2020-08-21 Some cells in DATE_IMPLEMENTED all ill-formatted as strings
  # this is why we have to jump through a few hoops here...

  # 2020-09-27 Name changes (sheet is now named 'Dataset')
  #            And some variables seem to have leading whitespace...
  # 2020-10-09 Leading whitespace is gone ;-)

  raw_dta <- readxl::read_excel(
    tmp_file, sheet = "Dataset",
    col_types = c("numeric", rep("text", 11), "date", rep("text", 3),
                  "date", "text")
  )

  raw_dta$DATE_IMPLEMENTED <- as.Date(raw_dta$DATE_IMPLEMENTED)

  df <- raw_dta
  names(df) <-tolower(names(df))
  names(df)[16] <- "alternative_source"

  df <- df %>%
    dplyr::select(-.data$pcode) %>% # 2020-08-21 is all NA
    dplyr::filter(!is.na(.data$date_implemented),
                  !is.na(.data$category)) %>%
    dplyr::rename(iso3c = .data$iso) %>%
    dplyr::mutate(timestamp = Sys.time())

  if (!silent) {
    message("Done downloading ACAPS NPI data\n")
    data_info("acaps_npi")
  }

  df
}
