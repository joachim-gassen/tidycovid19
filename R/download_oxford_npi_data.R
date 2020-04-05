#' Download non-pharmaceutical interventions data
#'
#' Downloads non-pharmaceutical interventions (NPI) data related to Covid-19
#' from the Oxford Covid-19 Government Response Tracker.
#' (\url{https://www.bsg.ox.ac.uk/research/research-projects/oxford-covid-19-government-response-tracker}).
#' It currently only uses the policy measures from that data and tidies them
#' into long format, defining observations to be interventions and discarding
#' observations with \code{NA} and unchanged \code{0} measures.
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
#' @details The Oxford data is currently not included in the data frame
#'     produced by \code{download_merged_data()} as the ACAPS NPI data seem
#'     to be of better quality overall. See
#'     \href{https://joachim-gassen.github.io/2020/04/exploring-and-benchmarking-oxford-government-response-data/}{this blog post}
#'     for a discussion.
#'
#' @examples
#' df <- download_oxford_npi_data(silent = TRUE, cached = TRUE)
#' df %>%
#'   dplyr::group_by(country) %>%
#'   dplyr::summarise(number_of_interventions = dplyr::n()) %>%
#'   dplyr::arrange(-number_of_interventions)
#'
#' @export
download_oxford_npi_data <- function(silent = FALSE, cached = FALSE) {
  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' needs to be a single logical value"
  )
  if (length(cached) > 1 || !is.logical(cached)) stop(
    "'silent' needs to be a single logical value"
  )

  if(cached) {
    if (!silent) message("Downloading cached version of Oxford NPI data...", appendLF = FALSE)
    df <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/oxford_npi.RDS")))
    if (!silent) message(sprintf("done. Timestamp is %s", df$timestamp[1]))
    return(df)
  }

  if (!silent) message("Start downloading Oxford NPI data\n")

  dta_url <- "https://www.bsg.ox.ac.uk/sites/default/files/OxCGRT_Download_latest_data.xlsx"

  tmp_file <- tempfile(".xlsx")
  utils::download.file(dta_url, tmp_file, quiet = silent, mode = "wb")
  raw_data <- readxl::read_excel(
    tmp_file,
    col_types = c("text", "text", "numeric",
                  rep(c("numeric", "numeric", "text"), 6),
                  rep(c("numeric", "text"), 5), rep("numeric", 3), "skip")
  )

  df <- raw_data
  # Fix column names for pivot_long()
  names(df)[seq(from = 4, by = 3, length.out = 7)] <- paste0("S", 1:7, "_measure")

  df <- df %>% dplyr::select(1:23) %>%
    # S7 has no "IsGeneral" value. I attach an NA var for consistency
    dplyr::mutate(S7_IsGeneral = NA) %>%
    tidyr::pivot_longer(4:24, names_pattern = "(.*)_(.*)", names_to = c("type", ".value")) %>%
    dplyr::rename(
      country = .data$CountryName,
      iso3c = .data$CountryCode,
      date = .data$Date,
      npi_measure = .data$measure,
      npi_is_general = .data$IsGeneral,
      npi_notes = .data$Notes
    ) %>%
    dplyr::mutate(date = lubridate::ymd(date))

  # Fix NPI type categories
  lup <- dplyr::tibble(
    type = paste(paste0("S", 1:7)),
    npi_type = sub("S\\d*_", "",
                   names(raw_data)[seq(from = 4, by = 3, length.out = 7)])
  )

  oxford_pm <- df %>%
    dplyr::left_join(lup, by = "type") %>%
    dplyr::select(.data$iso3c, .data$country, .data$date, .data$npi_type,
                  .data$npi_measure, .data$npi_is_general, .data$npi_notes) %>%
    dplyr::arrange(.data$iso3c, .data$npi_type, .data$date)

  # Removing stale observation that just reflect prior intervention (and thus
  # are unchanged from prior observation). Observations were the only change was
  # a missing note compared to prior observations are discared as well as most
  # notes seem to be not sticky.

  oxford_pm_events <- oxford_pm %>%
    dplyr::group_by(.data$iso3c, .data$npi_type) %>%
    dplyr::filter(
      (dplyr::row_number() == 1 &
         (!is.na(.data$npi_is_general) | !is.na(.data$npi_measure) | !is.na(.data$npi_notes)))  |
        (is.na(dplyr::lag(.data$npi_is_general)) & !is.na(.data$npi_is_general)) |
        (is.na(dplyr::lag(.data$npi_measure)) & !is.na(.data$npi_measure)) |
        (is.na(dplyr::lag(.data$npi_notes)) & !is.na(.data$npi_notes)) |
        (!is.na(dplyr::lag(.data$npi_is_general)) & is.na(.data$npi_is_general)) |
        (!is.na(dplyr::lag(.data$npi_measure)) & is.na(.data$npi_measure))  |
        (dplyr::lag(.data$npi_is_general) != .data$npi_is_general) |
        (dplyr::lag(.data$npi_measure) != .data$npi_measure) |
        (dplyr::lag(.data$npi_notes) != .data$npi_notes)
    ) %>%
    dplyr::ungroup()

  # Removing na and 0 measure observation but keeping observations that
  # set measure = 0 when prior to that it was > 0
  # (indicating that an intervention was removed)

  df <- oxford_pm_events %>%
    dplyr::filter(!is.na(.data$npi_measure)) %>%
    dplyr::group_by(.data$iso3c, .data$npi_type) %>%
    dplyr::filter((.data$npi_measure != 0) |
                    ((.data$npi_measure == 0) &
                       dplyr::lag(.data$npi_measure != 0)))


  # The fiscal measures data currewntly seem too messy to use. It is unclear
  # whether they measures budget levels or newly announced budgets. The
  # frequency of zeros seems to indicate the latter but then again some
  # observations (look at Canada CAN) are consistent with the former.
  #
  # Also at times it seems to indicate ordinal instead of monetary data
  # Look at Dominican Republic	DOM 2020-03-17 Fiscal measures	1

  if (!silent) message("Done downloading Oxford NPI data\n")
  df
}
