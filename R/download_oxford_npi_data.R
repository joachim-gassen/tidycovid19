#' Download Oxford non-pharmaceutical interventions data
#'
#' Downloads non-pharmaceutical interventions (NPI) data related to Covid-19
#' from the Oxford Covid-19 Government Response Tracker.
#' (\url{https://www.bsg.ox.ac.uk/research/research-projects/oxford-covid-19-government-response-tracker}).
#' It currently only uses the policy measures from that data and tidies them
#' into long format, defining observations to be interventions and discarding
#' observations with \code{NA} and unchanged \code{0} measures.
#'
#' @param type The type of data that you want to retrieve. Can be any subset of
#' \itemize{
#'  \item{"measures": }{Government response measures, recoded to event-day structure}
#'  \item{"index": }{The Strigency Indices as repored by the OxCGRT team}
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
#' @details The Oxford data is currently not included in the data frame
#'     produced by \code{download_merged_data()} as the ACAPS NPI data seem
#'     to be of better quality overall. See
#'     \href{https://joachim-gassen.github.io/2020/04/exploring-and-benchmarking-oxford-government-response-data/}{this blog post}
#'     and this
#'     \href{https://github.com/OxCGRT/covid-policy-tracker/issues/1}{Github issue}
#'     for a discussion.
#'
#' @examples
#' df <- download_oxford_npi_data(type = "measures", silent = TRUE, cached = TRUE)
#' df %>%
#'   dplyr::group_by(country) %>%
#'   dplyr::summarise(number_of_interventions = dplyr::n()) %>%
#'   dplyr::arrange(-number_of_interventions)
#'
#' @export
download_oxford_npi_data <- function(type = "measures", silent = FALSE, cached = FALSE) {
  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' needs to be a single logical value"
  )
  if (length(cached) > 1 || !is.logical(cached)) stop(
    "'cached' needs to be a single logical value"
  )
  if (!all(type %in% c('measures', 'index')))
    stop(
      "'type' needs to be a vector containing either both or one of 'measures' or 'index'."
    )

  if(cached) {
    if (!silent) message("Downloading cached version of Oxford NPI data...", appendLF = FALSE)
    lst <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/oxford_npi.RDS")))
    lst <- lst[match(type, c('measures', 'index'))]
    if (!silent) message(sprintf("done. Timestamp is %s", lst[[1]]$timestamp[1]))
  } else {
    if (!silent) message("Start downloading Oxford NPI data\n")

    # 2020-04-08 The csv file seems to include trailing separators. This creates
    # a warning.

    # 2020-04-29 New file format and location

    data_url <- "https://oxcgrtportal.azurewebsites.net/api/CSVDownload"
    if (!silent) raw_data <- readr::read_csv(
      data_url, col_types = readr::cols(), trim_ws = TRUE, quote = '"', guess_max = 1e6,
    )
    else suppressWarnings(
      raw_data <- readr::read_csv(data_url, col_types = readr::cols())
    )

    df <- raw_data
    # Fix column names for pivot_long()
    names(df)[c(seq(from = 4, by = 3, length.out = 8), 36,
                c(seq(from = 39, by = 2, length.out = 4)))] <-
      paste0(c(rep("C", 8), rep("H", 5)), c(1:8, 1:5), "_measure")

    df <- df %>% dplyr::select(1:26, 36:46) %>%
      # S7, S12, S13 have no "IsGeneral" value. I attach NA vars for consistency
      dplyr::mutate(H2_Flag = NA,
                    H3_Flag = NA,
                    H4_Flag = NA,
                    H5_Flag = NA,
                    ) %>%
      tidyr::pivot_longer(4:41, names_pattern = "(.*)_(.*)", names_to = c("type", ".value")) %>%
      dplyr::rename(
        country = .data$CountryName,
        iso3c = .data$CountryCode,
        date = .data$Date,
        npi_measure = .data$measure,
        npi_flag = .data$Flag,
        npi_notes = .data$Notes
      ) %>%
      dplyr::mutate(date = lubridate::ymd(date))

    # Fix NPI type categories
    lup <- dplyr::tibble(
      type = paste(paste0(c(rep("C", 8), rep("H", 5)), c(1:8, 1:5))),
      npi_type = sub("[CH]\\d*_", "",
                     names(raw_data)[c(seq(from = 4, by = 3, length.out = 8), 36,
                                       c(seq(from = 39, by = 2, length.out = 4)))])
    )

    oxford_pm <- df %>%
      dplyr::left_join(lup, by = "type") %>%
      dplyr::mutate(npi_notes = stringr::str_squish(.data$npi_notes)) %>%
      dplyr::select(.data$iso3c, .data$country, .data$date, .data$npi_type,
                    .data$npi_measure, .data$npi_flag, .data$npi_notes) %>%
      dplyr::arrange(.data$iso3c, .data$npi_type, .data$date)

    # Removing stale observations that just reflect prior intervention (and thus
    # are unchanged from prior observation). Observations were the only change was
    # a missing note compared to prior observations are discared as well as most
    # notes seem to be not sticky.

    oxford_pm_events <- oxford_pm %>%
      dplyr::group_by(.data$iso3c, .data$npi_type) %>%
      dplyr::filter(
        (dplyr::row_number() == 1 &
           (!is.na(.data$npi_flag) | !is.na(.data$npi_measure) | !is.na(.data$npi_notes)))  |
          (is.na(dplyr::lag(.data$npi_flag)) & !is.na(.data$npi_flag)) |
          (is.na(dplyr::lag(.data$npi_measure)) & !is.na(.data$npi_measure)) |
          (is.na(dplyr::lag(.data$npi_notes)) & !is.na(.data$npi_notes)) |
          (!is.na(dplyr::lag(.data$npi_flag)) & is.na(.data$npi_flag)) |
          (!is.na(dplyr::lag(.data$npi_measure)) & is.na(.data$npi_measure))  |
          (dplyr::lag(.data$npi_flag) != .data$npi_flag) |
          (dplyr::lag(.data$npi_measure) != .data$npi_measure) |
          (dplyr::lag(.data$npi_notes) != .data$npi_notes)
      ) %>%
      dplyr::ungroup()

    # Removing na and 0 measure observation but keeping observations that
    # set measure = 0 when prior to that it was > 0
    # (indicating that an intervention was removed)

    measures <- oxford_pm_events %>%
      dplyr::filter(!is.na(.data$npi_measure)) %>%
      dplyr::group_by(.data$iso3c, .data$npi_type) %>%
      dplyr::filter((.data$npi_measure != 0) |
                      ((.data$npi_measure == 0) &
                         dplyr::lag(.data$npi_measure != 0)))

    # 2020-04-30
    # The fiscal measures data used to be too messy to use.
    # I have not checked yet whether this is still the case.

    # 2020-04-20
    # Filtering the index data
    # Ingnoring the 'for_display' items as
    # any(!is.na(index$stringency_index) &
    #     (index$stringency_index != index$stringency_index_for_display))
    # [1] FALSE
    # which(!is.na(index$legacy_stringency_index) &
    #        (index$legacy_stringency_index != index$legacy_stringency_index_for_display))
    # [1]   119   120   121  2768  4626  7887  7888 13780 13781 15348 17285 17405 17406
    # Seems that the _for_display measures simply 'fix' some data inconsistencies
    # in recent observations by carrying forward old values with no documented
    # method

    index <- raw_data %>%
      dplyr::rename(
        country = .data$CountryName,
        iso3c = .data$CountryCode,
        stringency_index = .data$StringencyIndex,
        legacy_stringency_index = .data$LegacyStringencyIndex
      ) %>%
      dplyr::mutate(date = lubridate::ymd(.data$Date)) %>%
      dplyr::select(
        .data$country, .data$iso3c, .data$date,
        .data$stringency_index, .data$legacy_stringency_index
      ) %>% dplyr::filter(rowSums(!is.na(.)) > 3) %>%
      dplyr::arrange(iso3c, date)

    lst <- list(measures = measures, index = index)

    if (!silent) message("Done downloading Oxford NPI data\n")
  }
  if (length(type) == 1) lst[[type]] else lst
}
