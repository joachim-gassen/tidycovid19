#' Download current country-level World Bank data
#'
#' Uses the \code{wbstats} package to download recent country-level data from
#' the World Bank (\url{https://data.worldbank.org}).
#'
#' @param vars Specify the data items that you want to retrieve.
#' @param labels Give somewhat more informative variable names for the output
#'     data frame. Has to match the length of \code{vars} and needs to contain
#'     valid variable names.
#' @param var_def Do you want to retrieve a data frame containing the World Bank
#'     data definitions along with the actual data? Defaults to \code{FALSE}.
#' @param silent Whether you want the function to send some status messages to
#'     the console. Might be informative as downloading will take some time
#'     and thus defaults to \code{TRUE}.
#' @param cached Whether you want to download the cached version of the data
#'     from the {tidycovid19} Github repository instead of retrieving the
#'     data from the authorative source. Downloading the cached version is
#'     faster and the cache is updated daily. Defaults to \code{FALSE}.
#'
#' @return If \code{var_def = FALSE}, a data frame containing the
#'     data and a \code{timestamp} variable indicating the time of data
#'     retrieval. Otherwise, a list including the data frame with the
#'     data followed by a data frame containing the variable definitions.
#'
#'
#' @examples
#' df <- download_wbank_data(silent = TRUE, cached = TRUE)
#' df %>%
#'   dplyr::select(country, population) %>%
#'   dplyr::arrange(-population)
#'
#' lst <- download_wbank_data(silent = TRUE, cached = TRUE, var_def = TRUE)
#' lst[[1]] %>%
#'   tidyr::pivot_longer(5:10, names_to = "wbank_variable", values_to = "values") %>%
#'   dplyr::group_by(wbank_variable) %>%
#'   dplyr::summarise(non_na = sum(!is.na(values)))
#'
#' @export
download_wbank_data <- function(vars = c("SP.POP.TOTL", "AG.LND.TOTL.K2",
                                         "EN.POP.DNST", "EN.URB.LCTY",
                                         "SP.DYN.LE00.IN", "NY.GDP.PCAP.KD"),
                                labels = c("population", "land_area_skm",
                                           "pop_density", "pop_largest_city",
                                           "life_expectancy", "gdp_capita"),
                                var_def = FALSE,
                                silent = FALSE,
                                cached = FALSE) {
  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' needs to be a single logical value"
  )
  if (!silent) message("Start downloading World Bank data\n")

  if (!is.character(vars)) stop(
    "'vars' needs to be a character vector."
  )
  if (!is.null(labels) && !is.character(labels)) stop(paste(
    "'labels' needs to be either NULL or a character vector with the same",
    "length as 'vars'."
  ))
  if (length(var_def) > 1 || !is.logical(var_def)) stop(
    "'var_def' needs to be a single logical value"
  )
  if (length(cached) > 1 || !is.logical(cached)) stop(
    "'cached' needs to be a single logical value"
  )

  if(cached) {
    if (!identical(vars, c("SP.POP.TOTL", "AG.LND.TOTL.K2",
                           "EN.POP.DNST", "EN.URB.LCTY",
                           "SP.DYN.LE00.IN", "NY.GDP.PCAP.KD")) ||
        !identical(labels,  c("population", "land_area_skm",
                              "pop_density", "pop_largest_city",
                              "life_expectancy", "gdp_capita")))
      stop(paste(
        "'cached' == TRUE but either 'vars' or 'labels' is different from",
        "the default. You need to use 'cached' == FALSE when you want to",
        "retrieve customized World Bank data."
        ))
    if (!silent) message("Downloading cached version of World Bank data...", appendLF = FALSE)
    wb_list <-readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/wbank.RDS")))
    if (!silent) message(sprintf("done. Timestamp is %s", wb_list[[1]]$timestamp[1]))
    df <- wb_list[[1]]
    data_def <- wb_list[[2]]
  } else {
    pull_worldbank_data <- function(vars) {
      new_cache <- wbstats::wbcache()
      all_vars <- as.character(unique(new_cache$indicators$indicatorID))
      data_wide <- wbstats::wb(indicator = vars, mrv = 10, return_wide = TRUE)
      new_cache$indicators[new_cache$indicators[,"indicatorID"] %in% vars, ] %>%
        dplyr::rename(var_name = .data$indicatorID) %>%
        dplyr::mutate(var_def = paste(.data$indicator, "\nNote:",
                                      .data$indicatorDesc, "\nSource:",
                                      .data$sourceOrg)) %>%
        dplyr::select(.data$var_name, .data$var_def) -> wb_data_def
      new_cache$countries %>%
        dplyr::select(.data$iso3c, .data$iso2c, .data$country,
                      .data$region, .data$income) -> ctries
      dplyr::left_join(data_wide, ctries, by = "iso3c") %>%
        dplyr::rename(year = .data$date,
                      iso2c = .data$iso2c.y,
                      country = .data$country.y) %>%
        dplyr::select(.data$iso3c, .data$iso2c, .data$country,
                      .data$region, .data$income, dplyr::everything()) %>%
        dplyr::select(-.data$iso2c.x, -.data$country.x) %>%
        dplyr::filter(.data$region != "Aggregates") -> wb_data

      wb_data$year <- as.numeric(wb_data$year)
      wb_data_def<- dplyr::left_join(data.frame(var_name = names(wb_data),
                                                stringsAsFactors = FALSE),
                                     wb_data_def, by = "var_name")
      wb_data_def$var_def[1:6] <- c(
        "Three letter ISO country code as used by World Bank",
        "Two letter ISO country code as used by World Bank",
        "Country name as used by World Bank",
        "World Bank regional country classification",
        "World Bank income group classification",
        "Calendar year of observation"
      )
      wb_data_def <- rbind(wb_data_def, c("timestamp", "Timestamp of download"))
      return(list(wb_data, wb_data_def, Sys.time()))
    }

    wb_list <- pull_worldbank_data(vars)
    wb_data <- wb_list[[1]]
    wb_data_def <- wb_list[[2]]

    wb_data_def[wb_data_def$var_name %in% vars, ] <-
      wb_data_def[wb_data_def$var_name %in% vars, ][order(
        match(wb_data_def$var_name[wb_data_def$var_name %in% vars], vars)), ]
    wb_data_def$var_name[wb_data_def$var_name %in% vars] <- labels


    df <- wb_data %>%
      dplyr::group_by(.data$iso3c) %>%
      dplyr::arrange(.data$iso3c, .data$year) %>%
      dplyr::summarise_at(dplyr::vars(-.data$iso2c, -.data$country, -.data$region,
                                      -.data$income, -.data$year),
                          function(x) dplyr::last(stats::na.omit(x))
      ) %>%
      dplyr::select(dplyr::one_of(c("iso3c", vars))) %>%
      dplyr::left_join(wb_data %>%
                         dplyr::select(.data$iso3c, .data$country,
                                       .data$region, .data$income) %>%
                         dplyr::distinct(),
                       by = "iso3c") %>%
      dplyr::select(dplyr::one_of(c("iso3c", "country", "region", "income", vars))) %>%
      dplyr::mutate(timestamp = wb_list[[3]])

    names(df)[5:(ncol(df) - 1)] <- labels
    data_def <- wb_data_def[c(1,3,4,5,7:nrow(wb_data_def)), ]
    if (!silent) message("Done downloading World Bank data\n")
  }
  if(var_def) list(df, data_def) else df
}

