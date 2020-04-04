#' Visualize the spread of the Covid-19 pandemic
#'
#' Provides a flexible visualization of the country-level Covid-19 spread,
#' inpired by the displays created by John Burn-Murdoch from the Financial
#' Times. Uses data from the Johns Hopkins University CSSE team
#' (\url{https://github.com/CSSEGISandData/COVID-19}) and the ACAPS governmental
#' measures database
#' (\url{https://www.acaps.org/covid19-government-measures-dataset}).
#' If your are overwhelmed with the options of the plot, explore them with
#' \code{shiny_covid19_spread()} and use the 'copy code for plot to clipboard'
#' option.
#'
#' @param data The data frame to base the plot on. Should be a merged data
#'     frame obtained by \link{download_merged_data} and defaults to
#'     \code{download_merged_data(cached = TRUE, silent = TRUE)}.
#' @param type The statistic that you want to plot. Needs to be either "confirmed",
#'     "deaths", or "revovered".
#' @param min_cases Defines the zero point of your X axis (the 'event date').
#'     Defaults to 10 cases for deaths and 100 cases otherwise.
#' @param min_by_ctry_obs Limits the plot to countries that have at least that
#'     many days of dater since and including the event date. Defaults to 7.
#' @param edate_cutoff The upper limit of the X axis in event days.
#'     Defaults to 30.
#' @param data_date_str A date string to include in the annotation of the plot
#'     giving the time when the data was pulled. Defaults to the time stemp of the
#'     data. Note that you might run into issues with the default when running
#'     this in a non-english locale. Consider setting it by hand then.
#' @param per_capita If \code{TRUE} data is being plotted as per capita measure
#'     based on World Bank data. Defaults to \code{FALSE}.
#' @param log_scale Do you want the Y-axis to be log-scaled? Defaults to
#'     \code{TRUE}.
#' @param highlight A character vector of ISO3c (ISO 3166-1 alpha-3) codes that
#'     identify countries that you want to highlight. Using the
#'     \code{gghighlight} package, these observations are highlighted by color
#'     and labeled while the other are grayed out. In \code{NULL} (the default),
#'     all countries are labeled. This can cause very cluttered plots.
#' @param intervention If not default \code{NULL} then this identifies the
#'     intervention type that you want to be highlighted by a point marker.
#'     Valid intervention types are based on the ACAPS government measure data
#'     and include general lockdowns ('lockdown'), social distancing
#'     ('soc_dist'), movement restrictions ('mov_rest'), public health measures
#'     ('pub_health'), and social and economic measures ('soc_econ').
#'
#' @return A \link{ggplot2} object.
#'
#'
#' @examples
#' plot_covid19_spread()
#'
#' merged <- download_merged_data(cached = TRUE, silent = TRUE)
#' plot_covid19_spread(merged, highlight = "DEU", intervention = "lockdown")
#'
#' plot_covid19_spread(merged, type = "recovered", min_by_ctry_obs = 10,
#'   edate_cutoff = 40,
#'   highlight = c("ITA", "ESP", "FRA", "DEU", "USA"), intervention = "soc_dist"
#' )
#'
#' @export
plot_covid19_spread <- function(
  data = download_merged_data(cached = TRUE, silent = TRUE),
  type = "deaths", min_cases = ifelse(type == "deaths", 10, 100),
  min_by_ctry_obs = 7, edate_cutoff = 30,
  data_date_str = format(lubridate::as_date(data$timestamp[1]), "%B %d, %Y"),
  per_capita = FALSE, log_scale = TRUE, highlight = NULL, intervention = NULL) {
  if(!type %in% c("confirmed", "deaths", "recovered"))
    stop("Wrong 'type': Only 'confirmed', 'deaths', and 'recovered' are supported")

  data %>%
    dplyr::group_by(.data$iso3c) %>%
    dplyr::filter(!! rlang::sym(type) >= min_cases) %>%
    dplyr::mutate(edate = as.numeric(.data$date - min(.data$date))) %>%
    dplyr::filter(!is.na(.data$edate)) %>%
    dplyr::group_by(.data$country) %>%
    dplyr::filter(dplyr::n() >= min_by_ctry_obs) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$edate <= edate_cutoff) -> df

  if (per_capita) df <- df %>%
    dplyr::mutate(!! type := 1e5*(!! rlang::sym(type))/.data$population) %>%
    dplyr::filter(!is.na(!! rlang::sym(type)))

  if(!is.null(highlight) && !any(highlight %in% df$iso3c))
    stop(paste(
      "Non-NULL 'highlight' value but no countries matched in data",
      "(Did you specify correct ISO3c codes?)"
    ))

  if(!is.null(intervention) && ! intervention %in% names(df))
    stop(paste(
      "Non-NULL 'intervention' value but no variable present in data",
      "(valid intervention types are 'lockdown', 'soc_dist', 'mov_rest',",
      "'pub_health', and 'soc_econ')."
    ))


  if (is.null(intervention)) {
    caption_str <- paste(
      "Data: Johns Hopkins University Center for Systems Science",
      sprintf("and Engineering (JHU CSSE), obtained on %s.", data_date_str)
    )
  } else {
    caption_str <- paste(
      "Case data: Johns Hopkins University Center for Systems Science",
      "and Engineering (JHU CSSE). Interventions data: ACAPS.",
      sprintf("Data obtained on %s.", data_date_str)
    )
  }
  caption_str <- paste(
    caption_str,
    sprintf(
      "The sample is limited to countries with at least %d days of data.",
      min_by_ctry_obs
    )
  )

  if (!is.null(intervention)) caption_str <- paste(
    caption_str,
    sprintf(
      "Dots indicate governmental interventions of type '%s'.", intervention
    )
  )
  caption_str <- paste(strwrap(paste(
    caption_str,
    "Code: https://github.com/joachim-gassen/tidycovid19."
  ), width = 120), collapse = "\n")

  if (type == "deaths") {
    x_str <- sprintf("Days after %s reported death\n",
                     scales::label_ordinal(big.mark = ",")(min_cases))
    if (per_capita)
      y_str <- "Reported deaths per 100,000 inhabitants (logarithmic scale)"
    else y_str <- "Reported deaths (logarithmic scale)"
    title_str <- sprintf("The First %d Days: Reported Deaths", edate_cutoff)
  }
  if (type == "confirmed") {
    x_str <- sprintf("Days after %s confirmed case\n",
                     scales::label_ordinal(big.mark = ",")(min_cases))
    if (per_capita)
      y_str <- "Confirmed cases per 100,000 inhabitants (logarithmic scale)"
    else y_str <- "Confirmed cases (logarithmic scale)"
    title_str <- sprintf("The First %d Days: Confirmed Cases", edate_cutoff)
  }
  if (type == "recovered") {
    x_str <- sprintf("Days after %s recovered case\n",
                     scales::label_ordinal(big.mark = ",")(min_cases))
    y_str <- "Recovered cases (logarithmic scale)"
    title_str <- sprintf("The First %d Days: Recovered Cases", edate_cutoff)
  }

  p <- ggplot2::ggplot(df,
         ggplot2::aes(x = .data$edate, color = .data$country,
                      y = !! rlang::sym(type))) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = x_str,
      y = y_str,
      title = title_str,
      caption = caption_str
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.caption.position =  "plot",
      plot.caption = ggplot2::element_text(hjust = 0),
      axis.title.x = ggplot2::element_text(hjust = 1),
      axis.title.y = ggplot2::element_text(hjust = 1),
    )

  if(log_scale && per_capita)
    p <- p + ggplot2::scale_y_continuous(
      trans='log10',
      labels = scales::number_format(accuracy = 0.01, big.mark = ",")
    )
  if(!log_scale && per_capita)
    p <- p + ggplot2::scale_y_continuous(
      labels = scales::number_format(accuracy = 0.01, big.mark = ",")
    )
  if(log_scale && !per_capita)
    p <- p + ggplot2::scale_y_continuous(trans='log10', labels = scales::comma)
  if(!log_scale && !per_capita)
    p <- p + ggplot2::scale_y_continuous(labels = scales::comma)

  if(!is.null(intervention)) p <- p +
    ggplot2::geom_point(data = df %>%
                 dplyr::group_by(.data$iso3c) %>%
                 dplyr::filter(!! rlang::sym(intervention) > dplyr::lag(!! rlang::sym(intervention))))

  # Make R CHECK happy
  iso3c <- NULL
  country <- NULL

  if(!is.null(highlight)) {
    p <- p +
      gghighlight::gghighlight(iso3c %in% highlight,
                  label_key = country, use_direct_label = TRUE,
                  label_params = list(segment.color = NA, nudge_x = 1),
                  use_group_by = FALSE)
  } else {
    p <- p +
      gghighlight::gghighlight(TRUE,
                  label_key = country, use_direct_label = TRUE,
                  label_params = list(segment.color = NA, nudge_x = 1))
  }

  p
}
