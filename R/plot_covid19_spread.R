#' Visualize the spread of the Covid-19 pandemic
#'
#' Provides a flexible visualization of the country-level Covid-19 spread,
#' inpired by the displays created by John Burn-Murdoch from the Financial
#' Times. Uses data from the Johns Hopkins University CSSE team
#' (\url{https://github.com/CSSEGISandData/COVID-19}), the ACAPS governmental
#' measures database
#' (\url{https://www.acaps.org/covid19-government-measures-dataset}), and the
#' World Bank (\url{https://data.worldbank.org}).
#' If your are overwhelmed with the options of the plot, explore them with
#' \code{shiny_covid19_spread()} and use the 'copy code for plot to clipboard'
#' option.
#'
#' @param data The data frame to base the plot on. Should be a merged data
#'     frame obtained by \link{download_merged_data} and defaults to
#'     \code{download_merged_data(cached = TRUE, silent = TRUE)}.
#' @param type The statistic that you want to plot. Needs to be either
#'     "confirmed", "deaths", "recovered" or "active", defined as the difference
#'     of "confirmed", "deaths" and "recovered".
#' @param min_cases Defines the zero point of your X axis (the 'event date').
#'     Defaults to 100 cases for deaths and 1,000 cases otherwise.
#' @param min_by_ctry_obs Limits the plot to countries that have at least that
#'     many days of data since and including the event date. Defaults to 7.
#' @param edate_cutoff The upper limit of the X axis in event days.
#'     Defaults to 40.
#' @param data_date_str A date string to include in the annotation of the plot
#'     giving the time when the data was pulled. Defaults to the timestamp of the
#'     data. Note that you might run into issues with the default when running
#'     this in a non-english locale. Consider setting it by hand then.
#' @param cumulative If \code{TRUE} (the default) data is being plotted as
#'     cumulative (showing the total figures). If \code{FALSE}, (averaged) daily
#'     changes are plotted instead. See \code{change_ave} to set the averaging
#'     window.
#' @param change_ave Number of days to average over when you plot daily changes.
#' @param per_capita If \code{TRUE} data is being plotted as per capita measure
#'     based on World Bank data. Defaults to \code{FALSE}.
#' @param per_capita_x_axis If \code{TRUE}, the 'event date' cutoff for the x
#'     axis set by \code{min_cases} is evaluated based on by capita
#'     measures (cases per 100,000  inhabitants). Only feasible when
#'     \code{per_capita} is \code{TRUE}. Other than in older versions
#'     it now defaults to \code{TRUE} whenever \code{per_capita} is \code{TRUE}
#'     else \code{FALSE}.
#' @param population_cutoff Do you want to restrict the plot to countries that
#'     exceed a certain population cutoff? Takes a value in millions and
#'     defaults to 0. Useful for per capita displays.
#' @param log_scale Do you want the Y-axis to be log-scaled? Defaults to
#'     \code{TRUE}.
#' @param highlight A character vector of ISO3c (ISO 3166-1 alpha-3) codes that
#'     identify countries that you want to highlight. Using the
#'     \code{gghighlight} package, these observations are highlighted by color
#'     and labeled while the other are grayed out. In \code{NULL} (the default),
#'     all countries are labeled. This can cause very cluttered plots.
#' @param exclude_others If \code{TRUE}, unhighlighted countries are excluded
#'     from the plot. If \code{FALSE} (the default), unhighlighted countries
#'     are grayed out.
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
  type = "deaths",
  min_cases = ifelse(per_capita, ifelse(type == "deaths", 5, 50),
                     ifelse(type == "deaths", 1000, 10000)),
  min_by_ctry_obs = 7, edate_cutoff = 50,
  data_date_str = format(lubridate::as_date(data$timestamp[1]), "%B %d, %Y"),
  cumulative = TRUE, change_ave = 7, per_capita = FALSE,
  per_capita_x_axis = ifelse(per_capita, TRUE, FALSE), population_cutoff = 0,
  log_scale = TRUE, highlight = NULL, exclude_others = FALSE,
  intervention = NULL) {
  if(!type %in% c("confirmed", "deaths", "recovered", "active"))
    stop("Wrong 'type': Only 'confirmed', 'deaths', 'recovered' and 'active' are supported")

  if(!is.logical(cumulative)) stop ("'cumulative' needs to be a logical value")
  change_ave <- as.integer(change_ave)
  if(change_ave < 0)
    stop ("'change_ave' needs to be a positive integer")

  if(!per_capita && per_capita_x_axis)
    stop(paste(
      "'per_capita_x_axis' can be set to 'TRUE' only",
      "if 'per_capita' is set to 'TRUE'"
    ))

  if (population_cutoff > 0 || per_capita || per_capita_x_axis)
  message(paste(
    "Population data required. Observations for the following jurisdictions",
    "will be dropped as the World Bank is not providing population data for",
    "them: ", paste(unique(data$iso3c[is.na(data$population)]), collapse = ", ")
  ))

  if (population_cutoff > 0) {
    data <- data %>%
      dplyr::filter(.data$population > 1e6*population_cutoff)
  }

  data <- data %>%
    dplyr::mutate(active = .data$confirmed - .data$recovered - .data$deaths)

  if (!cumulative)
    data <- data %>%
      dplyr::group_by(.data$iso3c) %>%
      dplyr::mutate(
        delta = !! rlang::sym(type) - dplyr::lag(!! rlang::sym(type)),
        change = zoo::rollmean(.data$delta, change_ave, na.pad=TRUE, align="right")
      ) %>%
      dplyr::ungroup()

  if(per_capita_x_axis) {
    data <- data %>%
      dplyr::group_by(.data$iso3c) %>%
      dplyr::filter((!! rlang::sym(type)*1e5)/.data$population >= min_cases)
  } else {
    data <- data %>%
      dplyr::group_by(.data$iso3c) %>%
      dplyr::filter(!! rlang::sym(type) >= min_cases)
  }

  data %>%
    dplyr::mutate(edate = as.numeric(.data$date - min(.data$date))) %>%
    dplyr::filter(!is.na(.data$edate)) %>%
    dplyr::group_by(.data$country) %>%
    dplyr::filter(dplyr::n() >= min_by_ctry_obs) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$edate <= edate_cutoff) %>%
    dplyr::filter(!is.na(!! rlang::sym(type))) -> df

  if (log_scale) df <- df %>%
    dplyr::filter(!! rlang::sym(type) > 0)

  if (!cumulative) df <- df %>%
    dplyr::mutate(!! type := .data$change)

  if (per_capita) df <- df %>%
    dplyr::filter(!is.na(.data$population)) %>%
    dplyr::mutate(!! type := 1e5*(!! rlang::sym(type))/.data$population)

  df <- df %>%
    dplyr::filter(!is.na(!! rlang::sym(type)))

  if (log_scale)  df <- df %>%
    dplyr::filter(!! rlang::sym(type) > 0)

  if(!is.null(highlight) && exclude_others) df <- df %>%
    dplyr::filter(iso3c %in% highlight)

  if(!is.null(highlight) && (length(highlight) > 1 || highlight != "") && !any(highlight %in% df$iso3c))
    warning(paste(
      "Non-NULL 'highlight' value but no countries matched in data",
      "(Did you specify correct ISO3c codes or do values for 'min_cases',",
      "'min_by_ctry_obs' and/or 'edate_cutoff' lead to the exclusion",
      "of your selected countries' data?)"
    ))

  if(!is.null(intervention) && ! intervention %in% names(df))
    stop(paste(
      "Non-NULL 'intervention' value but no variable present in data",
      "(valid intervention types are 'lockdown', 'soc_dist', 'mov_rest',",
      "'pub_health', and 'soc_econ')."
    ))

  caption_str <- paste(
    "Case data: Johns Hopkins University Center for Systems Science",
    "and Engineering (JHU CSSE)."
  )
  if(!is.null(intervention)) {
    caption_str <- paste(caption_str, "Interventions data: ACAPS.")
  }
  if(per_capita || population_cutoff > 0) {
    caption_str <- paste(caption_str, "Population data: Worldbank.")
  }
  caption_str <- paste(
    caption_str,
    sprintf("Data obtained on %s.", data_date_str)
  )

  if(min_by_ctry_obs > 1 || population_cutoff > 0) {
    caption_str <- paste(caption_str, "The sample is limited to countries with")
    if(min_by_ctry_obs > 1)
      caption_str <- paste(
        caption_str,
        sprintf("at least %d days of data", min_by_ctry_obs)
      )
    if(min_by_ctry_obs > 1 && population_cutoff > 0)
      caption_str <- paste(caption_str, "and")
    if(population_cutoff > 0)
      caption_str <- paste(
        caption_str,
        sprintf("a population exceeding %.0f million", population_cutoff)
      )
    caption_str <- paste0(caption_str, ".")
  }

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

  if (per_capita_x_axis) {
    x_str <- sprintf(
      "Days after %s per 100,000 inhabitants exceeded %.1f",
      dplyr::case_when(
        type == "deaths" ~ "deaths\n",
        type == "confirmed" ~ "confirmed cases\n",
        type == "recovered" ~ "recovered cases\n",
        type == "active" ~ "active cases\n"
      ), min_cases
    )
  } else {
    x_str <- sprintf("Days after %s reported",
                     scales::label_ordinal(big.mark = ",")(min_cases))

    x_str <- paste(x_str, dplyr::case_when(
      type == "deaths" ~ "death\n",
      type == "confirmed" ~ "confirmed case\n",
      type == "recovered" ~ "recovered case\n",
      type == "active" ~ "active case\n"
    ))
  }

  if (!cumulative) y_str <- "Daily change in"
  else y_str <- "Reported"
  y_str <- paste(y_str, dplyr::case_when(
    type == "deaths" ~ "deaths",
    type == "confirmed" ~ "confirmed cases",
    type == "recovered" ~ "recovered cases",
    type == "active" ~ "active cases"
  ))
  if (per_capita) y_str <- paste(y_str, "per 100,000 inhabitants")
  if (log_scale && cumulative) y_str <- paste(y_str, "(logarithmic scale)")
  if (log_scale && !cumulative && change_ave > 1) y_str <- paste(
    y_str,
    sprintf("(%d days average, logarithmic scale)", change_ave)
  )
  if (!log_scale && !cumulative && change_ave > 1) y_str <- paste(
    y_str,
    sprintf("(%d days average)", change_ave)
  )
  if(nchar(y_str > 60))
     y_str <- paste(strwrap(y_str, width = 50), collapse = "\n")

  title_str <- sprintf("The First %d Days: Reported", edate_cutoff)
  if (!cumulative) title_str <- paste(title_str, "daily change in")
  title_str <- paste(title_str, dplyr::case_when(
    type == "deaths" ~ "deaths",
    type == "confirmed" ~ "confirmed cases",
    type == "recovered" ~ "recovered cases",
    type == "active" ~ "active cases"
  ))

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
