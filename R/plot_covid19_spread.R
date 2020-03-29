plot_covid19_spread <- function(
  df, type = "deaths", min_cases = ifelse(type == "deaths", 10, 100),
  min_by_ctry_obs = 7, edate_cutoff = 30,
  data_date_str = format(lubridate::as_date(df$timestamp[1]), "%B %d, %Y"),
  per_capita = FALSE, highlight = NULL, intervention = NULL) {
  if(!type %in% c("confirmed", "deaths", "recovered"))
    stop("Wrong 'type': Only 'confirmed', 'deaths', and 'recovered' are supported")

  df %>%
    dplyr::group_by(.data$iso3c) %>%
    dplyr::filter(!! rlang::sym(type) >= min_cases) %>%
    dplyr::mutate(edate = as.numeric(.data$date - min(.data$date))) %>%
    dplyr::filter(!is.na(.data$edate)) %>%
    dplyr::group_by(.data$country) %>%
    dplyr::filter(dplyr::n() >= min_by_ctry_obs) %>%
    dplyr::ungroup() -> df

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


  caption_str <- paste(
    "Data as provided by Johns Hopkins University Center for Systems Science",
    sprintf("and Engineering (JHU CSSE) and obtained on %s.", data_date_str),
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
    "Code: https://github.com/joachim-gassem/tidy_covid19."
  ), width = 160), collapse = "\n")

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

  p <- ggplot2::ggplot(df %>% dplyr::filter(.data$edate <= edate_cutoff),
         ggplot2::aes(x = .data$edate, color = .data$country,
                      y = !! rlang::sym(type))) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = x_str,
      y = y_str,
      title = title_str,
      caption = caption_str
    ) +
    ggplot2::scale_y_continuous(trans='log10', labels = scales::comma) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.caption.position =  "plot",
      plot.caption = ggplot2::element_text(hjust = 0),
      axis.title.x = ggplot2::element_text(hjust = 1),
      axis.title.y = ggplot2::element_text(hjust = 1),
    )

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
