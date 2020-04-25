#' Plot Covid-19 Stripes
#'
#' Provides a flexible visualization of the country-level Covid-19 spread,
#' plotted by colored daily lines to generate a stripes display. Uses data
#' from the Johns Hopkins University CSSE team
#' (\url{https://github.com/CSSEGISandData/COVID-19}) and the World Bank
#' (\url{https://data.worldbank.org}).
#'
#' @param data The data frame to base the plot on. Should be a merged data
#'     frame obtained by \link{download_merged_data} and defaults to
#'     \code{download_merged_data(cached = TRUE, silent = TRUE)}.
#' @param type The statistic that you want to plot. Needs to be either "confirmed",
#'     "deaths", "revovered" or "active", defined as the difference of "confirmed"
#'     and "recovered".
#' @param min_cases Only countries that have a maximum of \code{type} cases higher
#'     than \code{min_cases} during the data period are included in the plot.
#'     Uses reasonable defaults depending on \code{type} and
#'     \code{per_capita}.
#' @param cumulative If \code{TRUE}, data is being plotted as
#'     cumulative (showing the total figures). If \code{FALSE}, (the default)
#'     (averaged) daily changes are plotted instead. See \code{change_ave}
#'     to set the averaging window.
#' @param change_ave Number of days to average over when you plot daily changes.
#' @param per_capita If \code{TRUE} data is being plotted as per capita measure
#'     based on World Bank data. Defaults to \code{FALSE}.
#' @param population_cutoff Do you want to restrict the plot to countries that
#'     exceed a certain population cutoff? Takes a value in millions and
#'     defaults to 0. Useful for per capita displays.
#' @param diverging_color_scale Should be set to \code{TRUE} when a \code{type}
#'     is chosen that can have meaningful negative values (this is the case
#'     for "active" cases). Defaults to \code{FALSE}.
#' @param countries A character vector of ISO3c (ISO 3166-1 alpha-3) codes that
#'     you want to include. Note that including many countries can lead to
#'     long plotting times and might cause the plot to fail rendering when
#'     the display is to small.
#' @param sort_countries By default, countries are sorted alphabetically by
#'     ISO3c code. "start" will sort countries by when they first exceeded
#'     \code{min_cases}. "magnitude" will sort by decreasing maximum value
#'     for \code{type}. "countries" requires \code{countries} to be set and
#'     sorts the display in the order of the ISO3c codes provided.
#'     Alternatively, you can recode the \code{iso3c} in
#'     your data to a factor in the ordering that you prefer. See examples
#'     below.
#' @param data_date_str A date string to include in the annotation of the plot
#'     giving the time when the data was pulled. Defaults to the timestamp of the
#'     data. Note that you might run into issues with the default when running
#'     this in a non-english locale. Consider setting it by hand then.
#'
#' @return A \link{ggplot2} object.
#'
#'
#' @examples
#' plot_covid19_stripes()
#'
#' merged <- download_merged_data(cached = TRUE, silent = TRUE)
#' plot_covid19_stripes(merged, per_capita = TRUE, population_cutoff = 10)
#'
#' plot_covid19_stripes(merged,
#'   countries = c("ITA", "ESP", "FRA", "DEU", "USA"),
#'   sort_countries = "countries"
#' )
#'
#' sortdf <- dplyr::tibble(
#'   iso3c = unique(merged$iso3c),
#'   continent = countrycode::countrycode(iso3c, "iso3c", "continent")
#' ) %>% dplyr::arrange(continent, iso3c)
#' df <- merged
#' df$iso3c <- factor(merged$iso3c, sortdf$iso3c)
#' plot_covid19_stripes(df, type = "confirmed")
#'
#' @export
plot_covid19_stripes <- function(
  data = download_merged_data(cached = TRUE, silent = TRUE),
  type = "deaths",
  min_cases = ifelse(per_capita, ifelse(type == "deaths", 5, 50),
                     ifelse(type == "deaths", 500, 5000)),
  cumulative = FALSE, change_ave = 7, per_capita = FALSE,
  population_cutoff = 0, diverging_color_scale = FALSE,
  countries = NULL, sort_countries = NULL,
  data_date_str = format(lubridate::as_date(data$timestamp[1]), "%B %d, %Y")) {

  if(!type %in% c("confirmed", "deaths", "recovered", "active"))
    stop("Wrong 'type': Only 'confirmed', 'deaths', 'recovered' and 'active' are supported")

  if(!is.logical(cumulative)) stop ("'cumulative' needs to be a logical value")
  change_ave <- as.integer(change_ave)
  if(change_ave < 0)
    stop ("'change_ave' needs to be a positive integer")

  data <- data %>%
    dplyr::filter(.data$population > 1e6*population_cutoff) %>%
    dplyr::mutate(
      active = .data$confirmed - .data$recovered,
      orig_type = !! rlang::sym(type)
    )

  if (!cumulative)
    data <- data %>%
      dplyr::group_by(.data$iso3c) %>%
      dplyr::mutate(
        delta = !! rlang::sym(type) - dplyr::lag(!! rlang::sym(type)),
        change = zoo::rollmean(.data$delta, change_ave, na.pad=TRUE, align="right")
      ) %>%
      dplyr::ungroup()

  data <- data %>%
    dplyr::group_by(.data$iso3c) %>%
    dplyr::filter(!is.na(!! rlang::sym(type))) -> df

  if (!cumulative) df <- df %>%
    dplyr::mutate(!! type := .data$change)

  if (per_capita) df <- df %>%
    dplyr::filter(!is.na(.data$population)) %>%
    dplyr::mutate(
      !! type := 1e5*(!! rlang::sym(type))/.data$population,
      orig_type = 1e5*.data$orig_type/.data$population
    )

  df <- df %>%
    dplyr::filter(max(.data$orig_type, na.rm = TRUE) >= min_cases) %>%
    dplyr::filter(!is.na(!! rlang::sym(type))) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$iso3c, .data$country, .data$date,
                  .data$orig_type, !! rlang::sym(type))

  if(!diverging_color_scale) {
    df[df[, type] <= 0, type] <- min(df[df[, type] > 0, type])
  }

  if(!is.null(countries) && (length(countries) > 1 || countries != "")
     && !any(countries %in% df$iso3c))
    warning(paste(
      "Non-NULL 'countries' value but no countries matched in data",
      "(Did you specify correct ISO3c codes or do values for 'min_cases'",
      "lead to the exclusion of your selected countries' data?)"
    ))

  if (!is.null(countries) && (length(countries) > 1 || countries != "")) {
    df <- df %>% dplyr::filter(.data$iso3c %in% countries)
  }

  if (!is.null(sort_countries)) {
    if (!sort_countries %in% c("start", "magnitude", "countries")) stop(
      "'sort_countries' needs to be either 'start', 'magnitude' or 'countries'"
    )

    if (sort_countries == "start") {
      df %>%
        dplyr::group_by(.data$iso3c) %>%
        dplyr::filter(.data$orig_type > min_cases) %>%
        dplyr::summarise(min_date = min(.data$date)) %>%
        dplyr::arrange(.data$min_date) -> sortdf

      df$iso3c <- factor(df$iso3c, levels = sortdf$iso3c)
    }

    if (sort_countries == "magnitude") {
      df %>%
        dplyr::group_by(.data$iso3c) %>%
        dplyr::summarise(max_vals = max(.data$orig_type)) %>%
        dplyr::arrange(-.data$max_vals) -> sortdf

      df$iso3c <- factor(df$iso3c, levels = sortdf$iso3c)
    }
    if (sort_countries == "countries") {
      if (!(!is.null(countries) && (length(countries) > 1 || countries != ""))) {
        stop("'sort_countries' == 'countries' but 'countries' is not set")
      }

      df$iso3c <- factor(df$iso3c, levels = countries)
    }
  }
  caption_str <- paste(
    "Case data: Johns Hopkins University Center for Systems Science",
    "and Engineering (JHU CSSE)."
  )
  if(per_capita || population_cutoff > 0) {
    caption_str <- paste(caption_str, "Population data: Worldbank.")
  }
  caption_str <- paste(
    caption_str,
    sprintf("Data obtained on %s.", data_date_str)
  )

  if (min_cases > 0) {
    caption_str <- paste(
      caption_str, "The sample is limited to countries with",
      sprintf(
        ifelse(round(min_cases) == min_cases,
               "more than %d %s.", "more than %.2f %s."),
        min_cases,
        dplyr::case_when(
          type == "deaths" ~ "deaths",
          type == "confirmed" ~ "confirmed cases",
          type == "recovered" ~ "recovered cases",
          type == "active" ~ "active cases"
        )
      )
    )
  }

  if(population_cutoff > 0) {
    caption_str <- paste(
      caption_str, "The sample is limited to countries with",
      sprintf("a population exceeding %.0f million.", population_cutoff)
    )
  }

  caption_str <- paste(strwrap(paste(
    caption_str,
    "Code: https://github.com/joachim-gassen/tidycovid19."
  ), width = 120), collapse = "\n")

  type_str <-
    dplyr::case_when(
      type == "deaths" ~ "deaths\n",
      type == "confirmed" ~ "confirmed cases\n",
      type == "recovered" ~ "recovered cases\n",
      type == "active" ~ "active cases\n"
    )

  if (!cumulative) color_str <- paste( "Daily change in", type_str)
  else {
    substr(type_str, 1, 1) <- toupper(substr(type_str, 1, 1))
    color_str <- type_str
  }

  if (per_capita) {
    color_str <- paste(color_str, "per 100,000 inhabitants")
  }


  if (!cumulative && change_ave > 1) color_str <- paste(
    color_str,
    sprintf("(averaged over %d days)", change_ave)
  )

  title_str <- "Covid19 Stripes: Reported"
  if (!cumulative) title_str <- paste(title_str, "daily change in")
  title_str <- paste(title_str, dplyr::case_when(
    type == "deaths" ~ "deaths",
    type == "confirmed" ~ "confirmed cases",
    type == "recovered" ~ "recovered cases",
    type == "active" ~ "active cases"
  ))

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$date, color = !! rlang::sym(type))
  ) +
    ggplot2:: geom_segment(ggplot2::aes(xend = .data$date),
                           size = 2, y = 0, yend = 1)

  if (diverging_color_scale) {
    # See http://www.kennethmoreland.com/color-maps/ for colors
    p <- p + ggplot2::scale_color_gradient2(
      name = color_str,
      low = grDevices::rgb(0.230, 0.299, 0.754),
      mid = grDevices::rgb(0.865, 0.865, 0.865),
      high = grDevices::rgb(0.706, 0.016, 0.150),
      trans = "pseudo_log",
      breaks = c(0)
    )
  } else {
    p <- p + ggplot2::scale_color_continuous(
      name = color_str,
      type = "viridis",
      trans = "log10"
    )
  }

  p <- p +  ggplot2::facet_grid(rows = ggplot2::vars(iso3c)) +
    ggplot2::theme_minimal() +
    ggplot2::guides(
      color=ggplot2::guide_colourbar(
        title.vjust = 0.8,
        barheight = 0.5,
        barwidth = 10
      )
    ) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.caption.position =  "plot",
      plot.caption = ggplot2::element_text(hjust = 0),
      axis.title.x = ggplot2::element_text(hjust = 1),
      legend.position="bottom",
      strip.text.y.right = ggplot2::element_text(angle = 0),
      panel.spacing = ggplot2::unit(0, "lines")
    )  +
    ggplot2::labs(
      x = NULL,
      title = title_str,
      caption = caption_str
    )

  # Make R CHECK happy
  iso3c <- NULL

  p
}
