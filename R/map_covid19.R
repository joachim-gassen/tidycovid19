#' Map Covid-19
#'
#' Provides a choropleth world map of the country-level Covid-19 spread.
#' Uses data from the Johns Hopkins University CSSE team
#' (\url{https://github.com/CSSEGISandData/COVID-19}) and the World Bank
#' (\url{https://data.worldbank.org}). If called with multiple dates it
#' uses the {gganimate} package to generate an ainmated display.
#'
#' @param data The data frame to base the plot on. Should be a merged data
#'     frame obtained by \link{download_merged_data} and defaults to
#'     \code{download_merged_data(cached = TRUE, silent = TRUE)}.
#' @param type The statistic that you want to plot. Needs to be either "confirmed",
#'     "deaths", "revovered" or "active", defined as the difference of "confirmed"
#'     and "recovered".
#' @param cumulative If \code{TRUE}, data is being plotted as
#'     cumulative (showing the total figures). If \code{FALSE}, (the default)
#'     (averaged) daily changes are plotted instead. See \code{change_ave}
#'     to set the averaging window.
#' @param change_ave Number of days to average over when you plot daily changes.
#' @param per_capita If \code{TRUE} data is being plotted as per capita measure
#'     based on World Bank data. Defaults to \code{FALSE}.
#' @param dates The data date to use for the plot. Defaults to the most current
#'     date present in the data. If you provide a vector of dates, it will
#'     use the \code{gganimate} package to animate the colorpleth map.
#' @param diverging_color_scale Should be set to \code{TRUE} when a \code{type}
#'     is chosen that can have meaningful negative values (this is the case
#'     for "active" cases). Defaults to \code{FALSE}.
#' @param region Do you want to map the world (default) or focus on a
#'     certain region? Valid values are either 'World', 'Africa', 'Asia',
#'     'Europe', 'North America', 'Oceania', 'South America' or
#'     a named list (x,y) containing longitude and latidtude limits.
#' @param data_date_str A date string to include in the annotation of the plot
#'     giving the time when the data was pulled. Defaults to the timestamp of the
#'     data. Note that you might run into issues with the default when running
#'     this in a non-english locale. Consider setting it by hand then.
#' @param ... additional parameters that will passed on to
#'     \code{\link[gganimate]{animate}} if an animation is being prepared.
#'
#' @return A \code{\link[ggplot2]{ggplot2}} or a
#'     \code{\link[gganimate]{gganimate}} object.
#'
#'
#' @examples
#' map_covid19()
#'
#' merged <- download_merged_data(cached = TRUE, silent = TRUE)
#' map_covid19(merged, per_capita = TRUE, date = "2020-04-23")
#'
#' \dontrun{
#' # Create animation - takes a while to process
#'
#' anim <- map_covid19(merged, per_capita = T, dates = unique(merged_df$date))
#' }
#'
#' @export
map_covid19 <- function(
  data = download_merged_data(cached = TRUE, silent = TRUE),
  type = "deaths",
  cumulative = FALSE, change_ave = 7, per_capita = FALSE,
  dates = max(data$date), diverging_color_scale = FALSE,
  region = "World",
  data_date_str = format(lubridate::as_date(data$timestamp[1]), "%B %d, %Y"),
  ...) {

  if(!type %in% c("confirmed", "deaths", "recovered", "active"))
    stop("Wrong 'type': Only 'confirmed', 'deaths', 'recovered' and 'active' are supported")

  if(!is.logical(cumulative)) stop ("'cumulative' needs to be a logical value")
  change_ave <- as.integer(change_ave)
  if(change_ave < 0)
    stop ("'change_ave' needs to be a positive integer")

  data <- data %>%
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
    dplyr::filter(!is.na(!! rlang::sym(type))) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$iso3c, .data$country, .data$date,
                  .data$orig_type, !! rlang::sym(type))

  if(!diverging_color_scale) {
    df[df[, type] <= 0, type] <- min(df[df[, type] > 0, type])
  }

  df <- df %>% dplyr::filter(.data$date %in% lubridate::as_date(dates))

  caption_str <- paste(
    "Case data: Johns Hopkins University Center for Systems Science",
    "and Engineering (JHU CSSE)."
  )
  if(per_capita) {
    caption_str <- paste(caption_str, "Population data: Worldbank.")
  }
  caption_str <- paste(
    caption_str,
    sprintf("Data obtained on %s.", data_date_str)
  )

  caption_str <- paste(strwrap(paste(
    caption_str,
    "Code: https://github.com/joachim-gassen/tidycovid19."
  ), width = 100), collapse = "\n")

  type_str <-
    dplyr::case_when(
      type == "deaths" ~ "deaths\n",
      type == "confirmed" ~ "confirmed cases\n",
      type == "recovered" ~ "recovered cases\n",
      type == "active" ~ "active cases\n"
    )

  if (!cumulative) fill_str <- paste( "Daily change in", type_str)
  else {
    substr(type_str, 1, 1) <- toupper(substr(type_str, 1, 1))
    fill_str <- type_str
  }

  if (per_capita) {
    fill_str <- paste(fill_str, "per 100,000 inhabitants")
  }

  if (!cumulative && change_ave > 1) fill_str <- paste(
    fill_str,
    sprintf("(averaged over %d days)", change_ave)
  )

  title_str <- "Covid19:"
  title_str <- paste(title_str, dplyr::case_when(
    type == "deaths" ~ "Reported deaths",
    type == "confirmed" ~ "Confirmed cases",
    type == "recovered" ~ "Recovered cases",
    type == "active" ~ "Active cases"
  ))
  if (!cumulative) title_str <- paste(title_str, "(new cases per day)")
  else title_str <- paste(title_str, "(cumulative)")

  if (length(dates) == 1) {
    title_str <- paste(
      title_str, "as of",
      format(lubridate::as_date(dates), "%B %d, %Y")
    )
  } else {
    title_str <- paste0(title_str, ": {frame_time}")
  }

  create_map <- function(df) {
    p <- ggplot2::ggplot(df, ggplot2::aes(long, lat)) +
      ggplot2::geom_polygon(
        ggplot2::aes(group = group, fill = !! rlang::sym(type))
      )

    if (is.list(region) || region != "World") {
      if (is.list(region) && names(region) == c("x", "y")) rl <- region
      else if (region == "Europe") rl <- list(x = c(-22, 50), y = c(33, 70))
      else if (region == "Africa") rl <- list(x = c(-20, 50), y = c(-35, 35))
      else if (region == "Asia") rl <- list(x = c(25, 190), y = c(-25, 80))
      else if (region == "Oceania") rl <- list(x = c(90, 180), y = c(-50, 25))
      else if (region == "South America") rl <- list(x = c(-90, -35), y = c(-60, 18))
      else if (region == "North America") rl <- list(x = c(-170, -50), y = c(18, 82))
      else stop(paste(
        "'region' needs to be either 'World', 'Africa', 'Asia',",
        "'Europe', 'North America', 'Oceania', 'South America' or",
        "a named list (x,y) containing longitude and latidtude limits."
      ))
      p <- p + ggplot2::coord_quickmap(xlim = rl$x, ylim = rl$y)
    } else p + ggplot2::coord_quickmap()

    if (diverging_color_scale) {
      # See http://www.kennethmoreland.com/color-maps/ for colors
      p <- p + ggplot2::scale_color_gradient2(
        name = fill_str,
        low = grDevices::rgb(0.230, 0.299, 0.754),
        mid = grDevices::rgb(0.865, 0.865, 0.865),
        high = grDevices::rgb(0.706, 0.016, 0.150),
        trans = "pseudo_log",
        breaks = c(0),
        na.value = "gray80",
        guide = ggplot2::guide_colorbar(
          direction = "horizontal",
          barheight = ggplot2::unit(2, units = "mm"),
          barwidth = ggplot2::unit(100, units = "mm"),
          draw.ulim = FALSE,
          title.position = 'top',
          title.hjust = 0.5,
          title.vjust = 0.5
        )
      )
    } else {
      p <- p +  ggplot2::scale_fill_continuous(
        name = fill_str,
        type = "viridis",
        trans = "log10",
        na.value = "gray80",
        guide = ggplot2::guide_colorbar(
          direction = "horizontal",
          barheight = ggplot2::unit(2, units = "mm"),
          barwidth = ggplot2::unit(100, units = "mm"),
          draw.ulim = FALSE,
          title.position = 'top',
          title.hjust = 0.5,
          title.vjust = 0.5
        )
      )
    }

    p <- p +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "bottom",
        plot.title = ggplot2::element_text(hjust = 0.5),
        plot.caption = ggplot2::element_text(hjust = 0.5)
      ) +
      ggplot2::labs(
        title = title_str,
        caption = caption_str
      )

    p
  }

  suppressWarnings(world_map <- ggplot2::map_data("world") %>%
    dplyr::mutate(iso3c = countrycode::countrycode(.data$region, origin = "country.name",
                               destination = "iso3c"),
                  continent = countrycode::countrycode(.data$iso3c, "iso3c", "continent")) %>%
    dplyr::filter(.data$region != "Antarctica"))

  if (length(dates) == 1) {
    df <- world_map %>%
      dplyr::left_join(df, by = c("iso3c"))

    return(create_map(df))
  } else {
    message("A vector of dates found. Will create animation. This will take a while...")

    regions_on_map <- unique(world_map$region)

    dates <- dates[dates %in% unique(df$date)]

    reg_dates <- tidyr::expand_grid(regions_on_map, dates) %>%
      dplyr::rename(region = regions_on_map, date = dates) %>%
      dplyr::arrange(.data$region, .data$date)

    df <- world_map %>%
      dplyr::left_join(reg_dates, by = c("region")) %>%
      dplyr::left_join(df, by = c("iso3c", "date"))

    p <- create_map(df) + gganimate::transition_time(date)

    ani_args <- list(p, ...)
    if (!"fps" %in% names(ani_args)) ani_args <- c(ani_args, fps = 5)
    if (!"width" %in% names(ani_args)) ani_args <- c(ani_args, width = 1024)
    if (!"height" %in% names(ani_args)) ani_args <- c(ani_args, height = 768)

    return(do.call(gganimate::animate, ani_args))
  }

  # Make R CHECK happy
  iso3c <- NULL
  long <- NULL
  lat <- NULL
  group <- NULL

  p
}
