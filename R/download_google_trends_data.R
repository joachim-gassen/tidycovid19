download_google_trends_data <- function(search_term = "coronavirus",
                                    type = "country_day",
                                    silent = FALSE, cached = FALSE) {
  if (!is.character(search_term) || length(search_term) != 1) stop(
    "'serach_term' needs to be a single character value."
  )
  if (!all(type %in% c('country', 'country_day', 'region', 'city')))
    stop(
      "'type' needs to be a vector containing any of 'country', country_day', 'region', or 'city'."
    )
  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' needs to be a single logical value"
  )
  if (!silent) message("Start downloading Google Trends data\n")
  if (length(cached) > 1 || !is.logical(cached)) stop(
    "'silent' needs to be a single logical value"
  )

  if(cached) {
    if (search_term != "coronavirus")
      stop(paste(
        "'cached' == TRUE but 'search_term' != 'coronavirus'.",
        "You need to use 'cached' == FALSE when you want to",
        "retrieve customized Google Trends data."
      ))
    if (!silent) message("Downloading cached version of Google Trends data...", appendLF = FALSE)
    lst <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/google_trends.RDS")))
    if (!silent) message("done. Timestamp is %s", lst[[1]]$timestamp[1])
  } else {
    time <- paste("2020-01-01", Sys.Date())

    trends_global <- gtrendsR::gtrends(search_term, time = time)

    trends_global$interest_by_country %>%
      dplyr::filter(!is.na(.data$hits)) %>%
      dplyr::rename(gtrends_score = .data$hits) %>%
      dplyr::mutate(iso3c = countrycode::countrycode(.data$location, origin = "country.name",
                                                     destination = "iso3c"),
                    iso2c = countrycode::countrycode(.data$location, origin = "country.name",
                                                     destination = "iso2c")) %>%
      dplyr::select(.data$iso3c, .data$iso2c, .data$gtrends_score) %>%
      dplyr::mutate(timestamp = Sys.time()) -> gtrends_global

    pull_gt_country_data <- function(iso2c) {
      if (!silent) message(
        sprintf("Pulling Google trend data for %s ...", iso2c),
        appendLF = FALSE
      )
      gl <- gtrendsR::gtrends(search_term, geo = iso2c, time = time)
      if (!silent) message("done!")

      # Be nice to Google and sleep a little

      Sys.sleep(stats::runif(1, min = 2, max = 5))
      c(iso2c = iso2c, gl)
    }

    fix_hits <- function(v) {
      if(is.numeric(v)) v
      else {
        v[v == "<1"] <- 0.5
        as.numeric(v)
      }
    }

    parse_gt_list <- function(gtl) {
      gtl$interest_over_time %>%
        dplyr::filter(.data$hits != "NA") %>%
        dplyr::mutate(
          date = lubridate::as_date(.data$date),
          gtrends_score = fix_hits(.data$hits),
          iso2c = .data$geo,
          timestamp = Sys.time()
        ) %>%
        dplyr::select(.data$iso2c, .data$date,
                      .data$gtrends_score, .data$timestamp) -> gt_by_time
      if (is.data.frame(gtl$interest_by_region)) {
        gtl$interest_by_region %>%
          dplyr::filter(.data$hits != "NA") %>%
          dplyr::mutate(
            gtrends_score = fix_hits(.data$hits),
            iso2c = .data$geo,
            region = .data$location,
            timestamp = Sys.time()
          ) %>%
          dplyr::select(.data$iso2c, .data$region,
                        .data$gtrends_score, .data$timestamp) -> gt_by_region
      } else gt_by_region <- NULL
      if (is.data.frame(gtl$interest_by_city)) {
        gtl$interest_by_city %>%
          dplyr::filter(.data$hits != "NA") %>%
          dplyr::mutate(
            gtrends_score = fix_hits(.data$hits),
            iso2c = .data$geo,
            city = .data$location,
            timestamp = Sys.time()
          ) %>%
          dplyr::select(.data$iso2c, .data$city,
                        .data$gtrends_score, .data$timestamp) -> gt_by_city
      } else gt_by_city <- NULL
      list(gt_by_time, gt_by_region, gt_by_city)
    }

    extract_tibble_from_list <- function(type, lst) {
      if (type == 'country') return(gtrends_global %>% dplyr::select(-.data$iso2c))
      pos <- dplyr::case_when(
        type == "country_day" ~ 1,
        type == "region" ~ 2,
        type == "city" ~ 3
      )
      tibble_list <- lapply(lst, function(x) x[[pos]])
      tibble_list <- tibble_list[! sapply(tibble_list, is.null)]
      df <- do.call(rbind, tibble_list) %>%
        dplyr::mutate(iso3c = countrycode::countrycode(.data$iso2c, origin = "iso2c",
                                                       destination = "iso3c")) %>%
        dplyr::select(.data$iso3c, 2:4)
    }

    gt_ctry_lists <- lapply(gtrends_global$iso2c, pull_gt_country_data)
    gt_parsed_list <- lapply(gt_ctry_lists, parse_gt_list)

    lst <- lapply(type, extract_tibble_from_list, lst = gt_parsed_list)
    if (!silent) message("Finished downloading Google Trends data\n")
  }

  if (length(type) == 1) lst[[1]] else lst
}


