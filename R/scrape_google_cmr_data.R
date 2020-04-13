extract_line_graph_bitmaps <- function(pdf,
                                       pages = 1:(pdftools::pdf_info(pdf)[[2]] - 1)) {
  extract_page <- function(page) {
    bmap <- pdftools::pdf_render_page(pdf, page = page, numeric = TRUE,
                                      antialias = FALSE, dpi = 150)
    line_color_matches <- dplyr::as_tibble(
      which(abs(bmap[,,1] - 0.8549020) < 0.0001, arr.ind = TRUE)
    )
    if (nrow(line_color_matches) == 0) {
      # No figures found on page
      return(NA)
    }

    line_rows <- line_color_matches %>%
      dplyr::group_by(.data$row) %>%
      dplyr::filter(dplyr::n() > 100) %>%
      dplyr::ungroup()

    line_rows %>%
      dplyr::select(.data$row) %>%
      unique() %>%
      dplyr::arrange(.data$row) %>%
      dplyr::mutate(
        delta_lag = .data$row - dplyr::lag(.data$row),
        delta_lead = dplyr::lead(.data$row) - .data$row,
        first_line = is.na(.data$delta_lag) | .data$delta_lag > 100,
        last_line = is.na(.data$delta_lead) | .data$delta_lead > 100
      ) -> lr

    vpos_start <- lr %>%
      dplyr::filter(.data$first_line) %>%
      dplyr::select(.data$row) %>%
      dplyr::pull()

    vpos_end <- lr %>%
      dplyr::filter(.data$last_line) %>%
      dplyr::select(.data$row) %>%
      dplyr::pull()

    line_rows %>%
      dplyr::select(.data$col) %>%
      unique() %>%
      dplyr::arrange(.data$col) %>%
      dplyr::mutate(
        delta_lag = .data$col - dplyr::lag(.data$col),
        delta_lead = dplyr::lead(.data$col) - .data$col,
        first_col = is.na(.data$delta_lag) | .data$delta_lag > 5,
        last_col = is.na(.data$delta_lead) | .data$delta_lead > 5
      ) -> lc

    hpos_start <- lc %>%
      dplyr::filter(.data$first_col) %>%
      dplyr::select(.data$col) %>%
      dplyr::pull()

    hpos_end <- lc %>%
      dplyr::filter(.data$last_col) %>%
      dplyr::select(.data$col) %>%
      dplyr::pull()
    bitmaps <- list()
    for (i in 1:length(vpos_start)) {
      for (j in 1:length(hpos_start)) {
        # Some line graphs run over bottom and/or top grid line
        # Need to include offset
        vo <- round((vpos_end[i] - vpos_start[i])/4)
        bitmaps[[(i-1)*length(hpos_start) + j]] <-
          bmap[(vpos_start[i] - vo):(vpos_end[i] + vo), hpos_start[j]:hpos_end[j], ]
      }
    }
    bitmaps
  }
  bitmaps <- lapply(pages, extract_page)
}


parse_line_graph_bitmap <- function(bitmap,
                                    date_start = lubridate::ymd("2020-02-23"),
                                    date_end = lubridate::ymd("2020-04-05"),
                                    lower_bound = -0.8,
                                    upper_bound = 0.8) {
  days <- lubridate::as_date(date_start:date_end)
  dta <- sapply(
    1:dim(bitmap)[2],
    function (c) mean(which(abs(bitmap[, c, 1] - 0.2588235) < 0.001))/dim(bitmap)[1]
  )
  if(is.na(dta[length(dta)])) dta <- dta[1:(length(dta) - 1)]
  dta[is.nan(dta)] <- NA

  step <- (length(dta) - 1)/(length(days) - 1)
  values <- sapply(
    1:length(days),
    function(d) dta[round(1 + (d - 1)*step)]
  )
  oset = (upper_bound - lower_bound)/4
  lb = lower_bound - oset
  ub = upper_bound + oset
  dplyr::tibble(
    date = days,
    measure = round(ub - values*(ub - lb), 2)
  )
}

extract_clevel_line_graphs <- function(pdf) {
  iso2c <- substr(basename(pdf), 12, 13)
  bitmaps <- extract_line_graph_bitmaps(pdf, pages = 1:2)
  parse_line_graph_bitmap(bitmaps[[1]][[1]]) %>%
    dplyr::rename(retail_recreation = .data$measure) %>%
    dplyr::mutate(iso2c = iso2c) %>%
    dplyr::select(.data$iso2c, .data$date, .data$retail_recreation) %>%
    dplyr::left_join(parse_line_graph_bitmap(bitmaps[[1]][[2]]), by = "date") %>%
    dplyr::rename(grocery_pharmacy = .data$measure) %>%
    dplyr::left_join(parse_line_graph_bitmap(bitmaps[[1]][[3]]), by = "date") %>%
    dplyr::rename(parks = .data$measure) %>%
    dplyr::left_join(parse_line_graph_bitmap(bitmaps[[2]][[1]]), by = "date") %>%
    dplyr::rename(transit_stations = .data$measure) %>%
    dplyr::left_join(parse_line_graph_bitmap(bitmaps[[2]][[2]]), by = "date") %>%
    dplyr::rename(workplaces = .data$measure) %>%
    dplyr::left_join(parse_line_graph_bitmap(bitmaps[[2]][[3]]), by = "date") %>%
    dplyr::rename(residential = .data$measure)
}

#' Scrape Google Community Mobility Report Data
#'
#' Scrapes (and downloads) Google Covid-19 Community Mobility Reports
#' (\url{https://www.google.com/covid19/mobility/}).
#' As stated on this webpage, the "reports chart movement trends over time
#' by geography, across different categories of places such as retail and
#' recreation, groceries and pharmacies, parks, transit stations,
#' workplaces, and residential". Google prepares these reports ino PDF format
#' to help interested parties to assess responses to social distancing guidance
#' related to Covid-19.
#'
#' @param daily_data Whether you want the code to extract daily data by
#'     scanning the line graph figures that are included in the PDF. The
#'     resulting data are noisy but should be accurate by +/- one day and
#'     +/- 2 percent. Defaults to \code{TRUE}.
#' @param pdf_dir Path to the directory where the PDF files are stored.
#'     If \code{NULL} (the default), the code will download the PDFs from
#'     Google's website (see above). See \code{google_download_cmr_pdfs()}
#'     for downloading the PDFs.
#' @param silent Whether you want the function to send some status messages to
#'     the console. Might be informative as downloading will take some time
#'     and thus defaults to \code{TRUE}.
#' @param cached Whether you want to download the cached version of the data
#'     from the {tidycovid19} Github repository instead of retrieving the
#'     data from the authorative source. Downloading the cached version is
#'     faster and the cache is updated daily. Defaults to \code{FALSE}.
#'
#' @return If \code{daily_data == FALSE}, a data frame containing the
#'     country level data. Otherwise, a list containing the country level data
#'     and the country-day level data.
#'
#' @details The data is currently parsed from PDF files that Google provides.
#'    Thus, the code will most likely break as soon Google is changing the
#'    PDF format of their files.
#'
#' @examples
#'
#' df <- scrape_google_cmr_data(daily_data = FALSE, silent = TRUE, cached = TRUE)
#' df %>%
#'   dplyr::select(iso3c, retail_recreation) %>%
#'   dplyr::arrange(retail_recreation)
#'
#' lst <- scrape_google_cmr_data(silent = TRUE, cached = TRUE)
#' lst[[2]] %>%
#'   dplyr::group_by(date) %>%
#'   dplyr::summarize(
#'     retail_recreation = mean(retail_recreation, na.rm = TRUE)
#'   ) %>%
#'   ggplot2::ggplot(ggplot2::aes(x = date, y = retail_recreation)) +
#'   ggplot2::geom_line()
#'
#'
#' @export
scrape_google_cmr_data <- function(daily_data = TRUE, pdf_dir = NULL,
                                     silent = FALSE, cached = FALSE) {

  parse_cmr_ctry <- function(url, parse_line_graphs = FALSE) {

    value_for_var <- function(var_name) {
      var_pos <- which(df == var_name)[1]
      measure_pos <- value_pos[value_pos > var_pos & value_pos < var_pos + 4]
      if (length(measure_pos) > 1) {
        warning(sprintf("Error parsing PDF %s", url))
        return(NA)
      }
      if (length(measure_pos) == 0) {
        if (!silent) message(sprintf(
          "Not enough data for measure '%s' for country %s",
          var_name, iso2c))
        return(NA)
      }
      as.numeric(sub("%", "", df[measure_pos]))/100
    }

    iso2c <- substr(basename(url), 12, 13)
    if (!silent) message(
      sprintf("Parsing Google CMR PDF for %s ...", iso2c),
      appendLF = FALSE
    )
    if (is.null(pdf_dir)) {
      pdf <- tempfile(
        paste0(tools::file_path_sans_ext(basename(url)), "_"),
        fileext = ".pdf"
      )
      utils::download.file(url, pdf, quiet = TRUE, mode = "wb")
    } else pdf <- url

    df <- strsplit(paste(pdftools::pdf_text(pdf), collapse = "\n"), "\n")[[1]]
    # Windows provides strings with CR - remove them
    df <- sub("\r", "", df)

    value_pos <- which(stringr::str_detect(df, "^[-+]?\\d+%$"))
    rv <- dplyr::tibble(
      iso2c = iso2c,
      date = lubridate::mdy(
        stringr::str_extract(df[2], "([^\\s]+)\\s\\d+,\\s2020$")
      ),
      retail_recreation = value_for_var("Retail & recreation"),
      grocery_pharmacy = value_for_var("Grocery & pharmacy"),
      parks = value_for_var("Parks"),
      transit_stations = value_for_var("Transit stations"),
      workplaces = value_for_var("Workplaces"),
      residential = value_for_var("Residential")
    )

    if (!daily_data) {
      if (!silent) message("done!")
      return(rv)
    }

    df <- extract_clevel_line_graphs(pdf)
    if (!silent) message("done!")
    list(rv, df)
  }


  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' needs to be a single logical value"
  )
  if (length(cached) > 1 || !is.logical(cached)) stop(
    "'cached' needs to be a single logical value"
  )

  if(cached) {
    if (!silent) message("Downloading cached version of Google CMR data...", appendLF = FALSE)
    rv <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/google_cmr.RDS")))
    if (!silent) message(sprintf("done. Timestamp is %s", rv[[1]]$timestamp[1]))
  } else {
    if (!silent && is.null(pdf_dir)) message("Start downloading and scraping Google CMR data\n")
    else if (!silent) message("Start scraping Google CMR data\n")

    if(is.null(pdf_dir)) {
      cmr_url <- "https://www.google.com/covid19/mobility/"

      urls <- xml2::read_html(cmr_url) %>%
        rvest::html_nodes(css = ".download-link") %>%
        rvest::html_attr('href')
    } else urls <- list.files(path = pdf_dir, pattern = ".pdf", full.names = TRUE)

    ctry_urls <- urls[!stringr::str_detect(urls, "US_.*_Mobility")]

    lst <- lapply(ctry_urls, parse_cmr_ctry, daily_data)
    if (!daily_data) rv <- list(do.call(rbind, lst))
    else {
      rv <- list(
        do.call(rbind, lapply(lst, function(x) x[[1]])),
        do.call(rbind, lapply(lst, function(x) x[[2]]))
      )
    }

    rv <- lapply(rv, function(x) x %>%
      dplyr::mutate(iso3c = countrycode::countrycode(.data$iso2c, origin = "iso2c",
                                                     destination = "iso3c")) %>%
      dplyr::select(.data$iso3c, dplyr::everything(), -.data$iso2c) %>%
      dplyr::mutate(timestamp = Sys.time()))

    if (!silent) message("\nDone scraping Google CMR data\n")
  }
  if (!daily_data) rv[[1]] else rv
}


