download_acaps_npi_data <- function(silent = FALSE, cached = FALSE) {
  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' needs to be a single logical value"
  )
  if (length(cached) > 1 || !is.logical(cached)) stop(
    "'silent' needs to be a single logical value"
  )

  if(cached) {
    if (!silent) message("Downloading cached version of ACAPS NPI data...", appendLF = FALSE)
    df <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/acaps_npi.RDS")))
    if (!silent) message("done. Timestamp is %s", df$timestamp[1])
    return(df)
  }

  if (!silent) message("Start downloading ACAPS NPI data\n")

    url <- "https://data.humdata.org/dataset/acaps-covid19-government-measures-dataset"

  selector_path <- paste0(
    "#data-resources-0 > div > ul > li > ",
    "div.hdx-btn-group.hdx-btn-group-fixed > ",
    "a.btn.btn-empty.btn-empty-blue.hdx-btn.resource-url-analytics.ga-download"
  )

  dta_url <- xml2::read_html(url) %>%
    rvest::html_node(css = selector_path) %>% rvest::html_attr('href')

  tmp_file <- tempfile(".xlsx")
  utils::download.file(paste0("https://data.humdata.org", dta_url), tmp_file,
                       quiet = silent)
  raw_dta <- readxl::read_excel(tmp_file, sheet = "Database")

  df <- raw_dta
  names(df) <-tolower(names(df))
  names(df)[16] <- "alternative_source"

  # Some spelling inconsistencies:
  df$category[df$category == "Movement Restriction"] <- "Movement restrictions"
  df$category[df$category == "Movement Restrictions"] <- "Movement restrictions"
  df$category[df$category == "Social and Economic Measures"] <- "Social and economic measures"
  df$category[df$category == "Social Distancing"] <- "Social distancing"

  df <- df %>%
    dplyr::select(-.data$pcode) %>% # 2020-03-25 is all NA
    dplyr::filter(!is.na(.data$date_implemented),
                  !is.na(.data$category)) %>%
    dplyr::rename(iso3c = .data$iso) %>%
    dplyr::mutate(timestamp = Sys.time())

  if (!silent) message("Done downloading ACAPS NPI data\n")
  df
}
