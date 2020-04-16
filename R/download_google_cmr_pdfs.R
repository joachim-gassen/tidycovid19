#' Download Google Community Mobility Reports as PDF files
#'
#' Downloads Google Community Mobility Reports
#' (\url{https://www.google.com/covid19/mobility/}).
#' As stated on this webpage, the "reports chart movement trends over time
#' by geography, across different categories of places such as retail and
#' recreation, groceries and pharmacies, parks, transit stations,
#' workplaces, and residential". Google prepares these reports to help
#' interested parties to assess responses to social distancing guidance
#' related to Covid-19.
#'
#' @param pdf_dir Path to the directory where the PDF files will be stored.
#' @param date_indexed When \code{TRUE}, the function first reads the date
#'     contained in the PDF file names. If the data is newer than the last
#'     date contained in the directory names in \code{pdf_dir}, it downloads
#'     the new PDFs into a new directory named by date. If not, it downloads
#'     nothing. If \code{FALSE} (the default), it simply downloads the current
#'     PDFs to \code{pdf_dir} regardless of their date.
#' @param all_dates Currently, Google still seems to keep the PDFs from
#'     previous dates around. If you set this to \code{TRUE}, the function will
#'     try to download old PDFs as well and stores them in directories named by
#'     date. Required \code{date_indexed == TRUE} and defaults to \code{FALSE}.
#' @param silent Whether you want the function to send some status messages to
#'     the console. Might be informative as downloading will take some time
#'     and thus defaults to \code{TRUE}.
#'
#' @return The datestamp of the available PDFs.
#'
#' @examples
#' \dontrun{
#'  download_google_cmr_pdfs(dir "~/gogle_cmr_pdfs", silent = TRUE)
#' }
#'
#' @export
download_google_cmr_pdfs <- function(pdf_dir, date_indexed = FALSE,
                                     all_dates = FALSE, silent = FALSE) {
  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' needs to be a single logical value"
  )
  if (length(date_indexed) > 1 || !is.logical(date_indexed)) stop(
    "'date_indexed' needs to be a single logical value"
  )
  if (length(all_dates) > 1 || !is.logical(all_dates)) stop(
    "'date_indexed' needs to be a single logical value"
  )
  if (all_dates && !date_indexed) stop(
    "'all_dates' == 'TRUE' requires 'date_indexed' == 'TRUE'."
  )

  if (!dir.exists(pdf_dir)) stop(sprintf(
    "directory '%s' does not exist", pdf_dir
  ))
  if (!silent) message("Start downloading Google CMR PDFs\n")

  cmr_url <- "https://www.google.com/covid19/mobility/"

  urls <- xml2::read_html(cmr_url) %>%
    rvest::html_nodes(css = ".download-link") %>%
    rvest::html_attr('href')

  date_pdfs <- lubridate::ymd(substr(basename(urls[1]), 1, 10))

  if (date_indexed) {
    local_dates <- stringr::str_extract(
      list.dirs(pdf_dir, full.names = FALSE, recursive = FALSE),
      "\\d{4}-\\d{2}-\\d{2}"
    )
    if(length(local_dates) == 0) {
      last_local_date <- lubridate::ymd("2020-01-01")
    } else last_local_date <- max(lubridate::ymd(local_dates))

    if (!silent) {
      message(sprintf("Google PDFs are as of %s, ", date_pdfs),
              appendLF = FALSE)
      if (last_local_date == lubridate::ymd("2020-01-01"))
        message("no local data found: ", appendLF = FALSE)
      else message(sprintf("local PDFs are as of %s: ", last_local_date),
                   appendLF = FALSE)
    }
    if (date_pdfs <= last_local_date) {
      if(!silent) message("Not downloading current data.\n")
      if (!all_dates) return(date_pdfs)
    } else {
      pdf_dir = file.path(pdf_dir, format(date_pdfs, "%Y-%m-%d"))
      dir.create(pdf_dir)
      if(!silent) message(sprintf("Downloading to %s.\n", pdf_dir))
    }
  }

  if (!date_indexed || (date_indexed && (date_pdfs > last_local_date))) {
    invisible(lapply(urls, function(url) {
      save_to <- file.path(pdf_dir, basename(url))
      utils::download.file(url, save_to, quiet = silent, mode = "wb")
    }))
  }

  if(all_dates) {
    avail_dates <- c("2020-03-29", "2020-04-05", "2020-04-11")
    dates_to_download <-
      avail_dates[which(! avail_dates[1:(length(avail_dates) - 1)] %in% local_dates)]

    if (length(dates_to_download) == 0) {
      message("All data (old and new) already downloaded. Doing nothing\n")
    }

    for(dl_dates in dates_to_download) {
      pdf_dir = file.path(pdf_dir, dl_dates)
      dir.create(pdf_dir)
      fnames <- basename(urls)
      substring(fnames, 1, 10) <- dl_dates
      old_urls <- file.path(dirname(urls), fnames)
      invisible(lapply(old_urls, function(url) {
        save_to <- file.path(pdf_dir, basename(url))
        utils::download.file(url, save_to, quiet = silent, mode = "wb")
      }))
    }
    return(lubridate::ymd(avail_dates))
  }

  if (!silent) message("Finished downloading Google CMS PDFs\n")

  return(date_pdfs)
}
