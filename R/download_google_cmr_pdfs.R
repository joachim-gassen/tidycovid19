#' Download Google Community Mobility Reports as PDF files
#'
#' Downloads Google Community Mobility Reports
#' (\url{https://www.google.com/covid19/mobility/}).
#' As stated on this webpage, the "reports chart movement trends over time
#' by geography, across different categories of places such as retail and
#' recreation, groceries and pharmacies, parks, transit stations,
#' workplaces, and residential". Google prepares these reports to help
#' interestes parties to assess responses to social distancing guidance
#' related to Covid-19.
#'
#' @param pdf_dir Path to the directory where the PDF files will be stored.
#' @param silent Whether you want the function to send some status messages to
#'     the console. Might be informative as downloading will take some time
#'     and thus defaults to \code{TRUE}.
#'
#' @return A vector containing the filenames that were downloaded.
#'
#' @examples
#' \dontrun{
#'  download_google_cmr_pdfs(dir "~/gogle_cmr_pdfs", silent = TRUE)
#' }
#'
#' @export
download_google_cmr_pdfs <- function(pdf_dir, silent = FALSE) {
  if (length(silent) > 1 || !is.logical(silent)) stop(
    "'silent' needs to be a single logical value"
  )
  if (!dir.exists(pdf_dir)) stop(sprintf(
    "directory '%s' does not exist", pdf_dir
  ))
  if (!silent) message("Start downloading Google CMR PDFs\n")

  cmr_url <- "https://www.google.com/covid19/mobility/"

  urls <- xml2::read_html(cmr_url) %>%
    rvest::html_nodes(css = ".download-link") %>%
    rvest::html_attr('href')

  invisible(lapply(urls, function(url) {
    save_to <- file.path(pdf_dir, basename(url))
    utils::download.file(url, save_to, quiet = silent, mode = "wb")
  }))

  if (!silent) message("Finished downloading Google CMS PDFs\n")
}
