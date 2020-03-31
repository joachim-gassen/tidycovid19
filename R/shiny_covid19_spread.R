#' @title Explore the spread of Covid-19 interactively
#'
#' @description A shiny based web app that allows users to customize the
#'    \code{\link{plot_covid19_spread}} display. The display has been
#'    inpired by the displays created by John Burn-Murdoch for the Financial
#'    Times. You can use it to customize your \code{plot_covid19_spread()}
#'    display as it allows copying the plot generating code to the clipboard,
#'    thanks to the fine {rclipboard} package.
#'
#' @param data The data frame to base the plot on. Should be a merged data
#'     frame obtained by \link{download_merged_data} and defaults to
#'     \code{download_merged_data(cached = TRUE, silent = TRUE)}
#' @param ... Parameters that will be parsed on to \code{\link[shiny]{runApp}}.
#'
#' @details
#'
#' Uses data from the Johns Hopkins University CSSE team on the spread of the
#' SARS-CoV-2 virus and the Covid-19 pandemic
#' (\url{https://github.com/CSSEGISandData/COVID-19}), from the ACAPS
#' governmental measures database
#' (\url{https://www.acaps.org/covid19-government-measures-dataset}) and from
#' the World Bank (\url{https://data.worldbank.org}). See the
#' documentation of the separate download functions of the package for more
#' detail.
#'
#' @examples
#' \dontrun{
#'   shiny_covid19_spread()
#'
#'   df <- download_merged_data(cached = TRUE)
#'   shiny_covid19_spread(df)
#' }
#' @export

shiny_covid19_spread <- function(
  data = download_merged_data(cached = TRUE, silent = TRUE), ...
) {
  if (!is.data.frame(data))
    stop("data has to be a dataframe")
  if (!all(c("iso3c", "date", "deaths", "confirmed") %in% names(data)))
    stop(paste(
      "data does not contain essential variables necessary for preparing",
      "the plot. Consider using 'download_merged_data()' to download the data",
      "or run 'shiny_covid19_spread()' without parameters"
    ))
  shiny_data <- data

  pkg_app_dir <- system.file("app", package = "tidycovid19")
  file.copy(pkg_app_dir, tempdir(), recursive=TRUE)
  app_dir <- paste0(tempdir(), "/app")
  save(list = ls(pattern = "shiny"), file = paste0(app_dir, "/shiny_data.Rda"))
  on.exit(unlink(app_dir, recursive = TRUE))
  try(shiny::runApp(appDir = app_dir, ...))
}
