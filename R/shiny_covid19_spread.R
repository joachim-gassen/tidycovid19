#' @title Explore the spread of Covid-19 interactively
#'
#' @description A shiny based web app that allows users to customize the
#'    \code{\link{plot_covid19_spread}} display. The display has been
#'    inpired by the displays created by John Burn-Murdoch for the Financial
#'    Times. You can use it to customize your \code{plot_covid19_spread()}
#'    display as it allows copying the plot generating code to the clipboard,
#'    thanks to the fine {rclipboard} package.
#'
#' @param plot_options A named list containing parameters to
#'    initialize the plot in the shiny app. All parameters of
#'    \code{plot_covid19_spread()} are supported.
#'
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
#'   shiny_covid19_spread(
#'     plot_options = list(data = df, highlight = c("USA", "ESP", "ITA"))
#'   )
#' }
#' @export

shiny_covid19_spread <- function(plot_options = list(), ...) {
  default_params <- formals(plot_covid19_spread)
  for (i in 1:length(default_params)) {
    if(is.language(default_params[[i]]))
      default_params[[i]] <- eval(default_params[[i]], default_params)
  }

  if(!is.list(plot_options) || is.data.frame(plot_options)) stop(
    "'plot_options' needs to be a list containing 'plot_covid19_spread parameters'"
  )

  no_param <- !names(plot_options) %in% names(default_params)
  if (any(no_param)) {
    warning(sprintf(
      paste("List members of 'plot_options' that are not valid parameters",
            "of 'plot_covid19_spread()': %s"),
      names(plot_options)[no_param]
    ))
  }
  plot_options <- utils::modifyList(default_params, plot_options)

  if (!is.data.frame(plot_options$data))
    stop("data has to be present in 'plot_options' and needs to be a dataframe")
  if (!all(c("iso3c", "date", "deaths", "confirmed") %in% names(plot_options$data)))
    stop(paste(
      "data does not contain essential variables necessary for preparing",
      "the plot. Consider using 'download_merged_data()' to download the data",
      "or run 'shiny_covid19_spread()' without parameters"
    ))
  shiny_plot_options <- plot_options

  pkg_app_dir <- system.file("app", package = "tidycovid19")
  file.copy(pkg_app_dir, tempdir(), recursive=TRUE)
  app_dir <- paste0(tempdir(), "/app")
  save(list = ls(pattern = "shiny"), file = paste0(app_dir, "/shiny_data.Rda"))
  on.exit(unlink(app_dir, recursive = TRUE))
  try(shiny::runApp(appDir = app_dir, ...))
}
