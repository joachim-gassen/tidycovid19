#' Documentation of \code{tidycovid19} Data Sources
#'
#' Brief info on each data source included in \code{tidycovid19}
#'
#' @docType data
#'
#' @usage data(tidycovid19_data_sources)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @source Information has been collected by data providers (URLs included in
#'   data frame). Descriptions of data cleaning steps provided by the author
#'   of this package.
#'
#' @examples
#' data(tidycovid19_data_sources)
#' print(tidycovid19_data_sources)
#'
"tidycovid19_data_sources"

#' Variable Definitions of \code{tidycovid19} Merged Data Frame
#'
#' List variable definitions and data sources for the data frame
#' created by \code{download_merged_data()}.
#' Merge with \code{data(tidycovid19_data_sources)} to get full information
#' for each variable, including description of data source and link to URL.
#'
#' @docType data
#'
#' @usage data(tidycovid19_variable_definitions)
#'
#' @format An object of class \code{"data.frame"}.
#'
#'
#' @examples
#' vd <- tidycovid19_variable_definitions
#' ds <- tidycovid19_data_sources
#' data_info <- dplyr::left_join(vd, ds, by = c(var_source = "id"))
#' print(data_info)
#'
"tidycovid19_variable_definitions"
