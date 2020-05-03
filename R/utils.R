data_info <- function(data_source) {
  ds <- tidycovid19::tidycovid19_data_sources

  message("\nData Info:\n")

  lapply(strwrap(paste(
    ds$description[ds$id == data_source],
    "The column 'timestamp' reports the time the data was downloaded from",
    "its authoritative source."
  )), message)

  message("")
  lapply(strwrap(sprintf(paste(
    "For further information refer to: %s.\n"
  ), ds$url[ds$id == data_source])), message)
  message("")
}
