library(RSelenium)
library(dplyr)

# Need to run a docker instance in local development envrionment
# that sets up the "remote" Selenium driver, e.g. (for chrome browser):
#
# docker run -d -p 4445:4444 --restart unless-stopped selenium/standalone-chrome
#

scrape_apple_mtr_url <- function() {
  url = "https://www.apple.com/covid19/mobility"

  rem_dr <- remoteDriver(
    remoteServerAddr = "localhost",
    port = 4445L,
    browserName = "chrome"
  )

  rem_dr$open(silent = TRUE)
  Sys.sleep(1)
  rem_dr$navigate(url)
  Sys.sleep(1)

  web_element <- rem_dr$findElement(
    using = "css",
    "#download-card > div.download-button-container > a"
  )

  url <- web_element$getElementAttribute("href")[[1]]

  if (!is.character(url) || stringr::str_length(url) < 10) {
    stop(sprintf(
      "Failed to scrape valid URL. Returned value is %s", as.character(url)
    ))
  }

  message(sprintf("\nScraped URL for Mobility Trends Reports is '%s'.", url))


  if (file.exists("cached_data/apple_mtr_url.RDS")) {
    old_url <- readRDS("cached_data/apple_mtr_url.RDS")$url
  } else old_url <- "none"

  if(old_url == url) {
    message("URL unchanged from cached URL.\n")
  } else {
    message(sprintf("Old URL was '%s'. Storing new URL\n", old_url))
            saveRDS(tibble(url = url, timestamp = Sys.time()),
                    "cached_data/apple_mtr_url.RDS", version = 2)
  }
  rem_dr$close()
  return(url)
}
