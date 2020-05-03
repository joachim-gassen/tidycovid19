# Some example code on how to use the {tidycovid19}
# Not part of the package of the package itself

# --- Code to generate the cached data -----------------------------------------

# remotes::install_github("joachim-gassen/tidycovid19")
library(dplyr)
library(lubridate)
library(tidycovid19)
library(stringr)

source("scrape_apple_mtr_url.R", echo = FALSE)

acaps <- download_acaps_npi_data()
saveRDS(acaps, "cached_data/acaps_npi.RDS", version = 2)
wblist <- download_wbank_data(var_def = TRUE)
saveRDS(wblist, "cached_data/wbank.RDS", version = 2)
jhu_list <- download_jhu_csse_covid19_data(
  type = c("country", "country_region", "us_county")
)
saveRDS(jhu_list, "cached_data/jhu_csse_covid19.RDS", version = 2)

amtr_url <- scrape_apple_mtr_url()
amtr_list <- download_apple_mtr_data(
  amtr_url,
  type = c("country", "country_region", "country_city")
)
saveRDS(amtr_list, "cached_data/apple_mtr.RDS")

gcmr_list <- download_google_cmr_data(
  type = c("country", "country_region", "us_county")
)
saveRDS(gcmr_list, "cached_data/google_cmr.RDS", version = 2)

gtlist <- download_google_trends_data(
  type = c('country', 'country_day', 'region', 'city')
)
saveRDS(gtlist, "cached_data/google_trends.RDS", version = 2)

oxlist <- download_oxford_npi_data(type = c("measures", "index"))
saveRDS(oxlist, "cached_data/oxford_npi.RDS", version = 2)

# Code from download_merged_data() to avoid reloading the data

jhu_list <- readRDS("cached_data/jhu_csse_covid19.RDS")

cases <- jhu_list[[1]] %>%
    select(-timestamp)

npis <- readRDS("cached_data/acaps_npi.RDS") %>%
    mutate(npi_date = ymd(date_implemented)) %>%
    rename(npi_type = category) %>%
    select(iso3c, npi_date, log_type, npi_type)

amtr_list <- readRDS("cached_data/apple_mtr.RDS")

amtr <- amtr_list[[1]] %>%
  select(-timestamp) %>%
  rename_at(vars(-iso3c, -date), ~ paste0("apple_mtr_", .))

gcmr_list <- readRDS("cached_data/google_cmr.RDS")

gcmr <- gcmr_list[[1]] %>%
  select(-timestamp) %>%
  rename_at(vars(-iso3c, -date), ~ paste0("gcmr_", .))

gtrends_list <- readRDS("cached_data/google_trends.RDS")

gtrends_cd <- gtrends_list[[2]] %>%
  select(-timestamp)

gtrends_c <- gtrends_list[[1]] %>%
  rename(gtrends_country_score = gtrends_score) %>%
  select(-timestamp)

wb_list <- readRDS("cached_data/wbank.RDS")
wbank <-  wb_list[[1]] %>%
  select(-country, -timestamp)

calc_npi_measure <-function(type, var_name) {
  my_npi <- npis %>% filter(npi_type == type)
  cases %>%
  left_join(
      my_npi %>%
        rename(date = npi_date) %>%
        mutate(npi = ifelse(log_type == "Phase-out measure", -1, 1)) %>%
        select(iso3c, date, npi) %>%
        group_by(iso3c, date) %>%
        summarise(npi = sum(npi)),
      by = c("iso3c", "date")
    ) %>%
    group_by(iso3c) %>%
    mutate(
      npi = ifelse(is.na(npi), 0, npi),
      sum_npi = cumsum(npi)
    ) %>%
    ungroup() %>%
    select(.data$iso3c, .data$date, .data$sum_npi) -> df

  names(df)[3] <- var_name
  df
}

# 2020-04-01: There is a new populated category in the ACAPS NPI data
#             "Humanitarian exemption". I do not code it for the time
#             being as it contains only two Irish cases (parking for
#             essential workers and leeway for pharamacisist)

# 2020-04-16: The category "Social and economic measures" has been renamed
#             to "Governance and socio-economic measures" in the ACAPS data.
#             I reflect this name change by renaming the variable 'soc_econ'
#             'gov_soc_econ'.

merged <- cases %>%
  left_join(
    calc_npi_measure("Social distancing", "soc_dist"),
    by = c("iso3c", "date")
  ) %>%
  left_join(
    calc_npi_measure("Movement restrictions", "mov_rest"),
    by = c("iso3c", "date")
  ) %>%
  left_join(
    calc_npi_measure("Public health measures", "pub_health"),
    by = c("iso3c", "date")
  ) %>%
  left_join(
    calc_npi_measure("Governance and socio-economic measures", "gov_soc_econ"),
    by = c("iso3c", "date")
  ) %>%
  left_join(
    calc_npi_measure("Lockdown", "lockdown"),
    by = c("iso3c", "date")
  ) %>%
  left_join(amtr, by = c("iso3c", "date")) %>%
  left_join(gcmr, by = c("iso3c", "date")) %>%
  left_join(gtrends_cd, by = c("iso3c", "date")) %>%
  left_join(gtrends_c, by = "iso3c") %>%
  left_join(wbank, by = "iso3c") %>%
  group_by(iso3c) %>%
  mutate(
    has_npi = max(soc_dist) + max(mov_rest) +
      max(.data$pub_health) + max(gov_soc_econ) +
      max(lockdown) > 0,
    soc_dist = ifelse(has_npi, soc_dist, NA),
    mov_rest = ifelse(has_npi, mov_rest, NA),
    pub_health = ifelse(has_npi, pub_health, NA),
    gov_soc_econ = ifelse(has_npi, gov_soc_econ, NA),
    lockdown = ifelse(has_npi, lockdown, NA)
  ) %>%
  select(-has_npi) %>%
  ungroup() %>%
  mutate(timestamp = Sys.time())

saveRDS(merged, "cached_data/merged.RDS", version = 2)



# --- Shiny app ----------------------------------------------------------------

remotes::install_github("joachim-gassen/tidycovid19",
                        force = TRUE, upgrade = "never")
library(tidycovid19)
shiny_covid19_spread()


# --- Customize shiny app ------------------------------------------------------

shiny_covid19_spread(plot_options = list(
  type = "deaths", min_cases = 100, min_by_ctry_obs = 10,
  edate_cutoff = 40, per_capita = FALSE, cumulative = FALSE, change_ave = 7,
  highlight = c("FRA", "DEU", "ITA", "ESP", "GBR", "USA"),
  intervention = "lockdown"
))


# --- Some visuals -------------------------------------------------------------

library(tidycovid19)

merged <- download_merged_data(cached = TRUE, silent = TRUE)
plot_covid19_spread(merged)
plot_covid19_spread(merged, highlight = "DEU",
                    intervention = "lockdown")
plot_covid19_spread(merged, highlight = c("ITA", "ESP", "FRA", "DEU", "USA"),
                    intervention = "lockdown")
plot_covid19_spread(merged, highlight = c("ITA", "ESP", "FRA", "DEU", "USA"),
                    exclude_others = TRUE, intervention = "lockdown")

plot_covid19_spread(
  per_capita = TRUE, per_capita_x_axis = TRUE,
  population_cutoff = 10,
  min_cases = 0.1,
  highlight = c("ITA", "ESP", "FRA", "DEU", "USA", "BEL", "FRA", "NLD", "GBR"),
  intervention = "lockdown"
)


# --- Example clipping code produced by shiny_covid19_spread() ------------------

# Code generated by shiny_covid19_spread() of the {tidycovid19} package
# See: https://github.com/joachim-gassen/tidycovid19
# Run in R/Rstudio. See https://www.r-project.org and https://www.rstudio.com
# Uncomment the following to install the {tidycovid19} package

# remotes::install_github("joachim-gassen/tidycovid19)

library(tidycovid19)

plot_covid19_spread(
  type = "deaths", min_cases = 100, min_by_ctry_obs = 7,
  edate_cutoff = 30, per_capita = FALSE,
  highlight = c("BEL", "CHN", "FRA", "DEU", "IRN", "ITA", "KOR",
                "NLD", "ESP", "CHE", "GBR", "USA"),
  intervention = NULL
)


# --- Code from https://robjhyndman.com/hyndsight/logratios-covid19/ -----------

library(tidyverse)
library(tsibble)
library(tidycovid19)

updates <- download_merged_data(cached = TRUE)

countries <- c("AUS", "NZL", "ITA", "ESP", "USA", "GBR")

updates %>%
  mutate(cases_logratio = difference(log(confirmed))) %>%
  filter(
    iso3c %in% countries,
    date >= as.Date("2020-03-01")
  ) %>%
  ggplot(aes(x = date, y = cases_logratio, col = country)) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(. ~ country, ncol = 3) +
  xlab("Date")

updates %>%
  mutate(
    cases_logratio = difference(log(confirmed))
  ) %>%
  filter(iso3c %in% countries) %>%
  filter(date >= as.Date("2020-03-01")) %>%
  ggplot(aes(x = date, y = cases_logratio, col = country)) +
  geom_hline(yintercept = log(2)/c(2:7,14,21), col='grey') +
  geom_smooth(method = "loess", se = FALSE) +
  scale_y_continuous(
    "Daily increase in cumulative cases",
    breaks = log(1+seq(0,60,by=10)/100),
    labels = paste0(seq(0,60,by=10),"%"),
    minor_breaks=NULL,
    sec.axis = sec_axis(~ log(2)/(.),
                        breaks = c(2:7,14,21),
                        name = "Doubling time (days)")
  ) +
  theme_minimal() + theme(
    panel.grid = element_blank()
  )


# --- Find data inconsitencies in JHU CSSE data --------------------------------
library(tidycovid19)
library(dplyr)

df <- download_jhu_csse_covid19_data(cached = TRUE, silent = TRUE)

df %>%
  group_by(iso3c) %>%
  filter(recovered < lag(recovered) |
           recovered > lead(recovered)) -> odd_recovered

df %>%
  group_by(iso3c) %>%
  filter(deaths < lag(deaths) |
           deaths > lead(deaths)) -> odd_deaths

df %>%
  group_by(iso3c) %>%
  filter(confirmed < lag(confirmed) |
           confirmed > lead(confirmed))


# --- Check Deaths per Capita for China ----------------------------------------

library(tidyverse)
library(tidycovid19)

merged_data <- download_merged_data(cached = T, silent = T)
chn_deaths <- max(merged_data$deaths[merged_data$iso3c == "CHN"])
chn_population <- unique(merged_data$population[merged_data$iso3c == "CHN"])

c(chn_deaths, chn_population, 1e5*chn_deaths/chn_population)

5*chn_population/1e5

countries <- "CHN"
print(plot_covid19_spread(merged_data,
                          highlight = countries,
                          type = "deaths",
                          per_capita = TRUE,
                          exclude_others = TRUE,
                          min_cases = 0.1))

# ggsave("pic.png")

# --- Use old PDF scraping code ------------------------------------------------

# Install old package version that still contains the PDF scraping code
# remotes::install_github("joachim-gassen/tidycovid19", ref = "0990bc6")

library(tidycovid19)
library(tidyverse)
library(pdftools)
library(png)

pdf_url <- "https://www.gstatic.com/covid19/mobility/2020-04-05_BR_Mobility_Report_en.pdf"
pdf_convert(pdf_url, pages = 1, filenames = "google_cmr_de_p1.png", verbose = FALSE)

bitmaps <- tidycovid19:::extract_line_graph_bitmaps(pdf_url, 1)
png_file <- tempfile("bitmap_", fileext = ".png")
writePNG(bitmaps[[1]][[1]], "bitmap.png")

df <- tidycovid19:::parse_line_graph_bitmap(bitmaps[[1]][[1]])

# Make sure that you reinstall the current version of the package after you
# are done exploring the PDF scraping code

# remotes::install_github("joachim-gassen/tidycovid19")


# --- Show reduction in NPIs ---------------------------------------------------

#remotes::install_github("joachim-gassen/tidycovid19")
library(tidyverse)
library(tidycovid19)

acaps <- download_acaps_npi_data(cached = TRUE, silent = TRUE)
merged <- download_merged_data(cached = TRUE, silent = TRUE)

# Example
ggplot(merged %>% filter(iso3c == "AUT"), aes(x = date, y = mov_rest)) + geom_line()

df <- acaps %>%
  rename(date = date_implemented) %>%
  mutate(nobs = 1*(log_type == "Introduction / extension of measures") -
           1*(log_type != "Introduction / extension of measures")) %>%
  select(iso3c, date, log_type, category, nobs)

ggplot(df, aes(x = date, fill = category, weight = nobs)) +
  geom_histogram(data = df %>% filter(log_type == "Introduction / extension of measures"),
                 position = "stack", binwidth = 24*3600*7) +
  geom_histogram(data = df %>% filter(log_type != "Introduction / extension of measures"),
                 position = "stack", binwidth = 24*3600*7) +
  theme_minimal() +
  labs(title = "Implementation and Lifting of Interventions over Calendar Time",
       x = "Date",
       y = "Number of interventions",
       fill = "Intervention") +
  theme(legend.position = c(0.25, 0.8),
        legend.background = element_rect(fill = "white", color = NA),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7))
