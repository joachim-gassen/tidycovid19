# --- Code to generate the cached data -----------------------------------------

# remotes::install_github("joachim-gassen/tidycovid19")
library(dplyr)
library(lubridate)
library(tidycovid19)
library(stringr)

source("add_code/scrape_apple_mtr_url.R", echo = FALSE)

jhu_list <- download_jhu_csse_covid19_data(
  type = c("country", "country_region", "us_county"), silent = TRUE
)
saveRDS(jhu_list, "cached_data/jhu_csse_covid19.RDS", version = 2)

ecdc <- download_ecdc_covid19_data(silent = TRUE)
saveRDS(ecdc, "cached_data/ecdc_covid19.RDS", version = 2)

owid_data <- download_owid_data(silent = TRUE)
saveRDS(owid_data, "cached_data/owid_data.RDS", version = 2)

acaps <- download_acaps_npi_data(silent = TRUE)
saveRDS(acaps, "cached_data/acaps_npi.RDS", version = 2)

wblist <- download_wbank_data(var_def = TRUE, silent = TRUE)
saveRDS(wblist, "cached_data/wbank.RDS", version = 2)

amtr_url <- scrape_apple_mtr_url()
amtr_list <- download_apple_mtr_data(
  amtr_url,
  type = c("country", "country_region", "country_city"),
  silent = TRUE
)
saveRDS(amtr_list, "cached_data/apple_mtr.RDS")

gcmr_list <- download_google_cmr_data(
  type = c("country", "country_region", "country_sub_region", "us_county"),
  silent = TRUE
)
saveRDS(gcmr_list, "cached_data/google_cmr.RDS", version = 2)

gtlist <- download_google_trends_data(
  type = c('country', 'country_day', 'region', 'city'), silent = TRUE
)
saveRDS(gtlist, "cached_data/google_trends.RDS", version = 2)

oxlist <- download_oxford_npi_data(type = c("measures", "index"), silent = TRUE)
saveRDS(oxlist, "cached_data/oxford_npi.RDS", version = 2)


# Code from download_merged_data() to avoid reloading the data

jhu_list <- readRDS("cached_data/jhu_csse_covid19.RDS")

ecdc <- readRDS("cached_data/ecdc_covid19.RDS") %>%
  select(-timestamp, -country_territory) %>%
  dplyr::filter(!is.na(.data$iso3c) &
                  !.data$iso3c %in% c("XKX", "N/A", "MSF", "CNG1925", "RKS"))

ecdc_acc <- expand.grid(
  date = lubridate::as_date(min(ecdc$date):max(ecdc$date)),
  iso3c = unique(ecdc$iso3c),
  stringsAsFactors = FALSE
) %>% select(iso3c, date) %>%
  left_join(ecdc, by = c("iso3c", "date")) %>%
  mutate(
    cases = ifelse(is.na(cases), 0, cases),
    deaths = ifelse(is.na(deaths), 0, deaths)
  ) %>%
  group_by(iso3c) %>%
  mutate(
    ecdc_cases = cumsum(cases),
    ecdc_deaths = cumsum(deaths)
  ) %>%
  filter(ecdc_cases > 0 | ecdc_deaths > 0) %>%
  select(iso3c, date, ecdc_cases, ecdc_deaths) %>%
  ungroup()

owid_data <- readRDS("cached_data/owid_data.RDS") %>%
  select(-timestamp)

jhu_cases <- jhu_list[[1]] %>%
  select(-country, -timestamp)

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
  merged_base %>%
    left_join(
      my_npi %>%
        rename(date = npi_date) %>%
        mutate(npi = ifelse(log_type == "Phase-out measure", -1, 1)) %>%
        select(iso3c, date, npi) %>%
        group_by(iso3c, date) %>%
        summarise(npi = sum(npi), .groups = "drop"),
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

merged_base <- jhu_cases %>%
  full_join(ecdc_acc, by = c("iso3c", "date")) %>%
  arrange(iso3c, date) %>%
  mutate(country = countrycode::countrycode(iso3c, "iso3c", "country.name")) %>%
  select(iso3c, country, everything())

merged <- merged_base %>%
  left_join(owid_data, by = c("iso3c", "date")) %>%
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

