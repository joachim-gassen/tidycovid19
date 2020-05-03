library(tidyverse)

update <- FALSE

parse_ggmaps_list <- function(lst) {
  res <- lst$results[[1]]
  iso2c <- NA
  region_level_1_long <- NA
  region_level_1_short <- NA
  region_level_2_long <- NA
  region_level_2_short <- NA

  for (i in res$address_components) {
    if (i$types[[1]] == "country") iso2c <- i$short_name
    if (i$types[[1]] == "administrative_area_level_1") {
      region_level_1_long <- i$long_name
      region_level_1_short <- i$short_name
    }
    if (i$types[[1]] == "administrative_area_level_2") {
      region_level_2_long <- i$long_name
      region_level_2_short <- i$short_name
    }
  }

  return(tibble(
    iso2c = iso2c,
    region_level_1_long = region_level_1_long,
    region_level_1_short =  region_level_1_short,
    region_level_2_long = region_level_2_long,
    region_level_2_short = region_level_2_short
  ))
}

if (update) {
  url <- readRDS(gzcon(url("https://raw.githubusercontent.com/joachim-gassen/tidycovid19/master/cached_data/apple_mtr_url.RDS")))$url[1]
  raw_data <- read_csv(url, col_types = readr::cols())
  cities <- raw_data %>%
    filter(geo_type == "city") %>%
    select(region, alternative_name) %>%
    unique()
  sub_regions <- raw_data %>%
    filter(geo_type == "sub-region") %>%
    select(region, alternative_name) %>%
    unique()


  library(ggmap)
  register_google(key = "Get key from https://console.cloud.google.com/")
  cities_list <- geocode(cities$region, output = "all")
  sub_regions_list <- geocode(sub_regions$region, output = "all")

  save(cities, cities_list, sub_regions, sub_regions_list,
       file = "data-raw/apple_mtr_region_ggmap_data.RData", version = 2)
}

load("data-raw/apple_mtr_region_ggmap_data.RData")

cities_iso3c <- do.call(rbind, lapply(cities_list, parse_ggmaps_list)) %>%
  mutate(
    geo_type = "city",
    region = cities$region,
    alternative_name = cities$alternative_name,
    iso3c = countrycode::countrycode(iso2c, "iso2c", "iso3c")
  ) %>%
  select(geo_type, region, alternative_name, iso3c)

sub_regions_df <- do.call(rbind, lapply(sub_regions_list, parse_ggmaps_list))

sub_regions_df %>%
  mutate(
    region = sub_regions$region,
    alternative_name = sub_regions$alternative_name,
    iso3c = countrycode::countrycode(iso2c, "iso2c", "iso3c")
  ) %>%
  rename(region_ggmap = region_level_1_long) %>%
  select(iso3c, region, alternative_name, region_ggmap) -> sub_regions_iso3c

sub_regions_iso3c$new_iso3c <- NA
sub_regions_iso3c$new_iso3c[2] <- "BRA"
sub_regions_iso3c$new_iso3c[98] <- "IRL"
sub_regions_iso3c$new_iso3c[106] <- "ISR"
sub_regions_iso3c$new_iso3c[109] <- "FRA"
sub_regions_iso3c$new_iso3c[125] <- "AUS"
sub_regions_iso3c$new_iso3c[142] <- "MEX"
sub_regions_iso3c$new_iso3c[155] <- "ZAF"
sub_regions_iso3c$new_iso3c[227] <- "IRL"
sub_regions_iso3c$new_iso3c[272] <- "POL"
sub_regions_iso3c$new_iso3c[291] <- "IRL"
sub_regions_iso3c$new_iso3c[356] <- "ISR"
sub_regions_iso3c$new_iso3c[402] <- "ITA"
sub_regions_iso3c$new_iso3c[406] <- "POL"
sub_regions_iso3c$new_iso3c[445] <- "BRA"
sub_regions_iso3c$new_iso3c[463] <- "POL"
sub_regions_iso3c$new_iso3c[474] <- "NLD"
sub_regions_iso3c$new_iso3c[479] <- "ISR"
sub_regions_iso3c$new_iso3c[543] <- "AUS"
sub_regions_iso3c$new_iso3c[559] <- "IRL"
sub_regions_iso3c$new_iso3c[561] <- "NZL" # "West Coast Region" -- guess work...

sub_regions_iso3c <- sub_regions_iso3c %>%
  mutate(
    geo_type = "sub-region",
    iso3c = ifelse(!is.na(new_iso3c), new_iso3c, iso3c)
  ) %>%
  select(geo_type, region, alternative_name, iso3c)

apple_mtr_region_iso3c_link <- rbind(cities_iso3c, sub_regions_iso3c)
save(apple_mtr_region_iso3c_link, file = "R/sysdata.rda")
