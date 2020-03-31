# remotes::install_github("joachim-gassen/tidycovid19")
library(tidycovid19)

# Some examples - not part of the package
# Code to generate the cached data

df <- download_acaps_npi_data()
saveRDS(df, "cached_data/acaps_npi.RDS")
wblist <- download_wbank_data(var_def = TRUE)
saveRDS(wblist, "cached_data/wbank.RDS")
df <- download_jhu_csse_covid19_data()
saveRDS(df, "cached_data/jhu_csse_covid19.RDS")
gtlist <- download_google_trends_data(
  type = c('country', 'country_day', 'region', 'city')
)
saveRDS(gtlist, "cached_data/google_trends.RDS")
merged <- download_merged_data()
saveRDS(merged, "cached_data/merged.RDS")

lst <- download_wbank_data(silent = TRUE, cached = TRUE)


# Some visuals

plot_covid19_spread(merged)
plot_covid19_spread(merged, highlight = "DEU",
                    intervention = "lockdown")
plot_covid19_spread(merged, highlight = c("ITA", "ESP", "FRA", "DEU", "USA"),
                    intervention = "lockdown")

# Shiny app

shiny_covid19_spread()
