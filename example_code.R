library(tidycovid19)

# Code to generate the cached data

df <- download_acaps_npi_data(silent = TRUE)
saveRDS(df, "cached_data/acaps_npi.RDS")
wblist <- download_wbank_data(silent = TRUE, var_def = TRUE)
saveRDS(wblist, "cached_data/wbank.RDS")
df <- download_jhu_csse_covid19_data(silent = TRUE)
saveRDS(df, "cached_data/jhu_csse_covid19.RDS")
gtlist <- download_google_trends_data(silent = TRUE,
                                      type = c('country', 'country_day', 'region', 'city'))
saveRDS(gtlist, "cached_data/google_trends.RDS")
merged <- download_merged_data()
saveRDS(merged, "cached_data/merged.RDS")


# Some visuals

plot_covid19_spread(merged)
plot_covid19_spread(merged, highlight = "DEU",
                    intervention = "lockdown")
plot_covid19_spread(merged, highlight = c("ITA", "ESP", "FRA", "DEU", "USA"),
                    intervention = "lockdown")
