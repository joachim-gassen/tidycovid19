tidycovid19_data_sources <- read.csv("data-raw/tidycovid19_data_sources.csv",
                                     stringsAsFactors = FALSE)
save(tidycovid19_data_sources, file = "data/tidycovid19_data_sources.RData", version = 2)

library(tidycovid19)
df <- download_merged_data(cached = T, silent = F)

tidycovid19_variable_definitions <- data.frame(
  var_name = names(df),
  var_source = c(
    rep(NA, 3), rep("jhu_ccse", 3), rep("acaps_npi", 5),
    rep("apple_mtr", 3), rep("google_cmr", 6), rep("google_trends", 2),
    rep("wbank", 8), NA
  ),
  var_def = c(
    "Country name",
    "ISO3c country code as defined by ISO 3166-1 alpha-3",
    "Calendar date",
    "Confirmed Covid-19 cases as reported by JHU CSSE",
    "Covid-19-related deaths as reported by JHU CSSE",
    "Covid-19 recoveries as reported by JHU CSSE",
    "Number of social distancing measures reported up to date by ACAPS, net of lifted restrictions",
    "Number of movement restrictions reported up to date by ACAPS, net of lifted restrictions",
    "Number of public health measures reported up to date by ACAPS, net of lifted restrictions",
    "Number of social and economic measures reported up to date by ACAPS, net of lifted restrictions",
    "Number of lockdown measures reported up to date by ACAPS, net of lifted restrictions",
    "Apple Maps usage for driving directions, as percentage*100 relative to the baseline of Jan 13, 2020",
    "Apple Maps usage for walking directions, as percentage*100 relative to the baseline of Jan 13, 2020",
    "Apple Maps usage for public transit directions, as percentage*100 relative to the baseline of Jan 13, 2020",
    paste(
      "Google Community Mobility Reports data for the frequency that people visit",
      c(
       "retail and recreation places", "grocery stores and pharmacies",
       "parks", "transit stations", "workplaces", "residential places"
      ),
      "expressed as a percentage change relative to the baseline period Jan 3 - Feb 6, 2020"
    ),
    "Google search volume for the term 'coronavirus', relative across time with the country maximum scaled to 100",
    "Country-level Google search volume for the term 'coronavirus' over a period starting Jan 1, 2020, relative across countries with the country having the highest search volume scaled to 100 (time-stable)",
    "Country region as classified by the World Bank (time-stable)",
    "Country income group as classified by the World Bank (time-stable)",
    "Country population as reported by the World Bank (original identifier 'SP.POP.TOTL', time-stable)",
    "Country land mass in square kilometers as reported by the World Bank (original identifier 'AG.LND.TOTL.K2', time-stable)",
    "Country population density as reported by the World Bank (original identifier 'EN.POP.DNST', time-stable)",
    "Population in the largest metropolian area of the country as reported by the World Bank (original identifier 'EN.URB.LCTY', time-stable)",
    "Average life expectancy at birth of country citizens in years as reported by the World Bank (original identifier 'SP.DYN.LE00.IN', time-stable)",
    "Country gross domestic product per capita, measured in 2010 US-$ as reported by the World Bank (original identifier 'NY.GDP.PCAP.KD', time-stable)",
    "Date and time where data has been collected from authoritative sources"
  ),
  stringsAsFactors = FALSE
)

save(
  tidycovid19_variable_definitions,
  file = "data/tidycovid19_variable_definitions.RData",
  version = 2
)
