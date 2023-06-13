library(tidycovid19)
df <- download_merged_data(cached = T, silent = T)
tidycovid19_data_sources <- read.csv("data-raw/tidycovid19_data_sources.csv",
                                     stringsAsFactors = FALSE)
last_data <- data.frame(
  id = tidycovid19_data_sources$id,
  last_data = as.Date(unlist(
    lapply(
      c(
        "confirmed", "ecdc_cases", "total_vaccinations", "population",
        "soc_dist", "oxcgrt_government_response_index", "apple_mtr_driving",
        "gcmr_residential", "gtrends_score", "population"),
      function(x) max(dplyr::pull(df[!is.na(df[x]), "date"]))
    )
  ), origin = "1970-01-01"),
  stringsAsFactors = FALSE
)
tidycovid19_data_sources <- tidycovid19_data_sources %>%
  dplyr::left_join(last_data, by = "id")

save(tidycovid19_data_sources, file = "data/tidycovid19_data_sources.RData", version = 2)

tidycovid19_variable_definitions <- data.frame(
  var_name = names(df),
  var_source = c(
    rep(NA, 3), rep("jhu_ccse", 3), rep("ecdc_covid19", 2),
    rep("owid_data", 6), rep("acaps_npi", 5), rep("oxford_npi", 4),
    rep("apple_mtr", 3), rep("google_cmr", 6), rep("google_trends", 2),
    rep("wbank", 8), NA
  ),
  var_def = c(
    "ISO3c country code as defined by ISO 3166-1 alpha-3",
    "Country name",
    "Calendar date",
    "Confirmed Covid-19 cases as reported by JHU CSSE (accumulated)",
    "Covid-19-related deaths as reported by JHU CSSE (accumulated)",
    "Covid-19 recoveries as reported by JHU CSSE (accumulated)",
    "Covid-19 cases as reported by ECDC (accumulated, weekly post 2020-12-14)",
    "Covid-19-related deaths as reported by ECDC (accumulated, weekly post 2020-12-14)",
    "Accumulated test counts as reported by Our World in Data",
    "Definition of what constitutes a 'test'",
    "The share of COVID-19 tests that are positive, given as a rolling 7-day average",
    "Number of COVID-19 patients in hospital on a given day",
    "Number of COVID-19 patients in intensive care units (ICUs) on a given day",
    "Total number of COVID-19 vaccination doses administered",
    "Number of social distancing measures reported up to date by ACAPS, net of lifted restrictions",
    "Number of movement restrictions reported up to date by ACAPS, net of lifted restrictions",
    "Number of public health measures reported up to date by ACAPS, net of lifted restrictions",
    "Number of social and economic measures reported up to date by ACAPS, net of lifted restrictions",
    "Number of lockdown measures reported up to date by ACAPS, net of lifted restrictions",
    "Stringency index as provided by the Oxford COVID-19 Government Response Tracker",
    "Legacy stringency index based on old data format (prior April 25, 2020) as provided by the Oxford COVID-19 Government Response Tracker",
    "Overall government response index as provided by the Oxford COVID-19 Government Response Tracker",
    "Containment and health index as provided by the Oxford COVID-19 Government Response Tracker",
    "Apple Maps usage for driving directions, as percentage*100 relative to the baseline of Jan 13, 2020",
    "Apple Maps usage for walking directions, as percentage*100 relative to the baseline of Jan 13, 2020",
    "Apple Maps usage for public transit directions, as percentage*100 relative to the baseline of Jan 13, 2020",
    paste(
      "Google Community Mobility Reports data for the frequency that people visit",
      c(
       "retail and recreation places", "grocery stores and pharmacies",
       "parks", "transit stations", "workplaces", "residential places"
      ),
      "expressed as a percentage*100 change relative to the baseline period Jan 3 - Feb 6, 2020"
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
