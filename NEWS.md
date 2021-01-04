# tidycovid19 0.0.0.9000

* 2021-01-03: Added hopitalization and vaccination data from Our World in Data. This implies a renaming of the function `download_owid_testing_data()` to `download_owid_data()`

* 2020-06-07: Added some options to `download_google_trends_data()` to allow the download of Google Trends data for selected countries and a wider set of countries. Also included an option to omit the sleeping delay between Google Trends queries (usage not encouraged). 

* 2020-05-21: Changed definition of active cases

* 2020-05-10: Added ECDC Covid-19 case data

* 2020-05-10: Added OWID testing data

* 2020-05-10: Changed Google CMR data to percentage*100 for consistency

* 2020-05-10: Added additional coding examples in `example_code.R`

* 2020-05-03: Added long overdue data documentation

* 2020-05-03: Provided option to download regional and U.S. county JHU CSSE data

* 2020-05-03: Provided option to download regional and U.S. county Google CMR data

* 2020-05-03: Provided option to download regional Apple MTR data

* 2020-04-30: Incorporated the new format for OxCGRT Government Respose data

* 2020-04-26: Included `map_covid19()` to produce choropleth maps of the Covid-19 spread

* 2020-04-25: Included `plot_covid19_stripes()` to produce Covid-19 stripe visuals (good if you want to compare many countries quickly)

* 2020-04-17: Changed defaults in `plot_covid19_spread()` to have more reasonable defaults for per capita plots

* 2020-04-17: Added code to download Google's Community Mobility Report data in CSV format

* 2020-04-17: Removed Google PDF download and scraping code as Google made its data available via CSV files (Yeah!) 

* 2020-04-16: Included Apple Mobility Trends Reports data

* 2020-04-13: `*_covid19_spread()`: Added additional options for per capita displays

* 2020-04-12: Included code to scrape information provided by Google COVID-19 Community Mobility Reports

* 2020-04-10: `*_covid19_spread()`: Added option to exclude unhighlighted countries

* 2020-04-10: `shiny_covid19_spread()`: Changed API to include all plot options

* 2020-04-09: `*_covid19_spread()`: Added option for averaged daily changes

* 2020-04-05: Included Oxford NPI data

* 2020-03-31: `*_covid19_spread()`: Added option for linear Y-axis

* 2020-03-31: `shiny_covid19_spread()`: Added option to copy plot code to clipboard

* 2020-03-29: Initial in-development version on Github
