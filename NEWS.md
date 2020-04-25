# tidycovid19 0.0.0.9000

* Included `plot_covid19_stripes()` to produce Covid-19 stripe visuals (good if you want to compare many countries quickly)

* Changed defaults in `plot_covid19_spread()` to have more reasonable defaults for per capita plots.

* Added code to download Google's Community Mobility Report data in CSV format

* Removed Google PDF download and scraping code as Google made its data available via CSV files (Yeah!) 

* Included Apple Mobility Trends Reports data

* `*_covid19_spread()`: Added additional options for per capita displays

* Included code to scrape information provided by Google COVID-19 Community Mobility Reports

* `*_covid19_spread()`: Added option to exclude unhighlighted countries

* `shiny_covid19_spread()`: Changed API to include all plot options

* `*_covid19_spread()`: Added option for averaged daily changes

* Included Oxford NPI data

* `*_covid19_spread()`: Added option for linear Y-axis

* `shiny_covid19_spread()`: Added option to copy plot code to clipboard

* Initial in-development version on Github
