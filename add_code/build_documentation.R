unlink('~/github/tidycovid19/README_cache', recursive = TRUE)
rmarkdown::render("README.Rmd")
pkgdown::build_site()