# --- Shiny app ----------------------------------------------------------------

remotes::install_github("joachim-gassen/tidycovid19",
                        force = TRUE, upgrade = "never")
library(tidycovid19)
shiny_covid19_spread()

