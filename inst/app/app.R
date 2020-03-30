library(shiny)
library(dplyr)
library(shinyWidgets)
library(tidycovid19)

ui <- fluidPage(
  titlePanel("Explore the Spread of Covid-19"),
  tags$p(
    "This display is based on data",
    tags$ul(
      tags$li("from the",
              tags$a(href = "https://github.com/CSSEGISandData/COVID-19",
                     "Johns Hopkins University CSSE team"),
              "on the spread of the SARS-CoV-2 virus"),
      tags$li("from the",
              tags$a(href = "https://www.acaps.org/covid19-government-measures-dataset",
                     "ACAPS governmental measures database"),
              "and"),
      tags$li(HTML(paste0("from the ",
              tags$a(href = "https://data.worldbank.org", "World Bank"), ".")))
    ),
    "It has been inpired by the displays created by John Burn-Murdoch",
    "for the Financial Times."
  ),
  hr(),

  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "type", "Which statistics do you want to display?",
        c("Confirmed Cases" = "confirmed", "Reported deaths" = "deaths",
          "Recovered Cases" = "recovered"),
        "deaths"
      ),
      sliderInput(
        "min_cases",
        "Set day zero to day where countries have more than ... cases",
        min = 1, max = 1000, value = 100
      ),
      sliderInput(
        "min_by_ctry_obs",
        "Required number of days to include country",
        min = 1, max = 30, value = 7
      ),
      sliderInput(
        "edate_cutoff",
        "How many days do you want to display?",
        min = 10, max = 90, value = 30
      ),
      checkboxInput(
        "per_capita",
        "Display relative to population (per capita)",
        FALSE
      ),
      selectInput(
        "intervention",
        "Select intervention type to highlight by points",
        c("None" = "none", "General lockdowns" = "lockdown",
          "Social distancing"= 'soc_dist', "Movement restrictions" = 'mov_rest',
          "Public health measures" = 'pub_health',
          "Social and economic measures" = 'soc_econ'),
        "none"
      ),
      uiOutput("SelectCountriesHL")
    ),
    mainPanel(div(
      style = "position:relative",
      plotOutput(
        "Covid19Plot", width = "100%", height = "600px",
        hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"))
    ),
    uiOutput("Covid19PlotHover")
    )),
  hr(),
  fluidRow(
    column(
      12, align="center",
      HTML("Based on the <a href=https://joachim-gassen.github.io/tidycovid19>",
           "{tidycovid19} R Package</a>, <a href=https://twitter.com/JoachimGassen>",
           "Joachim Gassen</a>,",
           "<a href=https://www.wiwi.hu-berlin.de/rewe>",
           "Humboldt-Universität zu Berlin</a> and",
           "<a href=https://www.accounting-for-transparency.de>",
           "TRR 266 Accounting for Transparency</a>, 2020<p>")
    )
  )
)

server <- function(input, output) {
  load("shiny_data.Rda")
  df <- shiny_data

  dyn_data <- reactive({
    df %>%
      group_by(iso3c) %>%
      filter(!! sym(input$type) >= input$min_cases) %>%
      mutate(edate = as.numeric(date - min(date))) %>%
      filter(!is.na(edate)) %>%
      group_by(iso3c) %>%
      filter(n() >= input$min_by_ctry_obs) %>%
      ungroup() %>%
      filter(edate <= input$edate_cutoff) -> df_temp

    if (input$per_capita) df_temp <- df_temp %>%
        mutate(!! input$type := 1e5*(!! sym(input$type))/population) %>%
        filter(!is.na(!! sym(input$type)))

    return(df_temp)
  })

  ctry_selected <- reactiveVal("all")

  ctry_list <- reactive({
    dyn_data() %>%
      select(iso3c, country) %>%
      unique() -> ctries_temp
    ctries <- ctries_temp$iso3c
    names(ctries) <- ctries_temp$country
    ctries <- ctries[order(ctries_temp$country)]
    if (isolate(ctry_selected()[1]) == "all") ctry_selected(ctries)
    else ctry_selected(isolate(ctry_selected()[ctry_selected() %in% ctries]))
    return(ctries)
  })

  observeEvent(input$highlight, {
    ctry_sel <- isolate(ctry_selected())
    if (length(input$highlight) != length(ctry_sel) ||
        !all(input$highlight %in% ctry_sel))
      ctry_selected(input$highlight)
  })

  output$SelectCountriesHL <- renderUI(
    pickerInput(
      inputId = "highlight",
      label = "Which countries do you want to highlight?",
      choices = ctry_list(),
      selected = isolate(ctry_selected()),
      options = list(
        `actions-box` = TRUE,
        size = 10,
        `selected-text-format` = "count > 3"
      ),
      multiple = TRUE
    )
  )

  output$Covid19Plot <- renderPlot({
    req(input$highlight)
    plot_covid19_spread(
      type = input$type,
      min_cases = input$min_cases,
      min_by_ctry_obs = input$min_by_ctry_obs,
      per_capita = input$per_capita,
      edate_cutoff = input$edate_cutoff,
      intervention = if(input$intervention != "none") input$intervention else NULL,
      highlight = input$highlight
    )
  })

  output$Covid19PlotHover <- renderUI({
    req(input$plot_hover)
    hover <- input$plot_hover
    df <- dyn_data()
    if (!is.null(hover)) {
      hover$mapping$x <- "edate"
      hover$mapping$y <- input$type
    }
    point <- nearPoints(df, hover, threshold = 25, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)

    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", hover$coords_img$x + 2, "px; top:", hover$coords_img$y + 2, "px;")
    # actual tooltip created as wellPanel
    wellPanel(
      style = style, class = "well-sm",
      HTML(paste0(point$country),
           sprintf("<br> %s, %s: ", as.character(point$date), input$type),
           format(point[, input$type] %>% pull(), big.mark = ","))
    )
  })
}

shinyApp(ui = ui, server = server)
