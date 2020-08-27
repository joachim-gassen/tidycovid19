library(shiny)
library(shinyjs)
library(dplyr)
library(shinyWidgets)
library(tidycovid19)
library(rclipboard)
library(lubridate)
library(zoo)

load("shiny_data.Rda")

po <- shiny_plot_options

if(as.numeric(
  difftime(now(tzone = "Europe/Berlin"),
           po$data$timestamp[1]), units = "hours") > 24)
  po$data <- download_merged_data(cached = TRUE, silent = TRUE)

ui <- fluidPage(
  rclipboardSetup(),
  useShinyjs(),
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
    "It has been inspired by the displays created by John Burn-Murdoch",
    "for the Financial Times. Read this",
    tags$a(href = "https://joachim-gassen.github.io/2020/04/covid19-explore-your-visualier-dof/", "blog post"),
    "about using the visualizations and hover over the plot with your mouse to",
    "get more information about the respective data point."
  ),
  hr(),

  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "type", "Which statistics do you want to display?",
        c("Confirmed Cases" = "confirmed", "Reported deaths" = "deaths",
          "Recovered Cases" = "recovered", "Active cases" = "active"),
        po$type
      ),
      sliderInput(
        "min_cases",
        "Number of cases to mark day zero on X-axis",
        min = 0, max = max(20000, po$min_cases), step = 50, value = po$min_cases
      ),
      sliderInput(
        "min_by_ctry_obs",
        "Required number of days to include country",
        min = 0, max = max(30, po$min_by_ctry_obs), value = po$min_by_ctry_obs
      ),
      sliderInput(
        "edate_cutoff",
        "How many days do you want to display?",
        min = 10, max = max(270, po$edate_cutoff), value = po$edate_cutoff
      ),
      hr(),
      checkboxInput(
        "cumulative",
        "Deselect for daily changes instead of cumulative values",
        po$cumulative
      ),
      sliderInput(
        "change_ave",
        "For daily changes, days to average over",
        min = 1, max = max(po$change_ave, 14), value = po$change_ave
      ),
      hr(),
      checkboxInput(
        "per_capita",
        "Display relative to population (per capita)",
        po$per_capita
      ),
      checkboxInput(
        "per_capita_x_axis",
        "Construct the X axis relative to population (per capita)",
        po$per_capita_x_axis,
      ),
      helpText(
        "When checking the above, consider adjusting number of cases to",
        "reflect that cases are now counted by 100,000 inhabitants."
      ),
      sliderInput(
        "population_cutoff",
        paste("Limit sample to countries with at least .. million inhabitants",
              "(useful for per capita displays)"),
        min = 0, max = max(100, po$population_cutoff), value = po$population_cutoff
      ),
      hr(),
      checkboxInput(
        "log_scale",
        "Use logarithmic scaling for Y-axis",
        po$log_scale
      ),
      selectInput(
        "intervention",
        "Select intervention type to highlight by points",
        c("None" = "none", "General lockdowns" = "lockdown",
          "Social distancing"= 'soc_dist', "Movement restrictions" = 'mov_rest',
          "Public health measures" = 'pub_health',
          "Social and economic measures" = 'soc_econ'),
        ifelse(!is.null(po$intervention), po$intervention, "none")
      ),
      uiOutput("SelectCountriesHL"),
      checkboxInput(
        "exclude_others",
        "Check to exclude unhighlighted countries",
        po$exclude_others
      ),
      hr(),
      uiOutput("SendCodeToClipboard")
    ),
    mainPanel(div(
      style = "position:relative",
      plotOutput(
        "Covid19Plot", width = "100%", height = "600px",
        hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"))
    ),
    uiOutput("Covid19PlotHover"),
    hr(),
    HTML("<p><center>Based on the <a href=https://joachim-gassen.github.io/tidycovid19>",
         "{tidycovid19} R Package</a>, <a href=https://twitter.com/JoachimGassen>",
         "Joachim Gassen</a>,<br>",
         "<a href=https://www.wiwi.hu-berlin.de/rewe>",
         "Humboldt-Universität zu Berlin</a> and",
         "<a href=https://www.accounting-for-transparency.de>",
         "TRR 266 Accounting for Transparency</a>, 2020</center></p>")
    ))
)

server <- function(session, input, output) {

  dyn_data <- reactive({
    data <- po$data %>%
      filter(population > 1e6*input$population_cutoff) %>%
      mutate(active = confirmed - recovered)

    if (!input$cumulative)
      data <- data %>%
        group_by(iso3c) %>%
        mutate(
          delta = !! sym(input$type) - dplyr::lag(!! sym(input$type)),
          change = rollmean(delta, input$change_ave, na.pad=TRUE, align="right")
        ) %>%
        ungroup()

    if(input$per_capita_x_axis) {
      data <- data %>%
        group_by(iso3c) %>%
        filter((!! sym(input$type)*1e5)/population >= input$min_cases)
    } else {
      data <- data %>%
        group_by(iso3c) %>%
        filter(!! sym(input$type) >= input$min_cases)
    }

    data %>%
      mutate(edate = as.numeric(date - min(date))) %>%
      filter(!is.na(edate)) %>%
      group_by(iso3c) %>%
      filter(n() >= input$min_by_ctry_obs) %>%
      ungroup() %>%
      filter(edate <= input$edate_cutoff) -> df_temp

    if (!input$cumulative) df_temp <- df_temp %>%
      mutate(!! input$type := change) %>%
      filter(!is.na(!! sym(input$type)))

    if (input$per_capita) df_temp <- df_temp %>%
      mutate(!! input$type := 1e5*(!! sym(input$type))/population) %>%
      filter(!is.na(!! sym(input$type)))

    return(df_temp)
  })

  ctry_selected <- reactiveVal(
    if(!is.null(po$highlight)) {
      po$data %>%
          select(country, iso3c) %>%
          unique() %>%
          filter(iso3c %in% po$highlight) -> ctries_temp
        ctries <- ctries_temp$iso3c
        names(ctries) <- ctries_temp$country
        ctries <- ctries[order(ctries_temp$country)]
        ctries
    } else "all"
  )

  ctry_list <- reactive({
    dyn_data() %>%
      select(iso3c, country) %>%
      unique() -> ctries_temp
    ctries <- ctries_temp$iso3c
    names(ctries) <- ctries_temp$country
    ctries <- ctries[order(ctries_temp$country)]
    if (isolate(ctry_selected()[1]) == "all") ctry_selected(ctries)
    else {
      still_selectable_ctries <- isolate(ctry_selected()[ctry_selected() %in% ctries])
      if (length(still_selectable_ctries) == 0) still_selectable_ctries <- ""
      ctry_selected(still_selectable_ctries)
    }
    return(ctries)
  })

  plot_code <- reactive({
     paste(
      sep = "\n",
      '# Code generated by shiny_covid19_spread() of the {tidycovid19} package',
      '# See: https://github.com/joachim-gassen/tidycovid19',
      '# Run in R/Rstudio. See https://www.r-project.org and https://www.rstudio.com',
      '# Uncomment the following to install the {tidycovid19} package',
      '',
      '# remotes::install_github("joachim-gassen/tidycovid19)',
      '',
      'library(tidycovid19)',
      '',
      'plot_covid19_spread(',
      sprintf('  type = "%s", min_cases = %d, min_by_ctry_obs = %d,',
              input$type, input$min_cases, input$min_by_ctry_obs),
      sprintf('  per_capita = %s, per_capita_x_axis = %s, population_cutoff = %d, ',
              as.character(input$per_capita),
              as.character({if (input$per_capita) input$per_capita_x_axis else FALSE}),
              input$population_cutoff),
      sprintf('  log_scale = %s, cumulative = %s, change_ave = %d,',
              as.character(input$log_scale), as.character(input$cumulative),
              input$change_ave),
      sprintf('  edate_cutoff = %d', input$edate_cutoff),
      paste0(strwrap(sprintf('  highlight = %s,',
              paste0(capture.output(dput(unname(input$highlight))), collapse = "")), 66),
              collapse = "\n  "),
      sprintf('  exclude_others = %s, intervention = %s',
              as.character(input$log_scale), ifelse(input$intervention == "none", "NULL",
                     paste0('"', input$intervention, '"'))),
      ')'
    )
  })

  observeEvent(input$cumulative, {
    if (input$cumulative) shinyjs::disable("change_ave")
    else shinyjs::enable("change_ave")
  })

  observeEvent(input$per_capita, {
    if (input$per_capita) shinyjs::enable("per_capita_x_axis")
    else {
      updateCheckboxInput(session, "per_capita_x_axis", value = FALSE)
      shinyjs::disable("per_capita_x_axis")
    }
  })
  
  observeEvent(input$per_capita_x_axis, {
    if (input$per_capita_x_axis) {
      updateSliderInput(session, "min_cases", max = 100, step = 1, value = 5)
    } else {
      updateSliderInput(
        session, "min_cases", 
        max = max(20000, po$min_cases), step = 50, value = po$min_cases
      )
    }
  })

  observeEvent(input$highlight, ignoreNULL = FALSE, ignoreInit = TRUE, {
    ctry_sel <- isolate(ctry_selected())
    if (length(input$highlight) != length(ctry_sel) ||
        !all(input$highlight %in% ctry_sel)) {
      if (!is.null(input$highlight)) {
        ctry_selected(input$highlight)
      } else ctry_selected("")
    }
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

  output$SendCodeToClipboard <- renderUI({
    rclipButton("clipbtn", "Send Code for Plot to Clipboard",
                plot_code(), icon("clipboard"))
  })

  output$Covid19Plot <- renderPlot({
    req(input$min_by_ctry_obs)
    plot_covid19_spread(
      data = po$data,
      type = input$type,
      min_cases = input$min_cases,
      min_by_ctry_obs = input$min_by_ctry_obs,
      per_capita = input$per_capita,
      per_capita_x_axis = {if (input$per_capita) input$per_capita_x_axis else FALSE},
      population_cutoff = input$population_cutoff,
      log_scale = input$log_scale,
      cumulative = input$cumulative,
      change_ave = input$change_ave,
      edate_cutoff = input$edate_cutoff,
      intervention = if(input$intervention != "none") input$intervention else NULL,
      highlight = ctry_selected(),
      exclude_others = input$exclude_others
    )
  })

  output$Covid19PlotHover <- renderUI({
    req(input$plot_hover)
    hover <- input$plot_hover
    df <- dyn_data()
    if (input$exclude_others && isolate(ctry_selected()[1]) != "all" )
      df <- df %>% filter(iso3c %in% isolate(ctry_selected()))

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
                    "left:", hover$coords_css$x + 2, "px; top:", hover$coords_css$y + 2, "px;")
    # actual tooltip created as wellPanel
    panel_text <- sprintf(
      "%s, %s:<br>%s", point$country, as.character(point$date),
      format(point[, input$type] %>% pull(), digits = 3, big.mark = ",")
    )
    if (!input$cumulative) panel_text <- paste(panel_text, "daily change in<br>")
    panel_text <- paste(panel_text, case_when(
        input$type == "confirmed" ~ "confirmed cases",
        input$type == "deaths" ~ "reported deaths",
        input$type == "recovered" ~ "recoveries",
        input$type == "active" ~ "active cases"
    ))
    if (input$per_capita)
      panel_text <- paste(panel_text, "<br>per 100,000 inhabitants")
    wellPanel(style = style, class = "well-sm", HTML(panel_text))
  })
}

shinyApp(ui = ui, server = server)
