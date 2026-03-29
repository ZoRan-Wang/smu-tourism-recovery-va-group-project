library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

source("R/data_utils.R")
source("R/mod_cluster_ui.R")
source("R/mod_cluster_server.R")
source("R/mod_forecast_ui.R")
source("R/mod_forecast_server.R")

series_explorer_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      selectInput(ns("series_label"), "Target Series", choices = NULL),
      sliderInput(ns("lookback_years"), "Years to Display", min = 3, max = 10, value = 8),
      checkboxInput(ns("show_points"), "Show monthly points", value = TRUE)
    ),
    layout_column_wrap(
      width = 1 / 2,
      fill = FALSE,
      card(
        card_header("Time Series View"),
        card_body(plotOutput(ns("series_plot"), height = "360px"))
      ),
      card(
        card_header("Series Metadata"),
        card_body(DT::DTOutput(ns("series_meta")))
      ),
      card(
        card_header("Interpretation Cue"),
        card_body(
          p("Use this panel to compare country-level arrivals and monthly tourism indicators before clustering or forecasting."),
          p("A shared dataset keeps the three modules consistent: explore the series, cluster the recovery patterns, then forecast the future path.")
        )
      )
    )
  )
}

series_explorer_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observe({
      choices <- list_forecast_series(data()$long_monthly)
      updateSelectInput(
        session,
        "series_label",
        choices = choices$label,
        selected = "Visitor Arrivals: China"
      )
    })

    selected_series <- reactive({
      req(input$series_label)

      data()$long_monthly |>
        filter(label == input$series_label) |>
        arrange(date) |>
        filter(date >= max(date) - years(input$lookback_years))
    })

    output$series_plot <- renderPlot({
      plot_df <- selected_series()

      p <- ggplot(plot_df, aes(x = date, y = value)) +
        geom_line(color = "#0f6b6f", linewidth = 1) +
        labs(
          title = input$series_label,
          subtitle = "Monthly tourism time series",
          x = NULL,
          y = unique(plot_df$unit)
        ) +
        scale_y_continuous(labels = scales::label_comma()) +
        theme_minimal(base_size = 13)

      if (isTRUE(input$show_points)) {
        p <- p + geom_point(color = "#d86f45", size = 1.8)
      }

      p
    })

    output$series_meta <- DT::renderDT({
      meta <- data()$metadata |>
        filter(label == input$series_label) |>
        select(label, frequency, unit, source, series_id)

      DT::datatable(meta, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
    })
  })
}

ui <- page_navbar(
  title = "Singapore Tourism Recovery Prototype",
  theme = bs_theme(
    bg = "#f4f2eb",
    fg = "#1f2a2e",
    primary = "#0f6b6f",
    secondary = "#d86f45",
    base_font = font_google("Space Grotesk"),
    heading_font = font_google("Fraunces")
  ),
  nav_panel("Time Series Explorer", series_explorer_ui("series_module")),
  nav_panel("Cluster Prototype", mod_cluster_ui("cluster_module")),
  nav_panel("Forecasting Prototype", mod_forecast_ui("forecast_module")),
  nav_panel(
    "About",
    layout_column_wrap(
      width = 1,
      card(
        card_header("Prototype Scope"),
        card_body(
          p("This app now follows the instructor-recommended three-module structure built on one shared tourism time-series dataset."),
          tags$ul(
            tags$li("Time Series Explorer: inspect monthly series by country or tourism indicator."),
            tags$li("Cluster Prototype: group monthly observations into recovery states."),
            tags$li("Forecasting Prototype: compare baseline and model-based forecasts.")
          )
        )
      ),
      card(
        card_header("Run Notes"),
        card_body(
          tags$ol(
            tags$li("Keep this app running on port 3838."),
            tags$li("Open Quarto pages in parallel for prototype and proposal review."),
            tags$li("Use the same target series naming across explorer and forecast tabs.")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    load_tourism_data()
  })

  series_explorer_server("series_module", data = data)
  mod_cluster_server("cluster_module", data = reactive(data()$monthly_features))
  mod_forecast_server("forecast_module", data = data)
}

shinyApp(ui, server)
