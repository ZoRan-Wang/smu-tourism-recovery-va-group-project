library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (file.exists(file.path(current, "_quarto.yml"))) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Project root not found. Expected to locate _quarto.yml.")
    }

    current <- parent
  }
}

project_root <- find_project_root()

source(file.path(project_root, "app", "R", "data_utils.R"))
source(file.path(project_root, "app", "R", "mod_cluster_ui.R"))
source(file.path(project_root, "app", "R", "mod_cluster_server.R"))
source(file.path(project_root, "app", "R", "mod_forecast_ui.R"))
source(file.path(project_root, "app", "R", "mod_forecast_server.R"))

series_explorer_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      selectInput(ns("series_label"), "Target series", choices = NULL),
      sliderInput(ns("lookback_years"), "Years to display", min = 3, max = 10, value = 8),
      checkboxInput(ns("show_points"), "Show monthly points", value = TRUE)
    ),
    layout_column_wrap(
      width = 1 / 2,
      fill = FALSE,
      card(
        class = "va-card",
        card_header("Time Series View"),
        card_body(plotOutput(ns("series_plot"), height = "360px"))
      ),
      card(
        class = "va-card",
        card_header("Series Metadata"),
        card_body(DT::DTOutput(ns("series_meta")))
      ),
      card(
        class = "va-card",
        card_header("How to Read This"),
        card_body(
          p("Start here to compare country-level arrival series and supporting tourism indicators before moving into clustering or forecasting."),
          p("This tab is intentionally lightweight: it helps users confirm the series scope, unit, and direction of change before they switch into the deeper analytical modules.")
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
  title = "Singapore Tourism Time-Series VA Prototype",
  theme = bs_theme(
    bg = "#f4f2eb",
    fg = "#1f2a2e",
    primary = "#0f6b6f",
    secondary = "#d86f45",
    base_font = font_google("Space Grotesk"),
    heading_font = font_google("Space Grotesk")
  ),
  nav_panel("Time Series Visual Analysis", series_explorer_ui("series_module")),
  nav_panel("Time Series Clustering", mod_cluster_ui("cluster_module")),
  nav_panel("Forecasting", mod_forecast_ui("forecast_module")),
  nav_panel(
    "About",
    div(
      class = "va-preview",
      div(class = "va-kicker", "Prototype scope"),
      h2("About this integrated build"),
      layout_columns(
        card(
          class = "va-card va-preview-card",
          card_header("Current focus"),
          card_body(
            p("This Shiny app now combines the latest clustering, forecasting, and series-explorer work on one tourism time-series backbone."),
            tags$ul(
              tags$li("Visual analysis: inspect monthly series and metadata before deeper analysis."),
              tags$li("Clustering: compare country trajectories with Wang's chart-first priority-market workflow."),
              tags$li("Forecasting: compare baseline and model-based forecasts with context indicators.")
            )
          )
        ),
        card(
          class = "va-card va-preview-card",
          card_header("Shared data contract"),
          card_body(
            tags$ul(
              tags$li("Country-arrivals backbone: visitor_arrivals_full_dataset.xlsx"),
              tags$li("Processed clustering artifacts under data/processed/"),
              tags$li("Optional tourism context workbook: tourism_update.xlsx")
            )
          )
        ),
        col_widths = c(6, 6)
      )
    )
  )
)

ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app-theme.css")
  ),
  ui
)

server <- function(input, output, session) {
  shared_data <- reactive({
    load_tourism_data()
  })

  cluster_data <- reactive({
    load_clustering_country_wide()
  })

  series_explorer_server("series_module", data = shared_data)
  mod_cluster_server("cluster_module", data = cluster_data)
  mod_forecast_server("forecast_module", data = shared_data)
}

shinyApp(ui, server)
