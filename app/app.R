library(shiny)
library(bslib)

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
source(file.path(project_root, "app", "R", "mod_eda_ui.R"))
source(file.path(project_root, "app", "R", "mod_eda_server.R"))
source(file.path(project_root, "app", "R", "mod_cluster_ui.R"))
source(file.path(project_root, "app", "R", "mod_cluster_server.R"))
source(file.path(project_root, "app", "R", "mod_forecast_ui.R"))
source(file.path(project_root, "app", "R", "mod_forecast_server.R"))

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
  nav_panel("Time Series Visual Analysis", mod_eda_ui("eda_module")),
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
            p("This Shiny app now combines the latest EDA, clustering, and forecasting work on one tourism time-series backbone."),
            tags$ul(
              tags$li("Visual analysis: inspect source-market concentration and the pre-COVID versus recovery leaders."),
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

  mod_eda_server("eda_module", data = shared_data)
  mod_cluster_server("cluster_module", data = cluster_data)
  mod_forecast_server("forecast_module", data = shared_data)
}

shinyApp(ui, server)
