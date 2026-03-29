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
source(file.path(project_root, "app", "R", "mod_cluster_ui.R"))
source(file.path(project_root, "app", "R", "mod_cluster_server.R"))

ui <- page_navbar(
  title = "Singapore Tourism Time-Series VA Prototype",
  theme = bs_theme(
    bg = "#f4f2eb",
    fg = "#1f2a2e",
    primary = "#0f6b6f",
    secondary = "#d86f45",
    base_font = font_google("Space Grotesk"),
    heading_font = font_google("Fraunces")
  ),
  nav_panel(
    "Visual Analysis",
    layout_column_wrap(
      width = 1,
      card(
        card_header("Planned Module"),
        card_body(
          p("This module will become the comparative time-series visual analysis space."),
          tags$ul(
            tags$li("Country-level arrivals comparison."),
            tags$li("Transport-mode composition."),
            tags$li("Indexed and share-based line charts.")
          )
        )
      )
    )
  ),
  nav_panel("Time Series Clustering", mod_cluster_ui("cluster_module")),
  nav_panel(
    "Forecasting",
    layout_column_wrap(
      width = 1,
      card(
        card_header("Planned Module"),
        card_body(
          p("This module will become the forecasting space for selected tourism series."),
          tags$ul(
            tags$li("Train-test split for monthly arrivals."),
            tags$li("Baseline and forecast comparison."),
            tags$li("Forecast accuracy tables and residual checks.")
          )
        )
      )
    )
  ),
  nav_panel(
    "About",
    layout_column_wrap(
      width = 1,
      card(
        card_header("Prototype Scope"),
        card_body(
          p("This Shiny app is being refactored into a three-module time-series visual analytics prototype."),
          tags$ul(
            tags$li("Data: visitor_arrivals_full_dataset.xlsx"),
            tags$li("Clustering unit: country or market series across time."),
            tags$li("Controls: series subset, year window, normalization, cluster count."),
            tags$li("Insights: recovery position map, cluster narratives, and China placement summary.")
          )
        )
      ),
      card(
        card_header("Run Notes"),
        card_body(
          tags$ol(
            tags$li("Keep this app running on port 3838."),
            tags$li("Open the Quarto site in parallel for write-up updates."),
            tags$li("Use the clustering tab to validate the time-series workflow first.")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    load_clustering_country_wide()
  })

  mod_cluster_server("cluster_module", data = data)
}

shinyApp(ui, server)
