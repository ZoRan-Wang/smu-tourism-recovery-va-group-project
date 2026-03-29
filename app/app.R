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
    heading_font = font_google("Space Grotesk")
  ),
  nav_panel(
    "Visual Analysis",
    div(
      class = "va-preview",
      div(class = "va-kicker", "Module 1"),
      h2("Time Series Visual Analysis"),
      p("This panel will become the comparative exploration space for country and transport trajectories."),
      layout_columns(
        card(
          class = "va-card va-preview-card",
          card_header("Planned outputs"),
          card_body(
            tags$ul(
              tags$li("Country-level arrivals comparison."),
              tags$li("Transport-mode composition."),
              tags$li("Indexed and share-based line charts.")
            )
          )
        ),
        card(
          class = "va-card va-preview-card",
          card_header("Interaction goal"),
          card_body(
            p("Help users move from raw trends to relative market structure without leaving the app.")
          )
        ),
        col_widths = c(6, 6)
      )
    )
  ),
  nav_panel("Time Series Clustering", mod_cluster_ui("cluster_module")),
  nav_panel(
    "Forecasting",
    div(
      class = "va-preview",
      div(class = "va-kicker", "Module 3"),
      h2("Forecasting"),
      p("This panel will complete the app with a forward-looking view of selected tourism series."),
      layout_columns(
        card(
          class = "va-card va-preview-card",
          card_header("Planned outputs"),
          card_body(
            tags$ul(
              tags$li("Train-test split for monthly arrivals."),
              tags$li("Baseline and forecast comparison."),
              tags$li("Forecast accuracy tables and residual checks.")
            )
          )
        ),
        card(
          class = "va-card va-preview-card",
          card_header("Interaction goal"),
          card_body(
            p("Let users test simple versus richer models, then inspect forecast uncertainty and short-term directional change.")
          )
        ),
        col_widths = c(6, 6)
      )
    )
  ),
  nav_panel(
    "About",
    div(
      class = "va-preview",
      div(class = "va-kicker", "Prototype scope"),
      h2("About this refactor"),
      layout_columns(
        card(
          class = "va-card va-preview-card",
          card_header("Current focus"),
          card_body(
            p("This app is being reshaped into a three-module time-series visual analytics prototype."),
            tags$ul(
              tags$li("Data: visitor_arrivals_full_dataset.xlsx"),
              tags$li("Clustering unit: country or market series across time."),
              tags$li("Insights: recovery position map, cluster narratives, and China placement summary.")
            )
          )
        ),
        card(
          class = "va-card va-preview-card",
          card_header("Working rhythm"),
          card_body(
            tags$ol(
              tags$li("Validate the clustering module first."),
              tags$li("Keep the storyboard and proposal aligned with the app outputs."),
              tags$li("Use the remaining placeholders as integration targets for the other branches.")
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
  data <- reactive({
    load_clustering_country_wide()
  })

  mod_cluster_server("cluster_module", data = data)
}

shinyApp(ui, server)
