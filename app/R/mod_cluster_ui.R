library(shiny)
library(bslib)

mod_cluster_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      selectizeInput(
        ns("series_filter"),
        "Country Arrival Series",
        choices = NULL,
        multiple = TRUE
      ),
      sliderInput(ns("lookback_years"), "Years to Cluster", min = 3, max = 10, value = 8),
      sliderInput(ns("k_value"), "Number of Clusters (k)", min = 2, max = 6, value = 3),
      numericInput(ns("random_seed"), "Random Seed", value = 42, min = 1, step = 1),
      checkboxInput(ns("scale_series"), "Normalize each country trajectory", value = TRUE),
      actionButton(ns("run_cluster"), "Run Time-Series Clustering", class = "btn-primary")
    ),
    layout_column_wrap(
      width = 1 / 2,
      fill = FALSE,
      card(
        card_header("Silhouette Score"),
        card_body(textOutput(ns("silhouette_value")))
      ),
      card(
        card_header("Country Trajectory Scatter"),
        card_body(plotOutput(ns("cluster_plot"), height = "340px"))
      ),
      card(
        card_header("Cluster Profile"),
        card_body(DT::DTOutput(ns("profile_table")))
      ),
      card(
        card_header("Country Assignment"),
        card_body(DT::DTOutput(ns("assignment_table")))
      )
    )
  )
}
