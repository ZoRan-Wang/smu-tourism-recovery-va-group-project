library(shiny)
library(bslib)

mod_cluster_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      checkboxGroupInput(
        ns("period_filter"),
        "Period Filter",
        choices = c("pre_covid", "covid_shock", "recovery"),
        selected = c("pre_covid", "covid_shock", "recovery")
      ),
      checkboxGroupInput(
        ns("feature_cols"),
        "Features",
        choices = c(
          "visitor_arrivals",
          "china_share",
          "hotel_occ",
          "avg_stay_monthly_capped"
        ),
        selected = c(
          "visitor_arrivals",
          "china_share",
          "hotel_occ",
          "avg_stay_monthly_capped"
        )
      ),
      sliderInput(ns("k_value"), "Number of Clusters (k)", min = 2, max = 6, value = 3),
      numericInput(ns("random_seed"), "Random Seed", value = 42, min = 1, step = 1),
      checkboxInput(ns("scale_features"), "Scale features (z-score)", value = TRUE),
      actionButton(ns("run_cluster"), "Run Clustering", class = "btn-primary")
    ),
    layout_column_wrap(
      width = 1 / 2,
      fill = FALSE,
      card(
        card_header("Silhouette Score"),
        card_body(textOutput(ns("silhouette_value")))
      ),
      card(
        card_header("Cluster Scatter"),
        card_body(plotOutput(ns("cluster_plot"), height = "340px"))
      ),
      card(
        card_header("Cluster Profile"),
        card_body(DT::DTOutput(ns("profile_table")))
      ),
      card(
        card_header("State Timeline"),
        card_body(plotOutput(ns("timeline_plot"), height = "340px"))
      ),
      card(
        card_header("Assignment Preview"),
        card_body(
          div(
            class = "d-flex justify-content-end mb-3",
            downloadButton(ns("download_clusters"), "Download Assignments")
          ),
          DT::DTOutput(ns("assignment_table"))
        )
      )
    )
  )
}
