library(shiny)
library(bslib)

mod_cluster_ui <- function(id) {
  ns <- NS(id)
  series_choices <- available_clustering_series()
  default_choices <- default_clustering_series(n = min(8, length(series_choices)))

  tagList(
    div(
      class = "cluster-header",
      div(
        class = "cluster-header-copy",
        div(class = "cluster-kicker", "Module 2"),
        h2("Time Series Clustering"),
        p("Compare country recovery trajectories, check cluster quality, then read China’s placement in a cleaner step-by-step workspace.")
      ),
      div(
        class = "cluster-header-meta",
        span(class = "cluster-meta-chip", "Country series"),
        span(class = "cluster-meta-chip", "2017-2025"),
        span(class = "cluster-meta-chip", "Trajectory comparison")
      )
    ),
    div(
      class = "cluster-workspace",
      div(
        class = "cluster-controls",
        h3("Controls"),
        p(
          class = "cluster-controls-intro",
          "Choose a focused market set first. Then normalize, set the number of groups, and run the module."
        ),
        selectizeInput(
          ns("series_subset"),
          "Country series",
          choices = series_choices,
          selected = default_choices,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        ),
        sliderInput(
          ns("year_window"),
          "Year window",
          min = 2017,
          max = 2025,
          value = c(2017, 2025),
          step = 1,
          sep = ""
        ),
        radioButtons(
          ns("normalization_mode"),
          "Normalization mode",
          choices = c(
            "Indexed (base 100)" = "indexed",
            "Z-score" = "zscore",
            "Raw arrivals" = "raw"
          ),
          selected = "indexed",
          inline = FALSE
        ),
        sliderInput(
          ns("k_value"),
          "Number of clusters",
          min = 2,
          max = 8,
          value = 3,
          step = 1
        ),
        actionButton(ns("run_cluster"), "Run clustering", class = "btn-primary"),
        div(
          class = "cluster-controls-note",
          h4("Reading guide"),
          tags$ul(
            tags$li("Overview: check fit quality and China’s placement."),
            tags$li("Patterns: inspect each cluster trajectory without compression."),
            tags$li("Tables: review assignments and export results.")
          )
        )
      ),
      div(
        class = "cluster-main",
        navset_card_tab(
          id = ns("cluster_pages"),
          nav_panel(
            "Overview",
            div(
              class = "cluster-tab-stack",
              layout_columns(
                card(
                  class = "cluster-panel",
                  card_header("Cluster Quality"),
                  card_body(uiOutput(ns("quality_panel")))
                ),
                card(
                  class = "cluster-panel",
                  card_header("Cluster Insights"),
                  card_body(uiOutput(ns("insight_panel")))
                ),
                col_widths = c(5, 7)
              ),
              card(
                class = "cluster-panel",
                card_header("Recovery Position Map"),
                card_body(plotOutput(ns("recovery_position_plot"), height = "430px"))
              ),
              card(
                class = "cluster-panel",
                card_header("Cluster Diagnostics"),
                card_body(DT::DTOutput(ns("diagnostics_table")))
              )
            )
          ),
          nav_panel(
            "Patterns",
            card(
              class = "cluster-panel cluster-panel-plot",
              card_header("Representative Patterns"),
              card_body(plotOutput(ns("cluster_pattern_plot"), height = "680px"))
            )
          ),
          nav_panel(
            "Assignments",
            div(
              class = "cluster-tab-stack",
              card(
                class = "cluster-panel",
                card_header("Membership Table"),
                card_body(
                  div(
                    class = "cluster-toolbar",
                    downloadButton(ns("download_clusters"), "Download assignments")
                  ),
                  DT::DTOutput(ns("membership_table"))
                )
              ),
              card(
                class = "cluster-panel",
                card_header("Cluster Summary"),
                card_body(DT::DTOutput(ns("cluster_summary_table")))
              )
            )
          ),
          nav_panel(
            "Series Metrics",
            card(
              class = "cluster-panel",
              card_header("Recovery Metrics By Series"),
              card_body(DT::DTOutput(ns("recovery_metrics_table")))
            )
          )
        )
      )
    )
  )
}
