library(shiny)
library(bslib)

mod_cluster_ui <- function(id) {
  ns <- NS(id)
  series_choices <- available_clustering_series()
  default_choices <- default_clustering_series(n = min(8, length(series_choices)))

  layout_sidebar(
    sidebar = sidebar(
      width = 340,
      selectizeInput(
        ns("series_subset"),
        "Series subset",
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
      sliderInput(ns("k_value"), "Number of clusters", min = 2, max = 8, value = 3, step = 1),
      actionButton(ns("run_cluster"), "Run clustering", class = "btn-primary")
    ),
    layout_column_wrap(
      width = 1 / 2,
      fill = FALSE,
      card(
        card_header("Cluster Quality"),
        card_body(
          textOutput(ns("silhouette_value")),
          textOutput(ns("cluster_window_note"))
        )
      ),
      card(
        card_header("Cluster Diagnostics"),
        card_body(DT::DTOutput(ns("diagnostics_table")))
      ),
      card(
        card_header("Representative Patterns"),
        card_body(plotOutput(ns("cluster_pattern_plot"), height = "420px"))
      ),
      card(
        card_header("Recovery Position Map"),
        card_body(plotOutput(ns("recovery_position_plot"), height = "360px"))
      ),
      card(
        card_header("Cluster Insights"),
        card_body(
          p(class = "mb-2", textOutput(ns("cluster_narrative"))),
          p(class = "mb-0", textOutput(ns("china_narrative")))
        )
      ),
      card(
        card_header("Membership Table"),
        card_body(
          div(
            class = "d-flex justify-content-end mb-3",
            downloadButton(ns("download_clusters"), "Download assignments")
          ),
          DT::DTOutput(ns("membership_table"))
        )
      ),
      card(
        card_header("Cluster Summary"),
        card_body(DT::DTOutput(ns("cluster_summary_table")))
      ),
      card(
        card_header("Recovery Metrics By Series"),
        card_body(DT::DTOutput(ns("recovery_metrics_table")))
      )
    )
  )
}
