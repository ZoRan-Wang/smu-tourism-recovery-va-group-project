library(shiny)
library(bslib)

mod_cluster_ui <- function(id) {
  ns <- NS(id)
  series_choices <- available_clustering_series()
  default_choices <- default_clustering_series(n = min(8, length(series_choices)))
  focus_choices <- stats::setNames(default_choices, display_names_for_series(default_choices))
  default_focus <- if ("china" %in% default_choices) "china" else default_choices[[1]]

  tagList(
    div(
      class = "cluster-header",
      div(
        class = "cluster-header-copy",
        div(class = "cluster-kicker", "Module 2"),
        h2("Time Series Clustering"),
        p("Read the clustering result as a guided workspace: start with fit quality, inspect the dominant trajectory patterns, then focus on the selected priority market and the final assignments.")
      )
    ),
    div(
      class = "cluster-workspace",
      div(
        class = "cluster-controls",
        h3("Controls"),
        p(
          class = "cluster-controls-intro",
          "Keep the country set focused. Then choose the normalization, pick a working k, and use the focus selector to follow one series across the whole module."
        ),
        selectizeInput(
          ns("series_subset"),
          "Country series",
          choices = series_choices,
          selected = default_choices,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        ),
        selectInput(
          ns("focus_series"),
          "Focus market",
          choices = focus_choices,
          selected = default_focus
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
            tags$li("Start Here: confirm whether the clustering is stable and what the lead patterns are."),
            tags$li("Pattern Explorer: hover a line to inspect a country path, then compare it against the cluster mean."),
            tags$li("Focus Market in Context: use the position map after narrowing the market set."),
            tags$li("Assignments: export the final cluster labels after the picture is clear.")
          )
        )
      ),
      div(
        class = "cluster-main",
        navset_card_tab(
          id = ns("cluster_pages"),
          selected = "Dashboard",
          nav_panel(
            "Dashboard",
            div(
              class = "cluster-tab-stack",
              card(
                class = "cluster-panel",
                card_header("Read this clustering first"),
                card_body(
                  layout_columns(
                    uiOutput(ns("quality_panel")),
                    uiOutput(ns("insight_panel")),
                    col_widths = c(5, 7)
                  )
                )
              ),
              layout_columns(
                card(
                  class = "cluster-panel",
                  card_header("Pattern Atlas"),
                  card_body(
                    div(class = "cluster-helper-text", "Start here. Hover any line to inspect a country trajectory, compare it against the cluster mean, and use the focus selector to keep one market visible across the module."),
                    plotly::plotlyOutput(ns("dashboard_pattern_plot"), height = "780px")
                  )
                ),
                card(
                  class = "cluster-panel",
                  card_header("Cluster Profile"),
                  card_body(
                    div(class = "cluster-helper-text", "Keep this next to the atlas. It compresses each pattern into shock depth, ending level, and rebound multiple so the main recovery story stays readable."),
                    plotly::plotlyOutput(ns("cluster_profile_plot"), height = "520px")
                  )
                ),
                col_widths = c(8, 4)
              ),
              layout_columns(
                card(
                  class = "cluster-panel",
                  card_header("Recovery Position Map"),
                  card_body(
                    div(class = "cluster-helper-text", "Then compare each market's trough and ending level. Hover a point to inspect the market, its pattern, and the rebound multiple."),
                    plotly::plotlyOutput(ns("dashboard_recovery_plot"), height = "500px")
                  )
                ),
                card(
                  class = "cluster-panel",
                  card_header("Silhouette Scan"),
                  card_body(
                    div(class = "cluster-helper-text", "Use this to sanity-check the selected k. The chosen solution is highlighted against the full silhouette scan."),
                    plotly::plotlyOutput(ns("diagnostics_plot"), height = "500px")
                  )
                ),
                col_widths = c(7, 5)
              ),
              layout_columns(
                card(
                  class = "cluster-panel",
                  card_header("Focus Cluster Comparison"),
                  card_body(
                    div(class = "cluster-helper-text", "Finally, read the focus market against only the peers that share its cluster. This keeps the comparison clear instead of showing every country at once."),
                    plotly::plotlyOutput(ns("dashboard_focus_plot"), height = "420px")
                  )
                ),
                card(
                  class = "cluster-panel",
                  card_header("Priority Market Placement"),
                  card_body(uiOutput(ns("dashboard_china_context_panel")))
                ),
                col_widths = c(8, 4)
              )
            )
          ),
          nav_panel(
            "Pattern Explorer",
            div(
              class = "cluster-tab-stack",
              card(
                class = "cluster-panel cluster-panel-plot",
                card_header("Representative Patterns"),
                card_body(
                  div(class = "cluster-helper-text", "Hover a line to see the country, cluster, month, and value. The focus series is highlighted across its cluster panel."),
                  plotly::plotlyOutput(ns("pattern_explorer_plot"), height = "720px")
                )
              ),
              card(
                class = "cluster-panel",
                card_header("Pattern Summary"),
                card_body(uiOutput(ns("cluster_chip_bar")))
              )
            )
          ),
          nav_panel(
            "Focus Market in Context",
            div(
              class = "cluster-tab-stack",
              card(
                class = "cluster-panel",
                card_header("Recovery Position Map"),
                card_body(
                  div(class = "cluster-helper-text", "Hover a point to inspect the trough level, latest level, rebound multiple, and volatility. The selected focus market stays labelled."),
                  plotly::plotlyOutput(ns("china_recovery_plot"), height = "560px")
                )
              ),
              card(
                class = "cluster-panel",
                card_header("Focus Market Cluster Explorer"),
                card_body(
                  div(class = "cluster-helper-text", "Read the selected focus market against only the peers that share its cluster."),
                  plotly::plotlyOutput(ns("china_focus_plot"), height = "420px")
                )
              ),
              card(
                class = "cluster-panel",
                card_header("Focus Market Placement"),
                card_body(uiOutput(ns("china_context_panel")))
              )
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
              ),
              card(
                class = "cluster-panel",
                card_header("Series Metrics"),
                card_body(DT::DTOutput(ns("recovery_metrics_table")))
              )
            )
          )
        )
      )
    )
  )
}
