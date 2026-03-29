library(shiny)
library(dplyr)
library(ggplot2)
library(cluster)

mod_cluster_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    cluster_results <- eventReactive(input$run_cluster, {
      validate(
        need(length(input$series_subset) >= 2, "Select at least two series."),
        need(length(input$year_window) == 2, "Select a valid year window.")
      )

      panel <- prepare_country_clustering_data(
        data(),
        selected_series = input$series_subset,
        year_window = input$year_window,
        normalization = input$normalization_mode
      )

      mat <- panel$matrix
      d <- stats::dist(mat)

      validate(
        need(nrow(mat) > input$k_value, "Not enough series for the selected k."),
        need(nrow(mat) >= 3, "Need at least three series to form meaningful clusters.")
      )

      hc <- stats::hclust(d, method = "ward.D2")
      cluster_id <- stats::cutree(hc, k = input$k_value)
      solution <- summarize_cluster_solution(panel, cluster_id, d, china_series = "china")

      list(
        panel = panel,
        silhouette = solution$silhouette,
        membership = solution$membership,
        diagnostics = solution$diagnostics,
        summary = solution$summary,
        plot_data = solution$plot_data,
        series_features = solution$series_features,
        china_context = solution$china_context,
        china_note = solution$china_note,
        hc = hc
      )
    }, ignoreNULL = FALSE)

    output$silhouette_value <- renderText({
      res <- cluster_results()
      sprintf("Mean silhouette score: %.3f", res$silhouette)
    })

    output$cluster_window_note <- renderText({
      res <- cluster_results()
      sprintf(
        "Window: %s to %s | Normalization: %s | Series: %d",
        format(min(res$panel$dates), "%Y-%m"),
        format(max(res$panel$dates), "%Y-%m"),
        res$panel$normalization,
        length(res$panel$series)
      )
    })

    output$diagnostics_table <- DT::renderDT({
      res <- cluster_results()
      DT::datatable(
        res$diagnostics,
        rownames = FALSE,
        options = list(pageLength = 5, dom = "t")
      )
    })

    output$membership_table <- DT::renderDT({
      res <- cluster_results()
      DT::datatable(
        res$membership,
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    output$cluster_summary_table <- DT::renderDT({
      res <- cluster_results()
      DT::datatable(
        res$summary,
        rownames = FALSE,
        options = list(pageLength = 5, dom = "t")
      )
    })

    output$recovery_metrics_table <- DT::renderDT({
      res <- cluster_results()
      DT::datatable(
        res$series_features,
        rownames = FALSE,
        options = list(pageLength = 8, scrollX = TRUE)
      )
    })

    output$cluster_pattern_plot <- renderPlot({
      res <- cluster_results()

      ggplot(res$plot_data, aes(x = date, y = value, group = interaction(series, type), color = cluster)) +
        geom_line(
          data = subset(res$plot_data, type == "Series"),
          alpha = 0.15,
          linewidth = 0.4
        ) +
        geom_line(
          data = subset(res$plot_data, type == "Cluster mean"),
          linewidth = 1.3
        ) +
        facet_wrap(~cluster, ncol = 1, scales = "free_y") +
        labs(
          title = "Representative Time-Series Patterns by Cluster",
          subtitle = paste(
            "Selected country series clustered by trajectory similarity using",
            res$panel$normalization,
            "normalization"
          ),
          x = "Month",
          y = "Normalized value",
          color = "Cluster"
        ) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "none")
    })

    output$recovery_position_plot <- renderPlot({
      res <- cluster_results()

      ggplot(
        res$series_features,
        aes(x = trough_index, y = end_index, color = cluster, label = series)
      ) +
        geom_point(size = 3.2, alpha = 0.9) +
        geom_text(vjust = -0.8, size = 3.3, show.legend = FALSE) +
        labs(
          title = "Recovery Position Map",
          subtitle = "Lower trough values indicate deeper shocks; higher end values indicate stronger rebound",
          x = "Lowest normalized level during the selected window",
          y = "Final normalized level in the selected window",
          color = "Cluster"
        ) +
        theme_minimal(base_size = 13)
    })

    output$cluster_narrative <- renderText({
      res <- cluster_results()
      strongest <- res$summary |>
        arrange(desc(avg_end_index)) |>
        slice(1)

      sprintf(
        "%s is the strongest rebound group in the current selection. It ends around %.1f on the normalized scale and is represented by %s.",
        strongest$cluster_label,
        strongest$avg_end_index,
        strongest$representative_series
      )
    })

    output$china_narrative <- renderText({
      res <- cluster_results()
      res$china_note
    })

    output$download_clusters <- downloadHandler(
      filename = function() {
        sprintf("country-series-cluster-assignments-%s.csv", Sys.Date())
      },
      content = function(file) {
        res <- cluster_results()
        utils::write.csv(res$membership, file, row.names = FALSE)
      }
    )
  })
}
