library(shiny)
library(dplyr)
library(ggplot2)
library(cluster)

cluster_palette <- function(cluster_values) {
  levels <- unique(as.character(cluster_values))
  stats::setNames(
    grDevices::hcl.colors(length(levels), palette = "Green-Brown"),
    levels
  )
}

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
      solution <- summarize_cluster_solution(
        panel,
        cluster_id,
        d,
        china_series = "china",
        series_labels = clustering_display_lookup()
      )

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

    output$quality_panel <- renderUI({
      res <- cluster_results()
      selected_row <- res$diagnostics |>
        filter(k == input$k_value) |>
        slice(1)

      tags$div(
        class = "va-stat-grid",
        tags$div(
          class = "va-stat",
          tags$div(class = "va-stat-label", "Mean silhouette"),
          tags$div(class = "va-stat-value", sprintf("%.3f", res$silhouette))
        ),
        tags$div(
          class = "va-stat",
          tags$div(class = "va-stat-label", "Selected k"),
          tags$div(class = "va-stat-value", input$k_value)
        ),
        tags$div(
          class = "va-stat",
          tags$div(class = "va-stat-label", "Series in view"),
          tags$div(class = "va-stat-value", length(res$panel$series))
        ),
        tags$div(
          class = "va-stat va-stat-note",
          tags$div(
            class = "va-stat-note-text",
            sprintf(
              "Window %s to %s | %s normalization | selected-k silhouette %.3f",
              format(min(res$panel$dates), "%Y-%m"),
              format(max(res$panel$dates), "%Y-%m"),
              tools::toTitleCase(res$panel$normalization),
              selected_row$mean_silhouette
            )
          )
        )
      )
    })

    output$diagnostics_table <- DT::renderDT({
      res <- cluster_results()
      diagnostics <- res$diagnostics |>
        mutate(
          choice = ifelse(k == input$k_value, "Selected", ""),
          mean_silhouette = sprintf("%.3f", mean_silhouette)
        ) |>
        rename(
          `Number of clusters` = k,
          `Mean silhouette` = mean_silhouette,
          Status = choice
        )

      DT::datatable(
        diagnostics,
        rownames = FALSE,
        options = list(pageLength = 8, dom = "tip", ordering = FALSE, autoWidth = TRUE)
      )
    })

    output$membership_table <- DT::renderDT({
      res <- cluster_results()
      membership <- res$membership |>
        transmute(
          Series = series_name,
          Cluster = cluster,
          Pattern = cluster_label,
          Silhouette = sprintf("%.3f", silhouette),
          `End index` = end_index,
          `Trough index` = trough_index
        )

      DT::datatable(
        membership,
        rownames = FALSE,
        options = list(pageLength = 8, scrollX = FALSE, autoWidth = TRUE)
      )
    })

    output$cluster_summary_table <- DT::renderDT({
      res <- cluster_results()
      summary_tbl <- res$summary |>
        transmute(
          Cluster = cluster,
          Pattern = cluster_label,
          `Series count` = n_series,
          `Representative series` = representative_series_name,
          `Mean silhouette` = sprintf("%.3f", mean_silhouette),
          `Average end index` = avg_end_index,
          `Average trough index` = avg_trough_index,
          `Average rebound multiple` = avg_rebound_multiple,
          Members = members
        )

      DT::datatable(
        summary_tbl,
        rownames = FALSE,
        options = list(pageLength = 6, dom = "tip", autoWidth = TRUE)
      )
    })

    output$recovery_metrics_table <- DT::renderDT({
      res <- cluster_results()
      metrics_tbl <- res$series_features |>
        transmute(
          Series = series_name,
          Cluster = cluster,
          Pattern = cluster_label,
          `End index` = end_index,
          `Trough index` = trough_index,
          `Rebound multiple` = rebound_multiple,
          Volatility = volatility
        )

      DT::datatable(
        metrics_tbl,
        rownames = FALSE,
        options = list(pageLength = 8, scrollX = FALSE, autoWidth = TRUE)
      )
    })

    output$cluster_pattern_plot <- renderPlot({
      res <- cluster_results()
      palette <- cluster_palette(res$plot_data$cluster_view)
      y_axis_label <- switch(
        res$panel$normalization,
        indexed = "Indexed level (base = 100)",
        zscore = "Standardized level (z-score)",
        raw = "Monthly arrivals",
        "Value"
      )

      ggplot(
        res$plot_data,
        aes(x = date, y = value, group = interaction(series, type), color = cluster_view)
      ) +
        geom_line(
          data = subset(res$plot_data, type == "Series"),
          alpha = 0.18,
          linewidth = 0.5
        ) +
        geom_line(
          data = subset(res$plot_data, type == "Cluster mean"),
          linewidth = 1.4
        ) +
        facet_wrap(~cluster_view, ncol = 1, scales = "free_y") +
        labs(
          title = "Representative Time-Series Patterns by Cluster",
          subtitle = paste(
            "Selected country series clustered by trajectory similarity using",
            res$panel$normalization,
            "normalization"
          ),
          x = "Month",
          y = y_axis_label,
          color = "Cluster"
        ) +
        scale_color_manual(values = palette) +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "none",
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold")
        )
    })

    output$recovery_position_plot <- renderPlot({
      res <- cluster_results()
      palette <- cluster_palette(res$series_features$cluster_label)
      x_axis_label <- if (identical(res$panel$normalization, "raw")) {
        "Lowest monthly arrival level in the selected window"
      } else {
        "Lowest normalized level in the selected window"
      }
      y_axis_label <- if (identical(res$panel$normalization, "raw")) {
        "Final monthly arrival level in the selected window"
      } else {
        "Final normalized level in the selected window"
      }

      base_plot <- ggplot(
        res$series_features,
        aes(x = trough_index, y = end_index, color = cluster_label, label = series_name)
      )

      if (!identical(res$panel$normalization, "raw")) {
        base_plot <- base_plot +
          geom_hline(yintercept = 100, linetype = "dashed", color = "#c9b79d", linewidth = 0.4) +
          geom_vline(xintercept = 100, linetype = "dashed", color = "#c9b79d", linewidth = 0.4)
      }

      base_plot +
        geom_point(size = 3.4, alpha = 0.95) +
        geom_text(vjust = -0.8, size = 3.2, show.legend = FALSE) +
        labs(
          title = "Recovery Position Map",
          subtitle = "Lower trough values indicate deeper shocks; higher end values indicate stronger rebound",
          x = x_axis_label,
          y = y_axis_label,
          color = "Cluster pattern"
        ) +
        scale_color_manual(values = palette) +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank())
    })

    output$insight_panel <- renderUI({
      res <- cluster_results()
      strongest <- res$summary |>
        arrange(desc(avg_end_index)) |>
        slice(1)

      tags$div(
        class = "va-insight-stack",
        tags$div(
          class = "va-insight-pill",
          paste("Lead pattern:", strongest$cluster_label)
        ),
        tags$p(
          class = "va-insight-main",
          sprintf(
            "%s is the strongest rebound group in the current selection. It ends around %.1f on the normalized scale and is represented by %s.",
            strongest$cluster_label,
            strongest$avg_end_index,
            strongest$representative_series_name
          )
        ),
        tags$p(
          class = "va-insight-secondary",
          res$china_note
        )
      )
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
