library(shiny)
library(dplyr)
library(ggplot2)
library(cluster)
library(plotly)

cluster_palette <- function(cluster_values) {
  levels <- unique(as.character(cluster_values))
  palette_values <- c(
    "#0f766e", "#2cb5a7", "#b6e3dd", "#d2b07a",
    "#8e6438", "#274346", "#d87a59", "#7f99c2"
  )
  palette_values <- rep(palette_values, length.out = length(levels))
  stats::setNames(palette_values, levels)
}

cluster_reference_value <- function(normalization) {
  switch(
    normalization,
    indexed = 100,
    zscore = 0,
    raw = NA_real_,
    NA_real_
  )
}

axis_limits_with_padding <- function(x, center = NULL, prop = 0.16) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  span <- diff(rng)

  if (!is.finite(span) || span == 0) {
    anchor <- if (length(rng) == 0 || !is.finite(rng[1])) 0 else rng[1]
    span <- max(abs(anchor), 1) * 0.35
    rng <- c(anchor - span / 2, anchor + span / 2)
  }

  limits <- c(rng[1] - span * prop, rng[2] + span * prop)

  if (!is.null(center) && is.finite(center)) {
    limits[1] <- min(limits[1], center - span * 0.18)
    limits[2] <- max(limits[2], center + span * 0.18)
  }

  limits
}

format_cluster_value <- function(x, normalization) {
  if (identical(normalization, "raw")) {
    out <- format(round(x, 0), big.mark = ",", trim = TRUE)
  } else {
    out <- format(round(x, 2), nsmall = 2, trim = TRUE)
  }

  out[!is.finite(x)] <- "NA"
  out
}

build_pattern_tooltip <- function(df, normalization) {
  value_label <- switch(
    normalization,
    indexed = "Indexed level",
    zscore = "Z-score",
    raw = "Arrivals",
    "Value"
  )

  paste0(
    "<b>", df$series_name, "</b><br>",
    "Pattern: ", df$cluster_label, "<br>",
    "Month: ", format(df$date, "%Y-%m"), "<br>",
    value_label, ": ", format_cluster_value(df$value, normalization)
  )
}

build_recovery_tooltip <- function(df, normalization) {
  x_label <- switch(
    normalization,
    indexed = "Lowest indexed level",
    zscore = "Lowest z-score",
    raw = "Lowest monthly arrivals",
    "Lowest level"
  )
  y_label <- switch(
    normalization,
    indexed = "Final indexed level",
    zscore = "Final z-score",
    raw = "Final monthly arrivals",
    "Final level"
  )

  paste0(
    "<b>", df$series_name, "</b><br>",
    "Pattern: ", df$cluster_label, "<br>",
    x_label, ": ", format_cluster_value(df$trough_index, normalization), "<br>",
    y_label, ": ", format_cluster_value(df$end_index, normalization), "<br>",
    "Rebound multiple: ", format(round(df$rebound_multiple, 3), nsmall = 3), "<br>",
    "Volatility: ", format(round(df$volatility, 3), nsmall = 3)
  )
}

cluster_pattern_plot_object <- function(res, focus_series = NULL) {
  palette <- cluster_palette(res$summary$cluster_label)
  normalization <- res$panel$normalization
  ref_value <- cluster_reference_value(normalization)
  n_clusters <- length(unique(as.character(res$plot_data$cluster_view)))
  facet_cols <- if (n_clusters <= 2) {
    1
  } else if (n_clusters <= 4) {
    2
  } else {
    3
  }

  y_axis_label <- switch(
    normalization,
    indexed = "Indexed level (base = 100)",
    zscore = "Standardized level (z-score)",
    raw = "Monthly arrivals",
    "Value"
  )

  series_df <- res$plot_data |>
    filter(type == "Series")
  series_df$text <- build_pattern_tooltip(series_df, normalization)

  mean_df <- res$plot_data |>
    filter(type == "Cluster mean")
  mean_df$text <- paste0(
    "<b>", mean_df$cluster_label, " mean</b><br>",
    "Month: ", format(mean_df$date, "%Y-%m"), "<br>",
    y_axis_label, ": ", format_cluster_value(mean_df$value, normalization)
  )

  focus_df <- series_df |>
    filter(series == focus_series)

  other_df <- series_df |>
    filter(series != focus_series)

  plot <- ggplot() +
    geom_line(
      data = other_df,
      aes(x = date, y = value, group = series, text = text),
      color = "#bcc8ca",
      alpha = 0.34,
      linewidth = 0.48
    ) +
    geom_line(
      data = mean_df,
      aes(x = date, y = value, group = cluster_view, color = cluster_label, text = text),
      linewidth = 1.75
    ) +
    facet_wrap(~cluster_view, ncol = facet_cols, scales = "free_y") +
    labs(
      x = "Month",
      y = y_axis_label,
      color = "Pattern"
    ) +
    scale_color_manual(values = palette) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      panel.spacing = grid::unit(1.1, "lines"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    )

  if (nrow(focus_df) > 0) {
    plot <- plot +
      geom_line(
        data = focus_df,
        aes(x = date, y = value, group = series, text = text),
        color = "#d86f45",
        linewidth = 1.1,
        alpha = 0.98
      )
  }

  if (is.finite(ref_value)) {
    plot <- plot +
      geom_hline(yintercept = ref_value, linetype = "dashed", color = "#ccb79a", linewidth = 0.45)
  }

  if (identical(normalization, "raw")) {
    plot <- plot +
      scale_y_continuous(labels = scales::label_number(big.mark = ",", accuracy = 1))
  }

  suppressWarnings(
    plotly::ggplotly(plot, tooltip = "text") |>
      plotly::layout(
        hovermode = "closest",
        margin = list(l = 70, r = 20, t = 20, b = 70),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) |>
      plotly::config(displayModeBar = FALSE)
  )
}

recovery_position_plot_object <- function(res, focus_series = NULL) {
  df <- res$series_features
  df$text <- build_recovery_tooltip(df, res$panel$normalization)

  normalization <- res$panel$normalization
  ref_value <- cluster_reference_value(normalization)
  palette <- cluster_palette(df$cluster_label)

  x_axis_label <- switch(
    normalization,
    indexed = "Lowest indexed level in the selected window",
    zscore = "Lowest z-score in the selected window",
    raw = "Lowest monthly arrival level",
    "Lowest level"
  )
  y_axis_label <- switch(
    normalization,
    indexed = "Final indexed level in the selected window",
    zscore = "Final z-score in the selected window",
    raw = "Final monthly arrival level",
    "Final level"
  )

  highlight_series <- unique(c("china", focus_series, res$summary$representative_series))
  label_df <- df |>
    filter(series %in% highlight_series) |>
    distinct(series, .keep_all = TRUE)

  base_df <- df |>
    filter(!series %in% unique(c("china", focus_series)))

  focus_df <- df |>
    filter(series == focus_series)

  china_df <- df |>
    filter(series == "china")

  plot <- ggplot() +
    geom_point(
      data = base_df,
      aes(x = trough_index, y = end_index, color = cluster_label, text = text),
      size = 3.9,
      alpha = 0.92
    ) +
    labs(
      x = x_axis_label,
      y = y_axis_label,
      color = "Pattern"
    ) +
    scale_color_manual(values = palette) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    )

  if (nrow(focus_df) > 0) {
    plot <- plot +
      geom_point(
        data = focus_df,
        aes(x = trough_index, y = end_index, text = text),
        color = "#d86f45",
        size = 5.2,
        alpha = 1
      )
  }

  if (nrow(china_df) > 0) {
    plot <- plot +
      geom_point(
        data = china_df,
        aes(x = trough_index, y = end_index, text = text),
        color = "#0f5f62",
        size = 5.8,
        alpha = 1
      )
  }

  if (nrow(label_df) > 0) {
    plot <- plot +
      geom_text(
        data = label_df,
        aes(x = trough_index, y = end_index, label = series_name),
        vjust = -1.05,
        size = 3.6,
        color = "#435256",
        check_overlap = TRUE
      )
  }

  if (identical(normalization, "raw")) {
    plot <- plot +
      scale_x_continuous(
        trans = scales::pseudo_log_trans(base = 10),
        labels = scales::label_number(big.mark = ",", accuracy = 1)
      ) +
      scale_y_continuous(
        trans = scales::pseudo_log_trans(base = 10),
        labels = scales::label_number(big.mark = ",", accuracy = 1)
      )
  } else if (identical(normalization, "indexed")) {
    x_limits <- axis_limits_with_padding(df$trough_index, center = NULL)
    y_limits <- axis_limits_with_padding(df$end_index, center = ref_value)

    plot <- plot +
      coord_cartesian(xlim = x_limits, ylim = y_limits, clip = "off")

    if (is.finite(ref_value)) {
      plot <- plot +
        geom_hline(yintercept = ref_value, linetype = "dashed", color = "#ccb79a", linewidth = 0.45)
    }
  } else {
    x_limits <- axis_limits_with_padding(df$trough_index, center = ref_value)
    y_limits <- axis_limits_with_padding(df$end_index, center = ref_value)

    plot <- plot +
      coord_cartesian(xlim = x_limits, ylim = y_limits, clip = "off")

    if (is.finite(ref_value)) {
      plot <- plot +
        geom_hline(yintercept = ref_value, linetype = "dashed", color = "#ccb79a", linewidth = 0.45) +
        geom_vline(xintercept = ref_value, linetype = "dashed", color = "#ccb79a", linewidth = 0.45)
    }
  }

  suppressWarnings(
    plotly::ggplotly(plot, tooltip = "text") |>
      plotly::layout(
        hovermode = "closest",
        margin = list(l = 70, r = 20, t = 20, b = 70),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) |>
      plotly::config(displayModeBar = FALSE)
  )
}

mod_cluster_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$series_subset, {
      selected_series <- input$series_subset
      if (is.null(selected_series) || length(selected_series) == 0) {
        return()
      }

      choices <- stats::setNames(selected_series, display_names_for_series(selected_series))
      current_focus <- isolate(input$focus_series)
      default_focus <- if ("china" %in% selected_series) "china" else selected_series[[1]]
      next_focus <- if (!is.null(current_focus) && current_focus %in% selected_series) current_focus else default_focus

      updateSelectInput(
        session,
        "focus_series",
        choices = choices,
        selected = next_focus
      )
    }, ignoreNULL = FALSE)

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
      recommended_row <- res$diagnostics |>
        arrange(desc(mean_silhouette), k) |>
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
          tags$div(class = "va-stat-label", "Recommended k"),
          tags$div(class = "va-stat-value", recommended_row$k)
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

    output$cluster_chip_bar <- renderUI({
      res <- cluster_results()
      palette <- cluster_palette(res$summary$cluster_label)
      ordered_summary <- res$summary |>
        arrange(desc(avg_end_index))

      tags$div(
        class = "cluster-chip-grid",
        lapply(seq_len(nrow(ordered_summary)), function(i) {
          row <- ordered_summary[i, ]
          tags$div(
            class = "cluster-summary-chip",
            style = sprintf("border-color:%s33;", palette[[row$cluster_label]]),
            tags$span(
              class = "cluster-summary-dot",
              style = sprintf("background:%s;", palette[[row$cluster_label]])
            ),
            tags$div(
              class = "cluster-summary-copy",
              tags$div(class = "cluster-summary-title", row$cluster_label),
              tags$div(
                class = "cluster-summary-meta",
                sprintf(
                  "%s series | rep. %s | mean end %.1f",
                  row$n_series,
                  row$representative_series_name,
                  row$avg_end_index
                )
              )
            )
          )
        })
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
        options = list(pageLength = 6, dom = "tip", ordering = FALSE, autoWidth = TRUE)
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

    output$cluster_pattern_plot <- plotly::renderPlotly({
      res <- cluster_results()
      cluster_pattern_plot_object(res, focus_series = input$focus_series)
    })

    output$recovery_position_plot <- plotly::renderPlotly({
      res <- cluster_results()
      recovery_position_plot_object(res, focus_series = input$focus_series)
    })

    output$insight_panel <- renderUI({
      res <- cluster_results()
      strongest <- res$summary |>
        arrange(desc(avg_end_index)) |>
        slice(1)
      recommended_row <- res$diagnostics |>
        arrange(desc(mean_silhouette), k) |>
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
            "%s currently leads the rebound picture. Its representative market is %s and its average ending level is %.1f.",
            strongest$cluster_label,
            strongest$representative_series_name,
            strongest$avg_end_index
          )
        ),
        tags$p(
          class = "va-insight-secondary",
          sprintf(
            "The best silhouette in the current country set appears at k = %s. You are currently reading k = %s.",
            recommended_row$k,
            input$k_value
          )
        )
      )
    })

    output$focus_series_panel <- renderUI({
      res <- cluster_results()
      focus_series <- input$focus_series
      focus_row <- res$membership |>
        filter(series == focus_series) |>
        slice(1)

      validate(need(nrow(focus_row) == 1, "Select a focus series to see a closer reading."))

      tags$div(
        class = "cluster-context-grid",
        tags$div(
          class = "cluster-context-card",
          tags$div(class = "cluster-context-label", "Focus series"),
          tags$div(class = "cluster-context-value", focus_row$series_name),
          tags$p(
            class = "cluster-context-copy",
            sprintf("%s sits in %s and ends the selected window at %.1f.", focus_row$series_name, focus_row$cluster_label, focus_row$end_index)
          )
        ),
        tags$div(
          class = "cluster-context-card",
          tags$div(class = "cluster-context-label", "Shock and rebound"),
          tags$div(
            class = "cluster-context-metrics",
            tags$span(sprintf("Trough %.1f", focus_row$trough_index)),
            tags$span(sprintf("Rebound %.3f", focus_row$rebound_multiple))
          ),
          tags$p(
            class = "cluster-context-copy",
            "Use the hover state on the chart to compare the focus path against the cluster mean month by month."
          )
        )
      )
    })

    output$china_context_panel <- renderUI({
      res <- cluster_results()
      if (is.null(res$china_context) || nrow(res$china_context) == 0) {
        return(tags$p("China is not included in the current country selection."))
      }

      china_row <- res$china_context |>
        filter(series == "china") |>
        slice(1)
      peer_rows <- res$china_context |>
        filter(series != "china") |>
        slice_head(n = 4)

      tags$div(
        class = "cluster-context-grid",
        tags$div(
          class = "cluster-context-card cluster-context-card-wide",
          tags$div(class = "cluster-context-label", "China summary"),
          tags$div(class = "cluster-context-value", china_row$cluster_label),
          tags$p(class = "cluster-context-copy", res$china_note)
        ),
        tags$div(
          class = "cluster-context-card",
          tags$div(class = "cluster-context-label", "Closest peers"),
          if (nrow(peer_rows) == 0) {
            tags$p(class = "cluster-context-copy", "No peer series are available in the current subset.")
          } else {
            tags$ul(
              class = "cluster-context-list",
              lapply(seq_len(nrow(peer_rows)), function(i) {
                tags$li(
                  sprintf(
                    "%s (distance %.3f)",
                    peer_rows$peer_name[i],
                    peer_rows$distance_to_china[i]
                  )
                )
              })
            )
          }
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
