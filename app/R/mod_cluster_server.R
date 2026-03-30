library(shiny)
library(dplyr)
library(ggplot2)
library(cluster)

mod_cluster_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observe({
      choices <- list_country_arrival_series(data()$long_monthly)$label
      default_selection <- intersect(
        c(
          "Visitor Arrivals: China",
          "Visitor Arrivals: Malaysia",
          "Visitor Arrivals: India",
          "Visitor Arrivals: Indonesia",
          "Visitor Arrivals: Australia",
          "Visitor Arrivals: Japan"
        ),
        choices
      )

      updateSelectizeInput(
        session,
        "series_filter",
        choices = choices,
        selected = default_selection,
        server = TRUE
      )
    })

    cluster_results <- eventReactive(input$run_cluster, {
      validate(
        need(length(input$series_filter) >= 2, "Select at least two country-arrival series.")
      )

      prep <- prepare_time_series_cluster_data(
        long_monthly = data()$long_monthly,
        selected_series = input$series_filter,
        lookback_years = input$lookback_years,
        scale_series = input$scale_series
      )

      mat <- prep$matrix
      meta <- prep$metadata

      validate(
        need(nrow(mat) > input$k_value, "Not enough country series for current k."),
        need(ncol(mat) >= 12, "Not enough overlapping months for time-series clustering.")
      )

      set.seed(input$random_seed)
      km <- kmeans(mat, centers = input$k_value, nstart = 25)

      sil <- silhouette(km$cluster, dist(mat))
      sil_mean <- mean(sil[, "sil_width"], na.rm = TRUE)

      pca <- prcomp(mat)
      plot_df <- meta |>
        mutate(
          cluster = factor(paste0("Cluster ", km$cluster)),
          PC1 = pca$x[, 1],
          PC2 = pca$x[, 2]
        )

      profile <- plot_df |>
        select(cluster, mean_arrivals, volatility, latest_arrivals) |>
        group_by(cluster) |>
        summarise(across(everything(), mean), .groups = "drop")

      assignments <- plot_df |>
        select(label, cluster, mean_arrivals, volatility, latest_arrivals, start_date, end_date)

      list(
        silhouette = sil_mean,
        plot_df = plot_df,
        profile = profile,
        assignments = assignments
      )
    }, ignoreNULL = FALSE)

    output$silhouette_value <- renderText({
      res <- cluster_results()
      sprintf("Mean silhouette score: %.3f", res$silhouette)
    })

    output$cluster_plot <- renderPlot({
      res <- cluster_results()

      ggplot(res$plot_df, aes(x = PC1, y = PC2, color = cluster, label = label)) +
        geom_point(size = 3.2, alpha = 0.9) +
        geom_text(vjust = -0.7, size = 3.3, show.legend = FALSE) +
        scale_color_brewer(palette = "Dark2") +
        labs(
          title = "Country Time-Series Clustering (PCA projection)",
          subtitle = "Each point represents one country-arrivals trajectory over the selected lookback window",
          x = "Principal Component 1",
          y = "Principal Component 2",
          color = "Cluster"
        ) +
        theme_minimal(base_size = 13)
    })

    output$profile_table <- DT::renderDT({
      res <- cluster_results()
      DT::datatable(res$profile, rownames = FALSE, options = list(pageLength = 6, scrollX = TRUE))
    })

    output$assignment_table <- DT::renderDT({
      res <- cluster_results()
      DT::datatable(res$assignments, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
    })
  })
}
