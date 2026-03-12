library(shiny)
library(dplyr)
library(ggplot2)
library(cluster)

mod_cluster_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    cluster_results <- eventReactive(input$run_cluster, {
      validate(
        need(length(input$period_filter) > 0, "Select at least one period."),
        need(length(input$feature_cols) >= 2, "Select at least two features.")
      )

      prep <- prepare_cluster_data(
        data(),
        periods = input$period_filter,
        features = input$feature_cols,
        scale_features = input$scale_features
      )

      mat <- prep$matrix
      meta <- prep$metadata

      validate(
        need(nrow(mat) > input$k_value, "Not enough rows for current k."),
        need(nrow(mat) > 2, "Filtered data is too small.")
      )

      set.seed(input$random_seed)
      km <- kmeans(mat, centers = input$k_value, nstart = 25)

      sil <- silhouette(km$cluster, dist(mat))
      sil_mean <- mean(sil[, "sil_width"], na.rm = TRUE)

      pca <- prcomp(mat)
      plot_df <- data.frame(
        date = meta$date,
        period = meta$period,
        cluster = factor(paste0("State ", km$cluster)),
        PC1 = pca$x[, 1],
        PC2 = pca$x[, 2]
      )

      profile <- bind_cols(
        tibble(cluster = factor(paste0("State ", km$cluster))),
        as.data.frame(meta[, input$feature_cols, drop = FALSE])
      ) |>
        group_by(cluster) |>
        summarise(across(everything(), mean), .groups = "drop")

      assignments <- meta |>
        mutate(cluster = paste0("State ", km$cluster)) |>
        select(date, period, cluster, all_of(input$feature_cols))

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

      ggplot(res$plot_df, aes(x = PC1, y = PC2, color = cluster, shape = period)) +
        geom_point(size = 2.8, alpha = 0.85) +
        scale_color_brewer(palette = "Dark2") +
        labs(
          title = "Cluster State Scatter (PCA projection)",
          x = "Principal Component 1",
          y = "Principal Component 2",
          color = "Cluster",
          shape = "Period"
        ) +
        theme_minimal(base_size = 13)
    })

    output$profile_table <- DT::renderDT({
      res <- cluster_results()
      DT::datatable(res$profile, options = list(pageLength = 6, scrollX = TRUE))
    })

    output$assignment_table <- DT::renderDT({
      res <- cluster_results()
      DT::datatable(res$assignments, options = list(pageLength = 8, scrollX = TRUE))
    })
  })
}
