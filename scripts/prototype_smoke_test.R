library(dplyr)
library(ggplot2)
library(cluster)

source(file.path("app", "R", "data_utils.R"))

tourism_data <- load_tourism_data()

dir.create(file.path("artifacts", "plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("artifacts", "tables"), recursive = TRUE, showWarnings = FALSE)

# Time-series cluster smoke check
cluster_series <- c(
  "Visitor Arrivals: China",
  "Visitor Arrivals: Malaysia",
  "Visitor Arrivals: India",
  "Visitor Arrivals: Indonesia",
  "Visitor Arrivals: Australia",
  "Visitor Arrivals: Japan"
)

cluster_prep <- prepare_time_series_cluster_data(
  long_monthly = tourism_data$long_monthly,
  selected_series = cluster_series,
  lookback_years = 8,
  scale_series = TRUE
)

set.seed(42)
km <- kmeans(cluster_prep$matrix, centers = 3, nstart = 25)
sil <- silhouette(km$cluster, dist(cluster_prep$matrix))
sil_mean <- mean(sil[, "sil_width"], na.rm = TRUE)

cluster_profile <- cluster_prep$metadata |>
  mutate(cluster = factor(paste0("Cluster ", km$cluster))) |>
  select(cluster, mean_arrivals, volatility, latest_arrivals) |>
  group_by(cluster) |>
  summarise(across(everything(), mean), .groups = "drop")

write.csv(cluster_profile, file.path("artifacts", "tables", "cluster_profile.csv"), row.names = FALSE)

# Forecast smoke check
forecast_series <- prepare_forecast_series(
  tourism_data$long_monthly,
  "Visitor Arrivals: China"
)

forecast_results <- run_modeltime_forecast_workflow(
  series_df = forecast_series,
  horizon = 12
)

forecast_plot <- forecast_results$calibration_forecast_tbl |>
  modeltime::plot_modeltime_forecast(
    .interactive = FALSE,
    .legend_max_width = 25
  ) +
  labs(
    title = "Forecast Smoke Test",
    subtitle = "Seasonal Naive benchmark with ETS and ARIMA holdout comparison"
  )

ggsave(
  filename = file.path("artifacts", "plots", "forecast_smoke_plot.png"),
  plot = forecast_plot,
  width = 8,
  height = 4.5,
  dpi = 160
)

write.csv(forecast_results$accuracy_tbl, file.path("artifacts", "tables", "forecast_accuracy.csv"), row.names = FALSE)

summary_lines <- c(
  paste("timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste("cluster_series_count:", nrow(cluster_prep$matrix)),
  paste("cluster_window_months:", ncol(cluster_prep$matrix)),
  paste("forecast_series:", "Visitor Arrivals: China"),
  paste("cluster_silhouette_mean:", round(sil_mean, 4)),
  paste("forecast_models:", paste(forecast_results$accuracy_tbl$.model_desc, collapse = ", "))
)

writeLines(summary_lines, file.path("artifacts", "tables", "smoke_test_summary.txt"))

message("Smoke test complete.")
