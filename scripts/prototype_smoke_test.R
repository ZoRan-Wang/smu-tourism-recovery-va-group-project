library(readxl)
library(dplyr)
library(ggplot2)
library(cluster)

data_candidates <- c(
  file.path("data", "raw", "tourism_four_part_analysis_ready.xlsx"),
  file.path("..", "data", "raw", "tourism_four_part_analysis_ready.xlsx")
)

data_path <- data_candidates[file.exists(data_candidates)][1]
if (is.na(data_path)) {
  stop("Dataset not found. Expected data/raw/tourism_four_part_analysis_ready.xlsx")
}

df <- read_excel(data_path, sheet = "analysis_ready_all") |>
  mutate(date = as.Date(date))

features <- c(
  "visitor_arrivals",
  "china_share",
  "hotel_occ",
  "avg_stay_monthly_capped"
)

clean <- df |>
  select(date, period, all_of(features)) |>
  filter(if_all(all_of(features), ~ !is.na(.x)))

mat <- scale(as.matrix(clean[, features, drop = FALSE]))

candidate_k <- 2:7
wss <- sapply(candidate_k, function(k) {
  kmeans(mat, centers = k, nstart = 20)$tot.withinss
})

elbow_df <- data.frame(k = candidate_k, tot_withinss = wss)

dir.create(file.path("artifacts", "plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("artifacts", "tables"), recursive = TRUE, showWarnings = FALSE)

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line(color = "#0f6b6f", linewidth = 1.1) +
  geom_point(color = "#d86f45", size = 2.4) +
  scale_x_continuous(breaks = candidate_k) +
  labs(
    title = "Elbow Plot for Candidate K",
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Sum of Squares"
  ) +
  theme_minimal(base_size = 13)
ggsave(file.path("artifacts", "plots", "elbow_plot.png"), width = 8, height = 4.5, dpi = 160)

set.seed(42)
km <- kmeans(mat, centers = 3, nstart = 25)
sil <- silhouette(km$cluster, dist(mat))
sil_mean <- mean(sil[, "sil_width"], na.rm = TRUE)

pca <- prcomp(mat)
scatter_df <- data.frame(
  PC1 = pca$x[, 1],
  PC2 = pca$x[, 2],
  period = clean$period,
  cluster = factor(paste0("State ", km$cluster))
)

ggplot(scatter_df, aes(PC1, PC2, color = cluster, shape = period)) +
  geom_point(size = 2.8, alpha = 0.85) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Cluster State Scatter (k = 3)",
    subtitle = paste("Mean silhouette:", round(sil_mean, 3)),
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal(base_size = 13)
ggsave(file.path("artifacts", "plots", "cluster_scatter.png"), width = 8, height = 4.5, dpi = 160)

profile <- bind_cols(
  tibble(cluster = factor(paste0("State ", km$cluster))),
  as.data.frame(clean[, features, drop = FALSE])
) |>
  group_by(cluster) |>
  summarise(across(everything(), mean), .groups = "drop")

write.csv(profile, file.path("artifacts", "tables", "cluster_profile.csv"), row.names = FALSE)

summary_lines <- c(
  paste("timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste("rows_used:", nrow(clean)),
  paste("k_selected:", 3),
  paste("silhouette_mean:", round(sil_mean, 4))
)
writeLines(summary_lines, file.path("artifacts", "tables", "smoke_test_summary.txt"))

message("Smoke test complete.")
