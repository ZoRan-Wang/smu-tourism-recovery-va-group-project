if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman", repos = "https://cloud.r-project.org")
}

pacman::p_load(
  tidyverse,
  rpart,
  rpart.plot,
  caret
)

find_project_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (file.exists(file.path(current, "_quarto.yml"))) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Project root not found. Expected to locate _quarto.yml.")
    }

    current <- parent
  }
}

project_root <- find_project_root()
module_dir <- file.path(project_root, "team", "jin-qinhao", "Take-Home-Exercise2")
data_path <- file.path(module_dir, "data", "tourism_decision_tree_ready.csv")
output_dir <- file.path(module_dir, "outputs")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

df <- readr::read_csv(data_path, show_col_types = FALSE) %>%
  mutate(
    date = as.Date(date),
    period = factor(period, levels = c("pre_covid", "covid_shock", "recovery")),
    month = factor(month),
    quarter = factor(quarter),
    dataset_split = factor(dataset_split, levels = c("train", "test")),
    hotel_occ_level_tertile = factor(hotel_occ_level_tertile, levels = c("low", "medium", "high")),
    hotel_occ_level_business = factor(hotel_occ_level_business, levels = c("low", "medium", "high"))
  )

train_df <- df %>% filter(dataset_split == "train")
test_df <- df %>% filter(dataset_split == "test")

tree_formula <- hotel_occ_level_tertile ~ visitor_arrivals + china_share + avg_stay_monthly_capped + month

fit_tree <- rpart::rpart(
  formula = tree_formula,
  data = train_df,
  method = "class",
  parms = list(split = "gini"),
  control = rpart::rpart.control(
    cp = 0.005,
    maxdepth = 4,
    minsplit = 10,
    minbucket = 5,
    xval = 10
  )
)

printcp(fit_tree)

bestcp <- fit_tree$cptable[which.min(fit_tree$cptable[, "xerror"]), "CP"]
pruned_tree <- prune(fit_tree, cp = bestcp)

rpart.plot::rpart.plot(
  pruned_tree,
  type = 2,
  extra = 104,
  under = TRUE,
  fallen.leaves = TRUE,
  faclen = 0,
  tweak = 1.1,
  main = "Decision Tree for Hotel Occupancy Level"
)

png(
  filename = file.path(output_dir, "decision_tree_plot.png"),
  width = 1800,
  height = 1200,
  res = 180
)
rpart.plot::rpart.plot(
  pruned_tree,
  type = 2,
  extra = 104,
  under = TRUE,
  fallen.leaves = TRUE,
  faclen = 0,
  tweak = 1.1,
  main = "Decision Tree for Hotel Occupancy Level"
)
dev.off()

pred_class <- predict(pruned_tree, newdata = test_df, type = "class")
pred_prob <- predict(pruned_tree, newdata = test_df, type = "prob") %>%
  as.data.frame()

cm <- caret::confusionMatrix(
  data = pred_class,
  reference = test_df$hotel_occ_level_tertile
)

print(cm)

metrics_tbl <- tibble(
  metric = c("accuracy", "kappa"),
  value = c(cm$overall[["Accuracy"]], cm$overall[["Kappa"]])
)

importance_tbl <- tibble(
  variable = names(pruned_tree$variable.importance),
  importance = as.numeric(pruned_tree$variable.importance)
) %>%
  arrange(desc(importance))

print(metrics_tbl)
print(importance_tbl)

write.csv(as.data.frame(cm$table),
          file.path(output_dir, "decision_tree_confusion_matrix.csv"),
          row.names = FALSE)

write.csv(metrics_tbl,
          file.path(output_dir, "decision_tree_metrics.csv"),
          row.names = FALSE)

write.csv(importance_tbl,
          file.path(output_dir, "decision_tree_variable_importance.csv"),
          row.names = FALSE)

prediction_tbl <- bind_cols(
  test_df %>% select(date, hotel_occ, hotel_occ_level_tertile),
  tibble(predicted_class = pred_class),
  pred_prob
)

write.csv(prediction_tbl,
          file.path(output_dir, "decision_tree_test_predictions.csv"),
          row.names = FALSE)

cat("Decision tree analysis complete.\n")
cat("Outputs saved to:", output_dir, "\n")
