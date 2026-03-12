library(dplyr)
library(readxl)

resolve_data_path <- function() {
  candidates <- c(
    file.path("..", "data", "raw", "tourism_four_part_analysis_ready.xlsx"),
    file.path("data", "raw", "tourism_four_part_analysis_ready.xlsx")
  )

  path <- candidates[file.exists(candidates)][1]

  if (is.na(path) || !nzchar(path)) {
    stop("Dataset not found. Expected data/raw/tourism_four_part_analysis_ready.xlsx")
  }

  path
}

load_tourism_data <- function(path = resolve_data_path()) {
  read_excel(path, sheet = "analysis_ready_all") |>
    mutate(
      date = as.Date(date),
      period = factor(period, levels = c("pre_covid", "covid_shock", "recovery"))
    )
}

prepare_cluster_data <- function(df, periods, features, scale_features) {
  filtered <- df |>
    filter(period %in% periods) |>
    select(date, period, all_of(features))

  complete <- filtered |> filter(if_all(all_of(features), ~ !is.na(.x)))

  matrix <- as.matrix(complete[, features, drop = FALSE])

  if (scale_features) {
    matrix <- scale(matrix)
  }

  list(
    metadata = complete,
    matrix = matrix
  )
}
