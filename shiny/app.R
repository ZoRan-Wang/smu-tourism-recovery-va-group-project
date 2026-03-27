library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(scales)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    y
  } else {
    x
  }
}

candidate_project_dirs <- function() {
  env_dir <- Sys.getenv("TOURISM_PROJECT_DIR", unset = "")

  unique(
    Filter(
      nzchar,
      c(
        env_dir,
        getwd(),
      file.path(getwd(), "team", "jin-qinhao", "Take-Home-Exercise2"),
      file.path(
        getwd(),
        "smu-tourism-recovery-va-group-project",
        "team",
        "jin-qinhao",
        "Take-Home-Exercise2"
      ),
      file.path(
        Sys.getenv("USERPROFILE"),
        "Documents",
        "Playground",
        "smu-tourism-recovery-va-group-project",
        "team",
        "jin-qinhao",
        "Take-Home-Exercise2"
      ),
      Sys.glob(
        file.path(
          Sys.getenv("USERPROFILE"),
          "Desktop",
          "*",
          "team",
          "jin-qinhao",
          "Take-Home-Exercise2"
        )
      )
      )
    )
  )
}

locate_project_dir <- function() {
  required_files <- c(
    file.path("outputs", "decision_tree_metrics.csv"),
    file.path("outputs", "random_forest_metrics.csv"),
    file.path("data", "tourism_decision_tree_ready.csv")
  )

  for (dir in candidate_project_dirs()) {
    if (!dir.exists(dir)) {
      next
    }

    if (all(file.exists(file.path(dir, required_files)))) {
      return(normalizePath(dir, winslash = "/", mustWork = TRUE))
    }
  }

  stop(
    paste(
      "Could not locate the Take-Home-Exercise2 project folder.",
      "Set `TOURISM_PROJECT_DIR` or update `candidate_project_dirs()` in app.R if your files live elsewhere."
    ),
    call. = FALSE
  )
}

read_csv_safe <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  readr::read_csv(path, show_col_types = FALSE)
}

metric_value <- function(metrics_tbl, metric_name) {
  if (is.null(metrics_tbl)) {
    return(NA_real_)
  }

  value <- metrics_tbl %>%
    filter(metric == metric_name) %>%
    pull(value)

  value[1] %||% NA_real_
}

format_metric <- function(x) {
  out <- scales::percent(x, accuracy = 0.1)
  out[is.na(x)] <- "N/A"
  out
}

has_rows <- function(x) {
  !is.null(x) && nrow(x) > 0
}

max_or_na <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }

  max(x, na.rm = TRUE)
}

format_decimal <- function(x, digits = 3) {
  out <- sprintf(paste0("%.", digits, "f"), x)
  out[is.na(x)] <- "N/A"
  out
}

format_gap_pp <- function(x) {
  if (is.na(x)) {
    return("N/A")
  }

  paste0(ifelse(x > 0, "+", ""), scales::number(100 * x, accuracy = 0.1), " pp")
}

table_note <- function(message) {
  datatable(
    tibble(Note = message),
    options = list(dom = "t", paging = FALSE, ordering = FALSE),
    rownames = FALSE
  )
}

occupancy_class_label <- function(x) {
  x <- as.character(x)

  case_when(
    x == "low" ~ "Low Occupancy",
    x == "medium" ~ "Medium Occupancy",
    x == "high" ~ "High Occupancy",
    TRUE ~ x
  )
}

occupancy_class_choices <- c(
  "All" = "All",
  "Low Occupancy" = "low",
  "Medium Occupancy" = "medium",
  "High Occupancy" = "high"
)

occupancy_band_ranges <- function(data_tbl) {
  required_cols <- c("hotel_occ_level_tertile", "hotel_occ")

  if (!has_rows(data_tbl) || !all(required_cols %in% names(data_tbl))) {
    return(tibble())
  }

  data_tbl %>%
    filter(!is.na(hotel_occ_level_tertile), !is.na(hotel_occ)) %>%
    group_by(hotel_occ_level_tertile) %>%
    summarise(
      min_occ = min(hotel_occ, na.rm = TRUE),
      max_occ = max(hotel_occ, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      class_label = occupancy_class_label(hotel_occ_level_tertile),
      fill_color = c(low = "#f1dacb", medium = "#d7dde2", high = "#d8ece8")[hotel_occ_level_tertile],
      band_text = paste0(
        class_label,
        " ",
        scales::number(min_occ, accuracy = 0.1),
        "%-",
        scales::number(max_occ, accuracy = 0.1),
        "%"
      )
    ) %>%
    arrange(match(hotel_occ_level_tertile, model_class_levels))
}

occupancy_band_summary <- function(data_tbl) {
  band_tbl <- occupancy_band_ranges(data_tbl)

  if (!has_rows(band_tbl)) {
    return("Occupancy classes are relative tercile bands, so a 'Low Occupancy' label can still appear at a fairly high numeric level.")
  }

  paste(
    "These occupancy classes are relative tercile bands:",
    paste(band_tbl$band_text, collapse = "; "),
    "That is why a 'Low Occupancy' month can still sit above 70%."
  )
}

display_feature_label <- function(x) {
  x <- as.character(x)

  case_when(
    x == "low" ~ "Low Occupancy",
    x == "medium" ~ "Medium Occupancy",
    x == "high" ~ "High Occupancy",
    x %in% c("visitor_arrivals", "arrivals_million") ~ "Visitor arrivals",
    x == "visitor_arrivals_china" ~ "China visitor arrivals",
    x %in% c("avg_stay_monthly", "avg_stay_monthly_capped", "stay_days") ~ "Average stay",
    x %in% c("china_share", "china_share_pct") ~ "China share",
    grepl("^month[A-Z]", x) ~ paste("Month", sub("^month", "", x)),
    x == "month" ~ "Month",
    x == "quarter" ~ "Quarter",
    x == "period" ~ "Recovery stage",
    TRUE ~ tools::toTitleCase(gsub("_", " ", x))
  )
}

concept_feature_label <- function(x) {
  x <- as.character(x)

  case_when(
    x == "low" ~ "Low Occupancy",
    x == "medium" ~ "Medium Occupancy",
    x == "high" ~ "High Occupancy",
    x %in% c("visitor_arrivals", "arrivals_million") ~ "Visitor arrivals",
    x == "visitor_arrivals_china" ~ "China visitor arrivals",
    x %in% c("avg_stay_monthly", "avg_stay_monthly_capped", "stay_days") ~ "Average stay",
    x %in% c("china_share", "china_share_pct") ~ "China share",
    grepl("^month", x) ~ "Month",
    x == "quarter" ~ "Quarter",
    x == "period" ~ "Recovery stage",
    TRUE ~ tools::toTitleCase(gsub("_", " ", x))
  )
}

prediction_confidence <- function(pred_tbl) {
  prob_cols <- intersect(c("low", "medium", "high"), names(pred_tbl))

  if (length(prob_cols) == 0 || !has_rows(pred_tbl)) {
    return(rep(NA_real_, if (is.null(pred_tbl)) 0 else nrow(pred_tbl)))
  }

  probs <- as.data.frame(pred_tbl[prob_cols])
  conf <- do.call(pmax, c(probs, list(na.rm = TRUE)))
  conf[!is.finite(conf)] <- NA_real_
  conf
}

prepare_prediction_rows <- function(pred_tbl) {
  if (!has_rows(pred_tbl)) {
    return(tibble())
  }

  pred_tbl %>%
    mutate(
      date = as.Date(date),
      actual = hotel_occ_level_tertile,
      correct = actual == predicted_class,
      confidence = prediction_confidence(pred_tbl)
    ) %>%
    arrange(desc(date))
}

filter_prediction_rows <- function(pred_tbl, result_filter = "all", class_filter = "All") {
  pred_tbl <- prepare_prediction_rows(pred_tbl)

  if (!has_rows(pred_tbl)) {
    return(pred_tbl)
  }

  if (!identical(class_filter, "All")) {
    pred_tbl <- pred_tbl %>% filter(actual == class_filter)
  }

  if (identical(result_filter, "misclassified")) {
    pred_tbl <- pred_tbl %>% filter(!correct)
  } else if (identical(result_filter, "correct")) {
    pred_tbl <- pred_tbl %>% filter(correct)
  }

  pred_tbl
}

class_recall_table <- function(conf_tbl, model_name) {
  if (!has_rows(conf_tbl)) {
    return(tibble(model = character(), class = character(), total = numeric(), correct = numeric(), recall = numeric()))
  }

  conf_tbl %>%
    mutate(
      Prediction = tolower(Prediction),
      Reference = tolower(Reference)
    ) %>%
    group_by(Reference) %>%
    summarise(
      total = sum(Freq),
      correct = sum(if_else(Prediction == Reference, Freq, 0)),
      .groups = "drop"
    ) %>%
    rename(class = Reference) %>%
    right_join(tibble(class = c("low", "medium", "high")), by = "class") %>%
    mutate(
      model = model_name,
      total = coalesce(total, 0),
      correct = coalesce(correct, 0),
      recall = if_else(total > 0, correct / total, NA_real_)
    ) %>%
    select(model, class, total, correct, recall)
}

metric_rows_for_model <- function(model_name, accuracy, kappa, recall_tbl) {
  bind_rows(
    tibble(
      Model = model_name,
      Metric = c("Holdout accuracy", "Kappa"),
      Display = c(format_metric(accuracy), format_decimal(kappa))
    ),
    recall_tbl %>%
      transmute(
        Model = model_name,
        Metric = paste(display_feature_label(class), "recall"),
        Display = format_metric(recall)
      )
  )
}

build_confusion_plot <- function(conf_tbl, title_text, high_color) {
  conf_tbl %>%
    mutate(
      Reference = factor(Reference, levels = c("low", "medium", "high"), labels = occupancy_class_label(c("low", "medium", "high"))),
      Prediction = factor(Prediction, levels = c("low", "medium", "high"), labels = occupancy_class_label(c("low", "medium", "high")))
    ) %>%
    ggplot(aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = Freq), size = 5, color = "#1f2a2e") +
    scale_fill_gradient(low = "#f8f3ea", high = high_color) +
    labs(
      title = title_text,
      x = "Actual class",
      y = "Predicted class",
      fill = "Count"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid = element_blank()
    )
}

build_importance_plot <- function(importance_tbl, value_col, title_text, top_n, fill_color) {
  importance_tbl %>%
    mutate(
      score = .data[[value_col]],
      display = display_feature_label(variable)
    ) %>%
    slice_max(order_by = score, n = top_n, with_ties = FALSE) %>%
    ggplot(aes(x = reorder(display, score), y = score)) +
    geom_col(fill = fill_color, width = 0.72) +
    coord_flip() +
    labs(
      title = title_text,
      x = NULL,
      y = "Importance"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

build_prediction_plot <- function(pred_tbl, title_text = "Actual vs Predicted Occupancy Level") {
  class_map <- c(low = 1, medium = 2, high = 3)

  plot_tbl <- pred_tbl %>%
    transmute(
      date = as.Date(date),
      actual = hotel_occ_level_tertile,
      predicted = predicted_class,
      actual_value = unname(class_map[actual]),
      predicted_value = unname(class_map[predicted]),
      agreement = if_else(actual == predicted, "Correct", "Mismatch")
    )

  correct_n <- sum(plot_tbl$agreement == "Correct", na.rm = TRUE)
  mismatch_n <- sum(plot_tbl$agreement == "Mismatch", na.rm = TRUE)
  subtitle_text <- paste(correct_n, "correct |", mismatch_n, "mismatch | shaded columns = mismatch months")

  plot_tbl %>%
    ggplot(aes(x = date)) +
    geom_rect(
      data = function(x) dplyr::filter(x, agreement == "Mismatch"),
      aes(xmin = date - 12, xmax = date + 12, ymin = 0.7, ymax = 3.3),
      inherit.aes = FALSE,
      fill = "#fff0e8",
      alpha = 0.9
    ) +
    geom_segment(
      aes(xend = date, y = actual_value, yend = predicted_value, color = agreement, linewidth = agreement),
      alpha = 0.9
    ) +
    geom_point(aes(y = actual_value, shape = "Actual"), color = "#d86f45", size = 4) +
    geom_point(aes(y = predicted_value, shape = "Predicted"), color = "#0f6b6f", size = 3.9) +
    scale_color_manual(values = c(Correct = "#a8b8b3", Mismatch = "#e06b3c")) +
    scale_linewidth_manual(values = c(Correct = 1.15, Mismatch = 2.6), guide = "none") +
    scale_shape_manual(values = c(Actual = 16, Predicted = 17)) +
    scale_y_continuous(
      breaks = c(1, 2, 3),
      labels = occupancy_class_label(c("low", "medium", "high")),
      limits = c(0.7, 3.3)
    ) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      x = "Date",
      y = "Occupancy class",
      color = NULL,
      shape = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "#7a5a4c"),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
}

build_metric_comparison_plot <- function(tree_accuracy, tree_kappa, rf_accuracy, rf_kappa) {
  tibble(
    model = c("Decision Tree", "Decision Tree", "Random Forest", "Random Forest"),
    metric = c("Accuracy", "Kappa", "Accuracy", "Kappa"),
    value = c(tree_accuracy, tree_kappa, rf_accuracy, rf_kappa)
  ) %>%
    filter(!is.na(value)) %>%
    mutate(label = if_else(metric == "Accuracy", format_metric(value), format_decimal(value))) %>%
    ggplot(aes(x = model, y = value, fill = model)) +
    geom_col(width = 0.62, show.legend = FALSE) +
    geom_text(aes(label = label), vjust = -0.35, size = 4) +
    facet_wrap(~metric, scales = "free_y") +
    scale_fill_manual(values = c("Decision Tree" = "#d86f45", "Random Forest" = "#0f6b6f")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
    labs(
      title = "Accuracy and Kappa by Model",
      x = NULL,
      y = "Score"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
}

build_class_recall_plot <- function(recall_tbl) {
  recall_tbl %>%
    mutate(class = factor(occupancy_class_label(class), levels = occupancy_class_label(c("low", "medium", "high")))) %>%
    ggplot(aes(x = class, y = recall, fill = model)) +
    geom_col(position = position_dodge(width = 0.72), width = 0.64) +
    scale_fill_manual(values = c("Decision Tree" = "#d86f45", "Random Forest" = "#0f6b6f")) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      title = "Per-Class Recall on the Test Set",
      x = NULL,
      y = "Recall",
      fill = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
}

metric_display_info <- function(metric_name) {
  label_text <- switch(
    metric_name,
    hotel_occ = "Hotel occupancy (%)",
    visitor_arrivals = "Visitor arrivals",
    china_share_pct = "China share (%)",
    avg_stay_monthly = "Average stay (days)",
    tools::toTitleCase(gsub("_", " ", metric_name))
  )

  formatter <- switch(
    metric_name,
    hotel_occ = label_number(accuracy = 0.1),
    visitor_arrivals = label_comma(),
    china_share_pct = label_number(accuracy = 0.1, suffix = "%"),
    avg_stay_monthly = label_number(accuracy = 0.1),
    label_number()
  )

  list(
    label_text = label_text,
    formatter = formatter
  )
}

build_trend_plot <- function(data_tbl, metric_name) {
  metric_info <- metric_display_info(metric_name)

  plot_tbl <- data_tbl %>%
    arrange(date) %>%
    mutate(hotel_occ_level_tertile = factor(hotel_occ_level_tertile, levels = model_class_levels))

  if (identical(metric_name, "hotel_occ") && has_rows(occupancy_band_tbl)) {
    return(
      ggplot(plot_tbl, aes(x = date, y = .data[[metric_name]])) +
        geom_rect(
          data = occupancy_band_tbl,
          aes(xmin = -Inf, xmax = Inf, ymin = min_occ, ymax = max_occ, fill = hotel_occ_level_tertile),
          inherit.aes = FALSE,
          alpha = 0.42
        ) +
        geom_hline(
          yintercept = occupancy_band_tbl$max_occ[seq_len(max(0, nrow(occupancy_band_tbl) - 1))],
          color = "#8d9aa0",
          linewidth = 0.45,
          linetype = "dashed"
        ) +
        geom_line(color = "#6f7f84", linewidth = 1.1) +
        geom_point(color = "#0f6b6f", size = 2.7, alpha = 0.92) +
        scale_fill_manual(
          values = setNames(occupancy_band_tbl$fill_color, occupancy_band_tbl$hotel_occ_level_tertile),
          breaks = occupancy_band_tbl$hotel_occ_level_tertile,
          labels = occupancy_band_tbl$class_label,
          drop = FALSE
        ) +
        scale_y_continuous(labels = metric_info$formatter) +
        labs(
          title = paste(metric_info$label_text, "Over Time"),
          x = NULL,
          y = metric_info$label_text,
          fill = "Occupancy interval"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(face = "bold"),
          legend.position = "top",
          panel.grid.minor = element_blank()
        )
    )
  }

  ggplot(plot_tbl, aes(x = date, y = .data[[metric_name]])) +
    geom_line(color = "#7f9196", linewidth = 1.1) +
    geom_point(color = "#0f6b6f", size = 2.7, alpha = 0.92) +
    scale_y_continuous(labels = metric_info$formatter) +
    labs(
      title = paste(metric_info$label_text, "Over Time"),
      x = NULL,
      y = metric_info$label_text
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

build_metric_heatmap <- function(data_tbl, metric_name) {
  metric_info <- metric_display_info(metric_name)

  heatmap_tbl <- data_tbl %>%
    arrange(date) %>%
    mutate(
      month_num = as.integer(format(date, "%m")),
      month_label = factor(month.abb[month_num], levels = month.abb),
      year_label = factor(format(date, "%Y"), levels = rev(sort(unique(format(date, "%Y")))))
    )

  if (identical(metric_name, "hotel_occ")) {
    return(
      ggplot(
        heatmap_tbl,
        aes(
          x = month_label,
          y = year_label,
          fill = factor(hotel_occ_level_tertile, levels = model_class_levels)
        )
      ) +
        geom_tile(color = "#f3efe6", linewidth = 0.6) +
        geom_text(aes(label = round(hotel_occ, 0)), size = 3, color = "#1f2a2e") +
        scale_fill_manual(
          values = c(low = "#c77d4e", medium = "#8a9a94", high = "#0f6b6f"),
          breaks = model_class_levels,
          labels = occupancy_class_label(model_class_levels),
          drop = FALSE
        ) +
        labs(
          title = "Occupancy Band by Month",
          x = NULL,
          y = "Year",
          fill = "Occupancy interval"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(face = "bold"),
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 0, vjust = 0.5),
          legend.position = "top"
        )
    )
  }

  ggplot(heatmap_tbl, aes(x = month_label, y = year_label, fill = .data[[metric_name]])) +
    geom_tile(color = "#f3efe6", linewidth = 0.6) +
    scale_fill_gradient(
      low = "#f6efe0",
      high = "#0f6b6f",
      labels = metric_info$formatter
    ) +
    labs(
      title = paste(metric_info$label_text, "Heatmap"),
      x = NULL,
      y = "Year",
      fill = metric_info$label_text
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 0, vjust = 0.5)
    )
}

build_oob_plot <- function(oob_tbl) {
  ggplot(oob_tbl, aes(x = num_trees, y = oob_accuracy)) +
    geom_line(color = "#2f7d4f", linewidth = 1.2) +
    geom_point(color = "#2f7d4f", size = 2.5) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = "OOB Accuracy vs Number of Trees",
      x = "Number of trees",
      y = "OOB accuracy"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

model_class_levels <- c("low", "medium", "high")

model_feature_choices <- c(
  "Visitor arrivals" = "visitor_arrivals",
  "China visitor arrivals" = "visitor_arrivals_china",
  "China share (%)" = "china_share_pct",
  "Average stay (days)" = "avg_stay_monthly",
  "Month" = "month",
  "Quarter" = "quarter",
  "Recovery stage" = "period",
  "Year" = "year"
)

default_tree_builder_features <- c(
  "visitor_arrivals",
  "china_share_pct",
  "avg_stay_monthly",
  "month",
  "period"
)

default_rf_builder_features <- c(
  "visitor_arrivals",
  "visitor_arrivals_china",
  "china_share_pct",
  "avg_stay_monthly",
  "month",
  "period"
)

feature_sentence <- function(features) {
  if (length(features) == 0) {
    return("No predictors selected")
  }

  paste(display_feature_label(features), collapse = ", ")
}

extract_recall_value <- function(recall_tbl, class_name) {
  recall_tbl %>%
    filter(class == class_name) %>%
    pull(recall) %>%
    first(default = NA_real_)
}

prepare_modeling_data <- function(data_tbl) {
  if (!has_rows(data_tbl)) {
    return(tibble())
  }

  data_tbl %>%
    filter(!is.na(hotel_occ_level_tertile)) %>%
    transmute(
      date = as.Date(date),
      hotel_occ = hotel_occ,
      hotel_occ_level_tertile = factor(hotel_occ_level_tertile, levels = model_class_levels),
      visitor_arrivals = visitor_arrivals,
      visitor_arrivals_china = visitor_arrivals_china,
      china_share_pct = china_share_pct,
      avg_stay_monthly = avg_stay_monthly,
      month = factor(month, levels = 1:12, labels = month.abb, ordered = TRUE),
      quarter = factor(quarter, levels = 1:4, labels = paste("Q", 1:4), ordered = TRUE),
      period = factor(
        period,
        levels = c("pre_covid", "covid_shock", "recovery"),
        labels = c("Pre-Covid", "Covid Shock", "Recovery")
      ),
      year = year
    ) %>%
    tidyr::drop_na()
}

split_model_data <- function(data_tbl, train_ratio, seed) {
  if (!has_rows(data_tbl)) {
    stop("No rows are available for model building.", call. = FALSE)
  }

  train_ratio <- max(0.55, min(0.9, train_ratio %||% 0.8))
  seed <- suppressWarnings(as.integer(seed %||% 123L))
  if (is.na(seed)) {
    seed <- 123L
  }

  if (nrow(data_tbl) < 6) {
    stop("At least six rows are needed to build a live model.", call. = FALSE)
  }

  set.seed(seed)

  if (requireNamespace("caret", quietly = TRUE)) {
    idx <- caret::createDataPartition(
      y = data_tbl$hotel_occ_level_tertile,
      p = train_ratio,
      list = FALSE
    )
    idx <- as.integer(idx[, 1] %||% idx)
  } else {
    idx <- sort(sample(seq_len(nrow(data_tbl)), size = floor(nrow(data_tbl) * train_ratio)))
  }

  idx <- sort(unique(idx))

  if (length(idx) == 0 || length(idx) >= nrow(data_tbl)) {
    stop("Train/test split failed. Try a different partition ratio.", call. = FALSE)
  }

  train_tbl <- data_tbl[idx, , drop = FALSE]
  test_tbl <- data_tbl[-idx, , drop = FALSE]

  if (nrow(train_tbl) < 4 || nrow(test_tbl) < 2) {
    stop("The current partition leaves too few rows in train or test.", call. = FALSE)
  }

  list(
    train = train_tbl,
    test = test_tbl,
    train_n = nrow(train_tbl),
    test_n = nrow(test_tbl),
    train_ratio = train_ratio,
    seed = seed
  )
}

confusion_from_predictions <- function(actual, predicted) {
  actual <- factor(actual, levels = model_class_levels)
  predicted <- factor(predicted, levels = model_class_levels)

  as.data.frame(table(
    Prediction = predicted,
    Reference = actual
  ))
}

accuracy_from_confusion <- function(conf_tbl) {
  mat <- xtabs(Freq ~ Prediction + Reference, data = conf_tbl)
  total <- sum(mat)

  if (total == 0) {
    return(NA_real_)
  }

  sum(diag(mat)) / total
}

kappa_from_confusion <- function(conf_tbl) {
  mat <- xtabs(Freq ~ Prediction + Reference, data = conf_tbl)
  total <- sum(mat)

  if (total == 0) {
    return(NA_real_)
  }

  observed <- sum(diag(mat)) / total
  expected <- sum(rowSums(mat) * colSums(mat)) / (total ^ 2)

  if (isTRUE(all.equal(1 - expected, 0))) {
    return(NA_real_)
  }

  (observed - expected) / (1 - expected)
}

coerce_probability_tbl <- function(prob_values) {
  prob_tbl <- tibble::as_tibble(as.data.frame(prob_values))

  for (class_name in model_class_levels) {
    if (!class_name %in% names(prob_tbl)) {
      prob_tbl[[class_name]] <- NA_real_
    }
  }

  prob_tbl %>%
    select(all_of(model_class_levels))
}

clean_cp_table <- function(cp_table) {
  cp_tbl <- as.data.frame(cp_table)

  if (ncol(cp_tbl) >= 5) {
    names(cp_tbl)[1:5] <- c("CP", "nsplit", "rel_error", "xerror", "xstd")
  }

  tibble::as_tibble(cp_tbl)
}

build_tree_cp_plot <- function(cp_tbl, chosen_cp = NA_real_) {
  plot_tbl <- cp_tbl %>%
    mutate(
      tree_size = nsplit + 1,
      chosen = if (is.na(chosen_cp)) FALSE else abs(CP - chosen_cp) < 1e-9,
      label = if_else(chosen, paste("Chosen CP", format_decimal(CP, 3)), NA_character_)
    )

  ggplot(plot_tbl, aes(x = tree_size, y = xerror)) +
    geom_line(color = "#6d7a7f", linewidth = 1.1) +
    geom_errorbar(
      aes(ymin = pmax(xerror - xstd, 0), ymax = xerror + xstd),
      width = 0.12,
      color = "#a8b3b7"
    ) +
    geom_point(aes(fill = chosen), shape = 21, size = 4.2, color = "#1f2a2e", stroke = 0.8) +
    geom_label(
      data = function(x) dplyr::filter(x, chosen),
      aes(label = label),
      size = 3.5,
      fill = "#fff7ec",
      color = "#1f2a2e",
      linewidth = 0.15
    ) +
    scale_fill_manual(values = c(`TRUE` = "#d86f45", `FALSE` = "#f4f2eb"), guide = "none") +
    labs(
      title = "Cross-validated tree tuning path",
      x = "Tree size",
      y = "Cross-validated relative error"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

build_confidence_plot <- function(pred_tbl, title_text, accent = "#0f6b6f") {
  prepare_prediction_rows(pred_tbl) %>%
    ggplot(aes(x = date, y = confidence, color = correct)) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "#b7c2c5") +
    geom_line(color = "#d6dddf", linewidth = 0.85) +
    geom_point(size = 3) +
    scale_color_manual(values = c(`TRUE` = accent, `FALSE` = "#d86f45"), labels = c(`TRUE` = "Correct", `FALSE` = "Mismatch")) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      title = title_text,
      x = "Date",
      y = "Prediction confidence",
      color = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
}

build_single_recall_plot <- function(recall_tbl, title_text, fill_color) {
  recall_tbl %>%
    mutate(class = factor(occupancy_class_label(class), levels = occupancy_class_label(c("low", "medium", "high")))) %>%
    ggplot(aes(x = class, y = recall)) +
    geom_col(fill = fill_color, width = 0.62) +
    geom_text(aes(label = format_metric(recall)), vjust = -0.35, size = 4) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    labs(
      title = title_text,
      x = NULL,
      y = "Recall"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

build_tree_model_bundle <- function(data_tbl, features, train_ratio, seed, minsplit, maxdepth, cp_value, auto_cp) {
  tryCatch(
    {
      if (!requireNamespace("rpart", quietly = TRUE)) {
        stop("Package 'rpart' is required for the live tree builder.", call. = FALSE)
      }

      features <- intersect(features, names(data_tbl))
      if (length(features) == 0) {
        stop("Select at least one predictor before building the tree.", call. = FALSE)
      }

      split_tbl <- split_model_data(data_tbl, train_ratio, seed)
      model_formula <- stats::reformulate(features, response = "hotel_occ_level_tertile")
      grow_cp <- if (isTRUE(auto_cp)) min(cp_value, 0.005) else cp_value

      initial_fit <- rpart::rpart(
        formula = model_formula,
        data = split_tbl$train,
        method = "class",
        model = TRUE,
        control = rpart::rpart.control(
          minsplit = minsplit,
          maxdepth = maxdepth,
          cp = grow_cp,
          xval = 10
        )
      )

      cp_tbl <- clean_cp_table(initial_fit$cptable)
      chosen_cp <- if (has_rows(cp_tbl) && isTRUE(auto_cp)) {
        cp_tbl %>% arrange(xerror, xstd) %>% pull(CP) %>% first(default = cp_value)
      } else {
        cp_value
      }

      final_fit <- if (has_rows(cp_tbl) && isTRUE(auto_cp)) {
        rpart::prune(initial_fit, cp = chosen_cp)
      } else {
        initial_fit
      }

      prob_tbl <- coerce_probability_tbl(stats::predict(final_fit, split_tbl$test, type = "prob"))
      predicted_class <- colnames(prob_tbl)[max.col(as.matrix(prob_tbl), ties.method = "first")]
      conf_tbl <- confusion_from_predictions(split_tbl$test$hotel_occ_level_tertile, predicted_class)
      recall_tbl <- class_recall_table(conf_tbl, "Decision Tree")
      importance_values <- final_fit$variable.importance %||% numeric()

      prediction_tbl <- bind_cols(
        split_tbl$test %>%
          select(date, hotel_occ, hotel_occ_level_tertile),
        tibble(predicted_class = predicted_class),
        prob_tbl
      )

      list(
        error = NULL,
        model = final_fit,
        formula = model_formula,
        features = features,
        train_tbl = split_tbl$train,
        test_tbl = split_tbl$test,
        predictions = prediction_tbl,
        conf_tbl = conf_tbl,
        recall_tbl = recall_tbl,
        accuracy = accuracy_from_confusion(conf_tbl),
        kappa = kappa_from_confusion(conf_tbl),
        importance_tbl = tibble(
          variable = names(importance_values),
          importance = as.numeric(importance_values)
        ),
        cp_tbl = cp_tbl,
        chosen_cp = chosen_cp,
        train_n = split_tbl$train_n,
        test_n = split_tbl$test_n,
        train_ratio = split_tbl$train_ratio,
        seed = split_tbl$seed,
        leaf_count = sum(final_fit$frame$var == "<leaf>"),
        node_count = nrow(final_fit$frame)
      )
    },
    error = function(e) list(error = conditionMessage(e))
  )
}

rf_checkpoint_values <- function(num_trees) {
  unique(sort(c(
    max(20L, round(num_trees * 0.2)),
    max(40L, round(num_trees * 0.4)),
    max(60L, round(num_trees * 0.6)),
    max(80L, round(num_trees * 0.8)),
    as.integer(num_trees)
  )))
}

build_rf_model_bundle <- function(data_tbl, features, train_ratio, seed, num_trees, mtry, min_node_size) {
  tryCatch(
    {
      if (!requireNamespace("ranger", quietly = TRUE)) {
        stop("Package 'ranger' is required for the live random forest builder.", call. = FALSE)
      }

      features <- intersect(features, names(data_tbl))
      if (length(features) == 0) {
        stop("Select at least one predictor before building the forest.", call. = FALSE)
      }

      split_tbl <- split_model_data(data_tbl, train_ratio, seed)
      model_formula <- stats::reformulate(features, response = "hotel_occ_level_tertile")
      resolved_mtry <- max(1L, min(as.integer(mtry %||% 1L), length(features)))

      rf_fit <- ranger::ranger(
        formula = model_formula,
        data = split_tbl$train,
        probability = TRUE,
        classification = TRUE,
        importance = "impurity",
        num.trees = as.integer(num_trees),
        mtry = resolved_mtry,
        min.node.size = as.integer(min_node_size),
        seed = as.integer(split_tbl$seed)
      )

      prob_tbl <- coerce_probability_tbl(predict(rf_fit, data = split_tbl$test)$predictions)
      predicted_class <- colnames(prob_tbl)[max.col(as.matrix(prob_tbl), ties.method = "first")]
      conf_tbl <- confusion_from_predictions(split_tbl$test$hotel_occ_level_tertile, predicted_class)
      recall_tbl <- class_recall_table(conf_tbl, "Random Forest")

      prediction_tbl <- bind_cols(
        split_tbl$test %>%
          select(date, hotel_occ, hotel_occ_level_tertile),
        tibble(predicted_class = predicted_class),
        prob_tbl
      )

      checkpoints <- rf_checkpoint_values(as.integer(num_trees))
      oob_tbl <- bind_rows(lapply(
        checkpoints,
        function(check_trees) {
          checkpoint_fit <- ranger::ranger(
            formula = model_formula,
            data = split_tbl$train,
            probability = TRUE,
            classification = TRUE,
            importance = "none",
            num.trees = as.integer(check_trees),
            mtry = resolved_mtry,
            min.node.size = as.integer(min_node_size),
            seed = as.integer(split_tbl$seed)
          )

          tibble(
            num_trees = as.integer(check_trees),
            oob_accuracy = 1 - checkpoint_fit$prediction.error
          )
        }
      ))

      list(
        error = NULL,
        model = rf_fit,
        formula = model_formula,
        features = features,
        train_tbl = split_tbl$train,
        test_tbl = split_tbl$test,
        predictions = prediction_tbl,
        conf_tbl = conf_tbl,
        recall_tbl = recall_tbl,
        accuracy = accuracy_from_confusion(conf_tbl),
        kappa = kappa_from_confusion(conf_tbl),
        importance_tbl = tibble(
          variable = names(rf_fit$variable.importance),
          Overall = as.numeric(rf_fit$variable.importance)
        ),
        oob_tbl = oob_tbl,
        train_n = split_tbl$train_n,
        test_n = split_tbl$test_n,
        train_ratio = split_tbl$train_ratio,
        seed = split_tbl$seed,
        mtry = resolved_mtry,
        num_trees = as.integer(num_trees),
        min_node_size = as.integer(min_node_size)
      )
    },
    error = function(e) list(error = conditionMessage(e))
  )
}

load_model_bundle <- function(project_dir) {
  output_dir <- file.path(project_dir, "outputs")
  data_dir <- file.path(project_dir, "data")

  data_tbl <- read_csv_safe(file.path(data_dir, "tourism_decision_tree_ready.csv"))
  tree_metrics <- read_csv_safe(file.path(output_dir, "decision_tree_metrics.csv"))
  rf_metrics <- read_csv_safe(file.path(output_dir, "random_forest_metrics.csv"))
  tree_conf <- read_csv_safe(file.path(output_dir, "decision_tree_confusion_matrix.csv"))
  rf_conf <- read_csv_safe(file.path(output_dir, "random_forest_confusion_matrix.csv"))
  tree_importance <- read_csv_safe(file.path(output_dir, "decision_tree_variable_importance.csv"))
  rf_importance <- read_csv_safe(file.path(output_dir, "random_forest_variable_importance.csv"))
  tree_predictions <- read_csv_safe(file.path(output_dir, "decision_tree_test_predictions.csv"))
  rf_predictions <- read_csv_safe(file.path(output_dir, "random_forest_test_predictions.csv"))
  rf_oob <- read_csv_safe(file.path(output_dir, "random_forest_oob_accuracy.csv"))

  list(
    project_dir = project_dir,
    output_dir = output_dir,
    data_tbl = data_tbl,
    tree_metrics = tree_metrics,
    rf_metrics = rf_metrics,
    tree_conf = tree_conf,
    rf_conf = rf_conf,
    tree_importance = tree_importance,
    rf_importance = rf_importance,
    tree_predictions = tree_predictions,
    rf_predictions = rf_predictions,
    rf_oob = rf_oob
  )
}

metric_card <- function(title, value, subtitle = NULL, accent = "#0f6b6f") {
  card(
    class = "metric-card",
    style = paste0("border-top: 4px solid ", accent, ";"),
    card_body(
      p(class = "metric-title", title),
      h2(class = "metric-value", value),
      if (!is.null(subtitle)) p(class = "metric-subtitle", subtitle)
    )
  )
}

PROJECT_DIR <- locate_project_dir()
MODEL_BUNDLE <- load_model_bundle(PROJECT_DIR)

if (dir.exists(MODEL_BUNDLE$output_dir)) {
  addResourcePath("model_outputs", MODEL_BUNDLE$output_dir)
}

model_output_exists <- function(filename) {
  file.exists(file.path(MODEL_BUNDLE$output_dir, filename))
}

model_output_image <- function(filename, alt_text, title = NULL, caption = NULL) {
  div(
    class = "visual-panel",
    if (!is.null(title)) h4(title),
    if (model_output_exists(filename)) {
      tagList(
        tags$img(class = "full-card framed-image", src = paste0("model_outputs/", filename), alt = alt_text),
        if (!is.null(caption)) p(class = "visual-caption", caption)
      )
    } else {
      div(
        class = "visual-missing",
        paste("Missing visual file:", filename)
      )
    }
  )
}

snapshot_tile <- function(label, value, subtitle = NULL, accent = "#0f6b6f") {
  div(
    class = "snapshot-tile",
    style = paste0("--tile-accent:", accent, ";"),
    p(class = "snapshot-label", label),
    h3(class = "snapshot-value", value),
    if (!is.null(subtitle)) p(class = "snapshot-subtitle", subtitle)
  )
}

tree_accuracy <- metric_value(MODEL_BUNDLE$tree_metrics, "accuracy")
rf_accuracy <- metric_value(MODEL_BUNDLE$rf_metrics, "accuracy")
tree_kappa <- metric_value(MODEL_BUNDLE$tree_metrics, "kappa")
rf_kappa <- metric_value(MODEL_BUNDLE$rf_metrics, "kappa")
tree_recall_tbl <- class_recall_table(MODEL_BUNDLE$tree_conf, "Decision Tree")
rf_recall_tbl <- class_recall_table(MODEL_BUNDLE$rf_conf, "Random Forest")
all_recall_tbl <- bind_rows(tree_recall_tbl, rf_recall_tbl)
tree_low_recall <- tree_recall_tbl %>% filter(class == "low") %>% pull(recall) %>% first(default = NA_real_)
rf_low_recall <- rf_recall_tbl %>% filter(class == "low") %>% pull(recall) %>% first(default = NA_real_)
tree_high_recall <- tree_recall_tbl %>% filter(class == "high") %>% pull(recall) %>% first(default = NA_real_)
rf_high_recall <- rf_recall_tbl %>% filter(class == "high") %>% pull(recall) %>% first(default = NA_real_)
accuracy_gap <- if (is.na(tree_accuracy) || is.na(rf_accuracy)) NA_real_ else tree_accuracy - rf_accuracy
tree_top_feature_label <- if (has_rows(MODEL_BUNDLE$tree_importance)) {
  MODEL_BUNDLE$tree_importance %>%
    mutate(label = display_feature_label(variable)) %>%
    arrange(desc(importance)) %>%
    pull(label) %>%
    first(default = "N/A")
} else {
  "N/A"
}
rf_top_feature_label <- if (has_rows(MODEL_BUNDLE$rf_importance)) {
  MODEL_BUNDLE$rf_importance %>%
    mutate(label = display_feature_label(variable)) %>%
    arrange(desc(Overall)) %>%
    pull(label) %>%
    first(default = "N/A")
} else {
  "N/A"
}
rf_peak_oob <- if (has_rows(MODEL_BUNDLE$rf_oob)) max_or_na(MODEL_BUNDLE$rf_oob$oob_accuracy) else NA_real_

best_model <- if (is.na(tree_accuracy) || is.na(rf_accuracy)) {
  "Model outputs incomplete"
} else if (tree_accuracy >= rf_accuracy) {
  "Decision Tree"
} else {
  "Random Forest"
}

summary_tbl <- MODEL_BUNDLE$data_tbl %||% tibble(
  date = as.Date(character()),
  period = character(),
  visitor_arrivals = numeric(),
  china_share_pct = numeric(),
  hotel_occ = numeric(),
  avg_stay_monthly = numeric(),
  hotel_occ_level_tertile = character(),
  dataset_split = character()
)
live_model_tbl <- prepare_modeling_data(summary_tbl)
occupancy_band_tbl <- occupancy_band_ranges(summary_tbl)
occupancy_band_note <- occupancy_band_summary(summary_tbl)
train_rows <- sum(summary_tbl$dataset_split == "train", na.rm = TRUE)
test_rows <- sum(summary_tbl$dataset_split == "test", na.rm = TRUE)
coverage_text <- if (has_rows(summary_tbl)) {
  paste(format(min(summary_tbl$date, na.rm = TRUE), "%b %Y"), "to", format(max(summary_tbl$date, na.rm = TRUE), "%b %Y"))
} else {
  "N/A"
}
feature_count <- length(unique(c(
  if (has_rows(MODEL_BUNDLE$tree_importance)) concept_feature_label(MODEL_BUNDLE$tree_importance$variable) else character(),
  if (has_rows(MODEL_BUNDLE$rf_importance)) concept_feature_label(MODEL_BUNDLE$rf_importance$variable) else character()
)))
if (feature_count == 0) {
  feature_count <- 4L
}
data_metric_choices <- c(
  "Hotel occupancy (%)" = "hotel_occ",
  "Visitor arrivals" = "visitor_arrivals",
  "China share (%)" = "china_share_pct",
  "Average stay (days)" = "avg_stay_monthly"
)
date_limits <- if (has_rows(summary_tbl)) range(summary_tbl$date, na.rm = TRUE) else rep(Sys.Date(), 2)
split_choices <- sort(unique(summary_tbl$dataset_split))
builder_feature_count <- max(1L, length(model_feature_choices))
tree_top_n_max <- builder_feature_count
rf_top_n_max <- builder_feature_count
tree_preview_max <- max(8L, min(25L, nrow(live_model_tbl)))
rf_preview_max <- max(8L, min(25L, nrow(live_model_tbl)))

ui <- page_navbar(
  title = "Tourism Classification Explorer",
  theme = bs_theme(
    bg = "#f4f2eb",
    fg = "#1f2a2e",
    primary = "#0f6b6f",
    secondary = "#d86f45",
    base_font = font_collection("Aptos", "Segoe UI", "sans-serif"),
    heading_font = font_collection("Palatino Linotype", "Georgia", "serif")
  ),
  header = tags$head(
    tags$style(HTML(
      "
      .metric-card { min-height: 180px; background: rgba(255,255,255,0.82); }
      .metric-title { text-transform: uppercase; letter-spacing: 0.08em; font-size: 0.78rem; color: #5b666a; margin-bottom: 0.35rem; }
      .metric-value { margin-bottom: 0.45rem; font-weight: 700; }
      .metric-subtitle { color: #5b666a; margin-bottom: 0; }
      .section-note { color: #5b666a; max-width: 72ch; }
      .filepath { font-family: monospace; font-size: 0.92rem; word-break: break-all; color: #445156; }
      .app-hero { padding: 0.35rem 0 0.85rem 0; }
      .app-hero h1 { margin-bottom: 0.4rem; }
      .app-hero p { margin-bottom: 0; color: #4d5b60; max-width: 76ch; }
      iframe.tree-frame { width: 100%; height: 420px; border: 0; border-radius: 14px; background: white; }
      img.full-card { width: 100%; height: auto; max-height: 580px; object-fit: contain; display: block; border-radius: 12px; background: white; }
      img.framed-image { background: white; border: 1px solid rgba(31, 42, 46, 0.08); box-shadow: 0 14px 28px rgba(31, 42, 46, 0.08); }
      .visual-panel { background: rgba(255,255,255,0.72); border: 1px solid rgba(15, 107, 111, 0.12); border-radius: 16px; padding: 0.78rem; }
      .visual-panel h4 { margin-bottom: 0.7rem; font-size: 0.96rem; font-weight: 700; }
      .visual-caption { margin: 0.65rem 0 0; color: #5b666a; font-size: 0.86rem; line-height: 1.45; }
      .visual-missing { min-height: 220px; display: flex; align-items: center; justify-content: center; padding: 1rem; border: 1px dashed #b8c3c7; border-radius: 12px; color: #5b666a; text-align: center; background: rgba(255,255,255,0.78); }
      .visual-tab-intro { color: #4d5b60; max-width: 82ch; margin-bottom: 1rem; }
      .visual-tabset .tab-content { padding-top: 1rem; }
      .visual-tabset .nav-tabs { gap: 0.45rem; border-bottom: 1px solid rgba(15, 107, 111, 0.12); }
      .visual-tabset .nav-tabs .nav-link { border-radius: 999px; border: 1px solid rgba(15, 107, 111, 0.14); color: #2f4a4f; background: rgba(255,255,255,0.68); padding: 0.45rem 0.95rem; }
      .visual-tabset .nav-tabs .nav-link.active { background: #0f6b6f; color: #ffffff; border-color: #0f6b6f; }
      .visual-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(280px, 1fr)); gap: 1rem; }
      .visual-grid.single-wide { grid-template-columns: 1fr; }
      .page-stack { display: grid; gap: 1rem; }
      .model-workbench { display: grid; grid-template-columns: minmax(340px, 0.78fr) minmax(560px, 1.22fr); gap: 1rem; align-items: start; }
      .model-insights { display: grid; gap: 0.9rem; }
      .visual-column { min-width: 0; display: grid; gap: 1rem; align-content: start; }
      .visual-dock { position: static; top: auto; }
      .visual-dock { border: 1px solid rgba(15, 107, 111, 0.12); box-shadow: 0 18px 36px rgba(31, 42, 46, 0.08); background: linear-gradient(180deg, rgba(255,255,255,0.96), rgba(249,247,241,0.94)); }
      .visual-dock .card-header { font-weight: 700; letter-spacing: 0.01em; }
      .visual-dock .card-body { min-height: auto; padding: 0.85rem 0.9rem 0.95rem; display: grid; gap: 0.9rem; }
      .visual-stage { min-height: auto; }
      .visual-stage .tree-frame { height: 420px; }
      .visual-stage .visual-panel { min-height: 0; }
      .visual-stage .visual-panel img.full-card { max-height: 560px; object-fit: contain; background: white; }
      .plot-stage { min-height: 0; }
      .plot-stage .shiny-plot-output { width: 100% !important; }
      .insight-card { background: rgba(255,255,255,0.84); border: 1px solid rgba(31, 42, 46, 0.08); box-shadow: 0 8px 18px rgba(31, 42, 46, 0.05); }
      .insight-card .card-header { font-weight: 700; }
      .insight-card .card-body { overflow-x: auto; padding: 0.95rem 1rem; }
      .insight-card .dataTables_wrapper,
      .insight-card table.dataTable thead th,
      .insight-card table.dataTable tbody td { font-size: 0.9rem; }
      .snapshot-card .card-body { padding: 1rem; }
      .snapshot-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 0.7rem; margin-bottom: 0.95rem; }
      .snapshot-tile { border: 1px solid rgba(31, 42, 46, 0.08); border-top: 4px solid var(--tile-accent); border-radius: 14px; background: linear-gradient(180deg, rgba(255,255,255,0.98), rgba(247,244,236,0.92)); padding: 0.68rem 0.82rem 0.64rem; }
      .snapshot-label { margin-bottom: 0.22rem; font-size: 0.7rem; letter-spacing: 0.08em; text-transform: uppercase; color: #5b666a; }
      .snapshot-value { margin-bottom: 0.16rem; font-size: 1.12rem; line-height: 1.08; }
      .snapshot-subtitle { margin-bottom: 0; color: #5b666a; font-size: 0.8rem; line-height: 1.28; }
      .snapshot-note p { margin-bottom: 0.5rem; color: #36484d; }
      .snapshot-note ul { margin-bottom: 0; padding-left: 1.1rem; color: #4d5b60; }
      .visual-metric-strip { margin-top: 0.1rem; padding-top: 0.8rem; border-top: 1px solid rgba(15, 107, 111, 0.12); }
      .visual-metric-strip .snapshot-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 0.65rem; margin-bottom: 0; }
      .visual-metric-strip .snapshot-tile { min-height: 96px; background: linear-gradient(180deg, #f6df53 0%, #ecd449 100%); border: 0; box-shadow: inset 0 0 0 1px rgba(86, 74, 10, 0.08); }
      .visual-metric-strip .snapshot-label,
      .visual-metric-strip .snapshot-subtitle,
      .visual-metric-strip .snapshot-value { color: #43380d; }
      .metric-panel-card .card-body { padding: 0.95rem 1rem; }
      .metric-panel-card .visual-metric-strip { margin-top: 0; padding-top: 0; border-top: 0; }
      .metric-panel-card .snapshot-grid { margin-bottom: 0; }
      .message-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 0.7rem; }
      .message-tile { min-height: 92px; border: 1px solid rgba(15, 107, 111, 0.1); border-radius: 14px; background: linear-gradient(180deg, rgba(255,255,255,0.98), rgba(244,247,246,0.96)); padding: 0.78rem 0.88rem; }
      .message-label { margin-bottom: 0.28rem; font-size: 0.7rem; letter-spacing: 0.08em; text-transform: uppercase; color: #5b666a; }
      .message-text { margin-bottom: 0; color: #304246; font-size: 0.95rem; line-height: 1.42; font-weight: 600; }
      .control-stack { display: grid; gap: 1rem; }
      .control-card { border: 1px solid rgba(15, 107, 111, 0.14); box-shadow: 0 10px 24px rgba(31, 42, 46, 0.06); background: #a9d8df; }
      .control-card .card-header { font-weight: 700; background: #102225; color: #ffffff; }
      .control-card .card-body { display: grid; gap: 0.75rem; background: #a9d8df; }
      .control-card .shiny-input-container { margin-bottom: 0; }
      .control-card .btn { width: 100%; border-radius: 999px; font-weight: 600; }
      .control-card label { font-weight: 600; color: #1f2a2e; margin-bottom: 0.15rem; }
      .control-card .form-select,
      .control-card .form-control,
      .control-card .selectize-input { border-radius: 10px; }
      .control-card .selectize-dropdown-content { max-height: 240px; }
      .control-note { color: #4d5b60; margin-bottom: 0; line-height: 1.55; }
      .guide-list { margin-bottom: 0; padding-left: 1.1rem; color: #4d5b60; }
      .recipe-list { margin: 0; display: grid; grid-template-columns: minmax(112px, 0.45fr) minmax(0, 1fr); gap: 0.45rem 0.8rem; }
      .recipe-list dt { font-weight: 700; color: #304246; margin-bottom: 0; }
      .recipe-list dd { margin-bottom: 0; color: #4d5b60; }
      .status-pill { display: inline-flex; align-items: center; gap: 0.35rem; padding: 0.28rem 0.72rem; border-radius: 999px; background: rgba(15, 107, 111, 0.08); color: #0f6b6f; font-size: 0.82rem; font-weight: 700; letter-spacing: 0.04em; text-transform: uppercase; }
      .story-callout { margin-top: 0.75rem; padding: 0.8rem 0.9rem; border-radius: 14px; background: rgba(15, 107, 111, 0.05); border: 1px solid rgba(15, 107, 111, 0.08); color: #304246; }
      .stacked-visuals { display: grid; gap: 1rem; }
      .compact-note { color: #4d5b60; line-height: 1.55; font-size: 0.96rem; }
      @media (max-width: 1400px) {
        .model-workbench { grid-template-columns: minmax(300px, 0.82fr) minmax(700px, 1.38fr); }
      }
      @media (max-width: 1180px) {
        .model-workbench { grid-template-columns: 1fr; }
        .visual-dock { position: static; }
        .visual-dock .card-body,
        .visual-stage,
        .plot-stage { min-height: auto; }
        .visual-stage .tree-frame { height: 380px; }
        .snapshot-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
        .visual-metric-strip .snapshot-grid { grid-template-columns: repeat(2, minmax(0, 1fr)); }
        .message-grid { grid-template-columns: 1fr; }
      }
      "
    ))
  ),
  nav_panel(
    "Overview",
    div(
      class = "app-hero",
      h1("Tourism Classification Explorer"),
      p(
        "A single-file Shiny interface for reviewing classification tree and random forest outputs,",
        "including holdout performance, confusion patterns, feature importance, and prediction previews."
      )
    ),
    layout_column_wrap(
      width = 1 / 4,
      metric_card("Best Holdout Accuracy", format_metric(max_or_na(c(tree_accuracy, rf_accuracy))), paste(best_model, "leads on test accuracy"), "#0f6b6f"),
      metric_card("Decision Tree", format_metric(tree_accuracy), paste("Kappa:", format_decimal(tree_kappa)), "#d86f45"),
      metric_card("Random Forest", format_metric(rf_accuracy), paste("Kappa:", format_decimal(rf_kappa)), "#2f7d4f"),
      metric_card("Dataset Window", coverage_text, "Detected data coverage", "#7f5a83")
    ),
    layout_column_wrap(
      width = 1 / 2,
        card(
          card_header("Project Summary"),
          card_body(
          p(class = "section-note", "The overview and comparison tabs use your exported artifacts, while the model tabs now also include live builders so the visuals update when you change predictors or tuning settings."),
          tags$ul(
            tags$li("Target: hotel occupancy level tertile (Low Occupancy / Medium Occupancy / High Occupancy)"),
            tags$li("Conceptual features: visitor arrivals, China share, average stay, month"),
            tags$li("Data rows loaded: ", nrow(summary_tbl)),
            tags$li("Train / test rows: ", train_rows, " / ", test_rows),
            tags$li("Detected project folder:"),
            tags$li(tags$span(class = "filepath", PROJECT_DIR))
          )
        )
      ),
      card(
        card_header("Model Metrics"),
        card_body(
          DTOutput("overview_metrics")
        )
      ),
      card(
        card_header("Data Preview"),
        card_body(
          DTOutput("overview_data")
        )
      ),
      card(
        card_header("Recommendation"),
        card_body(
          uiOutput("recommendation_text")
        )
      )
    )
  ),
  nav_panel(
    "Data Explorer",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectInput("data_metric", "Trend metric", choices = data_metric_choices, selected = "hotel_occ"),
        checkboxGroupInput(
          "data_split_filter",
          "Dataset split",
          choices = split_choices,
          selected = split_choices
        ),
        dateRangeInput(
          "data_date_range",
          "Date range",
          start = date_limits[1],
          end = date_limits[2],
          min = date_limits[1],
          max = date_limits[2]
        ),
        helpText("Use this page to show the tourism pattern behind the labels before discussing model quality.")
      ),
      div(
        class = "page-stack",
        layout_column_wrap(
          width = 1 / 2,
          card(
            card_header("Trend Line"),
            card_body(
              plotOutput("data_trend_plot", height = "340px"),
              div(class = "compact-note", uiOutput("data_explorer_note"))
            )
          ),
          card(
            card_header("Year-Month Heatmap"),
            card_body(
              plotOutput("data_heatmap_plot", height = "340px"),
              div(class = "compact-note", uiOutput("data_heatmap_note"))
            )
          )
        ),
        card(
          card_header("Filtered Preview"),
          card_body(DTOutput("data_explorer_tbl"))
        )
      )
    )
  ),
  nav_panel(
    "Classification Tree",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        div(
          class = "control-stack",
          card(
            class = "control-card",
            card_header("Model Initiation"),
            card_body(
              p(class = "control-note", "Choose the core variables, then rebuild the tree to refresh the main visuals on the right."),
              selectizeInput(
                "tree_features",
                "Variables to include",
                choices = model_feature_choices,
                selected = default_tree_builder_features,
                multiple = TRUE,
                options = list(plugins = list("remove_button"), closeAfterSelect = FALSE)
              ),
              sliderInput("tree_train_ratio", "Train-test partition ratio", min = 0.55, max = 0.9, value = 0.8, step = 0.05),
              numericInput("tree_seed", "Random seed", value = 123, min = 1, step = 1),
              actionButton("tree_build", "Build / Refresh Tree", class = "btn-primary")
            )
          ),
          card(
            class = "control-card",
            card_header("Tree Tuning"),
            card_body(
              p(class = "control-note", "Keep only the important controls here so the layout stays clean and easier to present."),
              sliderInput("tree_minsplit", "Minimum split", min = 4, max = 24, value = 8, step = 1),
              sliderInput("tree_maxdepth", "Maximum depth", min = 2, max = 6, value = 4, step = 1),
              checkboxInput("tree_auto_cp", "Auto-select best CP", value = TRUE),
              sliderInput("tree_cp", "Baseline CP", min = 0.001, max = 0.08, value = 0.01, step = 0.001),
              actionButton("tree_tune", "Apply Tuning", class = "btn-outline-secondary")
            )
          ),
          card(
            class = "control-card",
            card_header("Display Filter"),
            card_body(
              p(class = "control-note", "These filters only affect the prediction comparison tab."),
              selectInput(
                "tree_result_filter",
                "Prediction rows",
                choices = c("All rows" = "all", "Misclassified only" = "misclassified", "Correct only" = "correct"),
                selected = "all"
              ),
              selectInput(
                "tree_class_filter",
                "Actual class filter",
                choices = occupancy_class_choices,
                selected = "All"
              )
            )
          )
        )
      ),
      div(
        class = "model-workbench",
        div(
          class = "model-insights",
          card(
            class = "insight-card",
            card_header("Tree Complexity Path"),
            card_body(plotOutput("tree_cp_plot", height = "220px"))
          ),
          card(
            class = "insight-card",
            card_header("CP Table"),
            card_body(DTOutput("tree_cp_tbl"))
          ),
          card(
            class = "insight-card",
            card_header("Key Message"),
            card_body(div(class = "compact-note", uiOutput("tree_notes")))
          )
        ),
        div(
          class = "visual-column",
          card(
            class = "visual-card visual-dock",
            card_header("Classification Tree"),
            card_body(
              div(
                class = "visual-tabset visual-stage",
                tabsetPanel(
                  type = "tabs",
                  tabPanel(
                    "Decision Tree",
                    uiOutput("tree_primary_visual_ui")
                  ),
                  tabPanel(
                    "Predicted vs Actual on Test Data",
                    div(
                      class = "visual-panel plot-stage",
                      plotOutput("tree_prediction_plot", height = "420px"),
                      p(
                        class = "visual-caption",
                        "Shaded red columns mark mismatch months. A longer vertical gap means the model missed by more than one class step."
                      )
                    )
                  ),
                  tabPanel(
                    "Confusion Matrix",
                    div(
                      class = "visual-panel plot-stage",
                      plotOutput("tree_conf_plot", height = "420px"),
                      p(
                        class = "visual-caption",
                        "A darker diagonal means clearer classification. Off-diagonal cells show which occupancy classes still get mixed up."
                      )
                    )
                  )
                )
              )
            )
          ),
          card(
            class = "insight-card metric-panel-card",
            card_header("Key Metrics"),
            card_body(
              div(
                class = "visual-metric-strip",
                uiOutput("tree_snapshot_ui")
              )
            )
          )
        )
      )
    )
  ),
  nav_panel(
    "Random Forest",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        div(
          class = "control-stack",
          card(
            class = "control-card",
            card_header("Model Initiation"),
            card_body(
              p(class = "control-note", "Pick the core variables and rebuild the forest. The main charts on the right update with the new ensemble."),
              selectizeInput(
                "rf_features",
                "Variables to include",
                choices = model_feature_choices,
                selected = default_rf_builder_features,
                multiple = TRUE,
                options = list(plugins = list("remove_button"), closeAfterSelect = FALSE)
              ),
              sliderInput("rf_train_ratio", "Train-test partition ratio", min = 0.55, max = 0.9, value = 0.8, step = 0.05),
              numericInput("rf_seed", "Random seed", value = 123, min = 1, step = 1),
              actionButton("rf_build", "Build / Refresh Forest", class = "btn-primary")
            )
          ),
          card(
            class = "control-card",
            card_header("Forest Tuning"),
            card_body(
              p(class = "control-note", "Keep only the important forest controls so the page stays compact and closer to the reference layout."),
              sliderInput("rf_num_trees", "Number of trees", min = 80, max = 400, value = 180, step = 20),
              sliderInput("rf_mtry", "Variables sampled per split", min = 1, max = max(1L, length(default_rf_builder_features)), value = min(3L, length(default_rf_builder_features)), step = 1),
              sliderInput("rf_min_node_size", "Minimum node size", min = 1, max = 12, value = 2, step = 1),
              sliderInput("rf_top_n", "Top variables to show", min = 1, max = rf_top_n_max, value = min(6L, rf_top_n_max)),
              actionButton("rf_tune", "Apply Tuning", class = "btn-outline-secondary")
            )
          ),
          card(
            class = "control-card",
            card_header("Display Filter"),
            card_body(
              p(class = "control-note", "These filters only affect the prediction comparison tab."),
              selectInput(
                "rf_result_filter",
                "Prediction rows",
                choices = c("All rows" = "all", "Misclassified only" = "misclassified", "Correct only" = "correct"),
                selected = "all"
              ),
              selectInput(
                "rf_class_filter",
                "Actual class filter",
                choices = occupancy_class_choices,
                selected = "All"
              )
            )
          )
        )
      ),
      div(
        class = "model-workbench",
        div(
          class = "model-insights",
          card(
            class = "insight-card",
            card_header("Random Forest Variable Importance"),
            card_body(plotOutput("rf_importance_plot", height = "280px"))
          ),
          card(
            class = "insight-card",
            card_header("OOB Accuracy"),
            card_body(plotOutput("rf_oob_plot", height = "240px"))
          ),
          card(
            class = "insight-card",
            card_header("Key Message"),
            card_body(div(class = "compact-note", uiOutput("rf_notes")))
          )
        ),
        div(
          class = "visual-column",
          card(
            class = "visual-card visual-dock",
            card_header("Random Forest"),
            card_body(
              div(
                class = "visual-tabset visual-stage",
                tabsetPanel(
                  type = "tabs",
                  tabPanel(
                    "Predicted vs Actual on Test Data",
                    div(
                      class = "visual-panel plot-stage",
                      plotOutput("rf_prediction_plot", height = "420px"),
                      p(
                        class = "visual-caption",
                        "Shaded red columns mark mismatch months. A longer vertical gap means the forest is predicting a clearly different class from the true one."
                      )
                    )
                  ),
                  tabPanel(
                    "Reference Export",
                    div(
                      class = "visual-panel plot-stage",
                      uiOutput("rf_reference_ui")
                    )
                  )
                ),
                div(
                  class = "visual-metric-strip",
                  uiOutput("rf_snapshot_ui")
                )
              )
            )
          )
        )
      )
    )
  ),
  nav_panel(
    "Compare Models",
    layout_column_wrap(
      width = 1 / 2,
      card(
        card_header("Accuracy and Kappa Comparison"),
        card_body(plotOutput("compare_metrics_plot", height = "340px"))
      ),
      card(
        card_header("Per-Class Recall"),
        card_body(plotOutput("compare_class_recall_plot", height = "340px"))
      ),
      card(
        card_header("Top Features by Model"),
        card_body(plotOutput("compare_importance_plot", height = "340px"))
      ),
      card(
        card_header("Model Selection Notes"),
        card_body(uiOutput("compare_notes"))
      ),
      card(
        card_header("Merged Metric Table"),
        card_body(DTOutput("compare_metrics_tbl"))
      )
    )
  ),
  nav_panel(
    "About",
    layout_column_wrap(
      width = 1 / 2,
      card(
        card_header("What This App Uses"),
        card_body(
          tags$ul(
            tags$li("decision_tree_metrics.csv"),
            tags$li("decision_tree_confusion_matrix.csv"),
            tags$li("decision_tree_variable_importance.csv"),
            tags$li("decision_tree_test_predictions.csv"),
            tags$li("random_forest_metrics.csv"),
            tags$li("random_forest_confusion_matrix.csv"),
            tags$li("random_forest_variable_importance.csv"),
            tags$li("random_forest_oob_accuracy.csv"),
            tags$li("random_forest_test_predictions.csv")
          )
        )
      ),
      card(
        card_header("How To Run"),
        card_body(
          tags$pre("shiny::runApp()\n# or\nshiny::runApp('path/to/this/shiny/folder')"),
          p("This folder is self-contained as long as `app.R`, `data/`, and `outputs/` stay together.")
        )
      ),
      card(
        card_header("Detected Paths"),
        card_body(
          p("Project directory"),
          div(class = "filepath", PROJECT_DIR),
          p("Outputs directory"),
          div(class = "filepath", MODEL_BUNDLE$output_dir)
        )
      ),
      card(
        card_header("Design Intent"),
        card_body(
          p("The overview page gives fast performance context."),
          p("The tree page emphasizes interpretability."),
          p("The forest page emphasizes predictive diagnostics."),
          p("The comparison page supports a final recommendation.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_data_tbl <- reactive({
    if (!has_rows(summary_tbl)) {
      return(tibble())
    }

    req(input$data_date_range)

    out <- summary_tbl %>%
      filter(
        date >= as.Date(input$data_date_range[1]),
        date <= as.Date(input$data_date_range[2])
      )

    if (length(input$data_split_filter) > 0) {
      out <- out %>% filter(dataset_split %in% input$data_split_filter)
    }

    out
  })

  observeEvent(input$rf_features, {
    selected_count <- max(1L, length(input$rf_features))
    current_mtry <- isolate(input$rf_mtry %||% min(3L, selected_count))

    updateSliderInput(
      session,
      "rf_mtry",
      max = selected_count,
      value = min(current_mtry, selected_count)
    )
  }, ignoreInit = TRUE)

  tree_live_bundle <- eventReactive(list(input$tree_build, input$tree_tune), {
    req(input$tree_train_ratio, input$tree_seed, input$tree_minsplit, input$tree_maxdepth, input$tree_cp)

    build_tree_model_bundle(
      data_tbl = live_model_tbl,
      features = input$tree_features,
      train_ratio = input$tree_train_ratio,
      seed = input$tree_seed,
      minsplit = input$tree_minsplit,
      maxdepth = input$tree_maxdepth,
      cp_value = input$tree_cp,
      auto_cp = isTRUE(input$tree_auto_cp)
    )
  }, ignoreNULL = FALSE)

  rf_live_bundle <- eventReactive(list(input$rf_build, input$rf_tune), {
    req(input$rf_train_ratio, input$rf_seed, input$rf_num_trees, input$rf_mtry, input$rf_min_node_size)

    build_rf_model_bundle(
      data_tbl = live_model_tbl,
      features = input$rf_features,
      train_ratio = input$rf_train_ratio,
      seed = input$rf_seed,
      num_trees = input$rf_num_trees,
      mtry = input$rf_mtry,
      min_node_size = input$rf_min_node_size
    )
  }, ignoreNULL = FALSE)

  tree_prediction_rows <- reactive({
    bundle <- tree_live_bundle()

    if (!is.null(bundle$error)) {
      return(tibble())
    }

    filter_prediction_rows(
      bundle$predictions,
      result_filter = input$tree_result_filter,
      class_filter = input$tree_class_filter
    )
  })

  rf_prediction_rows <- reactive({
    bundle <- rf_live_bundle()

    if (!is.null(bundle$error)) {
      return(tibble())
    }

    filter_prediction_rows(
      bundle$predictions,
      result_filter = input$rf_result_filter,
      class_filter = input$rf_class_filter
    )
  })

  output$overview_metrics <- renderDT({
    metrics_tbl <- bind_rows(
      metric_rows_for_model("Decision Tree", tree_accuracy, tree_kappa, tree_recall_tbl),
      metric_rows_for_model("Random Forest", rf_accuracy, rf_kappa, rf_recall_tbl)
    )

    datatable(metrics_tbl, options = list(dom = "t", pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  output$overview_data <- renderDT({
    preview_tbl <- MODEL_BUNDLE$data_tbl %>%
      mutate(date = as.character(date)) %>%
      transmute(
        date,
        hotel_occ,
        hotel_occ_level_tertile = occupancy_class_label(hotel_occ_level_tertile),
        dataset_split,
        visitor_arrivals,
        visitor_arrivals_china
      ) %>%
      head(12)

    datatable(preview_tbl, options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
  })

  output$data_trend_plot <- renderPlot({
    req(input$data_metric)
    plot_tbl <- filtered_data_tbl()
    validate(need(has_rows(plot_tbl), "No rows match the current Data Explorer filters."))
    build_trend_plot(plot_tbl, input$data_metric)
  })

  output$data_heatmap_plot <- renderPlot({
    req(input$data_metric)
    plot_tbl <- filtered_data_tbl()
    validate(need(has_rows(plot_tbl), "No rows match the current Data Explorer filters."))
    build_metric_heatmap(plot_tbl, input$data_metric)
  })

  output$data_explorer_note <- renderUI({
    if (identical(input$data_metric, "hotel_occ")) {
      return(
        tagList(
          p("This page now shows the raw hotel occupancy trend first, but keeps the three occupancy intervals as a background reference."),
          p(occupancy_band_note),
          p("The line chart shows how the actual series moves across the three intervals, while the heatmap uses the same intervals and writes the monthly occupancy value inside each tile.")
        )
      )
    }

    p("The line chart focuses on the full time path of the selected metric. Use the heatmap on the right to compare month-by-month intensity across years.")
  })

  output$data_heatmap_note <- renderUI({
    if (identical(input$data_metric, "hotel_occ")) {
      return(
        p("This second view keeps the three occupancy intervals visible while also showing the monthly value inside each cell.")
      )
    }

    p("This second view highlights seasonality and year-over-year intensity for the selected metric.")
  })

  output$data_explorer_tbl <- renderDT({
    preview_tbl <- filtered_data_tbl()

    if (!has_rows(preview_tbl)) {
      return(table_note("No rows match the current Data Explorer filters."))
    }

    preview_tbl <- preview_tbl %>%
      arrange(desc(date)) %>%
      transmute(
        Date = as.character(date),
        Split = dataset_split,
        Class = occupancy_class_label(hotel_occ_level_tertile),
        `Hotel Occ (%)` = round(hotel_occ, 1),
        Arrivals = round(visitor_arrivals),
        `China Share (%)` = round(china_share_pct, 1),
        `Average Stay` = round(avg_stay_monthly, 2)
      )

    datatable(preview_tbl, options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE)
  })

  output$recommendation_text <- renderUI({
    decision_text <- if (!is.na(tree_accuracy) && !is.na(rf_accuracy) && tree_accuracy >= rf_accuracy) {
      paste(
        "Decision Tree is the safer presentation choice here because it leads the forest by",
        format_gap_pp(accuracy_gap),
        "on holdout accuracy and is easier to explain."
      )
    } else if (!is.na(rf_accuracy) && !is.na(tree_accuracy)) {
      paste(
        "Random Forest leads on holdout accuracy by",
        format_gap_pp(-accuracy_gap),
        "so it is the better predictive choice if your focus is performance rather than interpretability."
      )
    } else {
      "Recommendation is unavailable until both model metric files are present."
    }

    tagList(
      p(decision_text),
      tags$ul(
        tags$li(paste("Low Occupancy recall is still weak:", "tree", format_metric(tree_low_recall), "| forest", format_metric(rf_low_recall))),
        tags$li(paste("High Occupancy recall remains stronger:", "tree", format_metric(tree_high_recall), "| forest", format_metric(rf_high_recall))),
        tags$li("Use the tree when you need transparent splits and classroom-friendly storytelling."),
        tags$li("Use the forest when you need stronger robustness checks such as OOB tracking and richer variable importance.")
      )
    )
  })

  output$tree_snapshot_ui <- renderUI({
    bundle <- tree_live_bundle()
    validate(need(is.null(bundle$error), bundle$error %||% "Tree model unavailable."))

    div(
      class = "snapshot-grid",
      snapshot_tile("Accuracy", format_metric(bundle$accuracy), "Test-set score", "#d86f45"),
      snapshot_tile("Kappa", format_decimal(bundle$kappa), "Agreement strength", "#d86f45"),
      snapshot_tile("Chosen CP", format_decimal(bundle$chosen_cp), "Cross-validated pruning", "#0f6b6f"),
      snapshot_tile("Leaf Nodes", as.character(bundle$leaf_count), "Final tree complexity", "#7f5a83")
    )
  })

  output$tree_recipe_ui <- renderUI({
    bundle <- tree_live_bundle()
    validate(need(is.null(bundle$error), bundle$error %||% "Tree model unavailable."))

    main_feature <- if (has_rows(bundle$importance_tbl)) {
      bundle$importance_tbl %>%
        arrange(desc(importance)) %>%
        pull(variable) %>%
        first(default = NA_character_) %>%
        display_feature_label()
    } else {
      "No dominant split yet"
    }

    tagList(
      div(class = "status-pill", "Live Model"),
      tags$dl(
        class = "recipe-list",
        tags$dt("Predictors"), tags$dd(feature_sentence(bundle$features)),
        tags$dt("Train / test"), tags$dd(paste(bundle$train_n, "/", bundle$test_n, "rows")),
        tags$dt("Partition"), tags$dd(paste0(percent(bundle$train_ratio, accuracy = 1), " train / ", percent(1 - bundle$train_ratio, accuracy = 1), " test")),
        tags$dt("Seed"), tags$dd(as.character(bundle$seed)),
        tags$dt("Chosen CP"), tags$dd(format_decimal(bundle$chosen_cp)),
        tags$dt("Tree size"), tags$dd(paste(bundle$node_count, "nodes,", bundle$leaf_count, "leaves")),
        tags$dt("Main split"), tags$dd(main_feature)
      ),
      div(
        class = "story-callout",
        paste("Presentation tip: start from", main_feature, "because it is driving the first big separation in the live tree.")
      )
    )
  })

  output$tree_metrics_tbl <- renderDT({
    bundle <- tree_live_bundle()

    if (!is.null(bundle$error)) {
      return(table_note(bundle$error))
    }

    metrics_tbl <- bind_rows(
      metric_rows_for_model("Decision Tree", bundle$accuracy, bundle$kappa, bundle$recall_tbl),
      tibble(
        Model = "Decision Tree",
        Metric = c("Chosen CP", "Leaf nodes", "Train rows", "Test rows"),
        Display = c(format_decimal(bundle$chosen_cp), as.character(bundle$leaf_count), as.character(bundle$train_n), as.character(bundle$test_n))
      )
    ) %>%
      select(Metric, Value = Display)

    datatable(metrics_tbl, options = list(dom = "t"), rownames = FALSE)
  })

  output$tree_conf_plot <- renderPlot({
    bundle <- tree_live_bundle()
    validate(need(is.null(bundle$error), bundle$error %||% "Tree model unavailable."))
    build_confusion_plot(bundle$conf_tbl, "Decision Tree Confusion Matrix", "#d86f45")
  })

  output$tree_importance_plot <- renderPlot({
    bundle <- tree_live_bundle()
    validate(need(is.null(bundle$error), bundle$error %||% "Tree model unavailable."))
    validate(need(has_rows(bundle$importance_tbl), "The current tree did not create a variable-importance ranking."))

    build_importance_plot(
      bundle$importance_tbl,
      value_col = "importance",
      title_text = "Decision Tree Variable Importance",
      top_n = min(input$tree_top_n, nrow(bundle$importance_tbl)),
      fill_color = "#d86f45"
    )
  })

  output$tree_notes <- renderUI({
    bundle <- tree_live_bundle()
    validate(need(is.null(bundle$error), bundle$error %||% "Tree model unavailable."))

    main_feature <- if (has_rows(bundle$importance_tbl)) {
      bundle$importance_tbl %>%
        arrange(desc(importance)) %>%
        pull(variable) %>%
        first(default = NA_character_) %>%
        display_feature_label()
    } else {
      "No dominant split"
    }

    hardest_row <- bundle$recall_tbl %>%
      filter(!is.na(recall)) %>%
      arrange(recall) %>%
      slice(1)

    best_row <- bundle$recall_tbl %>%
      filter(!is.na(recall)) %>%
      arrange(desc(recall)) %>%
      slice(1)

    hardest_text <- if (nrow(hardest_row) == 1) {
      paste(display_feature_label(hardest_row$class), "recall:", format_metric(hardest_row$recall))
    } else {
      "Recall breakdown unavailable."
    }

    best_text <- if (nrow(best_row) == 1) {
      paste(display_feature_label(best_row$class), "recall:", format_metric(best_row$recall))
    } else {
      "Recall breakdown unavailable."
    }

    tagList(
      p(paste("Main split:", main_feature, "| Strongest class:", best_text, "| Hardest class:", hardest_text)),
      p("Use the left CP path to explain why this final tree size was selected.")
    )
  })

  output$tree_primary_visual_ui <- renderUI({
    if (model_output_exists("decision_tree_visual_takehome2b_trim.png")) {
      return(
        model_output_image(
          "decision_tree_visual_takehome2b_trim.png",
          alt_text = "Decision tree visual from Take-Home-Exercise2b",
          title = NULL,
          caption = "This tree image comes directly from the Take-Home-Exercise2b.qmd analysis you referenced, so the displayed split path matches that report."
        )
      )
    }

    if (model_output_exists("decision_tree_visual_takehome2b_capture.png")) {
      return(
        model_output_image(
          "decision_tree_visual_takehome2b_capture.png",
          alt_text = "Decision tree visual from Take-Home-Exercise2b",
          title = NULL,
          caption = "This tree image comes directly from the Take-Home-Exercise2b.qmd analysis you referenced, so the displayed split path matches that report."
        )
      )
    }

    if (model_output_exists("decision_tree_visual_full_model_trim.png")) {
      return(
        model_output_image(
          "decision_tree_visual_full_model_trim.png",
          alt_text = "Full exploratory decision tree visual",
          title = NULL,
          caption = "This is the full tree from your earlier Take-Home 2 analysis before pruning, so the deeper split path remains visible."
        )
      )
    }

    if (model_output_exists("decision_tree_visual_full_model_capture.png")) {
      return(
        model_output_image(
          "decision_tree_visual_full_model_capture.png",
          alt_text = "Full exploratory decision tree visual",
          title = NULL,
          caption = "This is the full tree from your earlier Take-Home 2 analysis before pruning, so the deeper split path remains visible."
        )
      )
    }

    if (model_output_exists("decision_tree_visual_full_trim.png")) {
      return(
        model_output_image(
          "decision_tree_visual_full_trim.png",
          alt_text = "Full decision tree visual",
          title = NULL,
          caption = "This is the full Take-Home 2 tree visual, kept intact so the complete split structure remains visible."
        )
      )
    }

    if (model_output_exists("decision_tree_visual_full_capture.png")) {
      return(
        model_output_image(
          "decision_tree_visual_full_capture.png",
          alt_text = "Full decision tree visual",
          title = NULL,
          caption = "This is the full Take-Home 2 tree visual, kept intact so the complete split structure remains visible."
        )
      )
    }

    if (model_output_exists("decision_tree_visual_crop.png")) {
      return(
        model_output_image(
          "decision_tree_visual_crop.png",
          alt_text = "Decision tree visual",
          title = NULL,
          caption = "This compact version is used only when the full Take-Home 2 tree image is unavailable."
        )
      )
    }

    if (model_output_exists("decision_tree_static.png")) {
      return(
        model_output_image(
          "decision_tree_static.png",
          alt_text = "Decision tree visual",
          title = NULL,
          caption = "This static tree image is used when the Take-Home 2 visual image is unavailable."
        )
      )
    }

    if (model_output_exists("decision_tree_plot.png")) {
      return(
        model_output_image(
          "decision_tree_plot.png",
          alt_text = "Decision tree visual",
          title = NULL,
          caption = "This image shows the fuller decision path when the compact tree export is unavailable."
        )
      )
    }

    div(
      class = "visual-panel plot-stage",
      plotOutput("tree_live_plot", height = "420px"),
      p(
        class = "visual-caption",
        "The exported tree image is missing, so the app is showing the live rebuilt tree instead."
      )
    )
  })

  output$tree_live_plot <- renderPlot({
    bundle <- tree_live_bundle()
    validate(need(is.null(bundle$error), bundle$error %||% "Tree model unavailable."))
    validate(need(requireNamespace("rpart.plot", quietly = TRUE), "Package 'rpart.plot' is required for the live tree plot."))

    rpart.plot::prp(
      bundle$model,
      type = 4,
      extra = 104,
      under = TRUE,
      faclen = 0,
      varlen = 0,
      tweak = 1.08,
      fallen.leaves = TRUE,
      shadow.col = "#d9e6e6",
      branch = 0.35,
      roundint = FALSE,
      yesno = 2,
      box.palette = "GnBu",
      main = "Live Decision Tree for Occupancy Level"
    )
  })

  output$tree_prediction_plot <- renderPlot({
    pred_tbl <- tree_prediction_rows()
    validate(need(has_rows(pred_tbl), "No tree prediction rows match the selected filters."))
    build_prediction_plot(pred_tbl, "Decision Tree: Actual vs Predicted Class")
  })

  output$tree_confidence_plot <- renderPlot({
    pred_tbl <- tree_prediction_rows()
    validate(need(has_rows(pred_tbl), "No tree prediction rows match the selected filters."))
    build_confidence_plot(pred_tbl, "Decision Tree: Prediction Confidence", "#0f6b6f")
  })

  output$tree_cp_plot <- renderPlot({
    bundle <- tree_live_bundle()
    validate(need(is.null(bundle$error), bundle$error %||% "Tree model unavailable."))
    validate(need(has_rows(bundle$cp_tbl), "The current tree does not have a CP tuning table."))
    build_tree_cp_plot(bundle$cp_tbl, bundle$chosen_cp)
  })

  output$tree_cp_tbl <- renderDT({
    bundle <- tree_live_bundle()

    if (!is.null(bundle$error)) {
      return(table_note(bundle$error))
    }

    if (!has_rows(bundle$cp_tbl)) {
      return(table_note("The current tree does not have a CP tuning table."))
    }

    cp_tbl <- bundle$cp_tbl %>%
      mutate(Selected = if_else(abs(CP - bundle$chosen_cp) < 1e-9, "Yes", "")) %>%
      transmute(
        CP = number(CP, accuracy = 0.001),
        Splits = nsplit,
        `Rel. error` = round(rel_error, 3),
        `CV error` = round(xerror, 3),
        `CV std.` = round(xstd, 3),
        Selected
      )

    datatable(cp_tbl, options = list(dom = "t", pageLength = 4, scrollX = TRUE, autoWidth = TRUE), rownames = FALSE)
  })

  output$tree_reference_ui <- renderUI({
    html_exists <- model_output_exists("decision_tree_visual.html")
    png_exists <- model_output_exists("decision_tree_static.png")
    plot_exists <- model_output_exists("decision_tree_plot.png")

    if (!html_exists && !png_exists && !plot_exists) {
      return(div(class = "visual-missing", "No exported decision tree files were found in `outputs/`."))
    }

    div(
      class = "stacked-visuals",
      if (html_exists) {
        div(
          class = "visual-panel",
          h4("Previously exported interactive HTML"),
          tags$iframe(class = "tree-frame", src = "model_outputs/decision_tree_visual.html"),
          p(class = "visual-caption", "This is the earlier exported artifact kept for reference. The live tree on the first tab is the version that updates with your controls.")
        )
      },
      if (png_exists) {
        model_output_image(
          "decision_tree_static.png",
          alt_text = "Exported decision tree",
          title = "Previously exported static tree",
          caption = "Use this tab if you want to compare the live rebuilt tree against the earlier saved artifact."
        )
      },
      if (plot_exists) {
        model_output_image(
          "decision_tree_plot.png",
          alt_text = "Decision tree rule summary",
          title = "Rule summary export",
          caption = "This report-style diagram is still available as a backup presentation visual."
        )
      }
    )
  })

  output$tree_predictions_tbl <- renderDT({
    preview_tbl <- tree_prediction_rows()

    if (!has_rows(preview_tbl)) {
      return(table_note("No tree prediction rows match the selected filters."))
    }

    preview_tbl <- preview_tbl %>%
      transmute(
        Date = as.character(date),
        `Hotel Occ (%)` = if ("hotel_occ" %in% names(preview_tbl)) round(hotel_occ, 1) else NA_real_,
        Actual = occupancy_class_label(actual),
        Predicted = occupancy_class_label(predicted_class),
        Correct = if_else(correct, "Yes", "No"),
        Confidence = if_else(is.na(confidence), "N/A", percent(confidence, accuracy = 0.1))
      ) %>%
      head(input$tree_preview_n)

    datatable(preview_tbl, options = list(pageLength = input$tree_preview_n, scrollX = TRUE), rownames = FALSE)
  })

  output$rf_snapshot_ui <- renderUI({
    bundle <- rf_live_bundle()
    validate(need(is.null(bundle$error), bundle$error %||% "Random forest unavailable."))

    div(
      class = "snapshot-grid",
      snapshot_tile("Accuracy", format_metric(bundle$accuracy), "Test-set score", "#0f6b6f"),
      snapshot_tile("Kappa", format_decimal(bundle$kappa), "Agreement strength", "#0f6b6f"),
      snapshot_tile("Best OOB", format_metric(max_or_na(bundle$oob_tbl$oob_accuracy)), "Internal validation", "#2f7d4f"),
      snapshot_tile("Trees", as.character(bundle$num_trees), "Ensemble size", "#7f5a83")
    )
  })

  output$rf_recipe_ui <- renderUI({
    bundle <- rf_live_bundle()
    validate(need(is.null(bundle$error), bundle$error %||% "Random forest unavailable."))

    top_feature <- if (has_rows(bundle$importance_tbl)) {
      bundle$importance_tbl %>%
        arrange(desc(Overall)) %>%
        pull(variable) %>%
        first(default = NA_character_) %>%
        display_feature_label()
    } else {
      "No dominant feature yet"
    }

    tagList(
      div(class = "status-pill", "Live Model"),
      tags$dl(
        class = "recipe-list",
        tags$dt("Predictors"), tags$dd(feature_sentence(bundle$features)),
        tags$dt("Train / test"), tags$dd(paste(bundle$train_n, "/", bundle$test_n, "rows")),
        tags$dt("Partition"), tags$dd(paste0(percent(bundle$train_ratio, accuracy = 1), " train / ", percent(1 - bundle$train_ratio, accuracy = 1), " test")),
        tags$dt("Seed"), tags$dd(as.character(bundle$seed)),
        tags$dt("Trees"), tags$dd(as.character(bundle$num_trees)),
        tags$dt("mtry"), tags$dd(as.character(bundle$mtry)),
        tags$dt("Min node"), tags$dd(as.character(bundle$min_node_size)),
        tags$dt("Top feature"), tags$dd(top_feature)
      ),
      div(
        class = "story-callout",
        paste("Presentation tip: use", top_feature, "as the bridge between the forest overview and the final prediction diagnostics.")
      )
    )
  })

  output$rf_metrics_tbl <- renderDT({
    bundle <- rf_live_bundle()

    if (!is.null(bundle$error)) {
      return(table_note(bundle$error))
    }

    metrics_tbl <- bind_rows(
      metric_rows_for_model("Random Forest", bundle$accuracy, bundle$kappa, bundle$recall_tbl),
      tibble(
        Model = "Random Forest",
        Metric = c("Trees", "mtry", "Min node size", "Best OOB"),
        Display = c(as.character(bundle$num_trees), as.character(bundle$mtry), as.character(bundle$min_node_size), format_metric(max_or_na(bundle$oob_tbl$oob_accuracy)))
      )
    ) %>%
      select(Metric, Value = Display)

    datatable(metrics_tbl, options = list(dom = "t"), rownames = FALSE)
  })

  output$rf_conf_plot <- renderPlot({
    bundle <- rf_live_bundle()
    validate(need(is.null(bundle$error), bundle$error %||% "Random forest unavailable."))
    build_confusion_plot(bundle$conf_tbl, "Random Forest Confusion Matrix", "#0f6b6f")
  })

  output$rf_conf_plot_large <- renderPlot({
    bundle <- rf_live_bundle()
    validate(need(is.null(bundle$error), bundle$error %||% "Random forest unavailable."))
    build_confusion_plot(bundle$conf_tbl, "Random Forest Confusion Matrix", "#0f6b6f")
  })

  output$rf_importance_plot <- renderPlot({
    bundle <- rf_live_bundle()
    validate(need(is.null(bundle$error), bundle$error %||% "Random forest unavailable."))
    validate(need(has_rows(bundle$importance_tbl), "The current forest did not create a variable-importance ranking."))

    build_importance_plot(
      bundle$importance_tbl,
      value_col = "Overall",
      title_text = "Random Forest Variable Importance",
      top_n = min(input$rf_top_n, nrow(bundle$importance_tbl)),
      fill_color = "#0f6b6f"
    )
  })

  output$rf_importance_plot_large <- renderPlot({
    bundle <- rf_live_bundle()
    validate(need(is.null(bundle$error), bundle$error %||% "Random forest unavailable."))
    validate(need(has_rows(bundle$importance_tbl), "The current forest did not create a variable-importance ranking."))

    build_importance_plot(
      bundle$importance_tbl,
      value_col = "Overall",
      title_text = "Random Forest Variable Importance",
      top_n = min(input$rf_top_n, nrow(bundle$importance_tbl)),
      fill_color = "#0f6b6f"
    )
  })

  output$rf_oob_plot <- renderPlot({
    bundle <- rf_live_bundle()
    validate(need(is.null(bundle$error), bundle$error %||% "Random forest unavailable."))
    validate(need(has_rows(bundle$oob_tbl), "The current forest does not have an OOB trace."))
    build_oob_plot(bundle$oob_tbl)
  })

  output$rf_oob_plot_large <- renderPlot({
    bundle <- rf_live_bundle()
    validate(need(is.null(bundle$error), bundle$error %||% "Random forest unavailable."))
    validate(need(has_rows(bundle$oob_tbl), "The current forest does not have an OOB trace."))
    build_oob_plot(bundle$oob_tbl)
  })

  output$rf_recall_plot <- renderPlot({
    bundle <- rf_live_bundle()
    validate(need(is.null(bundle$error), bundle$error %||% "Random forest unavailable."))
    validate(need(has_rows(bundle$recall_tbl), "Per-class recall is unavailable for the current forest."))
    build_single_recall_plot(bundle$recall_tbl, "Random Forest Recall by Class", "#0f6b6f")
  })

  output$rf_notes <- renderUI({
    bundle <- rf_live_bundle()
    validate(need(is.null(bundle$error), bundle$error %||% "Random forest unavailable."))

    top_feature <- if (has_rows(bundle$importance_tbl)) {
      bundle$importance_tbl %>%
        arrange(desc(Overall)) %>%
        pull(variable) %>%
        first(default = NA_character_) %>%
        display_feature_label()
    } else {
      "No dominant feature"
    }

    hardest_row <- bundle$recall_tbl %>%
      filter(!is.na(recall)) %>%
      arrange(recall) %>%
      slice(1)

    hardest_text <- if (nrow(hardest_row) == 1) {
      paste(display_feature_label(hardest_row$class), "recall:", format_metric(hardest_row$recall))
    } else {
      "Recall breakdown unavailable."
    }

    div(
      class = "message-grid",
      div(
        class = "message-tile",
        p(class = "message-label", "Top driver"),
        p(class = "message-text", top_feature)
      ),
      div(
        class = "message-tile",
        p(class = "message-label", "Best OOB"),
        p(class = "message-text", format_metric(max_or_na(bundle$oob_tbl$oob_accuracy)))
      ),
      div(
        class = "message-tile",
        p(class = "message-label", "Hardest class"),
        p(class = "message-text", hardest_text)
      ),
      div(
        class = "message-tile",
        p(class = "message-label", "How to explain"),
        p(class = "message-text", "Use importance first, then move to the prediction comparison to show where the ensemble still makes mistakes.")
      )
    )
  })

  output$rf_prediction_plot <- renderPlot({
    pred_tbl <- rf_prediction_rows()
    validate(need(has_rows(pred_tbl), "No random forest prediction rows match the selected filters."))
    build_prediction_plot(pred_tbl, "Random Forest: Actual vs Predicted Class")
  })

  output$rf_confidence_plot <- renderPlot({
    pred_tbl <- rf_prediction_rows()
    validate(need(has_rows(pred_tbl), "No random forest prediction rows match the selected filters."))
    build_confidence_plot(pred_tbl, "Random Forest: Prediction Confidence", "#0f6b6f")
  })

  output$rf_reference_ui <- renderUI({
    reference_files <- c(
      "random_forest_importance.png",
      "random_forest_oob_accuracy.png",
      "random_forest_actual_vs_predicted.png"
    )

    existing_files <- reference_files[file.exists(file.path(MODEL_BUNDLE$output_dir, reference_files))]

    if (length(existing_files) == 0) {
      return(div(class = "visual-missing", "No exported random forest files were found in `outputs/`."))
    }

    div(
      class = "visual-grid",
      if ("random_forest_importance.png" %in% existing_files) {
        model_output_image(
          "random_forest_importance.png",
          alt_text = "Exported random forest importance",
          title = "Saved importance export",
          caption = "This is the earlier saved output retained for report consistency."
        )
      },
      if ("random_forest_oob_accuracy.png" %in% existing_files) {
        model_output_image(
          "random_forest_oob_accuracy.png",
          alt_text = "Exported random forest OOB",
          title = "Saved OOB export",
          caption = "Compare this artifact with the live OOB trace on the first tab."
        )
      },
      if ("random_forest_actual_vs_predicted.png" %in% existing_files) {
        model_output_image(
          "random_forest_actual_vs_predicted.png",
          alt_text = "Exported random forest actual vs predicted",
          title = "Saved prediction export",
          caption = "This older image is kept as a reference export."
        )
      }
    )
  })

  output$rf_predictions_tbl <- renderDT({
    preview_tbl <- rf_prediction_rows()

    if (!has_rows(preview_tbl)) {
      return(table_note("No random forest prediction rows match the selected filters."))
    }

    preview_tbl <- preview_tbl %>%
      transmute(
        Date = as.character(date),
        `Hotel Occ (%)` = if ("hotel_occ" %in% names(preview_tbl)) round(hotel_occ, 1) else NA_real_,
        Actual = occupancy_class_label(actual),
        Predicted = occupancy_class_label(predicted_class),
        Correct = if_else(correct, "Yes", "No"),
        Confidence = if_else(is.na(confidence), "N/A", percent(confidence, accuracy = 0.1))
      ) %>%
      head(input$rf_preview_n)

    datatable(preview_tbl, options = list(pageLength = input$rf_preview_n, scrollX = TRUE), rownames = FALSE)
  })

  output$compare_metrics_plot <- renderPlot({
    build_metric_comparison_plot(tree_accuracy, tree_kappa, rf_accuracy, rf_kappa)
  })

  output$compare_class_recall_plot <- renderPlot({
    validate(need(has_rows(all_recall_tbl), "Per-class recall is unavailable."))
    build_class_recall_plot(all_recall_tbl)
  })

  output$compare_importance_plot <- renderPlot({
    tree_tbl <- MODEL_BUNDLE$tree_importance %>%
      transmute(model = "Decision Tree", variable = concept_feature_label(variable), score = importance)

    rf_tbl <- MODEL_BUNDLE$rf_importance %>%
      transmute(model = "Random Forest", variable = concept_feature_label(variable), score = Overall)

    bind_rows(tree_tbl, rf_tbl) %>%
      group_by(model, variable) %>%
      summarise(score = sum(score), .groups = "drop") %>%
      group_by(model) %>%
      slice_max(order_by = score, n = 4, with_ties = FALSE) %>%
      ungroup() %>%
      ggplot(aes(x = reorder(variable, score), y = score, fill = model)) +
      geom_col(position = position_dodge(width = 0.72), width = 0.64) +
      coord_flip() +
      scale_fill_manual(values = c("Decision Tree" = "#d86f45", "Random Forest" = "#0f6b6f")) +
      labs(
        title = "Conceptual Feature Importance by Model",
        x = NULL,
        y = "Importance",
        fill = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "top",
        panel.grid.minor = element_blank()
      )
  })

  output$compare_notes <- renderUI({
    tagList(
      p(paste("Best holdout accuracy:", best_model)),
      tags$ul(
        tags$li(paste("Holdout accuracy gap:", if (is.na(accuracy_gap)) "N/A" else format_gap_pp(accuracy_gap), "(tree minus forest)")),
        tags$li(paste("Decision Tree accuracy:", format_metric(tree_accuracy), "| kappa:", format_decimal(tree_kappa))),
        tags$li(paste("Random Forest accuracy:", format_metric(rf_accuracy), "| kappa:", format_decimal(rf_kappa))),
        tags$li(paste("High Occupancy recall:", "tree", format_metric(tree_high_recall), "| forest", format_metric(rf_high_recall))),
        tags$li(paste("Low Occupancy recall:", "tree", format_metric(tree_low_recall), "| forest", format_metric(rf_low_recall))),
        tags$li("Decision Tree gives clearer rule-based storytelling for class presentations."),
        tags$li("Random Forest adds stronger diagnostic depth through OOB accuracy and expanded feature importance.")
      )
    )
  })

  output$compare_metrics_tbl <- renderDT({
    compare_tbl <- bind_rows(
      metric_rows_for_model("Decision Tree", tree_accuracy, tree_kappa, tree_recall_tbl),
      metric_rows_for_model("Random Forest", rf_accuracy, rf_kappa, rf_recall_tbl)
    )

    datatable(compare_tbl, options = list(dom = "t"), rownames = FALSE)
  })
}

shinyApp(ui, server)
