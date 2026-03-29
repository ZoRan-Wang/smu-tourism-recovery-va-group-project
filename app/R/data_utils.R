library(dplyr)
library(readxl)
library(tidyr)
library(purrr)
library(lubridate)
library(forecast)

resolve_data_path <- function() {
  candidates <- c(
    "C:/Users/12697/Downloads/Name your insight (4).xlsx",
    file.path("..", "data", "raw", "Name your insight (4).xlsx"),
    file.path("data", "raw", "Name your insight (4).xlsx"),
    file.path("..", "data", "raw", "tourism_four_part_analysis_ready.xlsx"),
    file.path("data", "raw", "tourism_four_part_analysis_ready.xlsx")
  )

  path <- candidates[file.exists(candidates)][1]

  if (is.na(path) || !nzchar(path)) {
    stop("Dataset not found. Expected data/raw/Name your insight (4).xlsx")
  }

  path
}

is_country_arrival_label <- function(label) {
  excluded_suffixes <- c(
    "ASEAN", "West Asia", "North Asia", "Americas", "Africa",
    "8-10 Days", "11-14 Days", "15 Days & Over"
  )

  startsWith(label, "Visitor Arrivals:") &&
    !label %in% paste("Visitor Arrivals:", excluded_suffixes)
}

read_tourism_metadata <- function(path = resolve_data_path()) {
  meta_raw <- read_excel(
    path,
    sheet = "My Series",
    range = "A1:BZ9",
    col_names = FALSE
  )

  tibble(
    col_id = seq_len(ncol(meta_raw)),
    label = as.character(unlist(meta_raw[1, ])),
    region = as.character(unlist(meta_raw[2, ])),
    frequency = as.character(unlist(meta_raw[4, ])),
    unit = as.character(unlist(meta_raw[5, ])),
    source = as.character(unlist(meta_raw[6, ])),
    series_id = as.character(unlist(meta_raw[8, ]))
  ) |>
    mutate(
      label = trimws(label),
      frequency = trimws(frequency),
      unit = trimws(unit),
      source = trimws(source),
      series_key = make.names(tolower(label), unique = TRUE)
    ) |>
    filter(!is.na(label), nzchar(label))
}

load_tourism_long_data <- function(path = resolve_data_path(), only_monthly = FALSE) {
  metadata <- read_tourism_metadata(path)
  max_cols <- max(metadata$col_id)

  raw <- read_excel(
    path,
    sheet = "My Series",
    skip = 29,
    col_names = FALSE
  ) |>
    select(seq_len(max_cols))

  names(raw) <- paste0("col_", seq_len(ncol(raw)))

  long_df <- raw |>
    mutate(date = as.Date(.data$col_1)) |>
    pivot_longer(
      cols = -c(date, col_1),
      names_to = "column_name",
      values_to = "value"
    ) |>
    mutate(col_id = as.integer(sub("col_", "", column_name))) |>
    left_join(metadata, by = "col_id") |>
    filter(
      !is.na(date),
      !is.na(label),
      !is.na(value)
    ) |>
    select(date, label, series_key, frequency, unit, source, series_id, value)

  if (only_monthly) {
    long_df <- long_df |>
      filter(frequency == "Monthly")
  }

  long_df |>
    arrange(label, date)
}

build_monthly_feature_table <- function(long_monthly) {
  feature_map <- c(
    visitor_arrivals = "Visitor Arrivals",
    visitor_arrivals_china = "Visitor Arrivals: China",
    visitor_arrivals_malaysia = "Visitor Arrivals: Malaysia",
    visitor_arrivals_india = "Visitor Arrivals: India",
    visitor_arrivals_indonesia = "Visitor Arrivals: Indonesia",
    visitor_arrivals_australia = "Visitor Arrivals: Australia",
    visitor_arrivals_asean = "Visitor Arrivals: ASEAN",
    visitor_arrivals_north_asia = "Visitor Arrivals: North Asia",
    hotel_occ = "Hotel Room Occupancy Rate",
    avg_stay_monthly = "Average Length of Stay",
    number_of_hotels = "Number of Hotels",
    total_room_revenue = "Total Room Revenue"
  )

  wide <- imap(feature_map, function(series_label, series_name) {
    long_monthly |>
      filter(label == series_label) |>
      select(date, value) |>
      rename(!!series_name := value)
  }) |>
    reduce(full_join, by = "date")

  wide |>
    mutate(
      period = case_when(
        date <= as.Date("2020-01-01") ~ "pre_covid",
        date <= as.Date("2021-12-01") ~ "covid_shock",
        TRUE ~ "recovery"
      ),
      period = factor(period, levels = c("pre_covid", "covid_shock", "recovery")),
      year = year(date),
      month = month(date),
      china_share = if_else(visitor_arrivals > 0, visitor_arrivals_china / visitor_arrivals, NA_real_),
      avg_stay_monthly_capped = pmin(
        avg_stay_monthly,
        quantile(avg_stay_monthly, probs = 0.95, na.rm = TRUE)
      )
    ) |>
    arrange(date)
}

load_tourism_data <- function(path = resolve_data_path()) {
  long_monthly <- load_tourism_long_data(path, only_monthly = TRUE)
  monthly_features <- build_monthly_feature_table(long_monthly)

  list(
    long_monthly = long_monthly,
    monthly_features = monthly_features,
    metadata = read_tourism_metadata(path)
  )
}

list_forecast_series <- function(long_monthly, min_obs = 24) {
  long_monthly |>
    group_by(label, unit) |>
    summarise(
      n_obs = sum(!is.na(value)),
      start_date = min(date),
      end_date = max(date),
      .groups = "drop"
    ) |>
    filter(n_obs >= min_obs) |>
    arrange(label)
}

list_country_arrival_series <- function(long_monthly, min_obs = 24) {
  list_forecast_series(long_monthly, min_obs = min_obs) |>
    filter(vapply(label, is_country_arrival_label, logical(1)))
}

prepare_supporting_indicator_context <- function(
    long_monthly,
    support_labels = c(
      "Hotel Room Occupancy Rate",
      "Average Length of Stay",
      "Total Room Revenue"
    )) {
  long_monthly |>
    filter(label %in% support_labels) |>
    arrange(label, date) |>
    group_by(label) |>
    mutate(
      normalized_value = as.numeric(scale(value)),
      value_change = value - lag(value, 12)
    ) |>
    ungroup()
}

prepare_country_context_panel <- function(
    long_monthly,
    country_label,
    support_labels = c(
      "Hotel Room Occupancy Rate",
      "Average Length of Stay",
      "Total Room Revenue"
    )) {
  country_series <- prepare_forecast_series(long_monthly, country_label) |>
    transmute(
      date,
      label = country_label,
      value,
      normalized_value = as.numeric(scale(value))
    )

  support_series <- prepare_supporting_indicator_context(long_monthly, support_labels) |>
    select(date, label, value, normalized_value)

  bind_rows(country_series, support_series) |>
    filter(!is.na(normalized_value))
}

prepare_cluster_data <- function(df, periods, features, scale_features) {
  filtered <- df |>
    filter(period %in% periods) |>
    select(date, period, all_of(features))

  complete <- filtered |>
    filter(if_all(all_of(features), ~ !is.na(.x)))

  matrix <- as.matrix(complete[, features, drop = FALSE])

  if (scale_features) {
    matrix <- scale(matrix)
  }

  list(
    metadata = complete,
    matrix = matrix
  )
}

prepare_forecast_series <- function(long_monthly, series_label) {
  long_monthly |>
    filter(label == series_label) |>
    arrange(date) |>
    mutate(value = as.numeric(value))
}

run_forecast_models <- function(series_df, horizon = 12, model_choice = c("ARIMA", "ETS")) {
  model_choice <- match.arg(model_choice)

  validate_series <- nrow(series_df) > horizon + 12
  if (!validate_series) {
    stop("Selected series is too short for the requested forecast horizon.")
  }

  train_n <- nrow(series_df) - horizon
  train_df <- series_df |> slice_head(n = train_n)
  test_df <- series_df |> slice_tail(n = horizon)

  start_year <- year(min(train_df$date))
  start_month <- month(min(train_df$date))

  train_ts <- ts(train_df$value, start = c(start_year, start_month), frequency = 12)
  test_ts <- ts(test_df$value, frequency = 12)

  baseline_fit <- forecast::snaive(train_ts, h = horizon)

  model_fit <- switch(
    model_choice,
    ARIMA = forecast::forecast(forecast::auto.arima(train_ts), h = horizon),
    ETS = forecast::forecast(forecast::ets(train_ts), h = horizon)
  )

  calc_metrics <- function(actual, predicted) {
    tibble(
      RMSE = sqrt(mean((actual - predicted)^2, na.rm = TRUE)),
      MAE = mean(abs(actual - predicted), na.rm = TRUE),
      MAPE = mean(abs((actual - predicted) / actual), na.rm = TRUE) * 100
    )
  }

  accuracy_tbl <- bind_rows(
    calc_metrics(as.numeric(test_df$value), as.numeric(baseline_fit$mean)) |>
      mutate(model = "Seasonal Naive", .before = 1),
    calc_metrics(as.numeric(test_df$value), as.numeric(model_fit$mean)) |>
      mutate(model = model_choice, .before = 1)
  ) |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))

  fitted_df <- bind_rows(
    train_df |> mutate(series = "Training"),
    test_df |> mutate(series = "Test")
  )

  forecast_df <- bind_rows(
    tibble(
      date = test_df$date,
      value = as.numeric(baseline_fit$mean),
      series = "Seasonal Naive"
    ),
    tibble(
      date = test_df$date,
      value = as.numeric(model_fit$mean),
      series = model_choice
    )
  )

  list(
    train = train_df,
    test = test_df,
    fitted_df = fitted_df,
    forecast_df = forecast_df,
    accuracy_tbl = accuracy_tbl
  )
}

run_modeltime_forecast_workflow <- function(series_df, horizon = 12) {
  required_pkgs <- c("rsample", "parsnip", "modeltime", "timetk", "yardstick")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    stop(
      "Missing forecasting packages: ",
      paste(missing_pkgs, collapse = ", "),
      ". Please install them before running the modeltime workflow."
    )
  }

  suppressPackageStartupMessages({
    library(modeltime)
    library(parsnip)
    library(rsample)
    library(yardstick)
  })

  validate_series <- nrow(series_df) > horizon + 12
  if (!validate_series) {
    stop("Selected series is too short for the requested forecast horizon.")
  }

  split_prop <- (nrow(series_df) - horizon) / nrow(series_df)
  splits <- rsample::initial_time_split(series_df, prop = split_prop)
  training_df <- rsample::training(splits)
  testing_df <- rsample::testing(splits)

  model_fit_ets <- modeltime::exp_smoothing() |>
    parsnip::set_engine("ets") |>
    parsnip::fit(value ~ date, data = training_df)

  model_fit_arima <- modeltime::arima_reg() |>
    parsnip::set_engine("auto_arima") |>
    parsnip::fit(value ~ date, data = training_df)

  models_tbl <- modeltime::modeltime_table(
    model_fit_ets,
    model_fit_arima
  )

  calibration_tbl <- models_tbl |>
    modeltime::modeltime_calibrate(new_data = testing_df)

  accuracy_tbl <- calibration_tbl |>
    modeltime::modeltime_accuracy() |>
    mutate(
      .model_desc = dplyr::recode(
        .model_desc,
        "ETS" = "ETS (Modeltime)",
        .default = .model_desc
      )
    )

  baseline_fit <- forecast::snaive(
    ts(
      training_df$value,
      start = c(lubridate::year(min(training_df$date)), lubridate::month(min(training_df$date))),
      frequency = 12
    ),
    h = nrow(testing_df)
  )

  baseline_metrics <- tibble(
    .model_id = 0L,
    .model_desc = "Seasonal Naive",
    .type = "Test",
    mae = yardstick::mae_vec(testing_df$value, as.numeric(baseline_fit$mean)),
    mape = yardstick::mape_vec(testing_df$value, as.numeric(baseline_fit$mean)),
    mase = NA_real_,
    smape = yardstick::smape_vec(testing_df$value, as.numeric(baseline_fit$mean)),
    rmse = yardstick::rmse_vec(testing_df$value, as.numeric(baseline_fit$mean)),
    rsq = yardstick::rsq_vec(testing_df$value, as.numeric(baseline_fit$mean))
  )

  full_accuracy_tbl <- bind_rows(baseline_metrics, accuracy_tbl) |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))

  calibration_forecast_tbl <- calibration_tbl |>
    modeltime::modeltime_forecast(
      new_data = testing_df,
      actual_data = series_df,
      keep_data = TRUE
    )

  best_model_id <- accuracy_tbl |>
    arrange(rmse) |>
    slice(1) |>
    pull(.model_id)

  refit_tbl <- models_tbl |>
    filter(.model_id == best_model_id) |>
    modeltime::modeltime_refit(data = series_df)

  future_forecast_tbl <- refit_tbl |>
    modeltime::modeltime_forecast(
      h = horizon,
      actual_data = series_df,
      keep_data = TRUE
    )

  list(
    splits = splits,
    training = training_df,
    testing = testing_df,
    models_tbl = models_tbl,
    calibration_tbl = calibration_tbl,
    accuracy_tbl = full_accuracy_tbl,
    calibration_forecast_tbl = calibration_forecast_tbl,
    refit_tbl = refit_tbl,
    future_forecast_tbl = future_forecast_tbl
  )
}
