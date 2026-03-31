required_packages <- data.frame(
  package = c(
    "shiny",
    "bslib",
    "readxl",
    "dplyr",
    "tidyr",
    "purrr",
    "lubridate",
    "ggplot2",
    "plotly",
    "DT",
    "cluster",
    "forecast",
    "parsnip",
    "rsample",
    "tidymodels",
    "timetk",
    "modeltime",
    "tsibble",
    "feasts",
    "yardstick"
  ),
  purpose = c(
    "App runtime",
    "App theming",
    "Excel ingestion",
    "Data wrangling",
    "Data reshaping",
    "Iteration helpers",
    "Date handling",
    "Charting",
    "Interactive charting",
    "Interactive table",
    "Clustering metrics",
    "Baseline forecasting models",
    "Forecast model specification layer",
    "Time-aware train/test split",
    "Forecasting workflow framework",
    "Time-series visualisation helpers",
    "Modeltime forecasting pipeline",
    "Time-indexed tibble support",
    "Decomposition and seasonal diagnostics",
    "Forecast accuracy metrics"
  ),
  stringsAsFactors = FALSE
)

installed <- rownames(installed.packages())
required_packages$installed <- required_packages$package %in% installed

cran_packages <- rownames(available.packages(repos = "https://cloud.r-project.org"))
required_packages$cran_available <- required_packages$package %in% cran_packages

out_path <- file.path("artifacts", "tables", "package_audit.csv")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
write.csv(required_packages, out_path, row.names = FALSE)

source(file.path("app", "R", "data_utils.R"))
stack_status <- forecast_stack_status()

runtime_audit <- data.frame(
  check = c(
    "fallback_ready",
    "modeltime_ready",
    "preferred_engine",
    "missing_modeltime_packages",
    "note"
  ),
  value = c(
    stack_status$fallback_ready,
    stack_status$modeltime_ready,
    stack_status$preferred_engine,
    if (length(stack_status$missing_modeltime_packages) == 0) {
      "None"
    } else {
      paste(stack_status$missing_modeltime_packages, collapse = ", ")
    },
    "Package audit records installation and CRAN availability only. Prototype render and smoke tests remain the runtime proof."
  ),
  stringsAsFactors = FALSE
)

runtime_path <- file.path("artifacts", "tables", "forecast_runtime_audit.csv")
write.csv(runtime_audit, runtime_path, row.names = FALSE)

message("Saved package audit to: ", out_path)
message("Saved forecast runtime audit to: ", runtime_path)
