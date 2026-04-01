library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

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

clean_country_series_label <- function(x) {
  x <- sub("^Visitor Arrivals:\\s*", "", x)
  x <- sub("\\.\\d+$", "", x)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x
}

project_root <- find_project_root()
raw_path <- file.path(project_root, "data", "raw", "visitor_arrivals_full_dataset.xlsx")
processed_dir <- file.path(project_root, "data", "processed")

if (!file.exists(raw_path)) {
  stop("Raw workbook not found at data/raw/visitor_arrivals_full_dataset.xlsx")
}

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

meta_raw <- read_excel(
  raw_path,
  sheet = "My Series",
  range = "A1:BZ8",
  col_names = FALSE
)

metadata <- tibble(
  column_index = seq_len(ncol(meta_raw)),
  raw_name = as.character(unlist(meta_raw[1, ])),
  frequency = as.character(unlist(meta_raw[4, ])),
  unit = as.character(unlist(meta_raw[5, ])),
  source = as.character(unlist(meta_raw[6, ])),
  series_id = as.character(unlist(meta_raw[8, ]))
) |>
  mutate(
    raw_name = trimws(raw_name),
    frequency = trimws(frequency),
    unit = trimws(unit),
    source = trimws(source)
  ) |>
  filter(
    frequency == "Monthly",
    grepl("^Visitor Arrivals:", raw_name),
    !grepl("^Visitor Arrivals: (Air|Sea|Land)(:|$)", raw_name),
    !grepl(
      "^Visitor Arrivals: (Total|ASEAN|Americas|Asia|Africa|Europe|North Asia|South Asia|West Asia|Oceania|Scandinavia|Age:|sa:)",
      raw_name
    ),
    !grepl("^Tourist Arrivals:", raw_name)
  ) |>
  mutate(
    country = clean_country_series_label(raw_name),
    series_key = make.names(tolower(country), unique = TRUE),
    unit = ifelse(is.na(unit) | unit == "", "Persons", unit),
    source = ifelse(is.na(source) | source == "", "STB / CEIC", source)
  )

raw_obs <- read_excel(
  raw_path,
  sheet = "My Series",
  skip = 29,
  col_names = FALSE,
  .name_repair = "minimal"
)

max_cols <- max(metadata$column_index)
raw_obs <- raw_obs[, seq_len(max_cols), drop = FALSE]
names(raw_obs) <- paste0("col_", seq_len(ncol(raw_obs)))

arrivals_long <- raw_obs |>
  mutate(date = as.Date(.data$col_1)) |>
  pivot_longer(
    cols = -c(date, col_1),
    names_to = "column_name",
    values_to = "arrivals"
  ) |>
  mutate(column_index = as.integer(sub("col_", "", column_name))) |>
  left_join(metadata, by = "column_index") |>
  filter(!is.na(date), !is.na(country), !is.na(arrivals)) |>
  transmute(
    date,
    raw_name,
    series_id = series_key,
    country,
    arrivals = as.numeric(arrivals),
    year = year(date),
    month = month(date),
    period = case_when(
      date <= as.Date("2020-01-01") ~ "pre_covid",
      date <= as.Date("2021-12-01") ~ "covid_shock",
      TRUE ~ "recovery"
    ),
    unit,
    source
  ) |>
  filter(date >= as.Date("2017-01-01"), date <= as.Date("2025-12-01")) |>
  arrange(series_id, date) |>
  group_by(series_id) |>
  mutate(
    indexed_arrivals = (arrivals / first(arrivals[arrivals > 0])) * 100,
    zscore_arrivals = as.numeric(scale(arrivals))
  ) |>
  ungroup()

arrivals_wide <- arrivals_long |>
  select(date, series_id, arrivals) |>
  pivot_wider(names_from = series_id, values_from = arrivals) |>
  arrange(date)

write.csv(arrivals_long, file.path(processed_dir, "arrivals_country_long.csv"), row.names = FALSE)
write.csv(arrivals_wide, file.path(processed_dir, "arrivals_country_wide.csv"), row.names = FALSE)

cat("Created shared arrivals backbone files in:", processed_dir, "\n")
