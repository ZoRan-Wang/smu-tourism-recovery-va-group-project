library(readxl)
library(dplyr)

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
raw_path <- file.path(project_root, "data", "raw", "visitor_arrivals_full_dataset.xlsx")
output_dir <- file.path(project_root, "data", "processed")

if (!file.exists(raw_path)) {
  stop("Raw workbook not found at data/raw/visitor_arrivals_full_dataset.xlsx")
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

raw_meta <- read_excel(raw_path, sheet = "My Series", col_names = FALSE, .name_repair = "minimal")
headers <- as.character(unlist(raw_meta[1, ]))
headers[is.na(headers) | headers == ""] <- "date_raw"
headers <- make.unique(headers)

observations <- read_excel(
  raw_path,
  sheet = "My Series",
  skip = 29,
  col_names = FALSE,
  .name_repair = "minimal"
)
colnames(observations) <- headers

country_map <- c(
  china = "Visitor Arrivals: China",
  india = "Visitor Arrivals: India",
  malaysia = "Visitor Arrivals: Malaysia",
  australia = "Visitor Arrivals: Australia",
  philippines = "Visitor Arrivals: Philippines",
  japan = "Visitor Arrivals: Japan",
  united_kingdom = "Visitor Arrivals: United Kingdom",
  thailand = "Visitor Arrivals: Thailand",
  germany = "Visitor Arrivals: Germany",
  hong_kong_sar_china = "Visitor Arrivals: Hong Kong SAR (China)"
)

missing_columns <- setdiff(unname(country_map), colnames(observations))
if (length(missing_columns) > 0) {
  stop(
    "Missing expected country series in workbook: ",
    paste(missing_columns, collapse = ", ")
  )
}

country_wide <- observations |>
  transmute(
    date = as.Date(date_raw),
    across(all_of(unname(country_map)), as.numeric)
  ) |>
  rename(!!!setNames(unname(country_map), names(country_map))) |>
  filter(!is.na(date)) |>
  arrange(date) |>
  filter(date >= as.Date("2017-01-01"), date <= as.Date("2025-12-01"))

expected_months <- seq.Date(as.Date("2017-01-01"), as.Date("2025-12-01"), by = "month")
if (!identical(country_wide$date, expected_months)) {
  stop("The filtered date range is not a continuous monthly sequence.")
}

if (anyNA(country_wide)) {
  stop("The selected clustering series still contain missing values.")
}

country_long <- do.call(
  rbind,
  lapply(names(country_map), function(series_id) {
    arrivals <- country_wide[[series_id]]
    data.frame(
      date = country_wide$date,
      series_id = series_id,
      country = gsub("_", " ", series_id),
      arrivals = arrivals,
      indexed_arrivals = arrivals / arrivals[1],
      zscore_arrivals = as.numeric(scale(arrivals)),
      year = as.integer(format(country_wide$date, "%Y")),
      month = as.integer(format(country_wide$date, "%m")),
      period = dplyr::case_when(
        country_wide$date <= as.Date("2020-01-01") ~ "pre_covid",
        country_wide$date <= as.Date("2021-12-01") ~ "covid_shock",
        TRUE ~ "recovery"
      ),
      stringsAsFactors = FALSE
    )
  })
)

series_metadata <- data.frame(
  series_id = names(country_map),
  country = gsub("_", " ", names(country_map)),
  source_column = unname(country_map),
  stringsAsFactors = FALSE
)

write.csv(
  country_wide,
  file.path(output_dir, "clustering_country_wide.csv"),
  row.names = FALSE
)
write.csv(
  country_long,
  file.path(output_dir, "clustering_country_long.csv"),
  row.names = FALSE
)
write.csv(
  series_metadata,
  file.path(output_dir, "clustering_series_metadata.csv"),
  row.names = FALSE
)

cat("Created processed clustering files in:", output_dir, "\n")
