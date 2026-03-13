if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman", repos = "https://cloud.r-project.org")
}

pacman::p_load(
  tidyverse,
  readxl,
  lubridate,
  writexl
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
raw_path <- file.path(module_dir, "data", "Name your insight (2).xlsx")
processed_dir <- file.path(module_dir, "data")

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

col_names <- c(
  "date",
  "avg_stay_monthly",
  "hotel_occ",
  "spend_per_capita",
  "tourism_receipts",
  "avg_stay_annual",
  "visitor_arrivals",
  "visitor_arrivals_china"
)

# Skip the metadata rows and start at the actual observation block.
raw_tbl <- readxl::read_excel(
  path = raw_path,
  sheet = "My Series",
  skip = 29,
  col_names = FALSE
) %>%
  setNames(col_names) %>%
  mutate(
    date = as.Date(date),
    across(-date, as.numeric)
  ) %>%
  arrange(date)

glimpse(raw_tbl)
colSums(is.na(raw_tbl))

monthly_clean <- raw_tbl %>%
  filter(
    !is.na(date),
    !is.na(avg_stay_monthly),
    !is.na(hotel_occ),
    !is.na(visitor_arrivals),
    !is.na(visitor_arrivals_china)
  ) %>%
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date),
    quarter = lubridate::quarter(date),
    period = case_when(
      date <= as.Date("2020-01-01") ~ "pre_covid",
      date <= as.Date("2021-12-01") ~ "covid_shock",
      TRUE ~ "recovery"
    ),
    china_share = visitor_arrivals_china / visitor_arrivals,
    china_share_pct = china_share * 100
  )

# Keep the raw stay-length column, but add a capped version for modeling.
stay_cap <- quantile(monthly_clean$avg_stay_monthly, probs = 0.95, na.rm = TRUE)

monthly_clean <- monthly_clean %>%
  mutate(
    avg_stay_monthly_capped = pmin(avg_stay_monthly, stay_cap),
    visitor_arrivals_millions = visitor_arrivals / 1000000,
    visitor_arrivals_china_thousands = visitor_arrivals_china / 1000
  )

occ_cuts <- quantile(monthly_clean$hotel_occ, probs = c(1/3, 2/3), na.rm = TRUE)

analysis_ready <- monthly_clean %>%
  mutate(
    cluster_z_visitor_arrivals = as.numeric(scale(visitor_arrivals)),
    cluster_z_china_share = as.numeric(scale(china_share)),
    cluster_z_hotel_occ = as.numeric(scale(hotel_occ)),
    cluster_z_avg_stay_monthly_capped = as.numeric(scale(avg_stay_monthly_capped)),
    hotel_occ_level_tertile = case_when(
      hotel_occ <= occ_cuts[[1]] ~ "low",
      hotel_occ <= occ_cuts[[2]] ~ "medium",
      TRUE ~ "high"
    ),
    hotel_occ_level_business = case_when(
      hotel_occ < 70 ~ "low",
      hotel_occ <= 85 ~ "medium",
      TRUE ~ "high"
    )
  ) %>%
  mutate(
    hotel_occ_level_tertile = factor(hotel_occ_level_tertile, levels = c("low", "medium", "high")),
    hotel_occ_level_business = factor(hotel_occ_level_business, levels = c("low", "medium", "high"))
  ) %>%
  mutate(
    dataset_split = if_else(row_number() <= floor(n() * 0.8), "train", "test")
  )

decision_tree_ready <- analysis_ready %>%
  select(
    date, year, month, quarter, period,
    visitor_arrivals, visitor_arrivals_china,
    china_share, china_share_pct,
    hotel_occ, avg_stay_monthly, avg_stay_monthly_capped,
    hotel_occ_level_tertile, hotel_occ_level_business,
    dataset_split
  )

readr::write_csv(monthly_clean, file.path(processed_dir, "tourism_monthly_clean.csv"))
readr::write_csv(decision_tree_ready, file.path(processed_dir, "tourism_decision_tree_ready.csv"))
readr::write_csv(analysis_ready, file.path(processed_dir, "tourism_four_part_analysis_ready.csv"))

writexl::write_xlsx(
  list(
    monthly_clean = monthly_clean,
    decision_tree_ready = decision_tree_ready,
    analysis_ready_all = analysis_ready
  ),
  path = file.path(processed_dir, "tourism_four_part_analysis_ready.xlsx")
)

message("Data preparation complete.")
message("Rows in monthly_clean: ", nrow(monthly_clean))
message("Rows in analysis_ready: ", nrow(analysis_ready))
message("Date range: ", min(analysis_ready$date), " to ", max(analysis_ready$date))
message("95% cap for avg_stay_monthly: ", round(stay_cap, 6))
message("Hotel occupancy cut points: ", round(occ_cuts[[1]], 6), " and ", round(occ_cuts[[2]], 6))
