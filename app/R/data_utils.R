library(dplyr)
library(readxl)
library(tidyr)
library(purrr)
library(lubridate)
library(forecast)

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

resolve_arrival_workbook <- function() {
  project_root <- find_project_root()

  candidates <- c(
    file.path(project_root, "data", "raw", "visitor_arrivals_full_dataset.xlsx"),
    file.path(project_root, "data", "raw", "tourism_four_part_analysis_ready.xlsx")
  )

  path <- candidates[file.exists(candidates)][1]

  if (is.na(path) || !nzchar(path)) {
    stop("Dataset not found. Expected a tourism workbook under data/raw/.")
  }

  path
}

resolve_processed_path <- function(filename) {
  file.path(find_project_root(), "data", "processed", filename)
}

resolve_context_workbook <- function(required = FALSE) {
  candidates <- c(
    file.path("..", "data", "raw", "tourism_update.xlsx"),
    file.path("data", "raw", "tourism_update.xlsx"),
    file.path("..", "data", "raw", "Name your insight (4).xlsx"),
    file.path("data", "raw", "Name your insight (4).xlsx"),
    file.path("..", "data", "raw", "tourism_four_part_analysis_ready.xlsx"),
    file.path("data", "raw", "tourism_four_part_analysis_ready.xlsx")
  )

  path <- candidates[file.exists(candidates)][1]

  if ((is.na(path) || !nzchar(path)) && isTRUE(required)) {
    stop("Context workbook not found. Expected tourism_update.xlsx or a compatible workbook under data/raw/.")
  }

  if (is.na(path) || !nzchar(path)) {
    return(NA_character_)
  }

  path
}

read_arrival_metadata <- function(path = resolve_arrival_workbook()) {
  meta <- read_excel(
    path,
    sheet = "My Series",
    col_names = FALSE,
    n_max = 8,
    .name_repair = "minimal"
  )

  headers <- as.character(meta[1, ])
  frequencies <- as.character(meta[4, ])
  units <- if (nrow(meta) >= 5) as.character(meta[5, ]) else rep(NA_character_, length(headers))
  sources <- if (nrow(meta) >= 6) as.character(meta[6, ]) else rep(NA_character_, length(headers))
  series_ids <- if (nrow(meta) >= 8) as.character(meta[8, ]) else rep(NA_character_, length(headers))

  data.frame(
    column_index = seq_along(headers),
    raw_name = headers,
    frequency = frequencies,
    unit = units,
    source = sources,
    series_id = series_ids,
    stringsAsFactors = FALSE
  )
}

clean_country_series_label <- function(x) {
  x <- sub("^Visitor Arrivals:\\s*", "", x)
  x <- sub("\\.\\d+$", "", x)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x
}

country_series_catalog <- function(path = resolve_arrival_workbook()) {
  meta <- read_arrival_metadata(path)

  meta |>
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
      series_label = make.unique(clean_country_series_label(raw_name))
    )
}

load_clustering_series_metadata <- function(path = resolve_arrival_workbook()) {
  processed_path <- resolve_processed_path("clustering_series_metadata.csv")

  if (file.exists(processed_path)) {
    meta <- read.csv(processed_path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    catalog <- country_series_catalog(path)
    meta <- catalog |>
      transmute(
        series_id = series_label,
        country = clean_country_series_label(raw_name),
        source_column = raw_name
      )
  }

  meta |>
    mutate(
      display_name = make.unique(clean_country_series_label(source_column))
    )
}

clustering_display_lookup <- function(path = resolve_arrival_workbook()) {
  meta <- load_clustering_series_metadata(path)
  lookup <- meta$display_name
  names(lookup) <- meta$series_id
  lookup
}

display_names_for_series <- function(series_ids, path = resolve_arrival_workbook()) {
  lookup <- clustering_display_lookup(path)
  labels <- unname(lookup[series_ids])

  missing_idx <- which(is.na(labels) | labels == "")
  if (length(missing_idx) > 0) {
    fallback <- gsub("_", " ", series_ids[missing_idx], fixed = TRUE)
    labels[missing_idx] <- tools::toTitleCase(fallback)
  }

  labels
}

load_clustering_country_wide <- function(path = resolve_arrival_workbook()) {
  processed_path <- resolve_processed_path("clustering_country_wide.csv")

  if (file.exists(processed_path)) {
    wide <- read.csv(processed_path, stringsAsFactors = FALSE, check.names = FALSE)
    wide$date <- as.Date(wide$date)
    return(wide)
  }

  catalog <- country_series_catalog(path)
  raw_data <- read_excel(
    path,
    sheet = "My Series",
    col_names = FALSE,
    skip = 29,
    .name_repair = "minimal"
  )
  raw_data <- as.data.frame(raw_data, stringsAsFactors = FALSE)

  wide <- data.frame(
    date = as.Date(raw_data[[1]]),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(catalog))) {
    col_idx <- catalog$column_index[i]
    series_name <- catalog$series_label[i]
    wide[[series_name]] <- suppressWarnings(as.numeric(raw_data[[col_idx]]))
  }

  wide |>
    filter(!is.na(date)) |>
    filter(date >= as.Date("2016-12-01")) |>
    arrange(date)
}

load_clustering_country_long <- function(path = resolve_arrival_workbook()) {
  processed_path <- resolve_processed_path("clustering_country_long.csv")

  if (file.exists(processed_path)) {
    long <- read.csv(processed_path, stringsAsFactors = FALSE, check.names = FALSE)
    long$date <- as.Date(long$date)
    return(long)
  }

  wide <- load_clustering_country_wide(path)
  series_cols <- setdiff(names(wide), "date")

  long <- data.frame(
    date = rep(wide$date, times = length(series_cols)),
    series = rep(series_cols, each = nrow(wide)),
    arrivals = as.vector(as.matrix(wide[, series_cols, drop = FALSE])),
    stringsAsFactors = FALSE
  )

  long |>
    mutate(
      year = as.integer(format(date, "%Y")),
      month = as.integer(format(date, "%m")),
      quarter = ((month - 1) %/% 3) + 1
    )
}

available_clustering_series <- function(path = resolve_arrival_workbook(), named = TRUE) {
  wide <- load_clustering_country_wide(path)
  series_ids <- setdiff(names(wide), "date")

  if (!named) {
    return(series_ids)
  }

  stats::setNames(series_ids, display_names_for_series(series_ids, path = path))
}

default_clustering_series <- function(path = resolve_arrival_workbook(), n = 8) {
  wide <- load_clustering_country_wide(path)
  series_cols <- setdiff(names(wide), "date")
  means <- vapply(series_cols, function(col) mean(wide[[col]], na.rm = TRUE), numeric(1))
  ordered <- names(sort(means, decreasing = TRUE))
  head(ordered, n = min(n, length(ordered)))
}

normalize_series_vector <- function(x, mode = c("indexed", "zscore", "raw")) {
  mode <- match.arg(mode)
  x <- as.numeric(x)

  if (mode == "raw") {
    return(x)
  }

  if (mode == "indexed") {
    base_value <- x[which(!is.na(x) & x != 0)[1]]
    if (is.na(base_value) || base_value == 0) {
      base_value <- x[which(!is.na(x))[1]]
    }
    if (is.na(base_value) || base_value == 0) {
      return(rep(NA_real_, length(x)))
    }
    return((x / base_value) * 100)
  }

  if (all(is.na(x))) {
    return(x)
  }

  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) {
    return(rep(0, length(x)))
  }

  as.numeric(scale(x))
}

prepare_country_clustering_data <- function(
    wide,
    selected_series,
    year_window,
    normalization = c("indexed", "zscore", "raw")) {
  normalization <- match.arg(normalization)

  if (!"date" %in% names(wide)) {
    stop("The wide table must contain a date column.")
  }

  selected_series <- intersect(selected_series, setdiff(names(wide), "date"))
  if (length(selected_series) < 2) {
    stop("Select at least two country series.")
  }

  start_year <- min(year_window)
  end_year <- max(year_window)

  windowed <- wide |>
    filter(
      as.integer(format(date, "%Y")) >= start_year,
      as.integer(format(date, "%Y")) <= end_year
    ) |>
    select(date, all_of(selected_series)) |>
    arrange(date)

  if (nrow(windowed) < 12) {
    stop("The selected window is too short for clustering.")
  }

  normalized <- windowed
  for (series_name in selected_series) {
    normalized[[series_name]] <- normalize_series_vector(windowed[[series_name]], normalization)
  }

  matrix_data <- t(as.matrix(normalized[, selected_series, drop = FALSE]))
  rownames(matrix_data) <- selected_series

  list(
    raw_wide = windowed,
    normalized_wide = normalized,
    matrix = matrix_data,
    dates = windowed$date,
    series = selected_series,
    normalization = normalization
  )
}

compute_series_recovery_features <- function(panel) {
  series_names <- panel$series

  do.call(
    rbind,
    lapply(series_names, function(series_name) {
      normalized_values <- panel$normalized_wide[[series_name]]
      raw_values <- panel$raw_wide[[series_name]]
      valid_idx <- which(!is.na(normalized_values))

      if (length(valid_idx) == 0) {
        return(NULL)
      }

      first_idx <- valid_idx[1]
      last_idx <- valid_idx[length(valid_idx)]
      trough_value <- min(normalized_values[valid_idx], na.rm = TRUE)
      end_value <- normalized_values[last_idx]

      data.frame(
        series = series_name,
        start_value = round(raw_values[first_idx], 1),
        end_value = round(raw_values[last_idx], 1),
        start_index = round(normalized_values[first_idx], 1),
        end_index = round(end_value, 1),
        trough_index = round(trough_value, 1),
        peak_index = round(max(normalized_values[valid_idx], na.rm = TRUE), 1),
        rebound_multiple = round(
          if (is.na(trough_value) || trough_value == 0) NA_real_ else end_value / trough_value,
          3
        ),
        volatility = round(
          if (length(valid_idx) <= 1) NA_real_ else stats::sd(diff(normalized_values[valid_idx]), na.rm = TRUE),
          3
        ),
        stringsAsFactors = FALSE
      )
    })
  )
}

assign_cluster_labels <- function(cluster_summary) {
  if (nrow(cluster_summary) == 0) {
    cluster_summary$cluster_label <- character(0)
    return(cluster_summary)
  }

  rank_order <- order(-cluster_summary$avg_end_index, cluster_summary$avg_trough_index)
  rank_position <- integer(length(rank_order))
  rank_position[rank_order] <- seq_along(rank_order)

  labels_by_rank <- paste("Recovery pattern", seq_len(nrow(cluster_summary)))
  if (nrow(cluster_summary) >= 1) {
    labels_by_rank[1] <- "Stronger rebound"
  }
  if (nrow(cluster_summary) == 2) {
    labels_by_rank[2] <- "Delayed recovery"
  }
  if (nrow(cluster_summary) >= 3) {
    labels_by_rank[2:(nrow(cluster_summary) - 1)] <- paste(
      "Broad recovery",
      seq_len(nrow(cluster_summary) - 2)
    )
    labels_by_rank[nrow(cluster_summary)] <- "Delayed recovery"
  }

  cluster_summary$cluster_label <- labels_by_rank[rank_position]
  cluster_summary
}

summarize_cluster_solution <- function(
    panel,
    cluster_id,
    distance_matrix,
    china_series = NULL,
    series_labels = NULL) {
  if (is.null(series_labels)) {
    series_labels <- stats::setNames(panel$series, panel$series)
  }

  series_name_for <- function(series_id) {
    label <- unname(series_labels[series_id])
    if (is.na(label) || label == "") {
      return(series_id)
    }
    label
  }

  sil <- as.data.frame(cluster::silhouette(cluster_id, distance_matrix))
  sil_width <- as.numeric(sil$sil_width)

  membership <- data.frame(
    series = rownames(panel$matrix),
    series_name = vapply(rownames(panel$matrix), series_name_for, character(1)),
    cluster = paste0("Cluster ", cluster_id),
    silhouette = round(sil_width, 3),
    stringsAsFactors = FALSE
  ) |>
    arrange(cluster, desc(silhouette), series_name)

  candidate_k <- 2:min(8, nrow(panel$matrix) - 1)
  diagnostics <- lapply(
    candidate_k,
    function(k) {
      cl <- stats::cutree(stats::hclust(distance_matrix, method = "ward.D2"), k = k)
      sil_k <- as.data.frame(cluster::silhouette(cl, distance_matrix))
      data.frame(
        k = k,
        mean_silhouette = round(mean(as.numeric(sil_k$sil_width), na.rm = TRUE), 3)
      )
    }
  ) |>
    bind_rows()

  series_features <- compute_series_recovery_features(panel)
  membership <- membership |>
    left_join(series_features, by = "series")

  dmat <- as.matrix(distance_matrix)
  cluster_levels <- sort(unique(cluster_id))

  cluster_summary <- lapply(cluster_levels, function(cl) {
    members <- membership$series[membership$cluster == paste0("Cluster ", cl)]
    cluster_dmat <- dmat[members, members, drop = FALSE]
    medoid <- if (length(members) == 1) {
      members[[1]]
    } else {
      members[which.min(rowSums(cluster_dmat))]
    }

    data.frame(
      cluster = paste0("Cluster ", cl),
      n_series = length(members),
      representative_series = medoid,
      representative_series_name = series_name_for(medoid),
      mean_silhouette = round(
        mean(membership$silhouette[membership$cluster == paste0("Cluster ", cl)], na.rm = TRUE),
        3
      ),
      avg_end_index = round(
        mean(membership$end_index[membership$cluster == paste0("Cluster ", cl)], na.rm = TRUE),
        1
      ),
      avg_trough_index = round(
        mean(membership$trough_index[membership$cluster == paste0("Cluster ", cl)], na.rm = TRUE),
        1
      ),
      avg_rebound_multiple = round(
        mean(membership$rebound_multiple[membership$cluster == paste0("Cluster ", cl)], na.rm = TRUE),
        3
      ),
      members = paste(vapply(members, series_name_for, character(1)), collapse = ", "),
      stringsAsFactors = FALSE
    )
  }) |>
    bind_rows() |>
    arrange(cluster)

  cluster_summary <- assign_cluster_labels(cluster_summary)

  membership <- membership |>
    left_join(cluster_summary[, c("cluster", "cluster_label")], by = "cluster") |>
    arrange(cluster, desc(end_index), series_name)

  member_plot_long <- lapply(seq_along(panel$series), function(i) {
    series_name <- panel$series[i]
    data.frame(
      date = panel$normalized_wide$date,
      value = panel$normalized_wide[[series_name]],
      series = series_name,
      series_name = series_name_for(series_name),
      cluster = membership$cluster[match(series_name, membership$series)],
      type = "Series",
      stringsAsFactors = FALSE
    )
  }) |>
    bind_rows()

  cluster_mean_long <- lapply(cluster_levels, function(cl) {
    cluster_name <- paste0("Cluster ", cl)
    members <- membership$series[membership$cluster == cluster_name]
    cluster_mean <- rowMeans(panel$normalized_wide[, members, drop = FALSE], na.rm = TRUE)

    data.frame(
      date = panel$normalized_wide$date,
      value = cluster_mean,
      series = paste0("Cluster mean ", cl),
      series_name = paste0("Cluster mean ", cl),
      cluster = cluster_name,
      type = "Cluster mean",
      stringsAsFactors = FALSE
    )
  }) |>
    bind_rows()

  plot_data <- bind_rows(member_plot_long, cluster_mean_long) |>
    left_join(cluster_summary[, c("cluster", "cluster_label")], by = "cluster") |>
    mutate(
      cluster = factor(cluster, levels = paste0("Cluster ", cluster_levels)),
      cluster_view = factor(
        paste0(as.character(cluster), " · ", cluster_label),
        levels = paste0(cluster_summary$cluster, " · ", cluster_summary$cluster_label)
      )
    )

  china_context <- NULL
  china_note <- "China is not included in the selected subset."

  if (!is.null(china_series) && china_series %in% membership$series) {
    china_cluster <- membership$cluster[membership$series == china_series][1]
    china_label <- membership$cluster_label[membership$series == china_series][1]
    china_members <- membership |>
      filter(cluster == china_cluster) |>
      mutate(distance_to_china = round(dmat[china_series, series], 3)) |>
      arrange(distance_to_china)

    representative_name <- cluster_summary$representative_series_name[cluster_summary$cluster == china_cluster][1]
    peer_names <- setdiff(china_members$series, china_series)
    peer_labels <- vapply(peer_names, series_name_for, character(1))

    china_note <- sprintf(
      "China falls in %s (%s). Its closest peers in the selected set are %s, and the representative series for this cluster is %s.",
      china_cluster,
      china_label,
      if (length(peer_labels) == 0) "none" else paste(peer_labels, collapse = ", "),
      representative_name
    )

    china_context <- china_members |>
      mutate(peer_name = vapply(series, series_name_for, character(1)))
  }

  list(
    silhouette = round(mean(sil_width, na.rm = TRUE), 3),
    diagnostics = diagnostics,
    membership = membership,
    summary = cluster_summary,
    plot_data = plot_data,
    series_features = membership |>
      select(series, series_name, cluster, cluster_label, end_index, trough_index, rebound_multiple, volatility) |>
      arrange(cluster, desc(end_index)),
    china_context = china_context,
    china_note = china_note
  )
}

load_shared_arrival_long_data <- function(path = resolve_arrival_workbook()) {
  metadata <- read_arrival_metadata(path) |>
    filter(
      frequency == "Monthly",
      grepl("^Visitor Arrivals", raw_name),
      !grepl("^Visitor Arrivals: sa:", raw_name)
    ) |>
    mutate(
      label = trimws(raw_name),
      unit = trimws(unit),
      source = trimws(source),
      series_id = trimws(series_id),
      series_key = make.names(tolower(label), unique = TRUE)
    )

  raw <- read_excel(
    path,
    sheet = "My Series",
    skip = 29,
    col_names = FALSE,
    .name_repair = "minimal"
  )

  max_cols <- max(metadata$column_index)
  raw <- raw[, seq_len(max_cols), drop = FALSE]
  names(raw) <- paste0("col_", seq_len(ncol(raw)))

  raw |>
    mutate(date = as.Date(.data$col_1)) |>
    pivot_longer(
      cols = -c(date, col_1),
      names_to = "column_name",
      values_to = "value"
    ) |>
    mutate(column_index = as.integer(sub("col_", "", column_name))) |>
    left_join(metadata |> select(column_index, label, series_key, frequency, unit, source, series_id), by = "column_index") |>
    filter(
      !is.na(date),
      !is.na(label),
      nzchar(label),
      !is.na(value)
    ) |>
    transmute(
      date,
      label,
      series_key,
      frequency,
      unit = ifelse(is.na(unit) | unit == "", "Persons", unit),
      source = ifelse(is.na(source) | source == "", "STB / CEIC", source),
      series_id,
      value = suppressWarnings(as.numeric(value))
    ) |>
    filter(!is.na(value)) |>
    arrange(label, date)
}

country_arrival_labels <- function(path = resolve_arrival_workbook()) {
  unique(trimws(country_series_catalog(path)$raw_name))
}

is_country_arrival_label <- function(label, path = resolve_arrival_workbook()) {
  trimws(label) %in% country_arrival_labels(path)
}

resolve_data_path <- function(required = FALSE) {
  resolve_context_workbook(required = required)
}

read_tourism_metadata <- function(path = resolve_context_workbook()) {
  if (is.na(path) || !nzchar(path)) {
    return(tibble(
      col_id = integer(),
      label = character(),
      region = character(),
      frequency = character(),
      unit = character(),
      source = character(),
      series_id = character(),
      series_key = character()
    ))
  }

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

load_tourism_long_data <- function(path = resolve_context_workbook(), only_monthly = FALSE) {
  if (is.na(path) || !nzchar(path)) {
    return(tibble(
      date = as.Date(character()),
      label = character(),
      series_key = character(),
      frequency = character(),
      unit = character(),
      source = character(),
      series_id = character(),
      value = numeric()
    ))
  }

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

load_support_indicator_long_data <- function(path = resolve_context_workbook()) {
  support_labels <- c(
    "Hotel Room Occupancy Rate",
    "Average Length of Stay",
    "Number of Hotels",
    "Total Room Revenue"
  )

  load_tourism_long_data(path, only_monthly = TRUE) |>
    filter(label %in% support_labels)
}

load_tourism_data <- function(
    arrivals_path = resolve_arrival_workbook(),
    context_path = resolve_context_workbook()) {
  arrivals_long <- load_shared_arrival_long_data(arrivals_path)
  context_long <- load_support_indicator_long_data(context_path)
  long_monthly <- bind_rows(arrivals_long, context_long) |>
    arrange(label, date)

  metadata <- bind_rows(
    read_arrival_metadata(arrivals_path) |>
      filter(
        frequency == "Monthly",
        grepl("^Visitor Arrivals", raw_name),
        !grepl("^Visitor Arrivals: sa:", raw_name)
      ) |>
      transmute(
        label = trimws(raw_name),
        frequency = trimws(frequency),
        unit = ifelse(is.na(unit) | trimws(unit) == "", "Persons", trimws(unit)),
        source = ifelse(is.na(source) | trimws(source) == "", "STB / CEIC", trimws(source)),
        series_id = trimws(series_id),
        series_key = make.names(tolower(trimws(raw_name)), unique = TRUE)
      ),
    read_tourism_metadata(context_path) |>
      filter(
        label %in% c(
          "Hotel Room Occupancy Rate",
          "Average Length of Stay",
          "Number of Hotels",
          "Total Room Revenue"
        )
      ) |>
      select(label, frequency, unit, source, series_id, series_key)
  ) |>
    distinct(series_key, .keep_all = TRUE)

  monthly_features <- build_monthly_feature_table(long_monthly)

  list(
    long_monthly = long_monthly,
    monthly_features = monthly_features,
    metadata = metadata
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

list_country_arrival_series <- function(long_monthly, min_obs = 24, path = resolve_arrival_workbook()) {
  allowed_labels <- country_arrival_labels(path)

  list_forecast_series(long_monthly, min_obs = min_obs) |>
    filter(label %in% allowed_labels)
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

prepare_time_series_cluster_data <- function(
    long_monthly,
    selected_series = NULL,
    lookback_years = 8,
    scale_series = TRUE) {
  available_series <- list_country_arrival_series(long_monthly)$label

  if (is.null(selected_series) || length(selected_series) == 0) {
    selected_series <- available_series
  }

  selected_series <- intersect(selected_series, available_series)

  if (length(selected_series) < 2) {
    stop("At least two country-arrival series are required for clustering.")
  }

  max_date <- max(long_monthly$date, na.rm = TRUE)
  cutoff_date <- max_date %m-% years(lookback_years)

  filtered <- long_monthly |>
    filter(label %in% selected_series, date >= cutoff_date) |>
    select(date, label, value)

  wide <- filtered |>
    tidyr::pivot_wider(names_from = label, values_from = value) |>
    arrange(date) |>
    tidyr::drop_na()

  if (nrow(wide) < 12) {
    stop("Not enough overlapping monthly observations after filtering.")
  }

  series_matrix <- wide |>
    select(-date) |>
    as.matrix() |>
    t()

  if (scale_series) {
    series_matrix <- t(scale(t(series_matrix)))
  }

  series_metadata <- filtered |>
    group_by(label) |>
    summarise(
      mean_arrivals = mean(value, na.rm = TRUE),
      volatility = sd(value, na.rm = TRUE),
      latest_arrivals = value[which.max(date)],
      start_date = min(date),
      end_date = max(date),
      .groups = "drop"
    ) |>
    filter(label %in% rownames(series_matrix))

  list(
    date_index = wide$date,
    matrix = series_matrix,
    metadata = series_metadata
  )
}

prepare_forecast_series <- function(long_monthly, series_label) {
  long_monthly |>
    filter(label == series_label) |>
    arrange(date) |>
    mutate(value = as.numeric(value))
}

forecast_stack_status <- function() {
  required_pkgs <- c("rsample", "parsnip", "modeltime", "timetk", "yardstick")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

  list(
    fallback_ready = requireNamespace("forecast", quietly = TRUE),
    modeltime_ready = length(missing_pkgs) == 0,
    missing_modeltime_packages = missing_pkgs,
    preferred_engine = if (length(missing_pkgs) == 0) "modeltime" else "fallback"
  )
}

compute_forecast_metrics <- function(actual, predicted) {
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  residuals <- actual - predicted
  valid <- !(is.na(actual) | is.na(predicted))

  actual <- actual[valid]
  predicted <- predicted[valid]
  residuals <- residuals[valid]

  if (length(actual) == 0) {
    return(
      tibble(
        mae = NA_real_,
        mape = NA_real_,
        mase = NA_real_,
        smape = NA_real_,
        rmse = NA_real_,
        rsq = NA_real_
      )
    )
  }

  nonzero_actual <- actual[actual != 0]
  mape <- if (length(nonzero_actual) == 0) {
    NA_real_
  } else {
    mean(abs((actual[actual != 0] - predicted[actual != 0]) / actual[actual != 0])) * 100
  }

  denominator <- abs(actual) + abs(predicted)
  smape <- if (all(denominator == 0)) {
    NA_real_
  } else {
    mean((2 * abs(actual - predicted)) / ifelse(denominator == 0, NA_real_, denominator), na.rm = TRUE) * 100
  }

  rsq <- if (length(actual) < 2 || stats::sd(actual) == 0 || stats::sd(predicted) == 0) {
    NA_real_
  } else {
    stats::cor(actual, predicted)^2
  }

  tibble(
    mae = mean(abs(residuals)),
    mape = mape,
    mase = NA_real_,
    smape = smape,
    rmse = sqrt(mean(residuals^2)),
    rsq = rsq
  )
}

build_forecast_split <- function(series_df, horizon = 12) {
  if (nrow(series_df) <= horizon + 12) {
    stop("Selected series is too short for the requested forecast horizon.")
  }

  split_idx <- nrow(series_df) - horizon

  list(
    training = series_df[seq_len(split_idx), , drop = FALSE],
    testing = series_df[seq.int(split_idx + 1, nrow(series_df)), , drop = FALSE]
  )
}

make_future_data <- function(series_df, horizon = 12) {
  tibble(
    date = seq.Date(from = max(series_df$date) %m+% months(1), by = "month", length.out = horizon)
  )
}

build_holdout_forecast_tbl <- function(actual_df, prediction_map) {
  bind_rows(lapply(names(prediction_map), function(model_name) {
    tibble(
      date = actual_df$date,
      actual = actual_df$value,
      prediction = as.numeric(prediction_map[[model_name]]),
      .model_desc = model_name
    )
  }))
}

build_future_forecast_tbl <- function(future_dates, prediction_map) {
  bind_rows(lapply(names(prediction_map), function(model_name) {
    tibble(
      date = future_dates$date,
      prediction = as.numeric(prediction_map[[model_name]]),
      .model_desc = model_name
    )
  }))
}

plot_forecast_results <- function(results, type = c("holdout", "future")) {
  type <- match.arg(type)

  if (type == "holdout") {
    predictions <- results$holdout_forecast_tbl

    ggplot() +
      geom_line(
        data = results$series_df,
        aes(x = date, y = value),
        color = "#b8c0c8",
        linewidth = 0.8
      ) +
      geom_line(
        data = results$testing,
        aes(x = date, y = value),
        color = "#1f2933",
        linewidth = 1
      ) +
      geom_line(
        data = predictions,
        aes(x = date, y = prediction, color = .model_desc),
        linewidth = 1
      ) +
      scale_color_manual(
        values = c(
          "Seasonal Naive" = "#d86f45",
          "ETS (Modeltime)" = "#0f6b6f",
          "ETS" = "#0f6b6f",
          "ARIMA" = "#6b4eff"
        )
      ) +
      labs(
        x = NULL,
        y = "Visitor arrivals (person)",
        color = "Model"
      ) +
      scale_y_continuous(labels = scales::label_comma()) +
      theme_minimal(base_size = 13)
  } else {
    predictions <- results$future_forecast_tbl

    ggplot() +
      geom_line(
        data = results$series_df,
        aes(x = date, y = value),
        color = "#b8c0c8",
        linewidth = 0.8
      ) +
      geom_line(
        data = predictions,
        aes(x = date, y = prediction, color = .model_desc),
        linewidth = 1.1
      ) +
      scale_color_manual(
        values = c(
          "Seasonal Naive" = "#d86f45",
          "ETS (Modeltime)" = "#0f6b6f",
          "ETS" = "#0f6b6f",
          "ARIMA" = "#6b4eff"
        )
      ) +
      labs(
        x = NULL,
        y = "Visitor arrivals (person)",
        color = "Refit model"
      ) +
      scale_y_continuous(labels = scales::label_comma()) +
      theme_minimal(base_size = 13)
  }
}

run_fallback_forecast_workflow <- function(series_df, horizon = 12) {
  split <- build_forecast_split(series_df, horizon = horizon)
  training_df <- split$training
  testing_df <- split$testing

  training_ts <- ts(
    training_df$value,
    start = c(lubridate::year(min(training_df$date)), lubridate::month(min(training_df$date))),
    frequency = 12
  )
  full_ts <- ts(
    series_df$value,
    start = c(lubridate::year(min(series_df$date)), lubridate::month(min(series_df$date))),
    frequency = 12
  )

  snaive_fit <- forecast::snaive(training_ts, h = nrow(testing_df))
  ets_model <- forecast::ets(training_ts)
  ets_fit <- forecast::forecast(ets_model, h = nrow(testing_df))
  arima_model <- forecast::auto.arima(training_ts)
  arima_fit <- forecast::forecast(arima_model, h = nrow(testing_df))

  prediction_map <- list(
    "Seasonal Naive" = as.numeric(snaive_fit$mean),
    "ETS" = as.numeric(ets_fit$mean),
    "ARIMA" = as.numeric(arima_fit$mean)
  )

  accuracy_tbl <- bind_rows(lapply(names(prediction_map), function(model_name) {
    compute_forecast_metrics(testing_df$value, prediction_map[[model_name]]) |>
      mutate(
        .model_id = match(model_name, names(prediction_map)) - 1L,
        .model_desc = model_name,
        .type = "Test",
        .before = 1
      )
  })) |>
    select(.model_id, .model_desc, .type, mae, mape, mase, smape, rmse, rsq) |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))

  best_model <- accuracy_tbl |>
    arrange(rmse) |>
    slice(1) |>
    pull(.model_desc)

  future_dates <- make_future_data(series_df, horizon = horizon)
  future_prediction_map <- list(
    "Seasonal Naive" = as.numeric(forecast::snaive(full_ts, h = horizon)$mean),
    "ETS" = as.numeric(forecast::forecast(forecast::ets(full_ts), h = horizon)$mean),
    "ARIMA" = as.numeric(forecast::forecast(forecast::auto.arima(full_ts), h = horizon)$mean)
  )

  list(
    engine = "fallback",
    engine_label = "forecast fallback",
    series_df = series_df,
    training = training_df,
    testing = testing_df,
    models_tbl = tibble(
      .model_id = 0:2,
      .model_desc = c("Seasonal Naive", "ETS", "ARIMA"),
      engine = "forecast"
    ),
    accuracy_tbl = accuracy_tbl,
    holdout_forecast_tbl = build_holdout_forecast_tbl(testing_df, prediction_map),
    future_forecast_tbl = build_future_forecast_tbl(future_dates, future_prediction_map),
    best_model_desc = best_model
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
  })

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

  training_ts <- ts(
    training_df$value,
    start = c(lubridate::year(min(training_df$date)), lubridate::month(min(training_df$date))),
    frequency = 12
  )

  baseline_fit <- forecast::snaive(training_ts, h = nrow(testing_df))
  ets_test_pred <- predict(model_fit_ets, new_data = testing_df)$.pred
  arima_test_pred <- predict(model_fit_arima, new_data = testing_df)$.pred

  prediction_map <- list(
    "Seasonal Naive" = as.numeric(baseline_fit$mean),
    "ETS (Modeltime)" = as.numeric(ets_test_pred),
    "ARIMA" = as.numeric(arima_test_pred)
  )

  accuracy_tbl <- bind_rows(lapply(names(prediction_map), function(model_name) {
    compute_forecast_metrics(testing_df$value, prediction_map[[model_name]]) |>
      mutate(
        .model_id = c("Seasonal Naive" = 0L, "ETS (Modeltime)" = 1L, "ARIMA" = 2L)[[model_name]],
        .model_desc = model_name,
        .type = "Test",
        .before = 1
      )
  })) |>
    select(.model_id, .model_desc, .type, mae, mape, mase, smape, rmse, rsq) |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))

  best_model <- accuracy_tbl |>
    arrange(rmse) |>
    slice(1) |>
    pull(.model_desc)

  future_dates <- make_future_data(series_df, horizon = horizon)
  full_ts <- ts(
    series_df$value,
    start = c(lubridate::year(min(series_df$date)), lubridate::month(min(series_df$date))),
    frequency = 12
  )

  ets_refit <- modeltime::exp_smoothing() |>
    parsnip::set_engine("ets") |>
    parsnip::fit(value ~ date, data = series_df)

  arima_refit <- modeltime::arima_reg() |>
    parsnip::set_engine("auto_arima") |>
    parsnip::fit(value ~ date, data = series_df)

  future_prediction_map <- list(
    "Seasonal Naive" = as.numeric(forecast::snaive(full_ts, h = horizon)$mean),
    "ETS (Modeltime)" = as.numeric(predict(ets_refit, new_data = future_dates)$.pred),
    "ARIMA" = as.numeric(predict(arima_refit, new_data = future_dates)$.pred)
  )

  list(
    engine = "modeltime",
    engine_label = "modeltime workflow",
    splits = splits,
    series_df = series_df,
    training = training_df,
    testing = testing_df,
    models_tbl = tibble(
      .model_id = 0:2,
      .model_desc = c("Seasonal Naive", "ETS (Modeltime)", "ARIMA"),
      engine = c("forecast", "modeltime", "modeltime")
    ),
    accuracy_tbl = accuracy_tbl,
    holdout_forecast_tbl = build_holdout_forecast_tbl(testing_df, prediction_map),
    future_forecast_tbl = build_future_forecast_tbl(future_dates, future_prediction_map),
    best_model_desc = best_model
  )
}

run_forecast_workflow <- function(series_df, horizon = 12, engine = c("auto", "modeltime", "fallback")) {
  engine <- match.arg(engine)
  stack_status <- forecast_stack_status()

  if (!stack_status$fallback_ready) {
    stop("Missing required package 'forecast'. Install it before running the forecasting workflow.")
  }

  selected_engine <- switch(
    engine,
    auto = stack_status$preferred_engine,
    modeltime = "modeltime",
    fallback = "fallback"
  )

  if (selected_engine == "modeltime" && !stack_status$modeltime_ready) {
    stop(
      "Modeltime forecasting is unavailable because these packages are missing: ",
      paste(stack_status$missing_modeltime_packages, collapse = ", "),
      "."
    )
  }

  if (selected_engine == "modeltime") {
    run_modeltime_forecast_workflow(series_df = series_df, horizon = horizon)
  } else {
    run_fallback_forecast_workflow(series_df = series_df, horizon = horizon)
  }
}
