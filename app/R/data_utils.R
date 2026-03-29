library(dplyr)
library(readxl)

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

read_arrival_metadata <- function(path = resolve_arrival_workbook()) {
  meta <- read_excel(
    path,
    sheet = "My Series",
    col_names = FALSE,
    n_max = 4,
    .name_repair = "minimal"
  )

  headers <- as.character(meta[1, ])
  frequencies <- as.character(meta[4, ])

  data.frame(
    column_index = seq_along(headers),
    raw_name = headers,
    frequency = frequencies,
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

  catalog <- meta |>
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

  catalog
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

  wide <- wide |>
    filter(!is.na(date)) |>
    filter(date >= as.Date("2016-12-01")) |>
    arrange(date)

  wide
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

  long <- long |>
    mutate(
      year = as.integer(format(date, "%Y")),
      month = as.integer(format(date, "%m")),
      quarter = ((month - 1) %/% 3) + 1
    )

  long
}

available_clustering_series <- function(path = resolve_arrival_workbook()) {
  wide <- load_clustering_country_wide(path)
  setdiff(names(wide), "date")
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
  normalization = c("indexed", "zscore", "raw")
) {
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

summarize_cluster_solution <- function(panel, cluster_id, distance_matrix, china_series = NULL) {
  sil <- as.data.frame(cluster::silhouette(cluster_id, distance_matrix))
  sil_width <- as.numeric(sil$sil_width)

  membership <- data.frame(
    series = rownames(panel$matrix),
    cluster = paste0("Cluster ", cluster_id),
    silhouette = round(sil_width, 3),
    stringsAsFactors = FALSE
  ) |>
    arrange(cluster, desc(silhouette), series)

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
      members = paste(members, collapse = ", "),
      stringsAsFactors = FALSE
    )
  }) |>
    bind_rows() |>
    arrange(cluster)

  cluster_summary <- assign_cluster_labels(cluster_summary)

  membership <- membership |>
    left_join(cluster_summary[, c("cluster", "cluster_label")], by = "cluster") |>
    arrange(cluster, desc(end_index), series)

  member_plot_long <- lapply(seq_along(panel$series), function(i) {
    series_name <- panel$series[i]
    data.frame(
      date = panel$normalized_wide$date,
      value = panel$normalized_wide[[series_name]],
      series = series_name,
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
      cluster = cluster_name,
      type = "Cluster mean",
      stringsAsFactors = FALSE
    )
  }) |>
    bind_rows()

  plot_data <- bind_rows(member_plot_long, cluster_mean_long) |>
    mutate(cluster = factor(cluster, levels = paste0("Cluster ", cluster_levels)))

  china_context <- NULL
  china_note <- "China is not included in the selected subset."

  if (!is.null(china_series) && china_series %in% membership$series) {
    china_cluster <- membership$cluster[membership$series == china_series][1]
    china_label <- membership$cluster_label[membership$series == china_series][1]
    china_members <- membership |>
      filter(cluster == china_cluster) |>
      mutate(distance_to_china = round(dmat[china_series, series], 3)) |>
      arrange(distance_to_china)

    representative <- cluster_summary$representative_series[cluster_summary$cluster == china_cluster][1]
    peer_names <- setdiff(china_members$series, china_series)

    china_note <- sprintf(
      "China falls in %s (%s). Its closest peers in the selected set are %s, and the representative series for this cluster is %s.",
      china_cluster,
      china_label,
      if (length(peer_names) == 0) "none" else paste(peer_names, collapse = ", "),
      representative
    )

    china_context <- china_members
  }

  list(
    silhouette = round(mean(sil_width, na.rm = TRUE), 3),
    diagnostics = diagnostics,
    membership = membership,
    summary = cluster_summary,
    plot_data = plot_data,
    series_features = membership |>
      select(series, cluster, cluster_label, end_index, trough_index, rebound_multiple, volatility) |>
      arrange(cluster, desc(end_index)),
    china_context = china_context,
    china_note = china_note
  )
}

load_tourism_data <- function(path = resolve_arrival_workbook()) {
  load_clustering_country_wide(path)
}
