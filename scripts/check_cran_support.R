required_packages <- data.frame(
  package = c(
    "shiny",
    "bslib",
    "readxl",
    "dplyr",
    "ggplot2",
    "cluster",
    "DT",
    "rpart",
    "tidyr"
  ),
  purpose = c(
    "App runtime",
    "App theming",
    "Excel ingestion",
    "Data wrangling",
    "Charting",
    "Silhouette metric",
    "Interactive table",
    "Decision-tree module prep",
    "Data reshaping"
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

message("Saved package audit to: ", out_path)
