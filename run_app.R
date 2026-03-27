required_pkgs <- c(
  "shiny",
  "bslib",
  "DT",
  "dplyr",
  "ggplot2",
  "readr",
  "tidyr",
  "scales"
)
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    sprintf("Missing packages: %s. Run install.packages(c(%s)).",
      paste(missing_pkgs, collapse = ", "),
      paste(sprintf("\"%s\"", missing_pkgs), collapse = ", ")
    ),
    call. = FALSE
  )
}

args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1) suppressWarnings(as.integer(args[[1]])) else NA_integer_
if (is.na(port)) {
  port <- suppressWarnings(as.integer(Sys.getenv("SHINY_PORT", "3838")))
}
if (is.na(port)) {
  port <- 3838
}

shiny::runApp(
  appDir = "app",
  host = "127.0.0.1",
  port = port,
  launch.browser = TRUE
)
