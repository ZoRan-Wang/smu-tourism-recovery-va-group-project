Sys.setenv(
  TOURISM_PROJECT_DIR = normalizePath(
    file.path("..", "shiny"),
    winslash = "/",
    mustWork = TRUE
  )
)

source(file.path("..", "shiny", "app.R"), local = TRUE)$value
