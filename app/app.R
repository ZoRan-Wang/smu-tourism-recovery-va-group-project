library(shiny)
library(bslib)

source("R/data_utils.R")
source("R/mod_cluster_ui.R")
source("R/mod_cluster_server.R")

ui <- page_navbar(
  title = "Singapore Tourism Recovery Prototype",
  theme = bs_theme(
    bg = "#f4f2eb",
    fg = "#1f2a2e",
    primary = "#0f6b6f",
    secondary = "#d86f45",
    base_font = font_google("Space Grotesk"),
    heading_font = font_google("Fraunces")
  ),
  nav_panel("Cluster Prototype", mod_cluster_ui("cluster_module")),
  nav_panel(
    "About",
    layout_column_wrap(
      width = 1,
      card(
        card_header("Prototype Scope"),
        card_body(
          p("This Shiny module prototypes clustering analysis for monthly tourism states."),
          tags$ul(
            tags$li("Data: tourism_four_part_analysis_ready.xlsx"),
            tags$li("Features: arrivals, China share, hotel occupancy, length of stay"),
            tags$li("Controls: period filter, k, scaling, random seed")
          )
        )
      ),
      card(
        card_header("Run Notes"),
        card_body(
          tags$ol(
            tags$li("Keep this app running on port 3838."),
            tags$li("Open Quarto site in parallel for write-up updates."),
            tags$li("Tune cluster k and compare silhouette scores.")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    load_tourism_data()
  })

  mod_cluster_server("cluster_module", data = data)
}

shinyApp(ui, server)
