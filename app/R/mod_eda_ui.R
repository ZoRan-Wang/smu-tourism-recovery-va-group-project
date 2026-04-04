library(shiny)
library(bslib)

mod_eda_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "cluster-header",
      div(
        class = "cluster-header-copy",
        div(class = "cluster-kicker", "Module 1"),
        h2("Time Series Visual Analysis"),
      )
    ),
    div(
      class = "cluster-workspace",
      div(
        class = "cluster-controls",
        h3("Controls"),
        selectInput(
          ns("ranking_period"),
          "Ranking period",
          choices = c(
            "Pre-COVID (2017-2019)" = "pre_covid",
            "COVID Shock (2020-2021)" = "covid_shock",
            "Recovery (2022-2025)" = "recovery"
          ),
          selected = "recovery"
        ),
        sliderInput(
          ns("map_year"),
          "Map year in selected period",
          min = 2022,
          max = 2025,
          value = 2025,
          step = 1,
          sep = ""
        ),
        sliderInput(
          ns("top_n"),
          "Top markets to show",
          min = 3,
          max = 10,
          value = 5,
          step = 1
        ),
        div(
          class = "cluster-controls-note",
          h4("Reading guide"),
          tags$ul(
            tags$li("Map: inspect a year that belongs to the selected ranking period."),
            tags$li("Top Markets: compare the pre-COVID and recovery leaders.")
          )
        )
      ),
      div(
        class = "cluster-main",
        navset_card_tab(
          id = ns("eda_pages"),
          selected = "Market Map",
          nav_panel(
            "Market Map",
            div(
              class = "cluster-tab-stack",
              layout_columns(
                card(
                  class = "cluster-panel",
                  card_header("EDA Overview"),
                  card_body(uiOutput(ns("overview_stats")))
                ),
                card(
                  class = "cluster-panel",
                  card_header("Selected Year Insight"),
                  card_body(uiOutput(ns("map_insight")))
                ),
                col_widths = c(7, 5)
              ),
              card(
                class = "cluster-panel",
                card_header("Visitor Arrivals by Source Country"),
                card_body(
                  plotly::plotlyOutput(ns("market_map"), height = "460px")
                )
              )
            )
          ),
          nav_panel(
            "Top Markets",
            div(
              class = "cluster-tab-stack",
              layout_columns(
                card(
                  class = "cluster-panel",
                  card_header("Pre-COVID Top Markets"),
                  card_body(
                    plotly::plotlyOutput(ns("pre_rank_plot"), height = "290px")
                  )
                ),
                card(
                  class = "cluster-panel",
                  card_header("Recovery Top Markets"),
                  card_body(
                    plotly::plotlyOutput(ns("recovery_rank_plot"), height = "290px")
                  )
                ),
                col_widths = c(6, 6)
              ),
              layout_columns(
                card(
                  class = "cluster-panel",
                  card_header("Selected Period Ranking Table"),
                  card_body(DT::DTOutput(ns("period_rank_table")))
                ),
                card(
                  class = "cluster-panel",
                  card_header("Ranking Interpretation"),
                  card_body(uiOutput(ns("ranking_insight")))
                ),
                col_widths = c(7, 5)
              )
            )
          )
        )
      )
    )
  )
}
