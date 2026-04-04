library(shiny)
library(bslib)

mod_forecast_ui <- function(id) {
  ns <- NS(id)

  div(
    class = "forecast-shell",
    div(
      class = "forecast-header",
      div(
        class = "forecast-header-copy",
        div(class = "va-kicker", "Forecasting Studio"),
        h2("Singapore tourism demand forecasting"),
        p("Build and compare forecasting models on country-level arrivals, then interpret the forecast through diagnostics and tourism-performance context in one single-page workspace.")
      ),
      div(
        class = "forecast-header-meta",
        div(class = "forecast-meta-chip", "Interactive analytics, not static charts"),
        div(class = "forecast-meta-chip", "Forecast + diagnostics + context"),
        div(class = "forecast-meta-chip", "Single-page model studio")
      )
    ),
    div(
      class = "forecast-workspace",
      div(
        class = "forecast-controls",
        h3("Model controls"),
        selectInput(ns("series_label"), "Country arrival series", choices = NULL),
        selectInput(
          ns("engine_preference"),
          "Forecast engine",
          choices = c(
            "Auto (best available)" = "auto",
            "Require modeltime" = "modeltime",
            "Use lightweight fallback" = "fallback"
          ),
          selected = "auto"
        ),
        sliderInput(ns("horizon"), "Test / forecast horizon", min = 6, max = 18, value = 12, step = 1),
        checkboxGroupInput(
          ns("model_choices"),
          "Models to compare",
          choices = c("Seasonal Naive", "ETS", "ARIMA"),
          selected = c("Seasonal Naive", "ETS", "ARIMA")
        ),
        checkboxGroupInput(
          ns("context_indicators"),
          "Context indicators",
          choices = c(
            "Hotel occupancy" = "Hotel Room Occupancy Rate",
            "Average stay length" = "Average Length of Stay",
            "Room revenue" = "Total Room Revenue"
          ),
          selected = c(
            "Hotel Room Occupancy Rate",
            "Average Length of Stay",
            "Total Room Revenue"
          )
        ),
        radioButtons(
          ns("rank_metric"),
          "Model ranking metric",
          choices = c("RMSE" = "rmse", "MAE" = "mae", "MAPE" = "mape"),
          selected = "rmse",
          inline = TRUE
        ),
        actionButton(ns("run_forecast"), "Run Forecasting", class = "btn-primary")
      ),
      div(
        class = "forecast-main",
        navset_card_tab(
          id = ns("forecast_pages"),
          full_screen = FALSE,
          height = "100%",
          nav_panel(
            "Forecast",
            div(
              class = "forecast-tab-grid forecast-tab-grid--forecast",
              card(
                class = "forecast-panel forecast-panel--hero",
                card_header("Holdout comparison"),
                card_body(plotOutput(ns("forecast_plot"), height = "300px"))
              ),
              card(
                class = "forecast-panel forecast-panel--hero",
                card_header("Forward forecast"),
                card_body(plotOutput(ns("future_plot"), height = "300px"))
              ),
              card(
                class = "forecast-panel",
                card_header("Accuracy summary"),
                card_body(
                  class = "forecast-copy-panel",
                  uiOutput(ns("accuracy_summary"))
                )
              ),
              card(
                class = "forecast-panel",
                card_header("Series summary"),
                card_body(
                  class = "forecast-copy-panel",
                  uiOutput(ns("series_summary"))
                )
              )
            )
          ),
          nav_panel(
            "Model Studio",
            div(
              class = "forecast-tab-grid forecast-tab-grid--model",
              card(
                class = "forecast-panel forecast-panel--hero",
                card_header("Model leaderboard"),
                card_body(plotOutput(ns("leaderboard_plot"), height = "300px"))
              ),
              card(
                class = "forecast-panel forecast-panel--hero",
                card_header("Holdout residual comparison"),
                card_body(plotOutput(ns("residual_plot"), height = "300px"))
              ),
              card(
                class = "forecast-panel forecast-panel--compact-copy",
                card_header("Engine status"),
                card_body(uiOutput(ns("engine_status")))
              ),
              card(
                class = "forecast-panel forecast-panel--compact-copy",
                card_header("Model interpretation"),
                card_body(uiOutput(ns("model_interpretation")))
              )
            )
          ),
          nav_panel(
            "Diagnostics",
            div(
              class = "forecast-tab-grid forecast-tab-grid--diagnostics",
              card(
                class = "forecast-panel forecast-panel--hero",
                card_header("Raw time series"),
                card_body(plotOutput(ns("raw_series_plot"), height = "250px"))
              ),
              card(
                class = "forecast-panel forecast-panel--hero",
                card_header("Seasonal pattern"),
                card_body(plotOutput(ns("seasonal_plot"), height = "250px"))
              ),
              card(
                class = "forecast-panel forecast-panel--hero",
                card_header("Decomposition"),
                card_body(plotOutput(ns("decomposition_plot"), height = "250px"))
              ),
              card(
                class = "forecast-panel",
                card_header("Split summary"),
                card_body(
                  class = "forecast-table-panel",
                  div(class = "forecast-table-wrap", DT::DTOutput(ns("split_table")))
                )
              )
            )
          ),
          nav_panel(
            "Context",
            div(
              class = "forecast-tab-grid forecast-tab-grid--context",
              card(
                class = "forecast-panel forecast-panel--wide",
                card_header("Tourism performance context"),
                card_body(plotOutput(ns("context_plot"), height = "360px"))
              ),
              card(
                class = "forecast-panel",
                card_header("Interpretation guide"),
                card_body(
                  tags$ol(
                    tags$li("Start from Holdout comparison to judge whether the model beats the baseline on unseen months."),
                    tags$li("Move to Model Studio to understand which model wins and where forecast errors remain."),
                    tags$li("Use Diagnostics to decide whether trend breaks or seasonality help explain the model result."),
                    tags$li("Use Context to compare arrivals against hotel occupancy, stay length, and room revenue before drawing business conclusions.")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}
