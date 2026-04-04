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
        p("Compare benchmark and model-based forecasts on country-level arrivals, then read the result through diagnostics and tourism-performance context within the integrated application.")
      ),
      div(
        class = "forecast-header-meta",
        div(class = "forecast-meta-chip", "Integrated with the shared arrivals backbone"),
        div(class = "forecast-meta-chip", "Holdout, diagnostics, and context in one module"),
        div(class = "forecast-meta-chip", "Preserves the three-module app structure")
      )
    ),
    div(
      class = "forecast-workspace",
      div(
        class = "forecast-controls",
        h3("Model controls"),
        p(
          class = "forecast-controls-intro",
          "Choose a source market, decide how the forecast engine should behave, and run the model comparison once the setup is ready."
        ),
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
        actionButton(ns("run_forecast"), "Run Forecasting", class = "btn-primary"),
        div(
          class = "forecast-controls-note",
          h4("What this module adds"),
          tags$ul(
            tags$li("It keeps the integrated app intact while giving forecasting a richer workspace."),
            tags$li("It exposes engine choice, diagnostics, residuals, and tourism context in one place."),
            tags$li("It compares models on the same holdout window and surfaces the best result automatically.")
          )
        )
      ),
      div(
        class = "forecast-main",
        uiOutput(ns("summary_cards")),
        navset_card_tab(
          id = ns("forecast_pages"),
          full_screen = FALSE,
          nav_panel(
            "Forecast",
            div(
              class = "forecast-tab-grid forecast-tab-grid--forecast",
              card(
                class = "forecast-panel forecast-panel--hero",
                card_header("Holdout comparison"),
                card_body(plotOutput(ns("forecast_plot"), height = "100%"))
              ),
              card(
                class = "forecast-panel forecast-panel--hero",
                card_header("Forward forecast"),
                card_body(plotOutput(ns("future_plot"), height = "100%"))
              ),
              card(
                class = "forecast-panel",
                card_header("Accuracy table"),
                card_body(
                  class = "forecast-table-panel",
                  div(class = "forecast-table-wrap", DT::DTOutput(ns("accuracy_table")))
                )
              ),
              card(
                class = "forecast-panel",
                card_header("Series summary"),
                card_body(textOutput(ns("series_summary")))
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
                card_body(plotOutput(ns("leaderboard_plot"), height = "100%"))
              ),
              card(
                class = "forecast-panel forecast-panel--hero",
                card_header("Holdout residual comparison"),
                card_body(plotOutput(ns("residual_plot"), height = "100%"))
              ),
              card(
                class = "forecast-panel",
                card_header("Engine status"),
                card_body(uiOutput(ns("engine_status")))
              ),
              card(
                class = "forecast-panel",
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
                card_body(plotOutput(ns("raw_series_plot"), height = "100%"))
              ),
              card(
                class = "forecast-panel forecast-panel--hero",
                card_header("Seasonal pattern"),
                card_body(plotOutput(ns("seasonal_plot"), height = "100%"))
              ),
              card(
                class = "forecast-panel forecast-panel--hero",
                card_header("Decomposition"),
                card_body(plotOutput(ns("decomposition_plot"), height = "100%"))
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
                card_body(plotOutput(ns("context_plot"), height = "100%"))
              ),
              card(
                class = "forecast-panel",
                card_header("Interpretation guide"),
                card_body(
                  tags$ol(
                    tags$li("Start from the holdout comparison to check whether the selected models improve on the baseline."),
                    tags$li("Use Model Studio to compare ranking results and residual behavior."),
                    tags$li("Read the diagnostic views before trusting a model that looks good on only one metric."),
                    tags$li("Use the tourism context panel to connect country arrivals with hotel pressure and broader performance indicators.")
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
