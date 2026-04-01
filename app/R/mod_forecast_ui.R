library(shiny)
library(bslib)

mod_forecast_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      selectInput(ns("series_label"), "Country Arrival Series", choices = NULL),
      sliderInput(ns("horizon"), "Test / Forecast Horizon", min = 6, max = 18, value = 12, step = 1),
      checkboxGroupInput(
        ns("model_choices"),
        "Models to compare",
        choices = c("Seasonal Naive", "ETS", "ARIMA"),
        selected = c("Seasonal Naive", "ETS", "ARIMA")
      ),
      actionButton(ns("run_forecast"), "Run Forecasting", class = "btn-primary"),
      hr(),
      p("Click Run Forecasting to generate the outputs below."),
      p("Forecast stack: modeltime workflow when available, otherwise a forecast-package fallback using the same benchmark labels."),
      p("Scope: country-level visitor arrivals on the shared arrivals backbone"),
      p("Hotel occupancy, stay length, and room revenue are used only as supporting context")
    ),
    navset_card_tab(
      id = ns("forecast_pages"),
      nav_panel(
        "Forecast Plot",
        layout_column_wrap(
          width = 1 / 2,
          fill = FALSE,
          card(
            card_header("Testing and Forecast Comparison"),
            card_body(plotOutput(ns("forecast_plot"), height = "360px"))
          ),
          card(
            card_header("Accuracy Table"),
            card_body(DT::DTOutput(ns("accuracy_table")))
          ),
          card(
            card_header("Series Summary"),
            card_body(textOutput(ns("series_summary")))
          ),
          card(
            card_header("Split Summary"),
            card_body(DT::DTOutput(ns("split_table")))
          )
        )
      ),
      nav_panel(
        "Tourism Performance Context",
        card(
          card_header("Country Arrivals vs Supporting Tourism Indicators"),
          card_body(plotOutput(ns("context_plot"), height = "420px"))
        )
      ),
      nav_panel(
        "Trend and Seasonal",
        layout_column_wrap(
          width = 1 / 2,
          fill = FALSE,
          card(
            card_header("Raw Time Series"),
            card_body(plotOutput(ns("raw_series_plot"), height = "360px"))
          ),
          card(
            card_header("Seasonal Pattern by Month"),
            card_body(plotOutput(ns("seasonal_plot"), height = "360px"))
          )
        )
      ),
      nav_panel(
        "Decomposition",
        card(
          card_header("Trend / Seasonal / Remainder"),
          card_body(plotOutput(ns("decomposition_plot"), height = "520px"))
        )
      ),
      nav_panel(
        "Forecast Notes",
        card(
          card_header("Interpretation Guide"),
          card_body(
            tags$ol(
              tags$li("Read the context panel first to position the selected arrivals series against hotel and stay indicators."),
              tags$li("Use the trend, seasonal, and decomposition views to justify whether a forecasting workflow is appropriate."),
              tags$li("Interpret the forecast plot together with the accuracy table rather than relying on one model label alone."),
              tags$li("Treat the baseline Seasonal Naive result as the minimum benchmark the modeltime candidates must beat.")
            )
          )
        )
      )
    )
  )
}
