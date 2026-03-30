library(shiny)
library(bslib)

mod_forecast_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      selectInput(ns("series_label"), "Country Arrival Series", choices = NULL),
      sliderInput(ns("horizon"), "Test / Forecast Horizon", min = 6, max = 18, value = 12, step = 1),
      radioButtons(
        ns("model_choice"),
        "Forecast Model",
        choices = c("ARIMA", "ETS"),
        selected = "ARIMA",
        inline = TRUE
      ),
      actionButton(ns("run_forecast"), "Run Forecasting", class = "btn-primary"),
      hr(),
      p("Baseline model: Seasonal Naive"),
      p("Advanced models: Auto ARIMA or ETS"),
      p("Scope: country-level visitor arrivals only")
    ),
    layout_column_wrap(
      width = 1 / 2,
      fill = FALSE,
      card(
        card_header("Forecast Plot"),
        card_body(plotOutput(ns("forecast_plot"), height = "360px"))
      ),
      card(
        card_header("Tourism Performance Context"),
        card_body(plotOutput(ns("context_plot"), height = "360px"))
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
        card_header("Forecast Notes"),
        card_body(
          p("The module compares a seasonal-naive baseline against one forecasting model on the same holdout window."),
          p("The selector is restricted to country-level visitor-arrival series from the shared tourism workbook.")
        )
      )
    )
  )
}
