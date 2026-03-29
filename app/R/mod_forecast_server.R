library(shiny)
library(dplyr)
library(ggplot2)

mod_forecast_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observe({
      choices <- list_country_arrival_series(data()$long_monthly)
      updateSelectInput(
        session,
        "series_label",
        choices = choices$label,
        selected = "Visitor Arrivals: China"
      )
    })

    forecast_results <- eventReactive(input$run_forecast, {
      validate(
        need(!is.null(input$series_label) && nzchar(input$series_label), "Select a target series.")
      )

      series_df <- prepare_forecast_series(data()$long_monthly, input$series_label)

      validate(
        need(nrow(series_df) > input$horizon + 12, "Series is too short for the current horizon.")
      )

      run_forecast_models(
        series_df = series_df,
        horizon = input$horizon,
        model_choice = input$model_choice
      )
    }, ignoreNULL = FALSE)

    output$series_summary <- renderText({
      series_df <- prepare_forecast_series(data()$long_monthly, input$series_label)
      paste(
        "Observations:", nrow(series_df),
        "| Start:", format(min(series_df$date), "%Y-%m"),
        "| End:", format(max(series_df$date), "%Y-%m"),
        "| Scope: country-level arrivals"
      )
    })

    output$forecast_plot <- renderPlot({
      res <- forecast_results()

      ggplot() +
        geom_line(
          data = res$fitted_df,
          aes(x = date, y = value, color = series),
          linewidth = 0.9
        ) +
        geom_line(
          data = res$forecast_df,
          aes(x = date, y = value, color = series),
          linewidth = 1.1,
          linetype = "dashed"
        ) +
        scale_color_manual(
          values = c(
            "Training" = "#0f6b6f",
            "Test" = "#d86f45",
            "Seasonal Naive" = "#6c757d",
            "ARIMA" = "#7a3e9d",
            "ETS" = "#7a3e9d"
          )
        ) +
        labs(
          title = input$series_label,
          subtitle = paste("Holdout horizon:", input$horizon, "months"),
          x = NULL,
          y = NULL,
          color = NULL
        ) +
        scale_y_continuous(labels = scales::label_comma()) +
        theme_minimal(base_size = 13)
    })

    output$context_plot <- renderPlot({
      req(input$series_label)

      context_df <- prepare_country_context_panel(
        data()$long_monthly,
        country_label = input$series_label
      )

      ggplot(context_df, aes(x = date, y = normalized_value, color = label)) +
        geom_line(linewidth = 1) +
        labs(
          title = "Country Arrivals vs Tourism Performance Indicators",
          subtitle = "All series are normalized so that direction and turning points can be compared",
          x = NULL,
          y = "Normalized z-score",
          color = NULL
        ) +
        theme_minimal(base_size = 13)
    })

    output$accuracy_table <- DT::renderDT({
      res <- forecast_results()
      DT::datatable(
        res$accuracy_tbl,
        rownames = FALSE,
        options = list(dom = "t", pageLength = 5, scrollX = TRUE)
      )
    })
  })
}
