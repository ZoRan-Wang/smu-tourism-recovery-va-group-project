library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)

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

    selected_series <- reactive({
      req(input$series_label)
      prepare_forecast_series(data()$long_monthly, input$series_label)
    })

    forecast_results <- eventReactive(input$run_forecast, {
      series_df <- selected_series()

      validate(
        need(nrow(series_df) > input$horizon + 12, "Series is too short for the current horizon.")
      )

      run_forecast_workflow(
        series_df = series_df,
        horizon = input$horizon,
        engine = "auto"
      )
    }, ignoreNULL = TRUE)

    output$series_summary <- renderText({
      series_df <- selected_series()
      paste(
        "Observations:", nrow(series_df),
        "| Start:", format(min(series_df$date), "%Y-%m"),
        "| End:", format(max(series_df$date), "%Y-%m"),
        "| Scope: country-level arrivals"
      )
    })

    output$split_table <- DT::renderDT({
      req(input$run_forecast > 0)
      res <- forecast_results()
      split_tbl <- tibble(
        segment = c("Training", "Testing"),
        start = c(min(res$training$date), min(res$testing$date)),
        end = c(max(res$training$date), max(res$testing$date)),
        n_obs = c(nrow(res$training), nrow(res$testing))
      )

      DT::datatable(
        split_tbl,
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE)
      )
    })

    output$forecast_plot <- renderPlot({
      req(input$run_forecast > 0)
      res <- forecast_results()

      plot_forecast_results(res, type = "holdout") +
        labs(
          title = input$series_label,
          subtitle = paste("Holdout horizon:", input$horizon, "months | Engine:", res$engine_label)
        )
    })

    output$context_plot <- renderPlot({
      req(input$series_label)

      context_df <- prepare_country_context_panel(
        data()$long_monthly,
        country_label = input$series_label
      )

      validate(
        need(nrow(context_df) > 0, "Supporting tourism indicators are not available for the current dataset.")
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

    output$raw_series_plot <- renderPlot({
      series_df <- selected_series()

      ggplot(series_df, aes(x = date, y = value)) +
        geom_line(linewidth = 1, color = "#0f6b6f") +
        geom_point(size = 1.8, color = "#d86f45") +
        labs(
          title = input$series_label,
          subtitle = "Monthly country-level visitor arrivals used for forecasting",
          x = NULL,
          y = "Visitor arrivals (person)"
        ) +
        scale_y_continuous(labels = scales::label_comma()) +
        theme_minimal(base_size = 13)
    })

    output$seasonal_plot <- renderPlot({
      series_df <- selected_series()

      series_df |>
        mutate(
          month_lab = month(date, label = TRUE, abbr = TRUE),
          year_num = year(date)
        ) |>
        ggplot(aes(x = month_lab, y = value, group = year_num, color = factor(year_num))) +
        geom_line(linewidth = 0.8, alpha = 0.65) +
        geom_point(size = 1.3, alpha = 0.8) +
        labs(
          title = "Seasonal Comparison by Month",
          subtitle = "Each coloured line represents one year",
          x = NULL,
          y = "Visitor arrivals (person)",
          color = "Year"
        ) +
        scale_y_continuous(labels = scales::label_comma()) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "none")
    })

    output$decomposition_plot <- renderPlot({
      req(input$run_forecast > 0)
      series_df <- selected_series()

      validate(
        need(nrow(series_df) >= 24, "At least 24 monthly observations are required for decomposition.")
      )

      ts_series <- ts(
        series_df$value,
        start = c(year(min(series_df$date)), month(min(series_df$date))),
        frequency = 12
      )

      decomposed <- stats::stl(ts_series, s.window = "periodic")
      forecast::autoplot(decomposed) +
        labs(
          title = "Trend / Seasonal / Remainder Decomposition",
          subtitle = "Used to explain the structural change before forecasting"
        )
    })

    output$accuracy_table <- DT::renderDT({
      req(input$run_forecast > 0)
      res <- forecast_results()
      accuracy_tbl <- res$accuracy_tbl |>
        mutate(across(where(is.numeric), ~ round(.x, 3)))

      DT::datatable(
        accuracy_tbl,
        rownames = FALSE,
        options = list(dom = "t", pageLength = 6, scrollX = TRUE)
      )
    })
  })
}
