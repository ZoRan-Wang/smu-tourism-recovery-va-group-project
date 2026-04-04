library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(bslib)

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
      selected_models <- input$model_choices

      validate(
        need(nrow(series_df) > input$horizon + 12, "Series is too short for the current horizon."),
        need(length(selected_models) > 0, "Select at least one forecasting model.")
      )

      res <- run_forecast_workflow(
        series_df = series_df,
        horizon = input$horizon,
        engine = input$engine_preference
      )

      normalize_model_choice <- function(model_name) {
        dplyr::case_when(
          grepl("^ETS", model_name) ~ "ETS",
          grepl("^ARIMA", model_name) ~ "ARIMA",
          TRUE ~ "Seasonal Naive"
        )
      }

      selected_holdout <- res$holdout_forecast_tbl |>
        filter(normalize_model_choice(.model_desc) %in% selected_models)

      selected_accuracy <- res$accuracy_tbl |>
        filter(normalize_model_choice(.model_desc) %in% selected_models)

      selected_models_tbl <- res$models_tbl |>
        filter(normalize_model_choice(.model_desc) %in% selected_models)

      if (nrow(selected_accuracy) == 0 || nrow(selected_holdout) == 0) {
        stop("No forecast results matched the selected model choices.")
      }

      res$holdout_forecast_tbl <- selected_holdout
      res$accuracy_tbl <- selected_accuracy
      res$models_tbl <- selected_models_tbl
      res$selected_models <- selected_models
      res$requested_engine <- input$engine_preference
      res
    }, ignoreNULL = TRUE)

    output$series_summary <- renderUI({
      series_df <- selected_series()

      tagList(
        div(
          class = "forecast-copy-stack",
          div(class = "forecast-copy-row", tags$strong("Observations:"), format(nrow(series_df), big.mark = ",")),
          div(class = "forecast-copy-row", tags$strong("Coverage:"), paste(format(min(series_df$date), "%Y-%m"), "to", format(max(series_df$date), "%Y-%m"))),
          div(class = "forecast-copy-row", tags$strong("Scope:"), "Country-level visitor arrivals"),
          div(class = "forecast-copy-row", tags$strong("Models compared:"), paste(input$model_choices, collapse = ", ")),
          div(class = "forecast-copy-note", "This summary describes the current time series used in the holdout comparison and future forecast.")
        )
      )
    })

    output$summary_cards <- renderUI({
      req(input$run_forecast > 0)
      res <- forecast_results()
      metric <- req(input$rank_metric)
      metric_label <- toupper(metric)

      best_row <- res$accuracy_tbl |>
        arrange(.data[[metric]]) |>
        slice(1)

      second_row <- res$accuracy_tbl |>
        arrange(.data[[metric]]) |>
        slice(2)

      improvement_note <- if (nrow(second_row) == 1 &&
        !is.na(second_row[[metric]]) &&
        !is.na(best_row[[metric]])) {
        paste0(
          "Best model improves on the next candidate by ",
          round(second_row[[metric]] - best_row[[metric]], 2),
          " ",
          metric_label,
          "."
        )
      } else {
        "Only one model result is currently selected."
      }

      div(
        class = "forecast-stat-grid",
        div(
          class = "forecast-stat",
          div(class = "forecast-stat-label", "Selected series"),
          div(class = "forecast-stat-value forecast-stat-value--text", input$series_label),
          div(class = "forecast-stat-note", "Country-level arrivals")
        ),
        div(
          class = "forecast-stat",
          div(class = "forecast-stat-label", "Best model"),
          div(class = "forecast-stat-value forecast-stat-value--text", best_row$.model_desc),
          div(class = "forecast-stat-note", paste("Executed with", res$engine_label))
        ),
        div(
          class = "forecast-stat",
          div(class = "forecast-stat-label", paste("Best", metric_label)),
          div(class = "forecast-stat-value", format(round(best_row[[metric]], 2), big.mark = ",")),
          div(class = "forecast-stat-note", paste("Holdout", metric_label))
        ),
        div(
          class = "forecast-stat",
          div(class = "forecast-stat-label", "Forecast horizon"),
          div(class = "forecast-stat-value", input$horizon),
          div(class = "forecast-stat-note", "Months ahead")
        ),
        div(
          class = "forecast-stat forecast-stat--wide",
          div(class = "forecast-stat-label", "Quick interpretation"),
          div(class = "forecast-stat-copy", improvement_note)
        )
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
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE,
          scrollX = TRUE,
          scrollY = "170px",
          scrollCollapse = TRUE
        )
      )
    })

    output$forecast_plot <- renderPlot({
      req(input$run_forecast > 0)
      res <- forecast_results()

      plot_forecast_results(res, type = "holdout") +
        labs(
          title = input$series_label,
          subtitle = paste(
            "Holdout horizon:", input$horizon, "months | Engine:", res$engine_label,
            "| Models:", paste(res$selected_models, collapse = ", ")
          )
        )
    })

    output$future_plot <- renderPlot({
      req(input$run_forecast > 0)
      res <- forecast_results()

      plot_forecast_results(res, type = "future") +
        labs(
          title = input$series_label,
          subtitle = paste("Forward projection using best holdout model:", res$best_model_desc)
        )
    })

    output$context_plot <- renderPlot({
      req(input$series_label)

      context_df <- prepare_country_context_panel(
        data()$long_monthly,
        country_label = input$series_label,
        support_labels = input$context_indicators
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

    output$leaderboard_plot <- renderPlot({
      req(input$run_forecast > 0)
      res <- forecast_results()
      metric <- req(input$rank_metric)

      plot_tbl <- res$accuracy_tbl |>
        mutate(
          metric_value = .data[[metric]],
          .model_desc = reorder(.model_desc, metric_value)
        )

      validate(
        need(nrow(plot_tbl) > 0, "No model results are available for plotting.")
      )

      ggplot(plot_tbl, aes(x = .model_desc, y = metric_value, fill = .model_desc)) +
        geom_col(width = 0.7, show.legend = FALSE) +
        geom_text(aes(label = scales::number(metric_value, accuracy = 0.01)), hjust = -0.05, size = 4) +
        coord_flip(clip = "off") +
        scale_fill_manual(values = c(
          "Seasonal Naive" = "#d86f45",
          "ETS (Modeltime)" = "#0f6b6f",
          "ETS" = "#0f6b6f",
          "ARIMA" = "#6b4eff"
        )) +
        labs(
          title = paste("Models ranked by", toupper(metric)),
          subtitle = "Lower is better for RMSE, MAE, and MAPE",
          x = NULL,
          y = toupper(metric)
        ) +
        theme_minimal(base_size = 13) +
        theme(plot.margin = margin(5.5, 35, 5.5, 5.5))
    })

    output$residual_plot <- renderPlot({
      req(input$run_forecast > 0)
      res <- forecast_results()

      residual_tbl <- res$holdout_forecast_tbl |>
        mutate(residual = actual - prediction)

      ggplot(residual_tbl, aes(x = date, y = residual, color = .model_desc)) +
        geom_hline(yintercept = 0, color = "#8a9199", linetype = "dashed") +
        geom_line(linewidth = 0.9) +
        geom_point(size = 2) +
        scale_color_manual(values = c(
          "Seasonal Naive" = "#d86f45",
          "ETS (Modeltime)" = "#0f6b6f",
          "ETS" = "#0f6b6f",
          "ARIMA" = "#6b4eff"
        )) +
        labs(
          title = "Residual path on the holdout window",
          subtitle = "A tighter band around zero suggests better month-to-month tracking",
          x = NULL,
          y = "Actual - predicted",
          color = "Model"
        ) +
        scale_y_continuous(labels = scales::label_comma()) +
        theme_minimal(base_size = 13)
    })

    output$engine_status <- renderUI({
      req(input$run_forecast > 0)
      res <- forecast_results()
      stack_status <- forecast_stack_status()

      missing_pkgs <- stack_status$missing_modeltime_packages
      missing_text <- if (length(missing_pkgs) == 0) "None" else paste(missing_pkgs, collapse = ", ")

      div(
        class = "forecast-copy-stack",
        div(class = "forecast-copy-row", tags$strong("Requested:"), res$requested_engine),
        div(class = "forecast-copy-row", tags$strong("Executed:"), res$engine_label),
        div(class = "forecast-copy-row", tags$strong("Fallback ready:"), ifelse(stack_status$fallback_ready, "Yes", "No")),
        div(class = "forecast-copy-row", tags$strong("Modeltime ready:"), ifelse(stack_status$modeltime_ready, "Yes", "No")),
        div(class = "forecast-copy-row forecast-copy-row--small", tags$strong("Missing packages:"), missing_text),
        div(
          class = "forecast-copy-note",
          if (identical(res$requested_engine, "modeltime")) {
            "Require modeltime is a strict mode. If that stack is unavailable, the app stops instead of falling back."
          } else {
            "Auto mode prefers modeltime when available and otherwise uses the lightweight fallback."
          }
        )
      )
    })

    output$model_interpretation <- renderUI({
      req(input$run_forecast > 0)
      res <- forecast_results()
      best_row <- res$accuracy_tbl |>
        arrange(.data[[input$rank_metric]]) |>
        slice(1)

      second_row <- res$accuracy_tbl |>
        arrange(.data[[input$rank_metric]]) |>
        slice(2)

      gap_note <- if (nrow(second_row) == 1) {
        paste0(
          "On ",
          toupper(input$rank_metric),
          ", the winning model leads the next candidate by ",
          round(second_row[[input$rank_metric]] - best_row[[input$rank_metric]], 2),
          "."
        )
      } else {
        "Only one forecast line is currently active, so there is no direct model gap to compare."
      }

      div(
        class = "forecast-copy-stack",
        div(class = "forecast-copy-row", tags$strong("Best model:"), best_row$.model_desc),
        div(class = "forecast-copy-note", gap_note),
        tags$ul(
          class = "forecast-copy-list",
          tags$li(tags$strong("Seasonal Naive:"), " benchmark repeating the historical seasonal pattern."),
          tags$li(tags$strong("ETS:"), " best when level, trend, and seasonality evolve smoothly."),
          tags$li(tags$strong("ARIMA:"), " useful when autocorrelation adds extra predictive signal.")
        )
      )
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

    output$accuracy_summary <- renderUI({
      req(input$run_forecast > 0)
      res <- forecast_results()

      metric <- req(input$rank_metric)
      accuracy_tbl <- res$accuracy_tbl |>
        mutate(
          rank_value = .data[[metric]],
          rmse = round(rmse, 2),
          mae = round(mae, 2),
          mape = round(mape, 2)
        ) |>
        arrange(rank_value) |>
        mutate(rank_label = row_number())

      summary_items <- lapply(seq_len(nrow(accuracy_tbl)), function(i) {
        row <- accuracy_tbl[i, ]
        tags$li(
          tags$strong(paste0("#", row$rank_label, " ", row$.model_desc, ": ")),
          paste0(
            "RMSE ", format(row$rmse, big.mark = ","),
            " | MAE ", format(row$mae, big.mark = ","),
            " | MAPE ", row$mape, "%"
          )
        )
      })

      tagList(
        div(
          class = "forecast-copy-stack",
          div(class = "forecast-copy-row", tags$strong("Ranking metric:"), toupper(metric)),
          div(class = "forecast-copy-note", "Lower values indicate better holdout performance. The models below are ranked by the selected metric, while RMSE, MAE, and MAPE are shown together for quick comparison."),
          tags$ul(class = "forecast-copy-list forecast-copy-list--spacious", summary_items)
        )
      )
    })
  })
}
