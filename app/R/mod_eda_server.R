library(shiny)
library(dplyr)
library(plotly)

eda_stat_card <- function(label, value, note = NULL, extra_class = NULL) {
  tags$div(
    class = paste("va-stat", extra_class),
    tags$div(class = "va-stat-label", label),
    tags$div(class = "va-stat-value", value),
    if (!is.null(note)) tags$div(class = "va-stat-note-text", note)
  )
}

eda_map_plot_object <- function(df, year_value) {
  df$text <- paste0(
    "<b>", df$country, "</b><br>",
    "Year: ", year_value, "<br>",
    "Annual arrivals: ", format(round(df$total_arrivals, 0), big.mark = ","), "<br>",
    "Average monthly arrivals: ", format(round(df$avg_monthly_arrivals, 0), big.mark = ",")
  )

  plotly::plot_ly(
    data = df,
    type = "choropleth",
    locations = ~map_country,
    locationmode = "country names",
    z = ~total_arrivals,
    text = ~text,
    hovertemplate = "%{text}<extra></extra>",
    colorscale = list(
      list(0, "#f5ead6"),
      list(0.5, "#d98a58"),
      list(1, "#8e3d24")
    ),
    marker = list(line = list(color = "#fffaf2", width = 0.5)),
    colorbar = list(title = "Annual arrivals")
  ) |>
    plotly::layout(
      margin = list(l = 0, r = 0, t = 10, b = 0),
      geo = list(
        showframe = FALSE,
        showcoastlines = TRUE,
        coastlinecolor = "#d9c9b5",
        projection = list(type = "natural earth"),
        bgcolor = "rgba(0,0,0,0)"
      ),
      paper_bgcolor = "rgba(0,0,0,0)"
    ) |>
    plotly::config(displayModeBar = FALSE)
}

eda_rank_plot_object <- function(df, highlight_label = NULL) {
  highlight_match <- rep(FALSE, nrow(df))

  if (!is.null(highlight_label) && nzchar(highlight_label)) {
    highlight_match <- df$label == highlight_label
  }

  df <- df |>
    arrange(.data$avg_monthly_arrivals) |>
    mutate(
      country_display = factor(.data$country, levels = .data$country),
      bar_color = if_else(highlight_match, "#d86f45", "#0f6b6f"),
      share_text = paste0(round(.data$overall_share_percent, 1), "%"),
      hover_text = paste0(
        "<b>", .data$country, "</b><br>",
        "Rank: ", .data$rank, "<br>",
        "Average monthly arrivals: ", format(round(.data$avg_monthly_arrivals, 0), big.mark = ","), "<br>",
        "Market share: ", sprintf("%.1f%%", .data$overall_share_percent)
      )
    )

  plotly::plot_ly(
    data = df,
    x = ~avg_monthly_arrivals,
    y = ~country_display,
    type = "bar",
    orientation = "h",
    marker = list(color = df$bar_color),
    text = ~share_text,
    textposition = "outside",
    hovertext = ~hover_text,
    hoverinfo = "text",
    cliponaxis = FALSE
  ) |>
    plotly::layout(
      margin = list(l = 110, r = 20, t = 10, b = 55),
      xaxis = list(title = "Average monthly arrivals", tickformat = ","),
      yaxis = list(title = ""),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    ) |>
    plotly::config(displayModeBar = FALSE)
}

mod_eda_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    shared_eda_data <- reactive({
      shared <- data()
      req(shared)

      required_fields <- c("eda_context_long", "eda_country_long")
      if (!all(required_fields %in% names(shared))) {
        stop("Shared EDA data is missing from load_tourism_data().")
      }

      shared
    })

    selected_period_ref <- reactive({
      tourism_period_reference() |>
        filter(period == input$ranking_period)
    })

    observeEvent(input$ranking_period, {
      ref <- selected_period_ref()
      req(nrow(ref) == 1)

      updateSliderInput(
        session = session,
        inputId = "map_year",
        label = paste("Map year in", ref$period_label[[1]]),
        min = ref$start_year[[1]],
        max = ref$end_year[[1]],
        value = ref$end_year[[1]]
      )
    }, ignoreNULL = FALSE)

    country_long <- reactive({
      shared_eda_data()$eda_country_long
    })

    geo_year <- reactive({
      prepare_eda_geo_year(country_long())
    })

    selected_ranking <- reactive({
      prepare_eda_period_rankings(
        period = input$ranking_period,
        top_n = input$top_n,
        country_long = country_long(),
        total_arrivals_long = shared_eda_data()$eda_context_long
      )
    })

    pre_ranking <- reactive({
      prepare_eda_period_rankings(
        period = "pre_covid",
        top_n = input$top_n,
        country_long = country_long(),
        total_arrivals_long = shared_eda_data()$eda_context_long
      )
    })

    recovery_ranking <- reactive({
      prepare_eda_period_rankings(
        period = "recovery",
        top_n = input$top_n,
        country_long = country_long(),
        total_arrivals_long = shared_eda_data()$eda_context_long
      )
    })

    full_pre_ranking <- reactive({
      prepare_eda_period_rankings(
        period = "pre_covid",
        top_n = dplyr::n_distinct(country_long()$country),
        country_long = country_long(),
        total_arrivals_long = shared_eda_data()$eda_context_long
      )
    })

    full_recovery_ranking <- reactive({
      prepare_eda_period_rankings(
        period = "recovery",
        top_n = dplyr::n_distinct(country_long()$country),
        country_long = country_long(),
        total_arrivals_long = shared_eda_data()$eda_context_long
      )
    })

    output$overview_stats <- renderUI({
      year_df <- geo_year() |>
        filter(year == input$map_year)
      ranking_df <- selected_ranking()
      pre_df <- pre_ranking()
      recovery_df <- recovery_ranking()

      validate(
        need(nrow(year_df) > 0, "No map data is available for the selected year."),
        need(nrow(ranking_df) > 0, "No ranking data is available for the selected period.")
      )

      year_leader <- year_df |>
        arrange(desc(total_arrivals), country) |>
        slice(1)
      period_leader <- ranking_df |>
        arrange(rank) |>
        slice(1)
      pre_leader <- pre_df |>
        arrange(rank) |>
        slice(1)
      recovery_leader <- recovery_df |>
        arrange(rank) |>
        slice(1)
      selected_top_share <- sum(ranking_df$overall_share_percent, na.rm = TRUE)

      tags$div(
        class = "va-stat-grid",
        eda_stat_card(
          label = paste("Map leader", input$map_year),
          value = year_leader$country[[1]],
          note = paste(format(round(year_leader$total_arrivals[[1]], 0), big.mark = ","), "annual arrivals")
        ),
        eda_stat_card(
          label = "Selected period leader",
          value = period_leader$country[[1]],
          note = paste(sprintf("%.1f%%", period_leader$overall_share_percent[[1]]), "average market share")
        ),
        eda_stat_card(
          label = paste("Top", input$top_n, "market share"),
          value = sprintf("%.1f%%", selected_top_share),
          note = paste("Combined share in", tourism_period_label(input$ranking_period))
        ),
        eda_stat_card(
          label = "Leader shift",
          value = paste(pre_leader$country[[1]], "/", recovery_leader$country[[1]]),
          note = "Pre-COVID vs recovery",
          extra_class = "va-stat-note"
        )
      )
    })

    output$market_map <- plotly::renderPlotly({
      map_df <- geo_year() |>
        filter(year == input$map_year)

      validate(
        need(nrow(map_df) > 0, "No yearly country totals are available for the selected year.")
      )

      eda_map_plot_object(map_df, year_value = input$map_year)
    })

    output$map_insight <- renderUI({
      map_df <- geo_year() |>
        filter(year == input$map_year) |>
        arrange(desc(total_arrivals), country)
      period_leader <- selected_ranking() |>
        arrange(rank) |>
        slice(1)

      validate(
        need(nrow(map_df) > 0, "No yearly insight is available for the selected year.")
      )

      leader <- map_df |> slice(1)
      top_five_share <- sum(head(map_df$total_arrivals, 5), na.rm = TRUE) / sum(map_df$total_arrivals, na.rm = TRUE) * 100

      tags$div(
        class = "va-insight-stack",
        tags$div(
          class = "va-insight-pill",
          paste("Map leader:", leader$country[[1]])
        ),
        tags$p(
          class = "va-insight-main",
          sprintf(
            "In %s, %s contributes the largest annual arrival total among the country-level source markets shown on the map. The top five mapped countries account for %.1f%% of the year's total, which keeps the concentration story visible.",
            input$map_year,
            leader$country[[1]],
            top_five_share
          )
        ),
        tags$p(
          class = "va-insight-secondary",
          sprintf(
            "For the selected ranking window, the leading market is %s. This keeps the map view and the ranking view aligned to the same EDA story.",
            period_leader$country[[1]]
          )
        )
      )
    })

    output$pre_rank_plot <- plotly::renderPlotly({
      eda_rank_plot_object(pre_ranking())
    })

    output$recovery_rank_plot <- plotly::renderPlotly({
      eda_rank_plot_object(recovery_ranking())
    })

    output$period_rank_table <- DT::renderDT({
      ranking_df <- selected_ranking() |>
        transmute(
          Rank = rank,
          Country = country,
          `Average monthly arrivals` = format(round(avg_monthly_arrivals, 0), big.mark = ","),
          `Market share (%)` = sprintf("%.2f", overall_share_percent)
        )

      DT::datatable(
        ranking_df,
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE)
      )
    })

    output$ranking_insight <- renderUI({
      ranking_df <- selected_ranking()

      pre_leader <- full_pre_ranking() |>
        slice(1)
      recovery_leader <- full_recovery_ranking() |>
        slice(1)
      period_leader <- ranking_df |>
        arrange(rank) |>
        slice(1)

      tags$div(
        class = "va-insight-stack",
        tags$div(
          class = "va-insight-pill",
          paste("Selected period:", tourism_period_label(input$ranking_period))
        ),
        tags$p(
          class = "va-insight-main",
          sprintf(
            "%s leads the selected ranking with %s average monthly arrivals and a %.1f%% share of Singapore's total visitor arrivals.",
            period_leader$country[[1]],
            format(round(period_leader$avg_monthly_arrivals[[1]], 0), big.mark = ","),
            period_leader$overall_share_percent[[1]]
          )
        ),
        if (nrow(pre_leader) == 1 && nrow(recovery_leader) == 1) {
          tags$p(
            class = "va-insight-secondary",
            sprintf(
              "The leading market changes from %s in the pre-COVID window to %s in the recovery window, which is the main before-versus-after story in this ranking view.",
              pre_leader$country[[1]],
              recovery_leader$country[[1]]
            )
          )
        } else {
          NULL
        }
      )
    })
  })
}
