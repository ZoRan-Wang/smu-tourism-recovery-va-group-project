# Singapore Tourism Recovery Visual Analytics Prototype

This project combines **Quarto publishing** and a **modular Shiny prototype** around one shared tourism time-series backbone:

1. Time Series Visual Analysis
2. Time Series Clustering
3. Time Series Forecasting

## Core Data Contract

Shared arrivals backbone:

- `data/raw/visitor_arrivals_full_dataset.xlsx`
- `data/processed/arrivals_country_long.csv`
- `data/processed/arrivals_country_wide.csv`
- `data/processed/clustering_country_wide.csv`
- `data/processed/clustering_country_long.csv`
- `data/processed/clustering_series_metadata.csv`

Optional supporting tourism context:

- `data/raw/tourism_update.xlsx`

Country-level visitor arrivals remain the common analytical target across explorer, clustering, and forecasting. Hotel occupancy, average length of stay, number of hotels, and total room revenue are used only as optional supporting context for interpretation when that workbook is present.

## Folder Structure

```text
smu-tourism-recovery-va-group-project/
├── _quarto.yml
├── index.qmd
├── app-guide.qmd
├── user-guide.qmd
├── poster.qmd
├── Proposal/
├── prototype/
│   ├── EDA.qmd
│   ├── CDA.qmd
│   ├── module-cluster.qmd
│   ├── forecasting.qmd
│   ├── package-audit.qmd
│   ├── ui-storyboard.qmd
│   └── wang-zhuoran-review-report.qmd
├── app/
│   ├── app.R
│   ├── R/
│   │   ├── data_utils.R
│   │   ├── mod_cluster_ui.R
│   │   ├── mod_cluster_server.R
│   │   ├── mod_forecast_ui.R
│   │   └── mod_forecast_server.R
│   └── www/
│       └── app-theme.css
├── scripts/
│   ├── check_cran_support.R
│   ├── prepare_clustering_country_data.R
│   ├── prepare_common_timeseries_data.R
│   └── prototype_smoke_test.R
├── styles/
│   └── poster.css
├── data/
│   ├── raw/
│   └── processed/
├── docs/
└── team/
```

## What Each Module Does

### Time Series Visual Analysis

- choose a monthly tourism series
- inspect recent trend and volatility
- check metadata such as source and unit

### Time Series Clustering

- align country-level arrivals series over a shared year window
- normalize them into comparable trajectories
- group similar recovery patterns into country-level time-series clusters
- review the dashboard, representative pattern atlas, focus-market placement, and assignment tables

### Time Series Forecasting

- select a country-level arrivals series
- choose a test horizon
- compare a seasonal-naive baseline with ETS and ARIMA
- position the chosen country series against hotel occupancy, average stay, and room revenue
- inspect holdout accuracy and projected future path
- run the full `modeltime` workflow when available, otherwise fall back to a lighter `forecast` implementation with the same benchmark labels

## Poster Files

The final poster materials are stored in the following relative locations:

- Poster source file: `poster.qmd`
- Poster stylesheet: `styles/poster.css`
- Rendered website poster page: `docs/poster.html`
- Rendered poster stylesheet copy: `docs/styles/poster.css`
- A1 poster image export (PNG): `docs/poster-a1.png`
- A1 poster image export (JPEG): `docs/poster-a1.jpg`

Use `poster.qmd` and `styles/poster.css` when editing the poster.  
Use the files under `docs/` when checking the rendered version that will be published or shared.

## Legacy Content Removed

The project previously contained an older standalone modelling direction built around decision-tree and random-forest materials. Those files have now been removed so the final branch stays focused on the current three-module time-series workflow.

The cleanup included:

- removing legacy prototype pages such as the older decision-tree pages
- removing the retired standalone `take-home-ex2.qmd` path
- removing the old `shiny/` bundle that was tied to the retired modelling workflow
- removing archived decision-tree and random-forest plots, tables, and exported widgets
- removing the old `team/jin-qinhao/Take-Home-Exercise2/` module from the live project structure
- updating the poster and documentation so they no longer reference the retired modelling panels

The live project scope is now limited to:

1. Time Series Visual Analysis
2. Time Series Clustering
3. Time Series Forecasting

## Quick Start

### 1. Check required packages

```bash
Rscript scripts/check_cran_support.R
```

### 2. Refresh the clustering processed files if needed

```bash
Rscript scripts/prepare_clustering_country_data.R
```

### 3. Refresh the shared arrivals backbone if needed

```bash
Rscript scripts/prepare_common_timeseries_data.R
```

### 4. Run the smoke test

```bash
Rscript scripts/prototype_smoke_test.R
```

### 5. Preview the Quarto site

```bash
quarto preview
```

### 6. Run the Shiny app

```bash
Rscript run_app.R 3838
```

## Definition of Done for the Current Direction

1. `prototype/module-cluster.qmd` and `prototype/forecasting.qmd` render successfully.
2. The app contains the explorer, clustering, and forecasting modules.
3. The user guide explains the shared arrivals backbone and supporting tourism context.
4. Validation scripts cover both clustering and forecasting dependencies.
