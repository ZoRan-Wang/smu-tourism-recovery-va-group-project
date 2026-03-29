# Singapore Tourism Recovery Visual Analytics Prototype

This project now centers on a **shared tourism time-series dataset** and a **three-module Shiny prototype**:

1. Time Series Explorer
2. Time Series Clustering
3. Time Series Forecasting

The older decision-tree and random-forest work under `team/jin-qinhao/Take-Home-Exercise2/` is kept as a legacy archive and is not the live project module.

## Core Dataset

Primary workbook:

- `data/raw/Name your insight (4).xlsx`

This workbook contains monthly tourism indicators such as:

- total visitor arrivals
- arrivals by country or region
- hotel room occupancy rate
- average length of stay
- number of hotels
- total room revenue

For the final forecasting direction, **country-level visitor arrivals** act as the common core dataset, while hotel occupancy, average length of stay, and room revenue are used as supporting indicators to explain how demand recovery is reflected in tourism performance.

## Project Structure

```text
group_project/
├── _quarto.yml
├── index.qmd
├── app-guide.qmd
├── user-guide.qmd
├── Proposal/
├── prototype/
│   ├── module-cluster.qmd
│   ├── forecasting.qmd
│   ├── package-audit.qmd
│   └── ui-storyboard.qmd
├── app/
│   ├── app.R
│   └── R/
│       ├── data_utils.R
│       ├── mod_cluster_ui.R
│       ├── mod_cluster_server.R
│       ├── mod_forecast_ui.R
│       └── mod_forecast_server.R
├── scripts/
│   ├── check_cran_support.R
│   └── prototype_smoke_test.R
├── data/
│   └── raw/
└── docs/
```

## What Each Module Does

### Time Series Explorer

- choose a monthly tourism series
- inspect recent trend and volatility
- check metadata such as source and unit

### Cluster Prototype

- turn monthly observations into recovery-state profiles
- compare clusters across pre-COVID, shock, and recovery periods
- review silhouette score and cluster profile table

### Forecasting Prototype

- select a country-level arrivals series
- choose a test horizon
- compare a baseline forecast and a model-based forecast
- compare the chosen country series against hotel and stay indicators
- inspect forecast accuracy and projected path

## Quick Start

### 1. Check required packages

```bash
"C:/Program Files/R/R-4.5.2/bin/Rscript.exe" scripts/check_cran_support.R
```

### 2. Run smoke test

```bash
"C:/Program Files/R/R-4.5.2/bin/Rscript.exe" scripts/prototype_smoke_test.R
```

### 3. Preview the Quarto site

```bash
quarto preview
```

### 4. Run the Shiny app

```bash
Rscript run_app.R 3838
```

## Definition of Done for Current Direction

1. `prototype/forecasting.qmd` renders successfully.
2. The app contains the explorer, clustering, and forecasting modules.
3. The user guide explains how to use the new app.
4. Validation scripts cover both clustering and forecasting dependencies.
