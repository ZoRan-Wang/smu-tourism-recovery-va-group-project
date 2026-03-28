# Jin Qinhao TODO List

Updated: 2026-03-29
Branch: `jin-qinhao`

## Core Goal
Replace the legacy decision-tree and random-forest direction with a forecasting module built on the new full tourism time-series dataset.

## Required Deliverables
1. Create `prototype/forecasting.qmd`.
2. Create `app/R/mod_forecast_ui.R` and `app/R/mod_forecast_server.R`.
3. Update `app-guide.qmd` so it documents the new three-module app instead of the legacy single-module cluster app.
4. Create a proper `user-guide.qmd` for the final project website.
5. Update the forecasting section and package list in `Proposal/Proposal.qmd`.
6. Update `scripts/check_cran_support.R` and `scripts/prototype_smoke_test.R` to include forecasting dependencies and checks.
7. Rewrite `README.md` so it matches the new data/model/app architecture.
8. Keep `team/jin-qinhao/Take-Home-Exercise2/` as a legacy archive and do not extend it as the main project module.

## Must Show
- A selectable target series.
- A train/test split.
- At least one baseline and one forecasting model.
- A forecast plot and an accuracy table.
- App controls for series selection, horizon, and model choice.

## Files Owned
- `prototype/forecasting.qmd`
- `app/R/mod_forecast_ui.R`
- `app/R/mod_forecast_server.R`
- `app-guide.qmd`
- `user-guide.qmd`
- `Proposal/Proposal.qmd` (forecasting section and package section)
- `scripts/check_cran_support.R`
- `scripts/prototype_smoke_test.R`
- `README.md`

## Do Not Spend Time On
- Extending decision tree or random forest as the final core module.
- Wiring the new app to the old take-home data folder.
- Treating `team/jin-qinhao/Take-Home-Exercise2/` as the live project app.

## Definition of Done
- `quarto render prototype/forecasting.qmd` passes.
- The forecast module runs inside the app.
- The user guide explains how to use the new three-module app.
