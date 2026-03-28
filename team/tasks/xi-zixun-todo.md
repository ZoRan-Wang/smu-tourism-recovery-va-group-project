# Xi Zixun TODO List

Updated: 2026-03-29
Branch: `Xi-Zixun`

## Core Goal
Rebuild the visual analysis track on the new full tourism time-series dataset and establish the shared processed-data foundation for the whole team.

## Required Deliverables
1. Create `scripts/prepare_common_timeseries_data.R`.
2. Create `data/processed/arrivals_country_long.csv` and `data/processed/arrivals_country_wide.csv`.
3. Rewrite `prototype/EDA.qmd` into `Prototype: Time Series Visual Analysis`.
4. Rewrite `index.qmd` so the homepage reflects the new comparative time-series direction.
5. Update `Proposal/Proposal.qmd` sections for background, problem reframing, common data, research questions, and visual analysis.
6. Add or replace proposal figures under `Proposal/picture/` generated from the new dataset.
7. Add a new pivot meeting record page for the instructor-directed change of direction.

## Must Show
- China versus selected peer markets.
- Indexed and raw time-series comparison.
- Share change or growth-rate change.
- At least one transport-mode comparison if it strengthens the story.
- A clear code -> result -> explanation flow.

## Files Owned
- `scripts/prepare_common_timeseries_data.R`
- `data/processed/*`
- `prototype/EDA.qmd`
- `index.qmd`
- `Proposal/Proposal.qmd` (intro, data, RQs, visual analysis section)
- `Proposal/picture/*` used by the visual-analysis section
- `meeting-records/second-meeting-record.qmd` or the equivalent standardized minutes path

## Do Not Spend Time On
- Expanding the old standalone `prototype/CDA.qmd` as a core final-project page.
- Keeping hotel occupancy as the main project story.
- Using the old four-part workbook as the primary shared dataset.

## Definition of Done
- `quarto render prototype/EDA.qmd` passes.
- The page uses the new full dataset or the shared processed outputs.
- The homepage and proposal no longer frame China as the only analytical unit.
