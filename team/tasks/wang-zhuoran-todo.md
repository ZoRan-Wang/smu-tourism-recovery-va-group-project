# Wang Zhuoran TODO List

Updated: 2026-03-29
Branch: `codex/WANG-ZHUORAN`

## Core Goal
Rebuild clustering as time-series clustering and align the app shell, clustering module, and storyboard with the new final-project direction.

## Required Deliverables
1. Rewrite `prototype/module-cluster.qmd` as a time-series clustering prototype.
2. Rewrite `app/R/mod_cluster_ui.R`.
3. Rewrite `app/R/mod_cluster_server.R`.
4. Update `app/app.R` so the app shell is ready for three modules: visual analysis, clustering, and forecasting.
5. Update `_quarto.yml` so the website navigation matches the new final architecture.
6. Rewrite `prototype/ui-storyboard.qmd` as the shared storyboard page for all three modules.
7. Update the clustering section and the UI/storyboard section in `Proposal/Proposal.qmd`.
8. Update `team/onboarding.qmd` so the branch ownership and workflow match the new team split.

## Must Show
- Cluster units are countries or transport series, not months.
- The clustering method is explicitly justified.
- Diagnostics and outputs are fully code-generated.
- The documented UI matches the actual Shiny controls.
- The storyboard covers all three final modules.

## Files Owned
- `prototype/module-cluster.qmd`
- `app/R/mod_cluster_ui.R`
- `app/R/mod_cluster_server.R`
- `app/app.R`
- `_quarto.yml`
- `prototype/ui-storyboard.qmd`
- `Proposal/Proposal.qmd` (clustering and UI/storyboard sections)
- `team/onboarding.qmd`

## Do Not Spend Time On
- Keeping the old month-based k-means design.
- Leaving the storyboard as a single-module cluster-only document.
- Preserving outdated navbar labels that reflect the old project direction.

## Definition of Done
- `quarto render prototype/module-cluster.qmd` passes.
- The clustering module runs on the shared processed data.
- The app shell and storyboard both reflect the final three-module structure.
