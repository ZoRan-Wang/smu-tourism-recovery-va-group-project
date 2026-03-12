# Full Check Report (2026-03-12)

## 1. Scope Checked

1. Migration to `G:\SMU\visual\group_project`
2. Repository publication and team onboarding link
3. Structure redesign against assignment + reference sites
4. Quarto build and output integrity
5. Shiny prototype module integrity
6. Data/reference import integrity
7. Package support + smoke-test outputs
8. Deployment status (GitHub Pages)

## 2. Results Summary

| Check Item | Result |
|---|---|
| Local migration complete | PASS |
| GitHub repo created and pushed | PASS |
| Team-accessible public link | PASS |
| Quarto render success | PASS |
| Shiny module source-load success | PASS |
| Assignment requirement coverage | PASS (initial architecture level) |
| Data/reference files imported | PASS |
| Package audit and smoke test | PASS |
| GitHub Pages live | PASS |

## 3. Evidence

### 3.1 Git/GitHub

1. Current branch: `main`
2. Remote:
   - `origin https://github.com/ZoRan-Wang/smu-tourism-recovery-va-group-project.git`
3. Latest commit: `b4af04ef9d5f043808af0338b1cc69c786eb7281`
4. Sync check:
   - `local = remote = b4af04ef9d5f043808af0338b1cc69c786eb7281`
5. Repo URL:
   - `https://github.com/ZoRan-Wang/smu-tourism-recovery-va-group-project`
6. Public accessibility check:
   - HTTP status `200`

### 3.2 GitHub Pages

1. Source: `main:/docs`
2. Build status: `built`
3. URL:
   - `https://zoran-wang.github.io/smu-tourism-recovery-va-group-project/`
4. Accessibility check:
   - HTTP status `200`

### 3.3 Quarto Build

1. Command: `quarto render`
2. Result: success
3. Generated pages include:
   - `docs/index.html`
   - `docs/take-home-ex2.html`
   - `docs/prototype/module-cluster.html`
   - `docs/prototype/package-audit.html`
   - `docs/prototype/ui-storyboard.html`
   - `docs/team/onboarding.html`

### 3.4 Shiny Prototype

1. Source-load check:
   - Command: `Rscript -e "setwd('app'); source('app.R')"`
   - Result: `app_source_ok`
2. Module files present:
   - `app/R/data_utils.R`
   - `app/R/mod_cluster_ui.R`
   - `app/R/mod_cluster_server.R`
3. Required controls/outputs implemented:
   - Period filter, features, k slider, seed, scaling toggle, run button
   - Silhouette score, cluster scatter, profile table, assignment preview

### 3.5 Data/References

1. Main dataset:
   - `data/raw/tourism_four_part_analysis_ready.xlsx`
   - SHA256: `0D193245FB54C6ABB26333D8065995BAF7CF7EC92B5F256F7112E3D12877023B`
2. Imported planning files:
   - `data/references/tourism_analysis_bilingual_plan.txt`
   - `data/references/tourism_data_cleaning_detailed_report.md`

### 3.6 Package Audit + Smoke Test

1. Package audit output:
   - `artifacts/tables/package_audit.csv`
   - all required packages: `cran_available = TRUE`, `installed = TRUE`
2. Smoke-test output:
   - `artifacts/plots/elbow_plot.png`
   - `artifacts/plots/cluster_scatter.png`
   - `artifacts/tables/cluster_profile.csv`
   - `artifacts/tables/smoke_test_summary.txt`
3. Current smoke metric:
   - `rows_used = 110`
   - `k_selected = 3`
   - `silhouette_mean = 0.6317`

## 4. Assignment-Requirement Coverage Mapping

| Requirement | Evidence | Status |
|---|---|---|
| Select one module | `take-home-ex2.qmd` declares Cluster Analysis | PASS |
| Check CRAN support | `prototype/package-audit.qmd` + `scripts/check_cran_support.R` | PASS |
| Test runnable code and visual outputs | `scripts/prototype_smoke_test.R` + artifacts | PASS |
| Define parameters and outputs for app | `prototype/module-cluster.qmd` + module UI/server files | PASS |
| Select suitable Shiny UI components | `prototype/ui-storyboard.qmd` and module UI file | PASS |
| Include UI design section | `take-home-ex2.qmd` section `## 5. UI Design` | PASS |
| Quarto HTML publication setup | `_quarto.yml` with `output-dir: docs`; Pages source is `/docs` | PASS |

## 5. Notes / Risks

1. This is a strong **initial architecture** and prototype baseline; it is not the final analytical write-up with full narrative results yet.
2. Optional packages previously observed as not installed (`factoextra`, `rpart.plot`) are not blocking the current implemented module.
3. Local generated folders/files `.quarto`, `_freeze`, `Rplots.pdf` are ignored by git and do not affect repository cleanliness.

## 6. Conclusion

The migration, publication, architecture redesign, and prototype baseline are complete and validated end-to-end.
