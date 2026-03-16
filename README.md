# Singapore Tourism Recovery Visual Analytics Prototype

This project combines **Quarto coursework publishing** and a **modular Shiny prototype** for:

Take-home Exercise 2: Prototyping Modules for Visual Analytics Shiny Application  
Due date: **March 15, 2026**

## What This Starter Includes

1. Submission-ready Quarto page for Take-home 2.
2. Cluster-analysis prototype module documentation and UI storyboard.
3. Modular Shiny app skeleton connected to the prepared tourism dataset.
4. Team onboarding page and collaboration workflow guidance.

## Folder Structure

```text
group_project/
├─ _quarto.yml
├─ index.qmd
├─ take-home-ex2.qmd
├─ app-guide.qmd
├─ prototype/
│  ├─ CDA.qmd
│  ├─ EDA.qmd
│  ├─ module-cluster.qmd
   ├─ package-audit.qmd
   └─ ui-storyboard.qmd
├─ team/
│  └─ onboarding.qmd
├─ styles/
│  └─ theme.scss
├─ app/
│  ├─ app.R
│  └─ R/
│     ├─ data_utils.R
│     ├─ mod_cluster_ui.R
│     └─ mod_cluster_server.R
├─ scripts/
│  ├─ start-dev.ps1
│  ├─ check_cran_support.R
│  └─ prototype_smoke_test.R
├─ data/
│  ├─ raw/
│  └─ references/
└─ artifacts/
   ├─ plots/
   └─ tables/
```

## Data and Reference Inputs Imported

1. `data/raw/tourism_four_part_analysis_ready.xlsx`
2. `data/references/tourism_analysis_bilingual_plan.txt`
3. `data/references/tourism_data_cleaning_detailed_report.md`

## Quick Start

Open the RStudio project file in the repository root:

`smu-tourism-recovery-va-group-project.Rproj`

1. Run package audit and smoke tests:

```bash
G:/SMU/visual/support/R-4.5.2/bin/Rscript.exe scripts/check_cran_support.R
G:/SMU/visual/support/R-4.5.2/bin/Rscript.exe scripts/prototype_smoke_test.R
```

2. Start Quarto preview:

```bash
quarto preview
```

3. Start Shiny app:

```bash
Rscript run_app.R 3838
```

## Optional One-Click Start (Windows)

```powershell
powershell -ExecutionPolicy Bypass -File scripts/start-dev.ps1
```

Dry run:

```powershell
powershell -ExecutionPolicy Bypass -File scripts/start-dev.ps1 -DryRun
```

## Collaboration Workflow

1. Create feature branch per module/page.
2. Run `quarto render` before opening PR.
3. Run app smoke test before merging module code.

## Design References Used

1. [Habari Tanzania](https://thehabaritanzania.netlify.app/#team)
2. [Decoding Chaos](https://decoding-chaos.netlify.app/)

## Repository

[smu-tourism-recovery-va-group-project](https://github.com/ZoRan-Wang/smu-tourism-recovery-va-group-project)

Team members can clone directly with this link.  
For write access, add collaborators in repository `Settings -> Collaborators`.

## Live Site

[GitHub Pages](https://zoran-wang.github.io/smu-tourism-recovery-va-group-project/)
