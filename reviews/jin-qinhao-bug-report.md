# Bug Report for jin-qinhao Branch

## Owner Branch
`jin-qinhao`

## Severity
P1

## Summary
The coursework Quarto page rewrites tracked data and output files during render, so `quarto render` is not idempotent.

## Introduced By
- Commit: `775ec00`
- Author: `qinhaojin2025-collab`
- Commit title: `Add JIN QINHAO take-home exercise 2 materials`

## Affected File
- `team/jin-qinhao/Take-Home-Exercise2/Take-Home-Exercise2.qmd`

## Affected Lines
- export cleaned data block: around lines 188-200
- export decision tree outputs block: around lines 443-472
- random forest export block: around lines 692-727

## What Is Wrong
The page does not just render analysis results for display. It actively writes back to tracked source directories during rendering:
- `data/tourism_monthly_clean.csv`
- `data/tourism_decision_tree_ready.csv`
- `data/tourism_four_part_analysis_ready.csv`
- `data/tourism_four_part_analysis_ready.xlsx`
- multiple `outputs/*.csv`, `outputs/*.html`, and `outputs/*.png`

Because these files are tracked in git, a normal `quarto render` changes the repository state.

## Why This Matters
The project workflow expects contributors to run `quarto render` before merge. At the moment, doing so produces repository noise even when no analytical logic has changed. That makes review harder and increases the chance of accidental commits of regenerated artifacts.

## Reproduction
1. Start from a clean clone of the repository.
2. Run `quarto render` from the repo root.
3. Run `git status --short`.
4. Observe modified files under:
   - `team/jin-qinhao/Take-Home-Exercise2/data/`
   - `team/jin-qinhao/Take-Home-Exercise2/outputs/`
   - generated docs that depend on those artifacts

## Recommended Fix
Use one of these patterns:
1. Move all write operations into dedicated prep scripts and keep the final Quarto page read-only.
2. Write temporary outputs to a git-ignored folder instead of tracked source directories.
3. If exported artifacts must remain in the repo, split generation from presentation so the published page does not regenerate them on every render.

## Review Outcome
This is a real workflow bug, not only a formatting issue. The page can render successfully, but it leaves the repo dirty immediately afterward.
