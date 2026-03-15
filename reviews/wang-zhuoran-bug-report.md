# Bug Report for WANG-ZHUORAN Branch

## Owner Branch
`codex/WANG-ZHUORAN`

## Severity
P2

## Summary
The clustering prototype document promises Shiny controls and outputs that the actual app does not implement.

## Introduced By
- Primary commit: `b1acbbb`
- Follow-up formatting commit: `776810b`
- Author: `ZoRan-Wang`
- Commit titles:
  - `Refine clustering prototype workflow`
  - `Refine clustering page into smaller analysis blocks`

## Affected Files
- `prototype/module-cluster.qmd`
- related implementation files for comparison:
  - `app/R/mod_cluster_ui.R`
  - `app/R/mod_cluster_server.R`

## Affected Lines
Prototype page claims these exposed UI elements and outputs in the current module:
- parameter table around lines 451-470
- output table around lines 480-498

Specifically, the page lists:
- `download_clusters`
- `downloadButton`
- `state_timeline_plot`
- `cluster_assignment_export`

But the current app implementation only exposes:
- silhouette text
- cluster scatter plot
- cluster profile table
- assignment preview table

## What Is Wrong
The report is written as if those timeline and export features are already part of the implemented Shiny module. They are not present in the current UI or server code.

## Why This Matters
This creates a mismatch between prototype documentation and executable behavior. A teammate reading the prototype page would reasonably expect those features to exist and might plan integration or review work around functionality that is not actually there.

## Reproduction
1. Read the UI parameter and output sections in `prototype/module-cluster.qmd`.
2. Compare them with the actual Shiny module files:
   - `app/R/mod_cluster_ui.R`
   - `app/R/mod_cluster_server.R`
3. Note that no download button, download handler, or timeline plot is implemented.

## Recommended Fix
Choose one direction and keep them aligned:
1. Implement the promised timeline/export features in the Shiny module, or
2. Mark them explicitly as planned extensions and limit the current-state tables to features that already exist.

## Review Outcome
This is a documentation-to-implementation drift issue. It does not block rendering, but it does weaken the reliability of the prototype specification.
