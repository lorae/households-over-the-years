# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

R-based analysis of how American household size and configuration have changed, using IPUMS Census/ACS microdata. The repo contains two independent sub-projects sharing a common data layer and R environment:

1. **k-means-clustering/** — K-means clustering to identify household archetypes (1900-2023). On hold.
2. **five-decade-aggregates/** — Descriptive statistics on household crowding and subfamily detection (1970-2020). Active research.

## Commands

**Restore dependencies (required first time):**
```r
renv::restore()
```

**Run the k-means clustering pipeline:**
```r
source("k-means-clustering/run-all.R")
```

**Run the five-decade aggregates pipeline:**
```r
source("five-decade-aggregates/run-all.R")
```

**Run individual analysis scripts** (each script is self-contained and can be sourced independently):
```r
source("k-means-clustering/src/figures/fig01-hhsize-decades-line.R")
source("five-decade-aggregates/src/generate-crowding-overall-subgroup.R")
```

**Run tests:**
```r
testthat::test_dir("five-decade-aggregates/tests/testthat")
```

## Architecture

### Sub-project: k-means-clustering/

Entry point: `k-means-clustering/run-all.R`

1. **Import** (`src/import-ipums-usa.R`) — Fetches 1900-2023 microdata from IPUMS API, stores in DuckDB at `data/db/ipums.duckdb`
2. **Aggregate to household** (`src/process-ipums-usa-household.R`) — Groups by SERIAL+YEAR, computes household composition
3. **Figures** (`src/figures/fig01-fig04`) — Descriptive line charts
4. **Clustering** (`src/cluster-elbow-plot.R`, `src/k6/`, `src/k9/`) — K-means model selection and archetype analysis

Storage:
- `k-means-clustering/output/figures/` — Plots (PNG/JPEG)
- `k-means-clustering/output/figure-data/` — CSV data underlying figures
- `k-means-clustering/output/tables/` — Cluster analysis tables
- `k-means-clustering/throughput/` — Intermediate RDS files (gitignored)

### Sub-project: five-decade-aggregates/

Entry point: `five-decade-aggregates/run-all.R`

1. **Import** (`src/import-ipums-usa-1970-2020.R`, `src/import-ipums-cps-1970-2020.R`) — Fetches 1970-2020 microdata
2. **Process person-level** (`src/process-ipums-usa-person-1970-2020.R`, `src/process-ipums-cps-person-1970-2020.R`) — Adds computed columns, adjusts dollars using `reference/inflators-1970-2020.csv`
3. **Generate tables** (`src/generate-*.R`) — Crowding, household size, doubled-up statistics by subgroup
4. **Figures & formatted tables** (`src/figures/`) — Choropleths, line charts, stacked bars, Excel tables

Storage:
- `five-decade-aggregates/output/raw/` — CSV data tables
- `five-decade-aggregates/output/` — Figures (PNG/JPEG) and formatted Excel tables
- `five-decade-aggregates/reference/` — Inflation adjustment data

### Shared resources

- `data/` — DuckDB databases and raw microdata (gitignored contents)
- `renv/`, `renv.lock`, `.Rprofile` — Shared R environment
- `.Renviron` — IPUMS API key (gitignored)

### Key External Dependency

The **demographr** package (`../demographr`) must be cloned as a sibling directory. Scripts load it via `devtools::load_all("../demographr")`.

### Environment Setup

Requires `.Renviron` in project root with `IPUMS_API_KEY=<key>` (copy from `example.Renviron`). R loads this automatically via `.Rprofile` + renv.

## Conventions

- **Package management**: `renv` with `renv.lock` pinning all versions. Run `renv::snapshot()` after adding packages.
- **Variable naming**: IPUMS variables are UPPERCASE (`NUMPREC`, `RELATE`, `YEAR`); derived variables use snake_case (`hh_size`, `age_bucket`).
- **Script naming**: Figures follow `fig##-description.R` (k-means) or descriptive names (five-decade); tables follow `table-description.R`.
- **Database pattern**: Scripts connect to DuckDB, query via `dbplyr`/`dplyr`, then `dbDisconnect()` at the end.
- **Household filter**: Only non-institutional households (`GQ %in% c(0, 1, 2)`).
- **All scripts run from repo root**: Paths in scripts are relative to the repo root (set by .Rproj), not to the script's own location.
