# Bedroom Allocation — KOB Decomposition Plan

Scope: build the Kitagawa-Oaxaca-Blinder decomposition for the bedroom-allocation sub-project, comparing 1970 and 2020, applying 1970 coefficients ("preferences") to the 2020 population.

| Emoji | Type |
|-------|--------------|
| 🏗️ | Architecture / scaffolding |
| 🧮 | Math / statistical rigor |
| 📈 | Regression / data prep |
| 🧪 | Tests |
| 📊 | Figures / output |
| 📝 | Docs |
| ❓ | Open decision |

---

## Goal & deliverables

**Comparison:** 1970 Census 1% sample vs. 2020 ACS.

**Spec:**
- Observation: household (one row per HH; weighted by HHWT).
- Outcome: number of bedrooms (`bedrooms_recode`, filtered ≥ 0).
- Regressors: HOH age, HOH race/ethnicity, HOH education, HOH nativity, HOH sex, household income, tenure, state of residence, # children under 18, # adults.
- Including composition counts is intentional: the question is *"given household composition, how do bedroom choices differ between 1970 and 2020?"*

**Two deliverables:**

**(A) Aggregate decomposition.** Apply 1970 coefficients to 2020 population. Split the 1970→2020 mean-bedroom gap into endowment (composition), coefficient (preference), and intercept (level) components per variable. Scalar outputs with SEs.

**(B) Household-level counterfactual distribution.** For each 2020 household, compute predicted bedrooms under 1970 coefficients. Tabulate the distribution of (actual − predicted): how many 2020 HHs have more/fewer bedrooms than 1970 would have given them, cut by tenure / race / cohort / etc. This is the publishable-sentence deliverable.

---

## Open decisions (blocking downstream work)

- ❓ **Income: continuous or decile-bucketed?** Pending coauthor consultation. Affects regression specification, kob-build-input logic, interpretation of the income component. Continuous = one scalar bar, simpler story. Bucketed = non-linear contribution map, richer story but misspecification is moot (saturated in buckets). Either works with the SE machinery.
- ❓ **Age: continuous or 10-year bucketed?** Bucketed aligns with existing pipeline; continuous has same tradeoffs as income.
- ❓ **# children and # adults: treat as continuous count, or top-coded categorical (0, 1, 2, 3, 4+)?** Both defensible. Categorical buys interpretive granularity; continuous buys a clean "per additional child, bedrooms increase by X" coefficient.
- ❓ **Do we run a 2020 regression?** Needed for (A)'s coefficient effect. Not needed for (B) alone. Default: yes, run both.

---

## File structure (proposed)

```
bedroom-allocation/
├── run-all.R                       # extend to source new kob scripts in order
├── src/
│   ├── process-households.R        # existing; unchanged
│   ├── fig01..fig04                # existing; unchanged
│   └── kob/
│       ├── build-kob-dataset.R     # HOH demographics + HH composition counts
│       ├── run-regressions.R       # fit 1970 + 2020, with survey-design SEs
│       ├── build-kob-input.R       # harmonize regression output + proportions → kob_input
│       ├── build-kob-output.R      # apply kob(), validate → kob_output
│       ├── hh-counterfactual.R     # per-household predictions for deliverable (B)
│       ├── kob-function.R          # ported engine, generalized with base_year arg
│       └── utils/
│           ├── kob-panels.R        # panel builders (ported from household-size-demographics)
│           └── regression-postprocess.R  # standardize_coefs, split_term_column, G-U
├── throughput/
│   ├── households.duckdb           # existing
│   ├── kob-dataset.duckdb          # NEW: regression-ready HH table
│   ├── regressions/                # NEW: {1970,2020}/bedroom.rds, with vcov saved
│   ├── kob_input.rds
│   └── kob_output.rds
├── output/figures/kob/             # NEW
└── tests/testthat/                 # NEW
```

Rationale: keeps KOB code under `src/` matching bedroom-allocation's existing flat-`src/` convention, while grouping the KOB-specific artifacts. When we eventually port the engine to demographr, only `kob-function.R` + `utils/` lift cleanly.

---

## Phase 1 — Data prep

- 🏗️ Create `src/kob/` directory structure.
- 📈 Write `build-kob-dataset.R`:
	- Read `throughput/households.duckdb`.
	- Join with person-level records to compute `n_children_under_18` and `n_adults` per SERIAL+YEAR (grouped pre-PERNUM==1 filter).
	- Derive HOH columns from PERNUM==1 row: `age_bucket` (or continuous), `race_eth_bucket` (7 levels), `educ_bucket` (≤HS / some college / 4yr+), `us_born` (from BPL), `sex`.
	- Derive `income_decile` within survey vintage (keep raw INCTOT and HHINCOME too for the continuous option).
	- Keep `STATEFIP`, `tenure`, `bedrooms_recode`, `HHWT`, `decade`.
	- For 2020: also keep `REPWT1`..`REPWT80` and `CLUSTER`/`STRATA`.
	- For 1970: keep whatever design variables exist (PSU/STRATA if available — research needed).
	- Filter `bedrooms_recode >= 0`, `OWNERSHP %in% c(1, 2)`, `GQ %in% c(0, 1, 2)`.
	- Filter to `decade %in% c(1970, 2020)`.
	- Write to `throughput/kob-dataset.duckdb`.
- 🧪 Assert: row count matches expected HH count per decade; no NAs in any regressor column.

---

## Phase 2 — Engine & utilities (spec-agnostic, can start now)

- 🏗️ Port `kob-function.R` to `src/kob/kob-function.R`, **generalized with `base_year` argument**. Default behavior: apply base-year coefs to other-year population (matches paper's direction when `base_year` is the earlier year). Remove hardcoded `_2000` / `_2019` suffixes — accept `base_year` and `compare_year` as parameters or have the function operate on generic `_base` / `_compare` column names.
- 🏗️ Port `regression-postprocess.R`:
	- `split_term_column()`
	- `complete_implicit_zeros()`
	- `gu_adjust()` (Gardeazabal-Ugidos 2004)
	- `standardize_coefs()` orchestrator
	- **Drop the `../dataduck` dependency** — inline what's needed or lift into demographr.
- 🏗️ Port `kob-panels.R` (panel builders). Same as household-size-demographics version minus the pretty-label hardcoding — lift label dict into an argument.
- 🧪 Unit tests for `kob()`:
	- Same tests as household-size-demographics' `test-kob-function.R`
	- Additional: base_year swap gives same total gap but swapped e/c magnitudes
	- Additional: decomposition validates against known analytical answer on toy data

---

## Phase 3 — Regressions with survey-design SEs (hardest phase)

- 📈 Write `run-regressions.R`:
	- Fit 2020 regression with Successive Differences Replication using 80 REPWT variables.
	- Fit 1970 regression with Taylor series via `srvyr` (need to verify 1970 has usable CLUSTER/STRATA; 1970 has `STRATA` per IPUMS but it's form-specific — **research required**).
	- Save full vcov matrix (not just SEs) — needed for correct G-U SE transformation and for the HH-level counterfactual's predicted-value SE.
	- Save as `throughput/regressions/{1970,2020}/bedroom.rds` with object schema: `list(coefs, vcov, n, design_info)`.
- 🧮 **1970 SE approach**: if Taylor series isn't viable, fall back to bootstrap or unweighted SEs (underestimates). Budget time here — this is the single biggest unknown.
- 🧮 Population proportion estimates with SEs (via `svrepstat` for 2020, `svymean` for 1970) — one call per categorical level per year.
- 🧪 Spot-check regression coefficients against naive unweighted `lm()` to confirm they're in the right ballpark (weighted should be close to unweighted on a large sample, but coefs on rare groups may differ).

---

## Phase 4 — Decomposition pipeline (A: aggregate)

- 🏗️ Write `build-kob-input.R`:
	- Read regression output + proportion output.
	- Apply `standardize_coefs()` (completes implicit zeros, applies G-U, splits term column).
	- Harmonize into `kob_input` tibble with columns `term, coef_base, coef_base_se, coef_compare, coef_compare_se, prop_base, prop_base_se, prop_compare, prop_compare_se, variable, value`.
	- Save to `throughput/kob_input.rds`.
- 🏗️ Write `build-kob-output.R`:
	- Apply `kob()` with `base_year = 1970`.
	- Validate against observed mean gap (`kob_output_validate()`).
	- Save to `throughput/kob_output.rds`.
- 🧪 Validation check fires no errors; totals match within tolerance.

---

## Phase 5 — Household-level counterfactual (B)

- 📈 Write `hh-counterfactual.R`:
	- Load 1970 coefficients (post-G-U adjustment) + 2020 HH dataset.
	- For each 2020 HH: compute `predicted_bedrooms_i = X_i · β_1970`.
	- Compute residual `actual_i - predicted_i`.
	- Tabulate distribution: deciles of residual, share above/below zero, share within ±0.5 bedrooms, etc.
	- Cross-tabulate residual by: tenure, race, HOH age bucket, build cohort.
	- Save summary tables to `output/figure-data/`.
- 🧮 Standard errors on predicted values: `Var(X_i · β) = X_i · vcov(β) · X_iᵀ`. Needs vcov saved in Phase 3.
- 📊 Headline figure: stacked histogram of actual vs. predicted bedrooms for 2020, by tenure.
- 📊 Headline figure: bar chart of share of HHs with "more/equal/fewer than predicted," by subgroup.

---

## Phase 6 — Figures (A: aggregate)

- 📊 Four-panel outcome column (coefficients / endowments / intercept / total) per outcome, using ported `kob-panels.R`.
- 📊 Subgroup bar charts: race coefficient effect, age endowment effect, etc.
- 📊 Observed-vs-counterfactual comparison chart (2020 actual mean vs. "2020 under 1970 preferences" counterfactual mean).

---

## Phase 7 — Statistical rigor (optional but wanted)

- 🧮 **Fix SE transformation in `gu_adjust()`.** G-U is a linear combination of coefficients; the same linear transform must apply to the vcov. Currently SEs pass through unchanged — understates uncertainty. Requires vcov to be saved in Phase 3.
- 🧮 **Fix SE for `complete_implicit_zeros()` rows.** Coefficient-of-zero for omitted reference is an identifying restriction, not an estimate — SE handling needs to be derived properly, not the `sqrt(Σ se²)` placeholder.
- 🧮 **Bootstrap alternative for total decomposition SEs.** The current product-rule independence assumption between β and p̄ is an approximation. A full bootstrap over the microdata gives exact SEs on all components. Feasible but expensive; can be a v2.
- 🧮 Unit tests against CRAN `oaxaca` package on a small dataset to verify G-U behavior matches published implementation.

---

## Phase 8 — Docs & housekeeping

- 📝 Write `docs/kob-decomposition-bedroom-allocation.qmd` — LaTeX methodology, adapted from the one in household-size-demographics. Fill the TODOs in that source doc (cite primary sources, describe regressions 1 & 2).
- 📝 Update `Data flow.md` in Obsidian to include the new KOB pipeline.
- 🏗️ Add `source("bedroom-allocation/src/process-households.R")` as step 0 in `run-all.R` (currently manual).
- 🏗️ Extend `run-all.R` to source the KOB pipeline in order: build-kob-dataset → run-regressions → build-kob-input → build-kob-output → hh-counterfactual → figures.

---

## Risks & known gotchas

- **1970 survey-design SEs are the biggest unknown.** Pre-REPWT era; Taylor may or may not apply cleanly. If stuck, fall back to unweighted SEs (understate uncertainty) and document the limitation.
- **State (~51 levels) balloons the term count** in the decomposition output. Consider also producing a collapsed Census-region (4 levels) view for summary figures.
- **Income top-coding differs between 1970 and 2020.** Affects continuous-income specification. If using deciles-within-vintage, top-coding is absorbed into the top decile and is a non-issue.
- **Index-number problem.** Our decomposition uses 1970 as base period. The alternative direction (applying 2020 coefs to 1970 pop) would give different per-component magnitudes. Pin this choice in the docs.
- **G-U adjustment only changes coefficients, not SEs, in current implementation.** Understates per-category uncertainty. Phase 7 addresses.
- **Engine currently assumes β and p̄ independent within-period.** They're not (estimated from same microdata). Phase 7 bootstrap addresses if we want exact SEs.
- **BEDROOMS == 0 is the IPUMS N/A sentinel (coded as −1 in `bedrooms_recode`).** Must filter `bedrooms_recode >= 0` consistently in both years.

---

## Dependencies

- `demographr` (or its successor `crosstabr` + `demographr`) — for `crosstab_mean()`, `crosstab_percent()`.
- `srvyr` — for Taylor-series variance estimation.
- Potentially `survey` directly for SDR.
- **Drop `../dataduck`** — port any needed utilities inline or into demographr.

---

## Suggested work order (next sessions)

1. **Now, while coauthor deliberates on continuous/categorical:**
	- Phase 1 (data prep — scripts can be written to produce both continuous and bucketed forms cheaply)
	- Phase 2 (engine + utils port, generalized with `base_year`)
2. **Once spec is locked:**
	- Phase 3 (regressions — biggest time sink, blocking)
	- Phase 4 (aggregate decomposition pipeline)
3. **Parallel to Phase 4:** Phase 5 (HH-level counterfactual) — shares the 1970 regression with Phase 3.
4. **Phase 6** (figures) after pipeline is stable.
5. **Phase 7** (statistical rigor) — optional; do if aiming for publication.
6. **Phase 8** (docs & run-all) ongoing.
