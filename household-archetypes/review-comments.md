# Review comments — single-mother-results write-up

Comments on `src/single-mother-results.Rmd`, ordered roughly by how much they matter. Check off as we address them.

## Big-lift items

- [x] **1. Sample composition is probably changing under our feet.** The narrow single-mother-only-with-minor-kids definition is conservative and good for interpretability, but the 1970 version of this household (rare, highly selected — widows, divorcees in a hostile legal regime, very few never-married mothers) is not the same population as the 2020 version (common, includes many never-married mothers). Any trend could be driven by who enters the sample. **Action:** add a Table 1 showing `n`, mean age, mean real `HHINCOME`, race/ethnicity composition, and renter/owner share by decade.

- [ ] **2. The "forced overconsumption" hypothesis isn't quite what Model 6 tests.** Model 6 says "within each bedroom count, burden is rising," which is consistent with forced overconsumption but also with falling incomes, rising rents at all sizes, or compositional change. Forced overconsumption would require something like: where studio supply dried up, did single mothers shift to 2BR and become more burdened? **Action:** clarify in the intro what this analysis *can* answer (descriptive trend in burden × unit size) vs what a causal claim would need.

- [ ] **3. The income denominator is doing invisible work.** Cost burden = rent/income. If single-mother real income has fallen, burden rises mechanically without rent moving. **Action:** add a companion figure showing weighted median real `HHINCOME` and median real `RENT` for this sample by decade so the mechanics are legible.

- [ ] **4. Pooling renters and homeowners in the regression is probably wrong.** `OWNCOST` includes principal (wealth-building, not consumption). **Action:** run the regression on renters only for the headline specification; repeat on homeowners as robustness. Expect the bedroom × decade interaction to get sharper in the renters-only version.

## Medium items

- [ ] **5. Studio baseline in Model 6 is structurally fragile.** Studios are a tiny share of single-mother households and the studio decade slope is the reference point for all interaction coefficients. **Action:** check the raw weighted mean cost burden for studios by decade before leaning on the "studios were flat" framing.

- [ ] **6. State FE is a bigger deal than the write-up makes it.** R² jumps from 0.016 to 0.035 just from state dummies — roughly half the explained variance is between-state. Migration of single mothers across states over 50 years could drive the national finding. **Action:** compare the Model 3 national decade trend to a within-state version; if the trend survives, we're robust; if not, the national finding is a composition/migration story.

- [ ] **7. Two missing-data puzzles to resolve.** `{what variable?}` in fig02a (1970 gap for renters) and `[where is 2000?]` in fig02b (2000 gap for owners). Likely causes: `RENT` coverage for single-mother subgroups is limited in the 1970 Form 1 Metro sample; `OWNCOST` may be constructed differently in the 2000 1% sample or lacks enough single-mother homeowners. **Action:** trace both before finalizing.

## Small items

- [ ] **8a. Test the fertility hand-wave in Fig 1.** The claim "likely unexplained by fertility" is testable. **Action:** condition on number of children in the household and see if the bedroom shift survives.

- [ ] **8b. Motivation mismatch in intro.** The opening says "we live in larger units than before" but the analysis is about single-mother households specifically. **Action:** either broaden the motivation with a general-population citation or narrow it to "single-mother households live in larger units."

- [ ] **8c. Fig04 paragraph glosses over race/Hispanic findings.** The Black coefficient dropping to zero with state FE, and the Hispanic coefficient shrinking but staying positive, are substantively interesting. **Action:** add one sentence on this in the fig04 narrative.

## Round 2 — raised after Table 1 was added

- [ ] **R2-1. Rising-real-income finding should show up in the Fig 05 narrative.** Right now the fig05 paragraph makes the cost-burden point without noting that real incomes were rising at the same time. **Action:** add one sentence like "this rise occurs despite substantial gains in real income (see Table 1), implying rent growth has outpaced income growth" to sharpen the punchline.

- [ ] **R2-2. Fig 02 fertility claim can now be verified directly from Table 1.** Currently it says bedrooms are rising "likely unexplained by fertility" — but Table 1 has `Mean # children`, so you can replace the hedge with specific numbers. **Action:** rewrite as "bedrooms rose from X to Y while mean number of children fell from A to B."

- [ ] **R2-3. Two open questions still unanswered.** `{what variable?}` in Fig 03a (1970 missing for renters) and `[where is 2000?]` in Fig 03b (2000 missing for owners). Likely causes: `RENT` coverage in the 1970 Form 1 Metro sample is limited, and the 2000 1% sample may have an `OWNCOST` construction issue or too few single-mother homeowners. **Action:** trace both before this is presentable. (Duplicates item 7 above; resolve in one place.)

- [ ] **R2-4. Conclusion is thin — it could really land the story.** Given Table 1 now shows rising real income, the conclusion could say: "single-mother households simultaneously moved into larger units AND became more cost-burdened, despite meaningful real-income gains — which means either rent growth outpaced income growth at the unit sizes they occupy, or the population composition shift (more never-married mothers with different economic trajectories) is doing the work." **Action:** rewrite the conclusion to land this framing.

- [ ] **R2-5. Composition caveat is under-discussed in the main text.** Table 1 reveals the composition shift but the narrative never says "this means our time trends may confound a real rent-market shift with a change in who counts as a single mother." **Action:** add a one-line intellectually-honest flag near the conclusion.

- [ ] **R2-6. Minor: fig04-table chunk label should be renamed.** The chunk that embeds the regression HTML is still labeled `fig04-table` in the Rmd, but the narrative now calls it Fig 05. **Action:** rename the chunk to `fig05-regression-table`.
