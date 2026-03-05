# To-dos

| Emoji | Type |
|-------|--------------|
| 🏗️    | Architecture |
| 📊    | Analysis     |
| 🗄️    | Data         |
| 🔒    | Security     |

## Open

- 🏗️ **#1** Refactor `cps-household-interrelationships.R` to not drop the output table at the start of the script. Currently it runs `DROP TABLE IF EXISTS` before processing 359 batches (~4 hours). If the script is accidentally started, the old data is destroyed immediately and you're forced to let it finish. Should check if the table already exists and skip/confirm before dropping.
- 🗄️ **#2** Add `RACE` and `HISPAN` to CPS import (`import-ipums-cps-1970-2020.R`) and re-pull extract (~20 min).
- 🏗️ **#3** Slim down `cps-household-interrelationships.R` to select only ID + relationship columns as input and output only IDs + computed subfamily fields. Makes it explicit which columns drive the computation and reduces memory during the 4-hour run.
- 🏗️ **#4** One-time migration: trim existing `ipums_person_with_subfamilies_over18` table to match the slimmed output schema (drop extra columns). Can be removed after next full re-run.
- 🗄️ **#5** New script: `add-demographics-to-interrelationships.R` — joins slim `ipums_person_with_subfamilies_over18` back to `ipums_person` to attach demographics (`race_eth`, `age_bucket`, `ASECWT`, etc.). Permanent pipeline step.
- 📊 **#6** New script: race-stratified household composition — Black, White, Hispanic interrelationships (1970–2020).
- 🔒 **#7** Stop printing IPUMS API key to R console in import scripts.

## Done

