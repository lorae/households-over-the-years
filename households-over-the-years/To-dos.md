# To-dos

| Emoji | Type |
|-------|--------------|
| 🏗️    | Architecture |
| 📊    | Analysis     |
| 🗄️    | Data         |
| 🔒    | Security     |

## Open

- 🏗️ **#1** Refactor `cps-household-interrelationships.R` to not drop the output table at the start of the script. Currently it runs `DROP TABLE IF EXISTS` before processing 359 batches (~4 hours). If the script is accidentally started, the old data is destroyed immediately and you're forced to let it finish. Should check if the table already exists and skip/confirm before dropping.
- 📊 **#6** New script: race-stratified household composition — Black, White, Hispanic interrelationships (1970–2020).
- 🔒 **#7** Stop printing IPUMS API key to R console in import scripts.

## Done

- 🗄️ **#2** Add `RACE` and `HISPAN` to CPS import (`import-ipums-cps-1970-2020.R`) and re-pull extract.
- 🗄️ **#5** Add `race_eth` derivation to CPS processing script (`process-ipums-cps-person-1970-2020.R`).

