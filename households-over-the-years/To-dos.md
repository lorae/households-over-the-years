# To-dos

## Architecture
- [ ] Refactor `cps-household-interrelationships.R` to not drop the output table at the start of the script. Currently it runs `DROP TABLE IF EXISTS` before processing 359 batches (~4 hours). If the script is accidentally started, the old data is destroyed immediately and you're forced to let it finish. Should check if the table already exists and skip/confirm before dropping.
