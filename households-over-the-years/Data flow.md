# Data flow

Data flow diagram for `five-decade-aggregates/` pipeline. See `run-all.R` for execution order.

## Legend

| Shape | Meaning |
|---|---|
| Rectangle | Script |
| Cylinder | Database / data file |
| Cylinder (pale, dotted) | Intermediate / temporary data |
| Stadium (rounded) | External input (API, config) |

## Diagram

```mermaid
flowchart TD
    classDef script fill:#4a90d9,color:#fff,stroke:#2c5f8a
    classDef data fill:#5cb85c,color:#fff,stroke:#3d8b3d
    classDef intermediate fill:#5cb85c22,color:#888,stroke:#3d8b3d,stroke-dasharray:5 5
    classDef input fill:#f0ad4e,color:#fff,stroke:#c87f0a

    %% --- Top row: external inputs (order controls left-to-right placement) ---
    ipums_api(["IPUMS USA API"]):::input
    env([".Renviron (IPUMS_API_KEY)"]):::input
    cps_api(["IPUMS CPS API"]):::input

    %% --- Import: IPUMS USA ---
    import_usa["src/<br>import-ipums-usa-1970-2020.R"]:::script
    raw_usa[("data/raw-microdata/<br>usa_NNNNN.xml + .dat.gz")]:::intermediate
    ipums_db[("data/five-decade-db/ipums.duckdb<br>(table: ipums)")]:::data

    %% --- Import: IPUMS CPS ---
    import_cps["src/<br>import-ipums-cps-1970-2020.R"]:::script
    raw_cps[("data/raw-microdata/<br>cps_NNNNN.xml + .dat.gz")]:::intermediate
    cps_db[("data/five-decade-db/ipums_cps.duckdb<br>(table: ipums)")]:::data

    ipums_api --> import_usa
    env --> import_usa
    import_usa --> raw_usa
    import_usa --> ipums_db
    raw_usa --> ipums_db

    env --> import_cps
    cps_api --> import_cps
    import_cps --> raw_cps
    import_cps --> cps_db
    raw_cps --> cps_db
```
