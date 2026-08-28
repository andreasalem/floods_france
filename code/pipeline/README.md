# Pipeline (rewrite)

Clean-room R rewrite of the thesis pipeline. Numbered scripts, one entry point
(`00_run_all.R`), portable paths via a single `config` block, dependencies pinned with `renv`.

**Status: public-data spine landed and verified.** Run from this directory:
`Rscript 03_gaspar_treatment.R && Rscript 04_flood_risk.R && Rscript verify_public_spine.R`
(paths configurable via env vars — see `config.R`). The differential audit
(`verification/VERIFICATION.md`) compares every output against the author's original
intermediates and the numbers printed in the thesis.

| Stage | Scripts (planned) | Data | Runnable outside CASD? | Verification |
|---|---|---|---|---|
| Treatment: CatNat flood declarations | `03_gaspar_treatment.R` | GASPAR (public) | **Yes — landed** | ✅ Exact match: all 125,043 commune-years identical to the original `gaspar_floods.csv`; thesis flood-history table matches all 21 years |
| Flood-risk exposure | `04_flood_risk.R` | Dottori RP100 raster (public) | **Yes — landed** | Diffed per-band against the original `communes_risk_RP100.gpkg` (see VERIFICATION.md) |
| DADS Postes cleaning | `01_dads_postes.R` | Confidential (CASD) | No | Owed to next CASD session |
| FARE/FICUS cleaning | `02_fare_ficus.R` | Confidential (CASD) | No | Owed to next CASD session |
| Matching + merge | `05_match_merge.R` | Confidential (CASD) | No | Owed to next CASD session |
| Descriptives | `06_descriptives.R` | Mixed | Partially | Public-data figures only |
| LP-DiD estimation | `07_lpdid_analysis.R` | Confidential (CASD) | No | R port of the Stata `lpdid` main spec (`../casd_export_2025-07/Analysis2Stata.do` is the reference implementation); numerical equivalence owed to next CASD session |

Every script that cannot be executed outside the enclave states so in its header, together
with what exactly remains unverified. See [`../README.md`](../README.md) for how the two code
tracks relate, and [`../../data/README.md`](../../data/README.md) for data sources.
