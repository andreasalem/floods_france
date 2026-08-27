# Analysis pipeline

All scripts were developed and run **inside the CASD secure enclave** (project working directory
`C:/Users/Public/Documents/Fontaine/Floods_Shock`); paths are enclave paths and the confidential
inputs (DADS Postes, FARE/FICUS) are not distributable. Public inputs (GASPAR, flood rasters)
can be re-downloaded from the sources listed in the root README.

## Run order

| Step | Script | What it does |
|---|---|---|
| 0 | `0_cleaning_code_commune_2000-20.R`, `0_cleaning_code_commune_2016-19.ipynb` | Extract commune codes from establishment files (fichiers Établissements), 2000–2020 |
| 1 | `1_DADS_Postes_script.R` | Clean DADS Postes: establishment-level employment and wage panel |
| 1 | `1_FARE_FICUS_script.R` | Clean FARE/FICUS firm financials |
| 2 | `2_Matching_dads_fare_ficus.R`, `2_matching.ipynb` | Match DADS establishments to FARE/FICUS firms |
| 3 | `3_gaspar.R` | Build the treatment: GASPAR CatNat flood declarations by commune × year |
| 3 | `3_Flood Risk.R` | Flood-risk raster processing (EAIP/TRI) → commune exposure measures |
| 4 | `4_Matching_dads_fare_ficus_gaspar.R` | Merge matched firm panel with flood treatment |
| 5 | `5_Descriptive_stats.R` | Descriptive statistics and data figures |
| 6 | `6_Analysis.R`, `Analysis2.R`, `Analysis3.R` | Establishment-level DiD estimators in R (fixest, did, fect, DIDmultiplegtDYN, HonestDiD) |
| 6 | `Analysis2Stata.do` | **Main specification:** LP-DiD (Dube–Girardi–Jordà–Taylor) on the establishment panel |
| 6 | `AnalysisStata.do`, `Analysis_communes.do` | Robustness estimators; commune-level analysis |
| 6 | `commune_dt_process.R`, `commune_dt_analysis.R` | Commune-level panel build and analysis |

Regression output exported (and disclosure-cleared) from the enclave is in
[`../output/tables/`](../output/tables/).

## Provenance

Files are the CASD code export of 2025-07-09, unmodified except for this README
(which supersedes the original three-line `ReadMe.txt` shipped with the export) and one
filename fix (`2_matching.ipynb.ipynb` → `2_matching.ipynb`).
