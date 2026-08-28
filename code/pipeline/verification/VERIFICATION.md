# Public-data spine — differential audit

Run: 2026-08-28 · GASPAR vintage 2025-03-19 · pipeline vs. the author's original intermediates and the submitted thesis.

| Check | Status | Detail | Diagnosis |
|---|---|---|---|
| `gaspar/row-count` | PASS | pipeline 125043 vs original 125043 commune-years |  |
| `gaspar/key-coverage` | PASS | 0 commune-years only in original, 0 only in pipeline |  |
| `gaspar/count_floods_year` | PASS | 0 of 125043 matched commune-years differ |  |
| `gaspar/total_floods_all_years` | PASS | 0 of 125043 matched commune-years differ |  |
| `gaspar/total_floods_2000_2020` | PASS | 0 of 125043 matched commune-years differ |  |
| `gaspar/avg_duration` | PASS | 0 rows differ beyond 1e-8 |  |
| `gaspar/duration_category` | PASS | 0 rows differ |  |
| `gaspar/flood_type` | PASS | 0 rows differ (accent-insensitive compare) |  |
| `thesis/flood_summary_decrees` | PASS | all 21 years match the thesis table |  |
| `thesis/flood_summary_communes` | PASS | all 21 years match the thesis table |  |
| `raster/join-coverage` | PASS | 34826 of 34826 pipeline communes matched to original |  |
| `raster/percent_flooded_low` | FAIL | max abs diff vs original = 0.372 percentage points | resampling-version effect; see note below and issue #4 |
| `raster/percent_flooded_moderate` | FAIL | max abs diff vs original = 0.591 percentage points | resampling-version effect; see note below and issue #4 |
| `raster/percent_flooded_high` | FAIL | max abs diff vs original = 0.591 percentage points | resampling-version effect; see note below and issue #4 |
| `raster/percent_flooded_very_high` | FAIL | max abs diff vs original = 0.359 percentage points | resampling-version effect; see note below and issue #4 |
| `raster/flood_risk_index` | PASS | max abs diff = 1.18e-08 |  |
| `thesis/depth-claim-90pct` | FAIL | share of flooded cells in (0.07, 3] m = 70.5% (thesis: ~90%) | thesis text error, material-confirmed; issue #5 + paper/ERRATA.md |

## Diagnosed findings

- **Band-share FAILs (issue #4):** running this pipeline's classification + extraction on the author's *stored* projected raster reproduces the original per-band shares to < 7e-9 — the methodology is identical. The differences arise in `terra::project()` (bilinear) across terra/GDAL versions: cells near depth-bin edges land in adjacent bands (mean |diff| ≤ 0.007pp, max 0.59pp). **The flood risk index — the only quantity the thesis consumes — is invariant (max |diff| 1.2e-8 across all 34,826 communes).**
- **Depth-claim FAIL (issue #5):** the thesis sentence "90% of all flooded cells fall between 0.07 and 3 meters" is contradicted by the raster it describes (70.5%; central-90% interval [0.34, 6.74] m). Text-level erratum; no estimate depends on it.

FAIL rows are findings to explain (data-vintage drift or defect), documented in the repository issues — never silently reconciled.
