# verify_public_spine.R — differential audit of the public-data pipeline stages
#
# Compares this pipeline's outputs against
#   (a) the author's original intermediates in LEGACY_DIR (the objects the thesis
#       actually used: gaspar_floods.csv, communes_risk_RP100.gpkg), and
#   (b) numbers printed in the submitted thesis (flood-history table; depth-distribution
#       claim).
# Writes verification/VERIFICATION.md with a PASS/FAIL line per check. A FAIL is a
# finding to investigate (vintage difference or defect), never something to hide.

suppressPackageStartupMessages({ library(data.table) })
source("config.R")

results <- list()
check <- function(id, pass, detail) {
  results[[length(results) + 1]] <<- data.table(id = id, status = fifelse(pass, "PASS", "FAIL"), detail = detail)
  msg("%s  %s — %s", fifelse(pass, "PASS", "FAIL"), id, detail)
}

norm_ascii <- function(x) gsub("'", "", iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT"))

# ==== A. GASPAR treatment vs original gaspar_floods.csv ===========================
mine <- fread(file.path(OUT_DIR, "gaspar_flood_treatment.csv"),
              colClasses = list(character = "INSEE_COM"))
legacy_file <- file.path(LEGACY_DIR, "gaspar_floods.csv")

if (file.exists(legacy_file)) {
  ref <- fread(legacy_file, colClasses = list(character = "INSEE_COM"))

  check("gaspar/row-count", nrow(mine) == nrow(ref),
        sprintf("pipeline %d vs original %d commune-years", nrow(mine), nrow(ref)))

  j <- merge(ref, mine, by = c("INSEE_COM", "year"), all = TRUE, suffixes = c("_ref", "_new"))
  only_ref <- j[is.na(flood_dummy_new)]
  only_new <- j[is.na(flood_dummy_ref)]
  check("gaspar/key-coverage", nrow(only_ref) + nrow(only_new) == 0,
        sprintf("%d commune-years only in original, %d only in pipeline", nrow(only_ref), nrow(only_new)))
  # always (re)written, so a clean run leaves empty files rather than stale diagnostics
  fwrite(only_ref[, .(INSEE_COM, year, flood_type_ref, cat_nat_code_ref)],
         file.path(VERIF_DIR, "gaspar_rows_only_in_original.csv"))
  fwrite(only_new[, .(INSEE_COM, year, flood_type_new, cat_nat_code_new)],
         file.path(VERIF_DIR, "gaspar_rows_only_in_pipeline.csv"))

  b <- j[!is.na(flood_dummy_ref) & !is.na(flood_dummy_new)]
  for (v in c("count_floods_year", "total_floods_all_years", "total_floods_2000_2020")) {
    d <- b[get(paste0(v, "_ref")) != get(paste0(v, "_new"))]
    check(paste0("gaspar/", v), nrow(d) == 0,
          sprintf("%d of %d matched commune-years differ", nrow(d), nrow(b)))
  }
  d <- b[abs(avg_duration_ref - avg_duration_new) > 1e-8]
  check("gaspar/avg_duration", nrow(d) == 0, sprintf("%d rows differ beyond 1e-8", nrow(d)))
  d <- b[duration_category_ref != duration_category_new]
  check("gaspar/duration_category", nrow(d) == 0, sprintf("%d rows differ", nrow(d)))
  d <- b[norm_ascii(flood_type_ref) != norm_ascii(flood_type_new)]
  check("gaspar/flood_type", nrow(d) == 0,
        sprintf("%d rows differ (accent-insensitive compare)", nrow(d)))
} else {
  msg("SKIP gaspar legacy compare — %s not found", legacy_file)
}

# ==== B. GASPAR vs thesis Table (History of flood events, 2000-2020) ==============
# Expected values transcribed from the submitted thesis (Appendix, tab:flood_summary),
# columns "CatNat decrees" and "Communes affected".
expected <- fread(text = "year,decrees,communes
2000,33,2227
2001,42,2879
2002,27,1163
2003,27,2261
2004,19,334
2005,21,842
2006,24,1072
2007,19,1070
2008,26,2177
2009,23,4425
2010,23,1981
2011,23,962
2012,18,729
2013,31,1511
2014,33,2057
2015,22,776
2016,29,2846
2017,23,303
2018,37,3426
2019,26,913
2020,28,1168")
fs <- fread(file.path(VERIF_DIR, "flood_summary_by_year.csv"))
cmp <- merge(expected, fs, by = "year", all.x = TRUE)
bad_d <- cmp[decrees  != catnat_decrees]
bad_c <- cmp[communes != communes_affected]
check("thesis/flood_summary_decrees", nrow(bad_d) == 0,
      if (nrow(bad_d)) paste("mismatch years:", paste(bad_d$year, collapse = ","),
                             "|", paste(sprintf("%d(exp %d)", bad_d$catnat_decrees, bad_d$decrees), collapse = ","))
      else "all 21 years match the thesis table")
check("thesis/flood_summary_communes", nrow(bad_c) == 0,
      if (nrow(bad_c)) paste("mismatch years:", paste(bad_c$year, collapse = ","),
                             "|", paste(sprintf("%d(exp %d)", bad_c$communes_affected, bad_c$communes), collapse = ","))
      else "all 21 years match the thesis table")

# ==== C. Flood-risk raster stage (only if 04 has run) =============================
risk_file <- file.path(OUT_DIR, "commune_flood_risk_RP100.csv")
if (file.exists(risk_file)) {
  suppressPackageStartupMessages(library(sf))
  mine_r <- fread(risk_file, colClasses = list(character = "INSEE_COM"))

  legacy_r_file <- file.path(LEGACY_DIR, "communes_risk_RP100.gpkg")
  if (file.exists(legacy_r_file)) {
    ref_r <- st_drop_geometry(st_read(legacy_r_file, quiet = TRUE))
    setDT(ref_r)
    ref_r <- ref_r[, .(INSEE_COM,
                       ref_low = percent_flooded_low, ref_mod = percent_flooded_moderate,
                       ref_high = percent_flooded_high, ref_vhigh = percent_flooded_very_high)]
    jr <- merge(mine_r, ref_r, by = "INSEE_COM", all = FALSE)
    check("raster/join-coverage", nrow(jr) == nrow(mine_r),
          sprintf("%d of %d pipeline communes matched to original", nrow(jr), nrow(mine_r)))
    for (pair in list(c("percent_flooded_low","ref_low"), c("percent_flooded_moderate","ref_mod"),
                      c("percent_flooded_high","ref_high"), c("percent_flooded_very_high","ref_vhigh"))) {
      dmax <- jr[, max(abs(get(pair[1]) - get(pair[2])), na.rm = TRUE)]
      check(paste0("raster/", pair[1]), dmax < 1e-6,
            sprintf("max abs diff vs original = %.3g percentage points", dmax))
    }
    jr[, idx_ref := ref_low + ref_mod + ref_high + ref_vhigh]
    dmax <- jr[, max(abs(flood_risk_index_RP100 - idx_ref), na.rm = TRUE)]
    check("raster/flood_risk_index", dmax < 1e-6, sprintf("max abs diff = %.3g", dmax))
  } else msg("SKIP raster legacy compare — %s not found", legacy_r_file)

  # thesis claim (Appendix): "90% of all flooded cells fall between 0.07 and 3 meters"
  stats_file <- file.path(OUT_DIR, "raster_depth_stats.csv")
  if (file.exists(stats_file)) {
    ds <- fread(stats_file)
    check("thesis/depth-claim-90pct", abs(ds$share_cells_007_3m - 0.90) < 0.03,
          sprintf("share of flooded cells in (0.07, 3] m = %.1f%% (thesis: ~90%%)", 100 * ds$share_cells_007_3m))
  }
} else {
  msg("SKIP raster checks — run 04_flood_risk.R first")
}

# ==== Write report ================================================================
# Diagnosed explanations for known FAILs — every FAIL must either be explained here
# (with its issue link) or is an open finding.
notes <- c(
  "raster/percent_flooded_low"       = "resampling-version effect; see note below and issue #4",
  "raster/percent_flooded_moderate"  = "resampling-version effect; see note below and issue #4",
  "raster/percent_flooded_high"      = "resampling-version effect; see note below and issue #4",
  "raster/percent_flooded_very_high" = "resampling-version effect; see note below and issue #4",
  "thesis/depth-claim-90pct"         = "thesis text error, material-confirmed; issue #5 + paper/ERRATA.md"
)
res <- rbindlist(results)
res[, note := fifelse(id %in% names(notes), notes[id], "")]
md <- c("# Public-data spine — differential audit",
        "",
        sprintf("Run: %s · GASPAR vintage 2025-03-19 · pipeline vs. the author's original intermediates and the submitted thesis.", format(Sys.Date())),
        "",
        "| Check | Status | Detail | Diagnosis |", "|---|---|---|---|",
        res[, sprintf("| `%s` | %s | %s | %s |", id, status, detail, note)],
        "",
        "## Diagnosed findings",
        "",
        "- **Band-share FAILs (issue #4):** running this pipeline's classification + extraction on the author's *stored* projected raster reproduces the original per-band shares to < 7e-9 — the methodology is identical. The differences arise in `terra::project()` (bilinear) across terra/GDAL versions: cells near depth-bin edges land in adjacent bands (mean |diff| ≤ 0.007pp, max 0.59pp). **The flood risk index — the only quantity the thesis consumes — is invariant (max |diff| 1.2e-8 across all 34,826 communes).**",
        "- **Depth-claim FAIL (issue #5):** the thesis sentence \"90% of all flooded cells fall between 0.07 and 3 meters\" is contradicted by the raster it describes (70.5%; central-90% interval [0.34, 6.74] m). Text-level erratum; no estimate depends on it.",
        "",
        "FAIL rows are findings to explain (data-vintage drift or defect), documented in the repository issues — never silently reconciled.")
writeLines(md, file.path(VERIF_DIR, "VERIFICATION.md"))
msg("Report: verification/VERIFICATION.md — %d PASS, %d FAIL",
    res[status == "PASS", .N], res[status == "FAIL", .N])
