# 03_gaspar_treatment.R — build the flood treatment from GASPAR CatNat declarations
#
# STATUS: runnable outside CASD (public data only).
#   Faithful rewrite of code/casd_export_2025-07/3_gaspar.R (the analysis part;
#   exploratory plotting is not reproduced). Verified against the author's original
#   output (gaspar_floods.csv) by verify_public_spine.R.
#
# INPUT : DATA_DIR/gaspar/catnat_gaspar.csv        (Géorisques, vintage 2025-03-19)
# OUTPUT: _outputs/gaspar_flood_treatment.csv      (commune × year flood events, same
#                                                   schema as the original gaspar_floods.csv)
#         _outputs/gaspar_flood_panel.csv          (balanced commune × year panel 1982–2024
#                                                   with history variables — the intent of the
#                                                   export's broken data.table block, fixed)
#         verification/flood_summary_by_year.csv   (decrees + communes per year, vs thesis Table)
#         verification/fig_duration_distribution.png
#
# Known divergences from the frozen export (each tracked as a GitHub issue):
#   - The export applies `:=` to a grouped tibble (would error as shipped); history
#     variables are implemented here with data.table properly.
#   - The export names the 2000–2020 count `floods_2000_2020`, but the file the thesis
#     actually used names it `total_floods_2000_2020`; this script uses the latter.

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})
source("config.R")

# ---- 1. Read CatNat declarations --------------------------------------------------
msg("Reading GASPAR CatNat ...")
catnat <- unique(fread(file.path(DATA_DIR, "gaspar", "catnat_gaspar.csv"),
                       sep = ";", colClasses = list(character = "cod_commune")))
setnames(catnat, "cod_commune", "INSEE_COM")

n_communes_raw <- uniqueN(catnat$INSEE_COM)
catnat <- catnat[!grepl("^97|^98", INSEE_COM)]              # drop DOM-TOM
n_communes_metro <- uniqueN(catnat$INSEE_COM)
msg("Communes: %d raw -> %d after DOM filter (export comments say 34,700 -> 34,576)",
    n_communes_raw, n_communes_metro)

# ---- 2. Dates and duration --------------------------------------------------------
# Year comes from the raw timestamp string: the original session parsed these
# local-naive timestamps in Europe/Paris and took lubridate::year(), which equals the
# string's leading year. (data.table::year() on a tz-aware POSIXct extracts the UTC
# year and silently shifts every "Jan 1 00:00" decree back one year — the first
# rewrite did exactly that; caught by the differential audit.)
catnat[, dat_deb_year := as.integer(substr(dat_deb, 1, 4))]
catnat[, `:=`(
  dat_deb = as.POSIXct(dat_deb, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Paris"),
  dat_fin = as.POSIXct(dat_fin, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Paris")
)]
stopifnot("date parsing produced NAs" = !anyNA(catnat$dat_deb), !anyNA(catnat$dat_fin))
# same-day decree = 1 day, as in the original (difftime + 1, rounded)
catnat[, duration_days := round(as.numeric(difftime(dat_fin, dat_deb, units = "days")) + 1, 0)]

# ---- 3. Identify inundation hazards ----------------------------------------------
# Same canonicalisation as the export's recode(). The current GASPAR file is clean
# UTF-8 (the export's mojibake variants came from read.delim without an encoding),
# so only the two accented variants below occur; the recode keeps the canonical
# spellings identical to the original output so flood_type strings match exactly.
recode_map <- c(
  "Inondations et/ou Coulées de Boue" = "Inondations et/ou Coulees de Boue",
  "Inondations Remontée Nappe"        = "Inondations Remontee de Nappe"
)
catnat[, lib_clean := fifelse(lib_risque_jo %chin% names(recode_map),
                              recode_map[lib_risque_jo], lib_risque_jo)]

flood_labels <- c(
  "Inondations et/ou Coulees de Boue",
  "Inondations Remontee de Nappe",
  "Lave Torrentielle",
  "Raz de Marée",
  "Inondations par choc mécanique des vagues"
)
catnat[, innondation := as.integer(lib_clean %chin% flood_labels)]

# Drift check: any inundation-looking label NOT in the treatment set gets flagged.
unmatched <- setdiff(grep("nondation|Lave|Marée|vagues", unique(catnat$lib_clean), value = TRUE),
                     flood_labels)
if (length(unmatched)) msg("NOTE: inundation-like labels not in treatment set: %s",
                           paste(unmatched, collapse = " | "))

floods <- catnat[innondation == 1]
msg("Flood clauses: %s rows, %s communes", format(nrow(floods), big.mark = ","),
    format(uniqueN(floods$INSEE_COM), big.mark = ","))

# ---- 4. Collapse to commune x year -----------------------------------------------
# One event = one (decree code, start date) pair — the same decree code appearing with
# two start dates in a year counts twice, exactly as in the original.
gaspar <- floods[, .(
  flood_dummy       = 1L,
  count_floods_year = uniqueN(paste(cod_nat_catnat, dat_deb)),
  avg_duration      = mean(duration_days, na.rm = TRUE),
  flood_type        = paste(unique(lib_clean), collapse = "; "),
  cat_nat_code      = paste(unique(cod_nat_catnat), collapse = "; ")
), keyby = .(INSEE_COM, year = dat_deb_year)]

gaspar[, total_floods_all_years  := sum(flood_dummy), by = INSEE_COM]
gaspar[, total_floods_2000_2020  := sum(flood_dummy[year %between% c(2000, 2020)]), by = INSEE_COM]
gaspar[, duration_category := fcase(
  avg_duration < 2,                        "less than 1 day",
  avg_duration >= 2 & avg_duration <= 22,  "1 to 22 days",
  avg_duration > 22,                       "more than 22 days"
)]

fwrite(gaspar, file.path(OUT_DIR, "gaspar_flood_treatment.csv"))
msg("Wrote gaspar_flood_treatment.csv (%s commune-years)", format(nrow(gaspar), big.mark = ","))

# ---- 5. Balanced panel with flood-history variables -------------------------------
# The controls the empirical strategy needs (rolling flood counts, years since last
# flood) on a balanced commune x year grid. This is what the export's data.table block
# intends; computed here on the full grid so zeros are explicit.
years <- seq(min(gaspar$year), max(gaspar$year))
panel <- CJ(INSEE_COM = unique(gaspar$INSEE_COM), year = years)
panel <- gaspar[, .(INSEE_COM, year, flood_dummy, count_floods_year, avg_duration)][panel, on = .(INSEE_COM, year)]
panel[is.na(flood_dummy), flood_dummy := 0L]
panel[is.na(count_floods_year), count_floods_year := 0L]
setkey(panel, INSEE_COM, year)

panel[, floods_last_10y := frollsum(flood_dummy, 10, align = "right", fill = NA), by = INSEE_COM]
panel[, floods_last_5y  := frollsum(flood_dummy,  5, align = "right", fill = NA), by = INSEE_COM]
panel[, floods_last_3y  := frollsum(flood_dummy,  3, align = "right", fill = NA), by = INSEE_COM]
panel[, last_flood_year := {
  lf <- fifelse(flood_dummy == 1, year, NA_integer_)
  nafill(lf, type = "locf")
}, by = INSEE_COM]
panel[, years_since_last_flood := year - last_flood_year]
panel[, last_flood_year := NULL]
panel[, floods_in_6yr_window := frollsum(flood_dummy, 6, align = "right", fill = NA), by = INSEE_COM]
panel[, max_floods_any_6yr := max(floods_in_6yr_window, na.rm = TRUE), by = INSEE_COM]
panel[, floods_in_6yr_window := NULL]

fwrite(panel, file.path(OUT_DIR, "gaspar_flood_panel.csv"))
msg("Wrote gaspar_flood_panel.csv (%s rows)", format(nrow(panel), big.mark = ","))

# ---- 6. Verification artifacts ----------------------------------------------------
# (a) decrees + communes per year — comparable to thesis Table "History of flood events"
flood_summary <- floods[, .(
  catnat_decrees    = uniqueN(paste(cod_nat_catnat, dat_deb_year)),
  communes_affected = uniqueN(INSEE_COM)
), keyby = .(year = dat_deb_year)]
fwrite(flood_summary, file.path(VERIF_DIR, "flood_summary_by_year.csv"))

# (b) duration distribution — comparable to thesis Figure "Distribution of flood event
#     durations" (duration_flood_gaspar.png)
p <- ggplot(floods[duration_days <= 30], aes(x = duration_days)) +
  geom_bar(fill = "skyblue", color = "black", linewidth = 0.2) +
  geom_vline(xintercept = c(1.5, 22.5), linetype = "dashed", color = "red") +
  labs(title = "Distribution of flood event durations (GASPAR CatNat)",
       subtitle = "Dashed lines: intensity-category bounds (<1 day / 1-22 days / >22 days); durations >30 days not shown",
       x = "Duration (days)", y = "Decree clauses") +
  theme_minimal()
ggsave(file.path(VERIF_DIR, "fig_duration_distribution.png"), p,
       width = 9, height = 5, dpi = 150, bg = "white")
msg("Verification artifacts written to %s", VERIF_DIR)
