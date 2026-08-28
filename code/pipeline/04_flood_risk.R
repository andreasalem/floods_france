# 04_flood_risk.R — commune flood-risk exposure from the Dottori et al. (2022)
#                   RP100 flood-depth raster
#
# STATUS: runnable outside CASD (public data only).
#   Faithful rewrite of code/casd_export_2025-07/"3_Flood Risk.R" (the RP100 index
#   actually used by the thesis). Verified against the author's original
#   communes_risk_RP100.gpkg by verify_public_spine.R.
#
# INPUT : DATA_DIR/Dottori/Europe_RP100_filled_depth.tif   (JRC river flood hazard maps)
#         commune polygons — COMMUNES_SOURCE = "legacy" uses the author's prepared
#         shapefile (identical polygons -> audit isolates methodology); "raw" builds
#         from DATA_DIR/communes-20220101-shp
# OUTPUT: _outputs/commune_flood_risk_RP100.csv / .gpkg    (per-band % flooded + index)
#         _outputs/raster_depth_stats.csv                  (depth distribution checks)
#         verification/fig_risk_index_distribution.png
#
# Method (as in the original, kept deliberately identical):
#   crop to (-5,10,40,52) -> project to EPSG:2154 (default bilinear resampling)
#   -> mask to the rnaturalearth France polygon -> classify depth into 4 bands
#   (0-0.5, 0.5-1.5, 1.5-3, >3 m) -> per commune, count band cells (cell-center rule)
#   -> % of commune area per band -> index = sum of the four %.
#   Documented method quirks (kept for fidelity, tracked as repo issues): bilinear
#   resampling BEFORE classification smooths depths across the band edges; depths in
#   (0, 0.0001) fall outside the reclass matrix; the thesis text says "100m grid
#   cells" while the source raster is ~3 arc-seconds.

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(data.table)
  library(ggplot2)
})
source("config.R")

t0 <- Sys.time()

# ---- 1. Raster: crop, project, mask ----------------------------------------------
msg("Loading RP100 raster ...")
flood <- rast(file.path(DATA_DIR, "Dottori", "Europe_RP100_filled_depth.tif"))
flood <- crop(flood, ext(-5, 10, 40, 52))
msg("Projecting to EPSG:2154 (several minutes) ...")
flood <- project(flood, "EPSG:2154")

msg("Masking to France ...")
france <- rnaturalearth::ne_countries(scale = "medium", country = "France", returnclass = "sf")
france <- st_transform(france, crs(flood))
flood  <- mask(crop(flood, vect(france)), vect(france))

# ---- 2. Depth distribution stats (thesis Appendix claims) ------------------------
v <- values(flood, na.rm = TRUE)
flooded <- v[v > 0]
qs <- quantile(flooded, probs = c(0.05, 0.5, 0.95))
depth_stats <- data.table(
  n_cells_flooded    = length(flooded),
  max_depth          = max(flooded),
  p05 = qs[1], p50 = qs[2], p95 = qs[3],
  share_cells_007_3m = mean(flooded > 0.07 & flooded <= 3)
)
fwrite(depth_stats, file.path(OUT_DIR, "raster_depth_stats.csv"))
msg("Depth stats: %.0f flooded cells, share in (0.07, 3] m = %.3f",
    depth_stats$n_cells_flooded, depth_stats$share_cells_007_3m)

# ---- 3. Classify into depth bands ------------------------------------------------
reclass <- matrix(c(
  0.000,  0.000, 0,   # no flooding
  0.0001, 0.5,   1,   # low
  0.5,    1.5,   2,   # moderate
  1.5,    3.0,   3,   # high
  3.0,    Inf,   4    # very high
), ncol = 3, byrow = TRUE)
categories <- classify(flood, reclass, include.lowest = TRUE)

# ---- 4. Commune polygons ---------------------------------------------------------
if (COMMUNES_SOURCE == "legacy") {
  shp <- file.path(LEGACY_DIR, "communes_shape_file.shp")
  msg("Communes: legacy prepared shapefile (%s)", shp)
  communes <- st_read(shp, quiet = TRUE)
} else {
  msg("Communes: building from raw shapefile ...")
  communes <- st_read(file.path(DATA_DIR, "communes-20220101-shp", "communes-20220101.shp"),
                      quiet = TRUE)
  names(communes)[names(communes) == "insee"] <- "INSEE_COM"
  communes <- communes[!grepl("^97|^98", communes$INSEE_COM), ]
}
communes <- st_transform(communes, 2154)
cvect <- vect(communes)
communes$area_total <- expanse(cvect, unit = "m")
cell_area <- prod(res(flood))

# ---- 5. Per-commune band shares (one multi-layer extract; same cell-center rule
#          and same numbers as the original's 4-iteration loop, ~4x faster) ---------
msg("Extracting band cell counts per commune (the slow step) ...")
bands  <- c("low", "moderate", "high", "very_high")
stack4 <- rast(lapply(1:4, function(k) categories == k))
names(stack4) <- bands
counts <- terra::extract(stack4, cvect, fun = sum, na.rm = TRUE)

risk <- data.table(INSEE_COM = communes$INSEE_COM, area_total = communes$area_total)
for (b in bands) {
  risk[[paste0("sum_flood_cells_", b)]] <- counts[[b]]
  risk[[paste0("flood_area_", b)]]      <- counts[[b]] * cell_area
  risk[[paste0("percent_flooded_", b)]] <- 100 * counts[[b]] * cell_area / risk$area_total
}
risk[, percent_any_flood := percent_flooded_low + percent_flooded_moderate +
       percent_flooded_high + percent_flooded_very_high]
# The thesis risk index: NA bands treated as 0, then the same sum
risk[, flood_risk_index_RP100 := fifelse(is.na(percent_any_flood), 0, percent_any_flood)]

fwrite(risk, file.path(OUT_DIR, "commune_flood_risk_RP100.csv"))
communes_out <- merge(communes["INSEE_COM"], as.data.frame(risk), by = "INSEE_COM")
st_write(communes_out, file.path(OUT_DIR, "commune_flood_risk_RP100.gpkg"),
         append = FALSE, quiet = TRUE)
msg("Wrote commune_flood_risk_RP100.{csv,gpkg} (%d communes)", nrow(risk))

# ---- 6. Verification figure ------------------------------------------------------
p <- ggplot(risk, aes(x = flood_risk_index_RP100)) +
  geom_histogram(bins = 60, fill = "skyblue", color = "black", linewidth = 0.2) +
  labs(title = "Flood risk index (RP100) across communes",
       subtitle = sprintf("n = %d communes; index = %% of commune surface flooded, summed over 4 depth bands",
                          nrow(risk)),
       x = "Risk index (0-100)", y = "Communes") +
  theme_minimal()
ggsave(file.path(VERIF_DIR, "fig_risk_index_distribution.png"), p,
       width = 9, height = 5, dpi = 150, bg = "white")

msg("Done in %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))
