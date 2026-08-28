# config.R — single source of truth for paths and options
#
# Every pipeline script sources this file first; no script hardcodes a path.
# Run everything from this directory (code/pipeline/), e.g.:  Rscript 00_run_all.R
#
# Override any location via environment variables (useful on another machine):
#   FLOODS_DATA_DIR   raw public data (GASPAR, Dottori rasters, shapefiles, ...)
#   FLOODS_LEGACY_DIR the author's original data_processing/ intermediates
#                     (optional — only the differential audit uses it)

DATA_DIR   <- path.expand(Sys.getenv("FLOODS_DATA_DIR",   "~/R_oba/PSE/Thesis/Data"))
LEGACY_DIR <- path.expand(Sys.getenv("FLOODS_LEGACY_DIR", "~/R_oba/PSE/Thesis/data_processing"))

if (!dir.exists(DATA_DIR)) stop("FLOODS_DATA_DIR not found: ", DATA_DIR,
                                "\nSet the env var to your local copy of the public data (see data/README.md).")

# Repo-relative output locations; scripts are run from code/pipeline/
OUT_DIR   <- file.path(getwd(), "_outputs")      # gitignored: data artifacts
VERIF_DIR <- file.path(getwd(), "verification")  # committed: report + small figures
dir.create(OUT_DIR,   showWarnings = FALSE, recursive = TRUE)
dir.create(VERIF_DIR, showWarnings = FALSE, recursive = TRUE)

# Commune polygons for the raster stage:
#   "legacy" = the author's prepared communes_shape_file.shp (identical input polygons,
#              so the differential audit isolates pure methodology differences)
#   "raw"    = build from the raw OSM/data.gouv shapefile in DATA_DIR
COMMUNES_SOURCE <- Sys.getenv("FLOODS_COMMUNES_SOURCE", "legacy")

options(scipen = 999)

msg <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), sprintf(...), "\n", sep = "")
