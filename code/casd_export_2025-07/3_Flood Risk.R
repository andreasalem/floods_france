library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(rnaturalearth)
setwd("/Users/andreasalem/R_oba/PSE/Thesis")




# Dottori et al. (2022) -------------------------------------------
# Source: https://data.jrc.ec.europa.eu/dataset/1d128b6c-a4ee-4858-9e34-6210707f3c81

## Load flood hazard raster (3 minutes) ---------
flood_raster <- rast("Data/Dottori/Europe_RP100_filled_depth.tif") %>% 
  crop(ext(-5, 10, 40, 52)) %>%
  project("EPSG:2154") # EPSG:2154 == meters


# Load shape file France
communes_sf_metropolitan <- st_read("data_processing/communes_shape_file.shp") %>% 
  st_transform(2154)



# Load shape of France (unified)
france_all <- ne_countries(scale = "medium", country = "France", returnclass = "sf") %>% 
  st_transform(crs(flood_raster))

# Crops the flood raster to the bounding box of France, and masks the raster so that only pixels inside the France polygon keep their values.
flood_raster <- mask(crop(flood_raster, vect(france_all)), 
                     vect(france_all))

# Inspect the raster!
depth_values <- values(flood_raster, na.rm = TRUE) # Extract all values as a vector
max(depth_values, na.rm = TRUE) 
quantile(depth_values, probs = seq(0, 1, 0.1), na.rm = TRUE)

boxplot(depth_values,
        horizontal = TRUE,
        main = "Boxplot of Flood Depths",
        xlab = "Flood Depth (m)",
        col = "lightgreen")

hist(depth_values,
     breaks = 50,
     main = "Histogram of Flood Depths",
     xlab = "Flood Depth (m)",
     col = "skyblue",
     border = "white")

density(depth_values, na.rm = TRUE) %>% 
  plot(
    main = "Density Plot of Flood Depths",
    xlab = "Flood Depth (m)",
    ylab = "Density",
    col = "blue",
    lwd = 2)


## Compute areas flooded, by intensity --------

## 1) Extract flood cells intersecting each commune:
communes_vect <- vect(communes_sf_metropolitan)

## 2) Compute total commune area (in m²) and cell area (in m²)
communes_sf_metropolitan$area_total <- expanse(communes_vect, unit = "m")
cell_area <- res(flood_raster)[1] * res(flood_raster)[2]

## 3) Define depth tresholds
# Category       Depth Range (m)   Risk Implication

# No flooding    0                No impact
# Low            0–0.5            Minor damage, walkable, localized impact
# Moderate       0.5–1.5          Road closures, property damage
# High           1.5–3            Major damage, serious structural risks
# Very High      >3               Life-threatening, total loss risk

# Dottori et al., and JRC’s Risk Data Hub and PESETA IV reports use these treshsolds to classify depth categories, Based on their global and EU-wide hazard/risk maps (e.g., Lisflood-FP or GloFAS-based models)
# I could valudate thresholds with local disaster agency or studies (e.g., BRGM, CEREMA in France)

depth_thresholds <- c(0, 0.5, 1, 1.5, 2, 3)

reclass_matrix <- matrix(c(
  0.000, 0.000, 0,    # No flooding
  0.0001, 0.5, 1,     # Low (0.0001, 0.5]
  0.5,    1.5, 2,     # Moderate (0.5, 1.5]
  1.5,    3.0, 3,     # High
  3.0,    Inf, 4      # Very High
), ncol = 3, byrow = TRUE)

## Reclassify raster using terra::classify()
flood_raster_categories <- classify(flood_raster, reclass_matrix, include.lowest = TRUE)
# flood_raster_categories_500 <- classify(flood_raster_500, reclass_matrix, include.lowest = TRUE)

# This replaces the original raster values with the classified categories. It doesn't preserve the original flood depth values.
# To keep the original and also have the classified version together, create a multi-layer object like a SpatRaster:
# it keep the two layers stacked:
# flood_stack <- c(flood_raster, flood_raster_categories)
# names(flood_stack) <- c("flood_depth", "flood_category")
# flood_raster[[1]]: the original depth
# flood_raster[[2]]: the categorical class

## Loop over categories 0 to 4 (this command take 49 minutes)
for (category in 1:4) {
  
  # Labels
  label <- c("low", "moderate", "high", "very_high")[category]
  description <- c("Low (0–0.5m)", "Moderate (0.5–1.5m)", 
                   "High (1.5–3.0m)", "Very High (>3.0m)")[category]
  cat(paste0("Working on category ", category, " — ", description, "\n"))
  
  # Variable Names
  sum_flood <- paste0("sum_flood_cells_", label)
  area_flood <- paste0("flood_area_", label)
  percent_flood <- paste0("percent_flooded_", label)
  
  # Binarize raster
  binary_raster <- flood_raster_categories == category
  cat("Extracting flooded pixels per commune...\n")
  
 
  # Sum flooded pixels inside each commune polygon, 
  extracted <- terra::extract(binary_raster, communes_vect, fun = sum, na.rm = TRUE)
  cat("Extracted. Now calculating area and percentage...\n")
  ## Add to dataframe
  communes_sf_metropolitan[[sum_flood]] <- extracted[,2]
  
  # Calculate area and percentage total flooded
  ## Flood area = sum of flooded cells × cell area
  communes_sf_metropolitan[[area_flood]] <- extracted[,2] * cell_area
  ## Flooded percentage (%)
  communes_sf_metropolitan[[percent_flood]] <- 
    (communes_sf_metropolitan[[area_flood]] / communes_sf_metropolitan$area_total) * 100

  cat("Added columns: ", sum_flood, ", ", area_flood, ", ", percent_flood, "\n\n")
  
}


colnames(communes_sf_metropolitan)
# Add a "any-flood" computation
communes_sf_metropolitan <- communes_sf_metropolitan %>%
  rowwise() %>%
  mutate(percent_any_flood = sum(c_across(all_of(
    paste0("percent_flooded_", c("low", "moderate", "high", "very_high")))), na.rm = TRUE)) %>%
  ungroup()

## Save Datasets -------------
st_write(communes_sf_metropolitan, "data_processing/communes_risk_RP100.gpkg", append=FALSE)
writeRaster(flood_raster, "data_processing/flood_raster_RP100.tif", overwrite = TRUE)


## Merge various return periods (not needed anymore, can skip it)--------

# load data (here i change colnames, but then i won0t need as i create colnames in the loop)
excluded_cols <- c("INSEE_COM", "nom", "REG", "DEP", "surf_ha", "area_total", "geom")

communes_sf_metropolitan_10 <- st_read("data_processing/communes_risk_RP10.gpkg") %>% 
  rename_with(.fn = ~ paste0(., "_RP10"), .cols = !all_of(excluded_cols))

communes_sf_metropolitan_500 <- st_read("data_processing/communes_risk_RP500.gpkg") %>% 
  rename_with(.fn = ~ paste0(., "_RP500"), .cols = !all_of(excluded_cols))

# check that "common" cols are indeed idenical, and drop them from one of the datassets
sapply(excluded_cols, function(col) {
  identical(communes_sf_metropolitan_10[[col]], communes_sf_metropolitan_500[[col]])
})

# Drop duplicate columns from df2 as well as it'ss geommmetriy
communes_sf_metropolitan_500 <- communes_sf_metropolitan_500 %>% select(-all_of(c("nom", "REG", "DEP", "surf_ha", "area_total", "geom"))) %>% 
  st_drop_geometry()

# merge
complete_risk_commune <- communes_sf_metropolitan_10 %>%
  left_join(communes_sf_metropolitan_500, by = "INSEE_COM")

rm(communes_sf_metropolitan_10, communes_sf_metropolitan_500)



# Create commune-level risk measure -------------

## Create Risk Index, FINAL (RP 100) ---------
communes_sf_metropolitan <- st_read("data_processing/communes_risk_RP100.gpkg")

communes_sf_metropolitan <- communes_sf_metropolitan %>%
  mutate(across(starts_with("percent_flooded_"), ~replace_na(., 0))) %>%
  mutate(
    flood_risk_index_RP100 =
      percent_flooded_low +
      percent_flooded_moderate +
      percent_flooded_high +
      percent_flooded_very_high
  )

# fast, easy, and interpretable
# Captures both extent and severity
# Higher value = more surface area affected across intensity levels

## Save the final dataset ------
st_write(communes_sf_metropolitan, "data_processing/risk_index_RP100.gpkg", append=FALSE)



## (draft) RISK INDEX 1 (not added to saved files!) ---------

# Compare distribution of risk
complete_risk_commune %>%
  st_drop_geometry() %>% 
  select(percent_any_flood_RP10, percent_any_flood_RP500) %>%
  pivot_longer(everything(), names_to = "ReturnPeriod", values_to = "Value") %>%
  ggplot(aes(x = Value, fill = ReturnPeriod)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Comparison",
       x = "Percent Flooded", y = "Density") +
  coord_cartesian(xlim = range(0,20))

# correlation
cor(complete_risk_commune$percent_any_flood_RP10,
    complete_risk_commune$percent_any_flood_RP500,
    use = "complete.obs")

# Compute commmunes whose percentage flooded varies a lot by return period

# ratio (jump in exposure with RP10 vs RP500)
commune_diff <- complete_risk_commune %>%
  mutate(
    diff_low = percent_flooded_low_RP500 - percent_flooded_low_RP10,
    diff_mod = percent_flooded_moderate_RP500 - percent_flooded_moderate_RP10,
    diff_high = percent_flooded_high_RP500 - percent_flooded_high_RP10,
    diff_vhigh = percent_flooded_very_high_RP500 - percent_flooded_very_high_RP10,
    
    ratio_low = percent_flooded_low_RP500 / (percent_flooded_low_RP10 + 1e-6),
    ratio_mod = percent_flooded_moderate_RP500 / (percent_flooded_moderate_RP10 + 1e-6),
    ratio_high = percent_flooded_high_RP500 / (percent_flooded_high_RP10 + 1e-6),
    ratio_vhigh = percent_flooded_very_high_RP500 / (percent_flooded_very_high_RP10 + 1e-6)
  )

# which communes are similar?
commune_diff <- commune_diff %>%
  mutate(
    similar_all_levels = abs(diff_low) < 0.01 &
      abs(diff_mod) < 0.01 &
      abs(diff_high) < 0.01 &
      abs(diff_vhigh) < 0.01
  )

# Risk jumpers:
commune_diff <- commune_diff %>%
mutate(
  low_to_high_risk = percent_flooded_low_RP10 < 0.01 &
    percent_flooded_low_RP500 > 0.3 |
    percent_flooded_moderate_RP10 < 0.01 &
    percent_flooded_moderate_RP500 > 0.3 |
    percent_flooded_high_RP10 < 0.01 &
    percent_flooded_high_RP500 > 0.3 |
    percent_flooded_very_high_RP10 < 0.01 &
    percent_flooded_very_high_RP500 > 0.3
)

# Show communes with large jump in exposure:
commune_diff %>%
  filter(low_to_high_risk) %>%
  select(INSEE_COM, nom, starts_with("percent_flooded_")) %>%
  arrange(desc(percent_flooded_very_high_RP500))

# Show communes with similar risk across RP10 and RP500:
commune_diff %>%
  filter(similar_all_levels) %>%
  select(INSEE_COM, nom, starts_with("percent_flooded_"))

# visualize jumpiness
# Points near the red line → similar exposure
# Points far above the red line → much worse under RP500
# Points on the x-axis but not on y-axis → barely affected under RP500

ggplot(commune_diff, aes(x = percent_flooded_very_high_RP10, y = percent_flooded_very_high_RP500)) +
  geom_point(aes(color = low_to_high_risk), alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "RP10 vs RP500: Percent Flooded (very high Intensity)",
       x = "RP10", y = "RP500") +
  theme_minimal()


##  (draft) RISK INDEX 2 (not added to saved files!) (see ChatGPT discussion on how to build it)---------

# single flood exposure index that accounts for frequency and severity (weights: RP10 = 0.4, RP500 = 0.6))
complete_risk_commune <- complete_risk_commune %>%
  mutate(
    flood_index_low     = 0.4 * percent_flooded_low_RP10     + 0.6 * percent_flooded_low_RP500,
    flood_index_mod     = 0.4 * percent_flooded_moderate_RP10 + 0.6 * percent_flooded_moderate_RP500,
    flood_index_high    = 0.4 * percent_flooded_high_RP10    + 0.6 * percent_flooded_high_RP500,
    flood_index_veryhigh= 0.4 * percent_flooded_very_high_RP10 + 0.6 * percent_flooded_very_high_RP500,
    
    flood_exposure_index = flood_index_low + flood_index_mod + flood_index_high + flood_index_veryhigh
  )

# (ignore) risk_index_norm: average severity per flooded area, so a commune 90% flooded at low depth will have lower score than one 30% flooded at very high depth --> how is it done is wrong, but iss it interesting to build a more nuanced risk index. But this can probably be done by using differenr return periods!
communes_sf_metropolitan$risk_index_norm <- with(communes_sf_metropolitan,
                                                 risk_index / (percent_flooded_low + percent_flooded_moderate + 
                                                                 percent_flooded_high + percent_flooded_very_high)
)

# Alternative Max Flood Exposure across Periods (helps capture worst-case scenarios, regardless of frequency.)
complete_risk_commune <- complete_risk_commune %>%
  mutate(
    max_percent_flooded = pmax(
      percent_flooded_low_RP10, percent_flooded_low_RP500,
      percent_flooded_moderate_RP10, percent_flooded_moderate_RP500,
      percent_flooded_high_RP10, percent_flooded_high_RP500,
      percent_flooded_very_high_RP10, percent_flooded_very_high_RP500,
      na.rm = TRUE
    )
  )

# Threshold-Based Classification (identifies communes with early/frequent exposure vs rare/catastrophic only.)
complete_risk_commune <- complete_risk_commune %>%
  mutate(
    exposed_RP10 = percent_any_flood_RP10 > 0,
    exposed_RP500 = percent_any_flood_RP500 > 0,
    
    exposure_typology = case_when(
      !exposed_RP10 & exposed_RP500 ~ "Only RP500 (rare-catastrophic)",
      exposed_RP10 & exposed_RP500 ~ "Both RP10 & RP500 (frequent & catastrophic)",
      exposed_RP10 & !exposed_RP500 ~ "Only RP10 (likely a data error?)",
      TRUE ~ "No exposure"
    )
  )

# 4: Composite Index (e.g., Risk = Hazard × Exposure × Vulnerability)
#Hazard = flood depth or area

#Exposure = people, assets in flooded zones

#Vulnerability = sensitivity of population/assets (e.g. GDP per capita, building quality)

#complete_risk_commune <- complete_risk_commune %>%
#  mutate(
#    flood_hazard = flood_area_very_high_RP500 + flood_area_high_RP500,
#    exposure = population_total,  # or firm count
#    vulnerability = 1 / GDP_per_capita,  # higher = more vulnerable
    
#    composite_flood_risk = flood_hazard * exposure * vulnerability
#  )

# see index distribution (can do thiss by RP, or with the finakl index)
hist(complete_risk_commune$risk_index_RP500)
summary(complete_risk_commune$risk_index_RP500)
ecdf(complete_risk_commune$risk_index_RP500) %>% plot()



# Plot 1 - risk index by commune! ----------
complete_risk_commune <- st_read("data_processing/risk_index_RP100.gpkg")

summary(complete_risk_commune$flood_risk_index_RP100)
hist(complete_risk_commune$flood_risk_index_RP100)
boxplot(complete_risk_commune$flood_risk_index_RP100)
ecdf(complete_risk_commune$flood_risk_index_RP100) %>% plot() # nice

complete_risk_commune %>%
  mutate(
    index_for_plot = ifelse(is.na(flood_risk_index_RP100) | flood_risk_index_RP100 == 0, NA, flood_risk_index_RP100)
  ) %>% 
  ggplot() +
  geom_sf(aes(fill = index_for_plot), color = "lightgray", size = 0.1) +
  labs(
    title = "Risk index, based on RP100",
    subtitle = "Communes colored by their overall risk index",
    fill = "Risk index, based on RP100",
    caption = "Source: Dottori et al. (2022)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.caption = element_text(size = 10, hjust = 0, face = "italic")
  )

#ggsave(filename = "Risk_flood_communes_RP100.png")
# Exported with Width: 2000 and Height: 1772.


# Plot 2 - Raster (flood area by depth) --------
flood_raster_100 <- rast("data_processing/flood_raster_RP100.tif")

flood_raster_dataframe_100 <- as.data.frame(flood_raster_100, xy = TRUE, na.rm = TRUE)

plot(flood_raster_100, main = "Flood Depth in France (RP100 Scenario)")

complete_risk_commune <- st_read("data_processing/risk_index_RP100.gpkg")
communes_sf_metropolitan_100 <- st_read("data_processing/communes_risk_RP100.gpkg")


# # Ok, but disorted
ggplot() +
  geom_tile(data = flood_raster_dataframe_100,
            aes(x = x, y = y, fill = Europe_RP100_filled_depth)) +
  scale_fill_viridis_c() +
  coord_quickmap() 

# Final version
ggplot() +
  #Raster layer (flood depth)
  geom_tile(data = flood_raster_dataframe_100, 
            aes(x = x, y = y, fill = Europe_RP100_filled_depth)) +
  scale_fill_viridis_c(name = "Flood depth (RP100)", na.value = NA) +
  
  #Commune boundaries (just outlines)
  geom_sf(data = communes_sf_metropolitan_100, 
          fill = NA, color = "grey", size = 0.2) +
  
  #Labels & theme
  labs(
    title = "Flood Depth under RP100 Scenario",
    subtitle = "Raster flood depth overlaid with commune boundaries",
    caption = "Source: Dottori et al. (2022)"
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.caption = element_text(size = 10, hjust = 0, face = "italic")
  )

#ggsave(filename = "Risk_flood_area_RP100.png")
# Exported with Width: 2000 and Height: 1772.

# do this if i want to plot not alll depth levels, but the categories of risk i defined
flood_raster_100 <- rast("data_processing/flood_raster_RP100.tif")
complete_risk_commune <- st_read("data_processing/risk_index_RP100.gpkg")

reclass_matrix <- matrix(c(
  0.000, 0.000, 0,    # No flooding
  0.0001, 0.5, 1,     # Low (0.0001, 0.5]
  0.5,    1.5, 2,     # Moderate (0.5, 1.5]
  1.5,    3.0, 3,     # High
  3.0,    Inf, 4      # Very High
), ncol = 3, byrow = TRUE)

## Reclassify raster using terra::classify()
flood_raster_100_categories <- classify(flood_raster_100, reclass_matrix, include.lowest = TRUE)

flood_raster_dataframe_categories_100 <- as.data.frame(flood_raster_100_categories, xy = TRUE)

names(flood_raster_dataframe_categories_100)[3] <- "category"

flood_raster_dataframe_categories_100 <- flood_raster_dataframe_categories_100 %>%
  filter(!is.na(category), category != 0) # remove no_flooding pixels

flood_raster_dataframe_categories_100$category <- factor(flood_raster_dataframe_categories_100$category, levels = 1:4, labels = c(
  "Low (> 0 & ≤ 0.5)",
  "Moderate (0.5–1.5m)",
  "High (1.5–3m)",
  "Very High (>3m)"
))

# plot it:
ggplot() +
  # Flood raster, categorized
  geom_tile(data = flood_raster_dataframe_categories_100, aes(x = x, y = y, fill = category)) +
  
  # Commune outlines
  geom_sf(data = complete_risk_commune, fill = NA, color = "grey", size = 0.2) +
  
  # Discrete fill scale
  scale_fill_viridis_d(
    name = "Flood Depth Category (RP100)",
    option = "C"
  ) +
  
  # Plot labels and theme
  labs(
    title = "Flooded Areas under RP100 Scenario",
    subtitle = "Classified flood depth (≥ 0.0001m)",
    caption = "Source: Dottori et al. (2022)"
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.caption = element_text(size = 10, hjust = 0, face = "italic")
  )

#ggsave(filename = "Risk_flood_area_RP100_binned.png")


# Now: do the same with the other return periods!!! Usually, small return period are less extreme floods, while a 500 years one is extreme! SO will cover more area
# WIth smaller return perdiods, we will have less cells flooded, as fewer communes are chronically hit --> chronic exposure → cumulative long-term damage.
# To build the risk exposure indiciator, I coiuld check how the flood_area of a commune varies with the return period. For those that does not change much, it means that they are chronoically exposed!!
# mach on this
# See which communes are vulnerable only in extreme cases (RP500), or regularly (RP10).

# When comparing return periods: Insurance & Policy Implications:
  # Insurance premiums often depend on return periods.
  # Urban planning might tolerate RP10 floods but require protective measures for RP100/RP500.
  # Adaptation Investments: Decision-makers may decide whether to invest in flood defenses based on how often large floods are expected (RP).


# PLOTS TO HAVE:
# for each return period, the raster map plotted, overlaying communes (DONE for RP10 and RP500)
# for each return period, the communes map with the risk measure (DONE, but need to re-define risk exposure)
# final map: communes with final risk measure (which incorporated different return periods)


# Plot rivers 1 ------------------------
rivers_raster <- rast("Data/Dottori/Europe_permanent_water_bodies.tif") %>% 
  crop(ext(-5, 10, 40, 52)) %>%
  project("EPSG:2154") # EPSG:2154 == meters


# Load shape file France
communes_shape <- st_read("data_processing/communes_shape_file.shp") %>% 
  st_transform(2154)



# Load shape of France (unified)
france_all <- ne_countries(scale = "medium", country = "France", returnclass = "sf") %>% 
  st_transform(crs(rivers_raster))

# Crops the flood raster to the bounding box of France, and masks the raster so that only pixels inside the France polygon keep their values.
rivers_raster <- mask(crop(rivers_raster, vect(france_all)), 
                     vect(france_all))

rivers_raster_dataframe <- as.data.frame(rivers_raster, xy = TRUE, na.rm = TRUE)

communes_sf_metropolitan_100 <- st_read("data_processing/communes_risk_RP100.gpkg")

colnames(rivers_raster_dataframe)


ggplot() +
  geom_tile(data = rivers_raster_dataframe,
            aes(x = x, y = y, fill = Europe_permanent_water_bodies)) +
  scale_fill_viridis_c() +
  coord_quickmap() 


# Final version
ggplot() +
  #Raster layer (flood depth)
  geom_tile(data = rivers_raster_dataframe, 
            aes(x = x, y = y, fill = Europe_permanent_water_bodies)) +
  scale_fill_viridis_c(name = "Water Depth", na.value = NA) +
  
  #Commune boundaries (just outlines)
  geom_sf(data = communes_sf_metropolitan_100, 
          fill = NA, color = "grey", size = 0.2) +
  
  #Labels & theme
  labs(
    title = "France's permanent water bodies",
    subtitle = "Raster river depth overlaid with commune boundaries",
    caption = "Source: Dottori et al. (2022)"
  ) +
  coord_sf() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.caption = element_text(size = 10, hjust = 0, face = "italic")
  )



# Plot rivers 2 ------------------------
# Source: https://data.europa.eu/data/datasets/cf09b647-fc76-46eb-9b5f-962fb531ca59-1?locale=en


rivers <- st_read( "/Users/andreasalem/Downloads/MasseDEauRiviere_FRA-shp/MasseDEauRiviere_FRA.shp")

bbox_geo <- ext(-5, 10, 40, 52) # Longitude/Latitude

# Convert bbox to an `sf` polygon for cropping
bbox_sf <- st_as_sfc(st_bbox(c(xmin = -5, xmax = 10, ymin = 40, ymax = 52), crs = 4326))

# 3. Crop rivers to bounding box in geographic coordinates
rivers <- st_transform(rivers, 4326)
rivers_cropped <- st_intersection(rivers, bbox_sf)

# 4. Reproject to EPSG:2154 (Lambert-93, meters)
rivers_final <- st_transform(rivers_cropped, 2154)

# 5. Plot
plot(st_geometry(rivers_final), col = "blue", main = "Rivers in Metropolitan France (EPSG:2154)")






# EAIP - 2019 -------------------------------------------
# https://www.georisques.gouv.fr/articles-risques/onrn/acceder-aux-indicateurs-enjeux

eaip <- read_delim("Data/ONRN_Entreprises_EAIP/ONRN_Entreprises_EAIP_2019.csv", delim = ";")







#-------- Gaspar - Risq (need to be adapted to new insee dataset! and is not relevant anyways) ------- 
gaspar_risq <- read.delim("Data/gaspar/risq_gaspar.csv", sep = ";")

unique(gaspar_risq$lib_risque) # 47 risks

innond_communes_list <- unique(gaspar_risq %>%
                                 filter(str_detect(lib_risque, "Inondation")) %>%
                                 pull(cod_commune)) # should I include these two as well: "Par une crue à débordement lent de cours d'eau" + "Par une crue torrentielle ou à montée rapide de cours d'eau"

innond_communes <- gaspar_risq %>%
  filter(cod_commune %in% innond_communes_list) %>% 
  rename(INSEE_COM = cod_commune)

innond_communes_list <- tibble(innond_communes_list) %>% 
  rename(INSEE_COM = innond_communes_list) %>% 
  mutate(is_innond = 1)

innond_communes_list %>% 
  summarise(unique_values = n_distinct(innond_communes_list)) # 16'586 communes

# merge with shapefile 

innond_communes_list <- left_join(communes_sf_metropolitan, innond_communes_list, by = "INSEE_COM") %>% 
  mutate(is_innond = ifelse(is.na(is_innond), 0, is_innond)) %>% 
  st_as_sf()

#ggplot(data = innond_communes_list) +
geom_sf(fill = "gray90", color = "black") +  # Plot all communes in gray
  geom_sf(data = innond_communes_list %>% filter(is_innond == 1), fill = "blue", color = "white") +  # Highlight communes with `is_innond = 1`
  labs(title = "Map of French Communes with Flooding Risk",
       subtitle = "Gray: All Communes | Blue: Communes with Inondation Risk") +
  theme_minimal()

ggplot(data = innond_communes_list) +
  geom_sf(fill = "blue", color = "white", alpha = 0.5) +  # Plot each commune with a translucent blue fill
  labs(title = "Unique Communes with Observed Risks on Map of France") +
  theme_minimal()


ggsave("Flood_risk.png", 
       plot = last_plot(),       
       width = 6000,               # in pixels
       height = 4428,              # in pixels
       units = "px",               # Specify units as pixels
       dpi = 300,
       bg = "white") 



