library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
library(scales)
library(leaflet) # interactive maps
library(RColorBrewer)
library(readxl)
library(tidyr)
library(kableExtra) # Tables export packages
library(xtable) # Tables export packages
library(ggrepel) # for figure labels
library(patchwork) # to stack figures
library(data.table)


setwd("/Users/andreasalem/R_oba/PSE/Thesis")
options(scipen = 999)

## IMPORT data TRI
insee_final <- st_read("data_processing/TRI.gpkg")

# import GASPAR ------------------------------------------------------------------

##-------- Gaspar - other files (risq is in "risk.R")

gaspar_risq <- read.delim("Data/gaspar/risq_gaspar.csv", sep = ";")

unique(gaspar_risq$lib_risque) # 47 risks

rm(gaspar_risq)

#gaspar_azi <- read.delim("Data/gaspar/azi_gaspar.csv", sep = ";") %>% distinct()

#gaspar_dicrim <- read.delim("Data/gaspar/dicrim_gaspar.csv", sep = ";") %>% distinct()

#gaspar_pprm <- read.delim("Data/gaspar/pprm_gaspar.csv", sep = ";") %>% distinct()

#gaspar_pprn <- read.delim("Data/gaspar/pprn_gaspar.csv", sep = ";") %>% distinct()

#gaspar_pprt <- read.delim("Data/gaspar/pprt_gaspar.csv", sep = ";") %>% distinct()

#gaspar_tim <- read.delim("Data/gaspar/tim_gaspar.csv", sep = ";") %>% distinct()



##-------- Gaspar - Cat Nat
gaspar_catnat <- fread("Data/gaspar/catnat_gaspar.csv", sep = ";") %>%
  distinct() %>%
  rename(INSEE_COM = cod_commune)


# exclude DOMs:
length(unique(gaspar_catnat$INSEE_COM)) # 34'700
gaspar_catnat <- gaspar_catnat %>% subset(!grepl("^97|^98", INSEE_COM))
length(unique(gaspar_catnat$INSEE_COM)) # 34'576 --> difference is 124 communes

## match insee and gaspar communes: NEED TO SOLVE THESE 34 cases + the 4 problemaic cases in INSEE:
gaspar_catnat %>%
  filter(!INSEE_COM %in% insee_final$INSEE_COM) %>%
  distinct(INSEE_COM, lib_commune) # there are still 34 communes that are not matching with insee. Except few, they are all commune anciennes!

insee_final %>% filter(is.na(REG)) # 4 
##

# clean data

## convert to years
gaspar_catnat <- gaspar_catnat %>%
  mutate(
    dat_deb = as.POSIXct(dat_deb, format = "%Y-%m-%d %H:%M:%S"),
    dat_fin = as.POSIXct(dat_fin, format = "%Y-%m-%d %H:%M:%S"), # add dat_arret
    # Extract year from each date column and store in new columns
    dat_deb_year = year(dat_deb),
    dat_fin_year = year(dat_fin),
    duration_days = round(as.numeric(difftime(dat_fin, dat_deb, units = "days")+1),0) # if start-end on same day: = 1, and so on
  )



# NAs ?
colSums(is.na(gaspar_catnat))

# Correct risk label (change code, it is not efficient!)
unique(gaspar_catnat$lib_risque_jo)

gaspar_catnat <- gaspar_catnat %>% 
  mutate(lib_risque_jo_clean = recode(lib_risque_jo,
                                      # Direct string replacements for known variants:
                                      "S�cheresse"                        = "Secheresse",
                                      "Sécheressse"                       = "Secheresse",
                                      "Sécheresse"                        = "Secheresse",
                                      "Chocs M�caniques li�s � l'action des Vagues" = "Chocs mecaniques lies a l'action des vagues",
                                      "Choc M�caniques li�s � l'action des Vagues"  = "Chocs mecaniques lies a l'action des vagues",
                                      "Chocs Mécaniques liés à l'action des Vagues" = "Chocs mecaniques lies a l'action des vagues",
                                      "Inondations et/ou Coul�es de Boue" = "Inondations et/ou Coulees de Boue",
                                      "Inondations et/ou Coulées de Boue" = "Inondations et/ou Coulees de Boue",
                                      "Inondations Remont�e Nappe"        = "Inondations Remontee de Nappe",
                                      "Inondations Remontée Nappe"        = "Inondations Remontee de Nappe",
                                      "Séismes"                           = "Seisme"
  )
  )


# Generate innundation dummy -----------------------------------------------------

# in Aléas de la base de données GASPAR, these are the Innondations one:
#Hydrologique
#1100000 Inondation
#1110000 Inondation - Par une crue (débordement de cours d’eau)
#1120000 Inondation - Par une crue à débordement lent de cours d’eau
#1130000 Inondation - Par une crue torrentielle ou à montée rapide de cours d’eau
#1140000 Inondation - Par ruissellement et coulée de boue
#1150000 Inondation - Par lave torrentielle (torrent et talweg)
#1160000 Inondation - Par remontées de nappes naturelles
#1170000 Inondation - Par submersion marine 


## mark which are inundation
innondation_label <- c(
  "Inondations et/ou Coulees de Boue",
  "Inondations Remontee de Nappe",
  "Lave Torrentielle",
  "Raz de Marée",
  "Inondations par choc mécanique des vagues"
)


## assign a dummy variables = 1 if i is an inundation
gaspar_catnat <- gaspar_catnat %>% 
  mutate(innondation = ifelse(lib_risque_jo_clean %in% innondation_label,1,0))


# Keep innundation dummy == 1 -----------------------------------------------------

# long format
gaspar_catnat_innondation_long <- gaspar_catnat %>% 
  filter(innondation == 1)

# compute flood per commune per year
gaspar_final <- gaspar_catnat_innondation_long %>% 
  mutate(year = dat_deb_year) %>% 
  group_by(INSEE_COM, year)%>% 
  summarise(
    flood_dummy = as.integer(n()>0),
    count_floods_year = n_distinct(paste(cod_nat_catnat, dat_deb)), # very important: if i keep just n_distinct(cod_nat_catnat) --> then a Code CatNat in the same year twice, will be counted as one! Here, it will be counted twice
    avg_duration = mean(duration_days, na.rm = T),
    flood_type = paste(unique(lib_risque_jo_clean), collapse = "; "),
    cat_nat_code = paste(unique(cod_nat_catnat), collapse = "; "))

# add total floods count ALL PERIODS
gaspar_final <- gaspar_final %>%
  left_join(
    gaspar_final %>%
      group_by(INSEE_COM) %>%
      summarise(total_floods_all_years = sum(flood_dummy, na.rm = TRUE), .groups = "drop"),
    by = "INSEE_COM"
  )


# add duration bins
gaspar_final <- gaspar_final %>%
  mutate(
    duration_category = case_when(
      avg_duration < 2 ~ "less than 1 day",
      avg_duration >= 2 & avg_duration <= 22 ~ "1 to 22 days",
      avg_duration > 22 ~ "more than 22 days",
      TRUE ~ NA_character_  # in case of missing duration
    )
  )

# add total floods count ONLY 2000-2020
gaspar_final[, floods_2000_2020 := sum(flood_dummy[year >= 2000 & year <= 2020], na.rm = T), by = INSEE_COM]

# nr of floods in previous 10 years
gaspar_final[, floods_last_10y := sapply(seq_len(.N), function(i){
  sum(year %between% c(year[i]-9, year[i]) & flood_dummy == 1)
}), by = INSEE_COM]


# nr of floods in previous 5 years
gaspar_final[, floods_last_5y := sapply(seq_len(.N), function(i){
  sum(year %between% c(year[i]-4, year[i]) & flood_dummy == 1)
}), by = INSEE_COM]

# dummy for flood in previous year


# years since last flood
gaspar_final[, years_since_last_flood := {
  flood_years <- year[flood_dummy == 1]
  out <- integer(.N)
  last_flood <- NA_integer_
  
  for (i in seq_len(.N)) {
    if (flood_dummy[i] == 1) {
      if (is.na(last_flood)) {
        out[i] <- 0
      } else {
        out[i] <- year[i] - last_flood
      }
      last_flood <- year[i]
    } else if (!is.na(last_flood)) {
        out[i] <-  year[i] - last_flood
    } else {
        out[i] <- NA_integer_
      }
  }
  out
  }, by = INSEE_COM]

# max Nr of floods in any 6 consecutive years
panel_insee_gaspar <- panel_insee_gaspar %>% 
  left_join(panel_insee_gaspar %>% 
              arrange(INSEE_COM,year) %>% 
              group_by(INSEE_COM) %>% 
              mutate(
                floods_in_6yr_window = slide_int(flood_dummy,sum,.before = 5, .after = 0, complete = T)
              ) %>% 
              ungroup() %>% 
              group_by(INSEE_COM) %>% 
              summarise(
                max_floods_any_6yr = max(floods_in_6yr_window, na.rm = T),
                .groups = "drop"),
            by = "INSEE_COM")



# wide format :
#gaspar_catnat_innondation_wide <- gaspar_final %>% 
#  pivot_wider(names_from = year,
#              values_from = count_floods_year,
#                values_fill = 0,
#               names_sort = TRUE) 


# Add floods to INSEE dataset ----------------------------------------------

#write_csv(gaspar_catnat_innondation_long, "data_processing/gaspar_long.csv")


# Plots and Tables ----------------------------------------------

## Table 1: frequency of floods by year, and communes impacted by year ----
table_1 <- gaspar_final %>% 
  group_by(dat_deb_year) %>% 
  summarise(unique_communes = n_distinct(INSEE_COM),
            unique_code_catNat = n_distinct(paste(cod_nat_catnat, dat_deb_year))) # replace with dat_deb if I want to see how many floods per year, not how many code cat nat (for me, a same code cat nat twice in the same year is considered as 2 floods!)

# Table 1: print latex table
table_1 %>%
  kable(format = "latex", booktabs = TRUE, caption = "Flood events and affected communes by year (from GASPAR data)", label = "tab:flood_summary")

## Figure 1: visualize Table 1 ----
# communes
table_1_a <- ggplot(table_1, aes(x = dat_deb_year, y = unique_communes)) +
  geom_bar(stat = "identity", color = "black", fill = "skyblue") +
  labs(title = "Communes impacted by a CatNat",
       x = NULL, 
       y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(min(table_1[["dat_deb_year"]]), max(table_1[["dat_deb_year"]]), by = 1))  # Add more ticks with a step of 1

# events
table_1_b <- ggplot(table_1, aes(x = dat_deb_year, y = unique_code_catNat)) +
  geom_bar(stat = "identity", color = "black", fill = "skyblue") +
  labs(title = "CatNat decrees",
       x = NULL, 
       y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(min(table_1[["dat_deb_year"]]), max(table_1[["dat_deb_year"]]), by = 1)) 

# stack the two plots
(table_1_a / table_1_b) + 
  plot_annotation(
    title = "Overview of Floods in the GASPAR CatNat dataset, 1982-2025",
    subtitle = "Source: Procédures administratives relatives aux risques (BD GASPAR)",
    theme = theme(plot.title = element_text(hjust = 0, face = "bold"),
                  plot.subtitle = element_text(hjust = 0))
  )

## Figure 2: types of floods, frequency of each -----
fig2 <- gaspar_catnat_innondation_long %>% 
  group_by(lib_risque_jo_clean) %>% 
  summarise(unique_communes = n_distinct(INSEE_COM),
            unique_code_catNat = n_distinct(paste(cod_nat_catnat, dat_deb_year))) %>% # the sum of the column unique_code_catNat should be the same as the total count in Table 1, however, some code CatNat are assigned to multiple flood types! hence the small discrepancy
  pivot_longer(cols = starts_with("unique"), names_to = "count_type", values_to = "count")


## assign orderding (by CatNat Episodes):
fig2 <- fig2 %>%
  mutate(lib_risque_jo_clean = factor(lib_risque_jo_clean, levels = rev(
    fig2 %>% 
      filter(count_type == "unique_code_catNat") %>%
      arrange(desc(count)) %>%
      pull(lib_risque_jo_clean)
  )))

# Figure 2: 
fig2 %>% 
  ggplot(aes(x = lib_risque_jo_clean, y = count, fill = count_type)) +
  geom_bar(stat = "identity", fill = "grey50", show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~count_type, scales = "free_x", labeller = as_labeller(c(
    "unique_code_catNat" = "CatNat Episodes",
    "unique_communes" = "Communes impacted"
  ))) +
  labs(title = "Overview of floods in the GASPAR CatNat dataset, 1982-2025",
       subtitle = "Source: Procédures administratives relatives aux risques (BD GASPAR)",
       y = NULL,
       x = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 10, face = "italic", hjust = 0)
  )


## Figure 3 - How often is a commune impacted? ------
# normal plot
gaspar_final %>% 
  distinct(INSEE_COM, .keep_all = TRUE) %>% 
  ggplot(aes(x = total_floods_all_years)) +
  geom_bar(color = "black", fill = "skyblue") +
  scale_x_continuous(breaks = seq(0, max(gaspar_final$total_floods_all_years), by = 5)) +
  labs(title = "How many times is a commune impacted by a flood Cat Nat?",
       subtitle = "Source: Procédures administratives relatives aux risques (BD GASPAR)",
       y = "Communes",
       x = "Number of Cat Nat Events (per commune)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 10, face = "italic", hjust = 0)
  )

# Plot that shows until 9+, and also, it computes percenage of total communes that are
gaspar_final %>% 
  group_by(INSEE_COM) %>%
  summarise(n_events = sum(flood_dummy, na.rm = T)) %>%
  ungroup() %>% 
  mutate(n_events_cat = ifelse(n_events >= 9, "9+", as.character(n_events))) %>% 
  count(n_events_cat, name = "n_communes") %>%
  arrange(as.numeric(gsub("\\+", "", n_events_cat))) %>%
  mutate(
    Total = nrow(insee_final),
    Percent = round(100 * n_communes / nrow(insee_final), 1)
  ) %>% 
  print() %>% 
  ggplot(aes(x = n_events_cat, y = n_communes)) +
  geom_col(color = "black", fill = "skyblue") +
  scale_x_discrete(
    name = "Number of CatNat Flood Events (per commune)"
  ) +
  labs(
    title = "How many times is a commune impacted by a flood Cat Nat?",
    subtitle = "Source: Procédures administratives relatives aux risques (BD GASPAR)",
    y = "Number of Communes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 10, face = "italic", hjust = 0)
  )

# do a map: 
insee_final <- insee_final %>% 
  select(INSEE_COM, TRI, nom, REG, DEP, surf_ha, Zones.d.emploi.2020, geom) %>% 
  left_join(gaspar_final %>%
              select(INSEE_COM, total_floods_all_years, total_floods_2000_2020) %>%
              distinct(),
            by = "INSEE_COM") 

# thiis shows just flooded once
insee_final %>%
  mutate(
    flooded_once = ifelse(total_floods_all_years == 1, 1, 0)) %>% 
  ggplot() +
  geom_sf(aes(fill = factor(flooded_once)), color = "gray80", size = 0.1) +
  scale_fill_manual(
    values = c("0" = "white", "1" = "navyblue"),
    labels = c("0" = "Other communes", "1" = "Flooded once"),
    name = "Flood Status"
  ) +
  labs(
    title = "Communes Flooded Exactly Once",
    subtitle = "All communes shown; flooded-once ones highlighted",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    panel.grid.major = element_line(color = "gray90", size = 0.3)
  )
  
# this all up to 9+
insee_final %>%
  mutate(
    flood_category = case_when(
      is.na(total_floods_2000_2020) ~ "0",
      total_floods_2000_2020 == 0 ~ "0",
      total_floods_2000_2020 == 1 ~ "1",
      total_floods_2000_2020 == 2 ~ "2",
      total_floods_2000_2020 == 3 ~ "3",
      total_floods_2000_2020 == 4 ~ "4",
      total_floods_2000_2020 == 5 ~ "5",
      total_floods_2000_2020 == 6 ~ "6",
      total_floods_2000_2020 == 7 ~ "7",
      total_floods_2000_2020 == 8 ~ "8",
      total_floods_2000_2020 >= 9 ~ "9+"
    ),
    flood_category = factor(flood_category, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9+", "NA"))
  ) %>% 
  ggplot() +
  geom_sf(aes(fill = flood_category), color = "gray80", size = 0.1) +
  scale_fill_manual(
    values = c(
      "0" = "white",
      "1" = "#bdd7e7",
      "2" = "#6baed6",
      "3" = "#3182bd",
      "4" = "#08519c",
      "5" = "#08306b",
      "6" = "#f4a582",
      "7" = "#d6604d",
      "8" = "#b2182b",
      "9+" = "#67001f",
      "NA" = "grey90"
    ),
    name = "Flood Count",
    na.value = "grey90"
  ) +
  coord_sf(crs = 2154) + # if i want the lat long to be curved!
  labs(
    title = "Cumulative number of recognized flood events (2000-2020)",
    subtitle = "",
    fill = "Floods"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3)
  )


## Figure 4 - Average time between floods
# distance between floods
gaspar_final %>% 
  arrange(INSEE_COM, year) %>%
  group_by(INSEE_COM) %>%
  summarise(years = list(year)) %>%
  ungroup() %>% 
  filter(lengths(years) >= 2) %>%  # Calculate differences between years for each commune with ≥2 events
  rowwise() %>%
  mutate(
    intervals = list(diff(sort(unlist(years)))),
    n_events = length(unlist(years))
  ) %>%
  unnest(intervals) %>% 
  group_by(n_events) %>%
  summarise(avg_years_between = round(mean(intervals), 1)) %>%
  #filter(n_events <= 9) %>% 
  ggplot(aes(x = n_events, y = avg_years_between)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(size = 3, color = "steelblue") +
  scale_x_continuous(breaks = 2:23) +
  scale_y_continuous(breaks = 1:14) +
  labs(
    title = "Average Number of Years Between Flood Events",
    subtitle = "By Number of Flood Events (communes with ≥2 events)",
    x = "Number of Flood Events per Commune",
    y = "Average Years Between Events"
  ) +
  theme_minimal()


## Figure 5 - Plots on duration of floods
# show all possible durations
ggplot(gaspar_catnat_innondation_long, aes(x = as.factor(dat_deb_year), y = duration_days, fill = lib_risque_jo_clean)) +
  geom_boxplot() +
  labs(
    title = "Flood Duration by Year and Type",
    x = "Year",
    y = "Duration (days)",
    fill = "Flood Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(0, 22))

# show by category
ggplot(gaspar_final, aes(x = as.factor(year), fill = duration_category)) +
  geom_bar(position = "fill") +  # Use 'stack' instead of 'fill' for absolute counts
  labs(
    title = "Distribution of Flood Durations by Year",
    x = "Year",
    y = "Share of Flood Events",
    fill = "Duration Category"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Plot all CatNat  ----------------------------------------

# see for each risk, how many communes and Cat Nat are present.
# There are 24 types of Cat Nat. They do not correspond to the 47 risks in gaspar_risq (unique(gaspar_risq$lib_risque)!
summary_CatNats <- gaspar_catnat %>% 
  group_by(lib_risque_jo_clean) %>% 
  summarise(unique_communes = n_distinct(INSEE_COM),
            unique_code_catNat = n_distinct(cod_nat_catnat))

# plot them
## create data to be plotted
plot_catNat <- summary_CatNats %>% 
  pivot_longer(cols = starts_with("unique"), names_to = "count_type", values_to = "count") %>% 
  mutate(lib_risque_jo_clean = ifelse(lib_risque_jo_clean == "Mouvements de terrain différentiels consécutifs à la sécheresse et à la réhydratation des sols",
                                      "Mouvements de terrain différentiels consécutifs\nà la sécheresse et à la réhydratation des sols",
                                      lib_risque_jo_clean))
## determine order: 
ordering <- plot_catNat %>% 
  filter(count_type == "unique_code_catNat") %>%
  arrange(desc(count)) %>%
  pull(lib_risque_jo_clean)

## assign orderding (by CatNat Episodes):
plot_catNat <- plot_catNat %>%
  mutate(lib_risque_jo_clean = factor(lib_risque_jo_clean, levels = rev(ordering)))


## plot:
plot_catNat %>% 
  ggplot(aes(x = lib_risque_jo_clean, y = count, fill = count_type)) +
  geom_bar(stat = "identity", fill = "grey50", show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~count_type, scales = "free_x", labeller = as_labeller(c(
    "unique_code_catNat" = "CatNat Episodes",
    "unique_communes" = "Communes impacted"
  ))) +
  labs(title = "Overview of GASPAR CatNat dataset, 1982-2025",
       subtitle = "Source: Procédures administratives relatives aux risques (BD GASPAR)",
       y = NULL,
       x = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 10, face = "italic", hjust = 0)
  )

#
ggsave("Figures/All CatNats.png", 
       plot = last_plot(),       
       width = 3300,               # in pixels
       height = 2000,              # in pixels
       units = "px",               # Specify units as pixels
       dpi = 300,
       bg = "white")


## Plot year of all CatNat ----------------

# we can see that many CAT NAT are recorded by year. (1999 --> is this a bias???? See paper on cartographie: they explain)
summary_CatNats_years <- gaspar_catnat %>% 
  group_by(dat_deb_year) %>% 
  summarise(unique_communes = n_distinct(INSEE_COM),
            unique_code_catNat = n_distinct(cod_nat_catnat))

# Second plot: unique communes impactwed
p1 <- ggplot(summary_CatNats_years, aes(x = dat_deb_year, y = unique_communes)) +
  geom_bar(stat = "identity", color = "black", fill = "skyblue") +
  labs(title = "Communes impacted by a CatNat",
       x = NULL, 
       y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(min(summary_CatNats_years$dat_deb_year), max(summary_CatNats_years$dat_deb_year), by = 2))  # Add more ticks with a step of 1


# Second plot: unique cat nat
p2 <- ggplot(summary_CatNats_years, aes(x = dat_deb_year, y = unique_code_catNat)) +
  geom_bar(stat = "identity", color = "black", fill = "skyblue") +
  labs(title = "CatNat decrees",
       x = NULL, 
       y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(min(summary_CatNats_years$dat_deb_year), max(summary_CatNats_years$dat_deb_year), by = 2))  # Add more ticks with a step of 1

# Stack the two plots
(p1 / p2) + 
  plot_annotation(
    title = "Overview of GASPAR CatNat dataset, 1982-2025",
    subtitle = "Source: Procédures administratives relatives aux risques (BD GASPAR)",
    theme = theme(plot.title = element_text(hjust = 0, face = "bold"),
                  plot.subtitle = element_text(hjust = 0))
  )

#
ggsave("Figures/All CatNats_years.png", 
       plot = last_plot(),       
       width = 3300,               # in pixels
       height = 2000,              # in pixels
       units = "px",               # Specify units as pixels
       dpi = 300,
       bg = "white")


## Plot distribution of CatNat  (makes more sense to plot for all insee dataset no?) ----------------------------------------

# count how many cat per commune
gaspar_catnat <- gaspar_catnat %>%
  group_by(INSEE_COM) %>%
  mutate(count_CatNat_commune = n_distinct(paste(cod_nat_catnat, dat_deb))) # most hit commune is Nice!


# Plot histogram of unique communes
gaspar_catnat %>% 
  distinct(INSEE_COM, .keep_all = TRUE) %>% 
  ggplot(aes(x = count_CatNat_commune)) +
  geom_bar(color = "black", fill = "skyblue") +
  scale_x_continuous(breaks = seq(0, max(gaspar_catnat$count_CatNat_commune), by = 5)) +
  labs(title = "How many times is a commune impacted by a Cat Nat?",
       subtitle = "Source: Procédures administratives relatives aux risques (BD GASPAR)",
       y = "Communes",
       x = "Number of Cat Nat Events (per commune)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0),
    plot.caption = element_text(size = 10, face = "italic", hjust = 0)
  )

#
ggsave("Figures/All CatNats_distribution.png", 
       plot = last_plot(),       
       width = 3300,               # in pixels
       height = 2000,              # in pixels
       units = "px",               # Specify units as pixels
       dpi = 300,
       bg = "white")


## Plot box plot of duration for each catnat  -----------------------------------
ggplot(gaspar_catnat, aes(x = as.factor(lib_risque_jo_clean), y = duration_days)) +
  geom_boxplot(fill = "skyblue", outlier.shape = 21, outlier.fill = "red") +
  labs(title = "Distribution of CatNat Event Durations by Type",
       x = "CatNat Type Code",
       y = "Duration (days)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))


# check for discrepancies between long and wide format! ---------------
# according to long
a <- gaspar_catnat_innondation_long %>%
  group_by(INSEE_COM) %>%
  summarise(total_floods_long = n_distinct(cod_nat_catnat), .groups = "drop")

# according to wide
b <- gaspar_catnat_innondation_wide %>%
  
  
  comparison <- left_join(a, b, by = "INSEE_COM") %>%
  mutate(check = total_floods_long == total_floods_wide)

# Check how many mismatches
table(comparison$check)

problems <- comparison %>%
  filter(check == FALSE)
problems %>% view()

problem_insee <- comparison %>%
  filter(check == FALSE) %>%
  pull(INSEE_COM)

gaspar_catnat_innondation_long %>%
  filter(INSEE_COM %in% problem_insee) %>% view()




# References --------------------------------------------------------------

# GASPAR: there are 2 links. One here (i did not use those) https://www.data.gouv.fr/fr/datasets/base-nationale-de-gestion-assistee-des-procedures-administratives-relatives-aux-risques-gaspar/
# and one here (downloaded on 19th March): https://www.georisques.gouv.fr/donnees/bases-de-donnees/procedures-administratives-relatives-aux-risques

