library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tableone)
library(haven)
library(data.table)
library(arrow)
library(sf)
library(pacman)
library(readxl)
library(haven)
library(stringr)

p_load(gganimate, transformr, geoarrow)

setwd("C:/Users/Public/Documents/Fontaine/Floods_Shock")
# Create commune level dataset -----

## Load data 2000-2020 -----
all_vars = c("mono_etab", "EFF_ALL", "EFF_3112", "EFF_MOY_ET","HOURS","HWAGE","HWAGE_3112",
             "AVGWAGE_3112","AVGWAGE","ETP",
             "r004_SIRET","redi_r216_SIRET","redi_e001_SIRET","redi_e200_SIRET" ,
             "redi_r310_SIRET", "b330_SIRET", "CAPISOC_SIRET", "b319_SIRET",
             "ape_diff", "CDI", "CDD", "interim", "othercontract") 

sample <- read_parquet("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/final_sample.parquet",
                          col_select = c("SIRET","ANNEE", "INSEE_COM", "flood_dummy", "flood_type", "duration_category", "flood_risk_index_RP100",
                                         all_vars))
setDT(sample)
gc()


### Merge with APE crosswalk -----
unique_APE <- read_xlsx("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/crosswalk_APEs.xlsx")
sample <- merge(sample, unique_APE[,c("Final_Sector", "ape_diff", "rev2", "Section")], by = "ape_diff", all.x = T)
rm(unique_APE)

## create sector shares (count, not by employment share, that i do in the summary staqtistics)
sect_count <- sample[,.(num_SIRETs = uniqueN(SIRET)), by = .(INSEE_COM, Final_Sector)]
tot_count <- sample[,.(tot_SIRETs = uniqueN(SIRET)), by = .(INSEE_COM)]
sect_share <- merge(sect_count, tot_count, by = "INSEE_COM")
sect_share[, sector_share := num_SIRETs/tot_SIRETs]
sect_share_wide <- dcast(sect_share, INSEE_COM ~ Final_Sector, value.var = "sector_share")
sect_share_wide$`NA`<- NULL


## Remove outliers before collapsing

## Collapse
names(sample)
dt_commune <- sample[,.(
  EFF_ALL            = sum(EFF_ALL, na.rm = T),
  EFF_3112           = sum(EFF_3112, na.rm = T),
  HOURS              = sum(HOURS, na.rm = T),
  ETP                = sum(ETP, na.rm = T),
  r004_SIRET         = sum(r004_SIRET, na.rm = T),
  redi_r216_SIRET    = sum(redi_r216_SIRET, na.rm = T),
  redi_e200_SIRET    = sum(redi_e200_SIRET, na.rm = T),
  redi_r310_SIRET    = sum(redi_r310_SIRET, na.rm = T),
  b330_SIRET         = sum(b330_SIRET, na.rm = T),
  CAPISOC_SIRET      = sum(CAPISOC_SIRET, na.rm = T),
  b319_SIRET         = sum(b319_SIRET, na.rm = T), 
  
  EFF_MOY_ET         = mean(EFF_MOY_ET, na.rm = T),
  HWAGE              = mean(HWAGE, na.rm = T),
  HWAGE_3112         = mean(HWAGE_3112, na.rm = T),
  AVGWAGE_3112       = mean(AVGWAGE_3112, na.rm = T),
  AVGWAGE            = mean(AVGWAGE, na.rm = T)
  
), by = .(INSEE_COM, ANNEE)]

## add sector shares
dt_commune <- merge(dt_commune, sect_share_wide, by = "INSEE_COM", all.x = T)
rm(sect_count, sect_share, sect_share_wide, sect_count, tot_count)

## remove main dataset and keep the collapsed one
rm(sample)
gc()

## Load GASPAR (1982-2020)-----
gaspar <- read_csv("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/flooding/gaspar_final.csv") 
setDT(gaspar)
setorder(gaspar, INSEE_COM, year)

# i already computed vars (at the commune level) such as: (need to correct some counts)
gaspar$floods_last_10y <- NULL
gaspar$floods_last_5y <- NULL
gaspar$floods_last_3y <- NULL

# floods in 2010-2019 period
gaspar[, floods_2010_2019 := sum(flood_dummy[year >= 2010 & year <= 2019], na.rm = T), by = INSEE_COM]

# floods in 1982-1999 period
gaspar[, floods_1982_1999 := sum(flood_dummy[year >= 1982 & year <= 1999], na.rm = T), by = INSEE_COM]


### Add to main dataset -----
setnames(gaspar, "year", "ANNEE")

# add flood dummy
dt_commune <- merge(dt_commune, 
                         gaspar[,.(INSEE_COM, ANNEE, flood_dummy, flood_type, duration_category, cat_nat_code)],
                         by = c("INSEE_COM", "ANNEE"), all.x = T)

dt_commune[is.na(flood_dummy), flood_dummy:=0]

# add gross counts
dt_commune <- merge(dt_commune, 
                    unique(gaspar[,.(INSEE_COM, floods_1982_1999, floods_2000_2020, floods_2010_2019)]),
                    by = "INSEE_COM", all.x = T)


# recompute floods in last 10, 5, and 3 years
all_years <- unique(dt_commune[, .(INSEE_COM, ANNEE)])
floods_only <- unique(dt_commune[flood_dummy == 1, .(INSEE_COM, ANNEE)])
windows <- c(3, 5, 10)

for (X in windows) {
  flood_counts <- floods_only[,.(past_year = ANNEE), by = INSEE_COM][
    all_years, on = .(INSEE_COM), allow.cartesian = T
  ][
    ANNEE > past_year & ANNEE <= past_year + X
  ][
    , .N, by = .(INSEE_COM, ANNEE)
  ]
  varname <- paste0("floods_last_",X, "y")
  
  if(varname %in% names(gaspar)){
    gaspar[,(varname):=NULL]
  }
  dt_commune <- merge(
    dt_commune, flood_counts, by = c("INSEE_COM", "ANNEE"), all.x = T
  )
  
  dt_commune[is.na(N), N :=0]
  setnames(dt_commune, "N", varname)
  
  #  first year is always NA
  dt_commune[dt_commune[,.I[which.min(ANNEE)], by = INSEE_COM]$V1, (varname) := NA]
}
rm(all_years, flood_counts, floods_only)


## Load geom, INSEE covariates, and risk index -----
insee_final <- st_read("1 - Data processing/Clean/flooding/TRI.gpkg")
risk <- st_read("1 - Data processing/Clean/flooding/risk_index_RP100.gpkg")

risk_final <- left_join(insee_final, risk %>% 
                          select(INSEE_COM, percent_flooded_low, percent_flooded_moderate, percent_flooded_high, percent_flooded_very_high, 
                                 flood_risk_index_RP100) %>% st_drop_geometry()
                        , by = "INSEE_COM")
rm(insee_final, risk)

### Add to main dataset -----
setDT(risk_final)

dt_commune <- merge(
  dt_commune, 
  risk_final, by = "INSEE_COM", all.x = TRUE
)


# Load EAIP data -----
EAIP_path <- "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Import/EAIP/"

etab_EAIP     <- read_delim(paste0(EAIP_path, "ONRN_Entreprises_EAIP_etablissement professionnels/ONRN_Entreprises_EAIP_2019.csv"), delim = ";") %>% setDT()
pop_CE_EAIP   <- read_delim(paste0(EAIP_path, "ONRN_Population_EAIP_cours de eau/ONRN_Population_EAIP_CE.csv"), delim = ";") %>% setDT()
pop_SM_EAIP   <- read_delim(paste0(EAIP_path, "ONRN_Population_EAIP_submersion marine/ONRN_Population_EAIP_SM.csv"), delim = ";") %>% setDT()
batim_CE_EAIP <- read_delim(paste0(EAIP_path, "ONRN_Emprise_totale_batiments_EAIP_cours de eau/ONRN_Emprise_totale_bat_EAIP_CE.csv"), delim = ";",  locale = locale(encoding = "Latin1")) %>% setDT()
batim_SM_EAIP <- read_delim(paste0(EAIP_path, "ONRN_Emprise_totale_batiments_EAIP_submersion marine/ONRN_Emprise_totale_bat_EAIP_SM.csv"), delim = ";", locale = locale(encoding = "Latin1")) %>% setDT()



## Add it to main dataset -----

# Nr and prop. of professional establishment in EAIP by commune
dt_commune <- merge(dt_commune, etab_EAIP[,.(INSEE_COM, Etabl_nr_EAIP = ` NbEnt_EAIP `, Etabl_share_EAIP = PROPEAIP,
                                             NOM_COM_M,	NOM_DEP,	INSEE_DEP,	NOM_REG,	INSEE_REG)],   by = "INSEE_COM", all.x = TRUE)

dt_commune[, Etabl_share_EAIP:=Etabl_share_EAIP/100]

# Population in EAIP
dt_commune <- merge(dt_commune, pop_CE_EAIP[,.(INSEE_COM, pop_CE_EAIP = `Population dans EAIP CE`)], by = "INSEE_COM", all.x = TRUE) 
dt_commune <- merge(dt_commune, pop_SM_EAIP[,.(INSEE_COM, pop_SM_EAIP = `Population dans EAIP SM`)], by = "INSEE_COM", all.x = TRUE) 

#Surface area with buildings in EAIP
dt_commune <- merge(dt_commune, batim_CE_EAIP[,.(INSEE_COM, batim_CE_EAIP = `Surface totale bâtiments dans EAIP CE (m²)`)], by = "INSEE_COM", all.x = TRUE) 
dt_commune <- merge(dt_commune, batim_SM_EAIP[,.(INSEE_COM, batim_SM_EAIP = `Surface totale bâtiments dans EAIP SM (m²)`)], by = "INSEE_COM", all.x = TRUE) 


# Dataset is ready for analysis --------

# export EAIP data in .dta; will add to main analysis in stata.
                          # Here is where I could add INSEE time variant covariates! At any administrative level COM REG DEP
                          # Since i dont, i collapse to keep 1 row per commune
export_for_matching <- dt_commune[,.(INSEE_COM, INSEE_DEP, INSEE_REG, floods_1982_1999, TRI,
                                     percent_flooded_low, percent_flooded_moderate, percent_flooded_high, percent_flooded_very_high,
                                     Etabl_nr_EAIP, Etabl_share_EAIP, pop_CE_EAIP, pop_SM_EAIP, batim_CE_EAIP, batim_SM_EAIP)]

export_for_matching <- unique(export_for_matching, by ="INSEE_COM")
export_for_matching[, INSEE_COM := forcats::fct_inorder(factor(INSEE_COM))]

write_dta(export_for_matching, "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/flooding/commune_EAIP_risk_profile.dta")

# export Communes Litorales: will add to main analysis in stata
coastal_communes <- read_xlsx("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Import/loi-littoral-1986-cog-2022.xlsx", sheet = 2, skip = 2) %>% setDT()

coastal_communes <- coastal_communes[,.(litorale_sea      = as.integer(any(CLASSEMENT == "Mer")),
                                        litorale_lake     = as.integer(any(CLASSEMENT == "Lac")),
                                        litorale_estuary  = as.integer(any(CLASSEMENT == "Estuaire"))),
                                     by = "INSEE_COM"]
coastal_communes$litorale <- 1
write_dta(coastal_communes, "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/flooding/coastal_communes.dta")


# export Niveau de vie
vie_communes <- read_xlsx("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Import/Niveau_de_vie_2013_a_la_commune.xlsx") %>% setDT() %>% 
  rename(niveau_vie = "Niveau de vie Commune",
         INSEE_COM  = "Code Commune")

q90 <- quantile(vie_communes$niveau_vie, 0.9, na.rm = T)
q30 <- quantile(vie_communes$niveau_vie, 0.3, na.rm = T)

vie_communes[,income_category := fifelse(niveau_vie >= q90, "High income (top 10%)",
                                         fifelse(niveau_vie <= q30, "Low income (bottom 30%)",
                                                 "Middle income (30-90%)"))]
#drop one duplicate obs
vie_communes <- vie_communes %>% distinct(INSEE_COM, .keep_all = T)

write_dta(vie_communes %>% select(INSEE_COM, niveau_vie, income_category), "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/flooding/poverty_communes.dta")



# export urban vs rural dummy
rural_urban <- read_xlsx("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Import/communes_rural_urban_FET2021-D4.xlsx", sheet = 4, skip = 2) %>% setDT() %>% 
  rename(INSEE_COM = "Code géographique communal",
         urb_rur_type = "Typologie urbain/rural")

rural_urban <- rural_urban %>% 
  mutate(
    urb_rur_type = urb_rur_type %>% 
      str_squish() %>% 
      str_to_lower(),
    urban = case_when(
      str_detect(urb_rur_type, "^urbain") ~ 1,
      str_detect(urb_rur_type, "^rural")  ~ 0,
      TRUE                                ~ NA_real_)
    )
write_dta(rural_urban, "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/flooding/urban_rural_communes.dta")






# save whole commune dataset
#geom_vector <- unique(dt_commune[,.(INSEE_COM, geom)])
#st_write(geom_vector, "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Temp/vector_communes_geom.gpkg")
#write_geoparquet(dt_commune, "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/communes_dt_2000_2020.parquet")


# plot variables in sample -------
dt_commune %>% 
  filter(ANNEE == 2019, r004_SIRET > 0) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = r004_SIRET), color = "gray80", size = 0.1) +
  scale_fill_viridis_c(
    option = "C", trans = "log") +
  coord_sf(crs = 2154) +
  labs(title = "Value added - 2019") +
  theme_minimal()

ggsave("C:/Users/Public/Documents/Fontaine/Floods_Shock/2 - Data analysis/Figures/Descriptive/Value added.png",
       width = 6000, height = NA, units = "px", dpi = 600) # increased


# drop geometry and save as .dta
dt_commune_nogeo<- st_drop_geometry(dt_commune)
dt_commune_nogeo$geom <- NULL
dt_commune_nogeo <- dt_commune_nogeo %>% filter(ANNEE<= 2019)
dt_commune_nogeo <- dt_commune_nogeo %>% select(-c(41:61)) # drop INSEE covariates
dt_commune_nogeo <- dt_commune_nogeo %>% rename(zempt2020 = Zones.d.emploi.2020)

write_dta(dt_commune_nogeo, "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/subsamples/communes_2000_2019.dta")
write_parquet(dt_commune_nogeo, "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/subsamples/communes_2000_2019.parquet")

