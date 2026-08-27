library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
library(scales)
library(RColorBrewer)
library(readxl)
library(tidyr)
library(kableExtra) # Tables export packages
library(xtable) # Tables export packages
library(dplyr)
library(data.table)
library(haven)
library(slider)
library(purrr)
library(lubridate)
library(pacman)
library(tictoc)
library(arrow)


setwd("C:/Users/Public/Documents/Fontaine/Floods_Shock")
options(scipen = 999)

######################################
# merge risk and insee
insee_final <- st_read("1 - Data processing/Clean/flooding/TRI.gpkg")
risk <- st_read("1 - Data processing/Clean/flooding/risk_index_RP100.gpkg")

risk_final <- left_join(insee_final, risk %>% 
                          select(INSEE_COM, percent_flooded_low, percent_flooded_moderate, percent_flooded_high, percent_flooded_very_high, 
                                 flood_risk_index_RP100) %>% st_drop_geometry()
                        , by = "INSEE_COM")
risk_final$geom <- NULL
write_csv(risk_final, "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/flooding/risk_final.csv")

###################################### NEXT STEP (merge risk_final and gaspar_final to main data) I DO IN PYTHON (faster). I also recompute count of floods at firm level (not commune level!)



