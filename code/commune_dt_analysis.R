library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tableone)
library(haven)
library(arrow)
library(sf)
library(pacman)
p_load(gganimate, transformr, geoarrow)

setwd("C:/Users/Public/Documents/Fontaine/Floods_Shock")

# Load commune level dataset -----
dt_communes <- read_parquet("1 - Data processing/Clean/communes_dt_2005_2019.dta")
  

colnames(dt_commune)