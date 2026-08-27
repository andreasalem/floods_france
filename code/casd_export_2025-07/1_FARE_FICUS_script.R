gc()
rm(list=ls())
library(tidyverse)
library(haven)
library(Hmisc)   # summary stats
library(panelr)
library(readr)
library(codebookr)
library(data.table)


setwd("C:/Users/Public/Documents/Fontaine/Floods_Shock")

# INPUTS locations:
# years 2001-2007: "C:\\Users\\Public\\Documents\\Fontaine\\1_Donnees_init\\FICUS"
# years 2008-2020: "C:\\Users\\Public\\Documents\\Fontaine\\1_Donnees_init\\FARE"



sum(is.na(ficus$r004))

# FICUS --------
ficus <-  read_csv("C:\\Users\\Public\\Documents\\Fontaine\\1_Donnees_init\\FICUS\\ficusprofil2000.csv")
glimpse(ficus)

ficus$ANNEE <- 2000 

annee_list<-seq(2001,2007,1)
for (annee in annee_list) {
  ficus_an <- read_csv(paste("C:\\Users\\Public\\Documents\\Fontaine\\1_Donnees_init\\FICUS\\ficusprofil",annee,".csv", sep = ""))
  ficus_an$ANNEE <- annee
  ficus <- rbind(ficus, ficus_an)
  rm(ficus_an)
}

# Rename variables 
ficus$r004 = ficus$VAHT - ficus$IMPOTAX - ficus$SUBVEXP # No NAs 
ficus <- ficus %>% select(siren = SIREN , ape_diff = APE, depcom = COM, r004, b330 = EMPDETT, redi_r216 = SALTRAI, redi_e200 = EFFSALM, redi_r310 = CATOTAL, CAPISOC, b319 = PBCAI, ANNEE)
ficus$redi_e001 <- NA

# Order 
ficus <- ficus[order(ficus$siren, ficus$ANNEE),] 

# Duplicates 
ficus <- ficus %>% distinct(siren, ANNEE, .keep_all = TRUE)

# Save
write.table(ficus, "C:\\Users\\Public\\Documents\\Fontaine\\Floods_Shock\\1 - Data processing\\Clean\\ficus")


## FARE  --------
fare <- read_csv("C:\\Users\\Public\\Documents\\Fontaine\\1_Donnees_init\\FARE\\fare2008.csv")
names(fare) <- sub("_08", "", names(fare))
fare$ANNEE <- 2008
fare$r004 = fare$VAHT - fare$IMPOTAX - fare$SUBVEXP # No NA so ok
fare <- fare %>% select(siren, ape_diff = ape, depcom = COM, r004, redi_r216 = SALTRAI, redi_e001 = eff_3112, redi_e200 = EFFSALM, redi_r310 = CATOTAL, ANNEE)
fare$b330 <- NA 
fare$CAPISOC <- NA 
fare$b319 <- NA


annee_list<-seq(2009,2020,1)
for (annee in annee_list) {
  if (annee > 2011 & annee != 2015) {
    namepath <- paste("C:\\Users\\Public\\Documents\\Fontaine\\1_Donnees_init\\FARE\\fare",annee, "_meth", annee, ".csv", sep = "")
  }
  else if (annee== 2011 | annee == 2015) {
    namepath <- paste("C:\\Users\\Public\\Documents\\Fontaine\\1_Donnees_init\\FARE\\fare",annee, "_meth", annee+1, ".csv", sep = "")
  }
  else {
    namepath <- paste("C:\\Users\\Public\\Documents\\Fontaine\\1_Donnees_init\\FARE\\fare",annee, ".csv", sep = "")
  }
  
  fare_an <- read_csv(namepath)
  if (annee >= 2016) {
    names(fare_an)[names(fare_an) == "redi_E200"] = "redi_e200"
    names(fare_an)[names(fare_an) == "redi_E001"] = "redi_e001"
    names(fare_an)[names(fare_an) == "APE_DIFF"] = "ape_diff"
  }
  if (annee == 2017) {
    fare_an$redi_e001 <- NA
  }
  fare_an$ANNEE <- annee
  fare <- rbind(fare, fare_an)
  rm(fare_an)
}

# Order 
fare <- fare[order(fare$siren, fare$ANNEE),] 

# Duplicates 
fare <- fare %>% distinct(siren, ANNEE, .keep_all = TRUE)

# Save
write.table(fare, "C:\\Users\\Public\\Documents\\Fontaine\\Floods_Shock\\1 - Data processing\\Clean\\fare")



# Merge FARE and FICUS --------

#ficus <- read.table("C:\\Users\\Public\\Documents\\Fontaine\\Floods_Shock\\1 - Data processing\\Clean\\ficus")
ficus <- ficus %>% rename(SIREN = siren)
ficus$SIREN <- str_pad(as.character(ficus$SIREN),9, side="left",pad="0")

fare <- read.table("C:\\Users\\Public\\Documents\\Fontaine\\Floods_Shock\\1 - Data processing\\Clean\\fare")
fare <- fare %>% rename(SIREN = siren)
fare$SIREN <- str_pad(as.character(fare$SIREN),9,side="left",pad="0")


FICUSFARE <- rbind(fare, ficus)

write_parquet(FICUSFARE, "C:\\Users\\Public\\Documents\\Fontaine\\Floods_Shock\\1 - Data processing\\Clean\\FICUS_FARE_2000_to_2020.parquet")
#write.table(FICUSFARE, "C:\\Users\\Public\\Documents\\Fontaine\\Floods_Shock\\1 - Data processing\\Clean\\FICUS_FARE_2000_to_2020")



# Save vector of unique SIRENs --------
FICUSFARE <- fread("C:\\Users\\Public\\Documents\\Fontaine\\Floods_Shock\\1 - Data processing\\Clean\\FICUS_FARE_2000_to_2020",
                   colClasses = c("SIREN"="character"))

FICUSFARE <- FICUSFARE[,-1] # since the first col is just the row number (see below check, do it before removing extra column)

unique_sirens <- unique(FICUSFARE["SIREN"])
fwrite(unique_sirens, "C:\\Users\\Public\\Documents\\Fontaine\\Floods_Shock\\1 - Data processing\\Clean\\liste_SIREN_FARE_FICUS_2000_to_2020.csv")
