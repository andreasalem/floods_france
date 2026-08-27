gc()
rm(list=ls())
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(haven)
library(panelr)
library(readr)
library(fst)
library(data.table)
library(arrow)



##########################################################
# Load DADS (extract what i need)

# change columns HERE to add heterogenity vars. Then run until line
DADS <- read_parquet("C:\\Users\\Public\\Documents\\Fontaine\\Floods_Shock\\1 - Data processing\\Clean\\DADS_2000_to_2020_all.parquet",
                     col_select = c("YEAR", "SIRET", "ZEMPT", "EFF_ALL", "EFF_3112", "EFF_MOY_ET", "HOURS", "HWAGE","HWAGE_3112", "AVGWAGE_3112", "AVGWAGE", "ETP","HIRES", "HIRES2", "SEPARATIONS",
                                    "SEPARATIONS2", "CDI", "CDD", "interim", "othercontract", "SHORTJOB_1M", "SHORTJOB_3M","SHORTJOB_6M","SHORTJOB_1TO3M","SHORTJOB_3TO6M","SHORTJOB_6TO12M",
                                    "SHORTJOB_MORE6M","SHORTJOB_MORE1Y")) %>% as.data.table() %>%  mutate(SIREN = substr(SIRET,1,9))

DADS[,SIREN := as.character(SIREN)]
DADS[,SIRET := as.character(SIRET)]
DADS <- DADS %>% rename(ANNEE=YEAR)
DADS <- DADS[order(DADS$SIRET),]

# checks
colnames(DADS)
DADS[, .N, by = ANNEE]
DADS <- DADS[ANNEE != 1]



#Load FICUS FARE -------------
FICUSFARE <- read_parquet("C:\\Users\\Public\\Documents\\Fontaine\\Floods_Shock\\1 - Data processing\\Clean\\FICUS_FARE_2000_to_2020.parquet")


# Match DADS and FICUS FARE -------------
FICUSFARE_DADS <- merge(DADS, FICUSFARE, by = c("SIREN", "ANNEE"), all.x = TRUE)
rm(DADS, FICUSFARE)
gc()

FICUSFARE_DADS <- FICUSFARE_DADS[order(FICUSFARE_DADS$SIRET, FICUSFARE_DADS$ANNEE ),]
colnames(FICUSFARE_DADS)


# save temporary dataset ------------
#write_parquet(FICUSFARE_DADS, "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Temp/FICUSFARE_DADS/FICUSFARE_DADS_1.parquet")
write_parquet(FICUSFARE_DADS, "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Temp/FICUSFARE_DADS/FICUSFARE_DADS_1_heterogen.parquet")


## Add commnue from DADS data
FICUSFARE_DADS <- read_parquet("C:\\Users\\Public\\Documents\\Fontaine\\Floods_Shock\\1 - Data processing\\Temp\\FICUSFARE_DADS\\FICUSFARE_DADS_1.parquet",
                     col_select = c("ANNEE", "SIRET")) %>% as.data.table()


etab_commune <- read_parquet("C:\\Users\\Public\\Documents\\Fontaine\\Floods_Shock\\1 - Data processing\\Temp\\DADS\\SIRET-codeComune\\all_etablissements.parquet",
                               col_select = c("ANNEE", "SIRET", "INSEE_COM")) %>% as.data.table()

# check col types
sapply(FICUSFARE_DADS, typeof)
sapply(etab_commune, typeof)

# clean
etab_commune[, ANNEE:=as.integer(ANNEE)]
etab_commune <- etab_commune[!is.na(INSEE_COM)]
etab_commune <- etab_commune[nchar(INSEE_COM) == 5]

# checks
etab_commune[is.na(INSEE_COM), .N]

# if ANNE-SIRET have multiple INSEE_COM, keep the first
etab_commune <- etab_commune[, .SD[1], by = .(ANNEE, SIRET)] 

# merge
FICUSFARE_DADS_communes <- merge(FICUSFARE_DADS, etab_commune,
                             by = c("SIRET", "ANNEE"),
                             all.x = TRUE)

# checks
FICUSFARE_DADS_communes[is.na(INSEE_COM), .N]

# if INSEE_COM is missing for a year, but for other years there is, I use the ones from other years
FICUSFARE_DADS_communes[, INSEE_COM := ifelse(
  is.na(INSEE_COM),
  unique(INSEE_COM[!is.na(INSEE_COM)]),
  INSEE_COM
), by = SIRET]

# save
write_parquet(FICUSFARE_DADS_communes, "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Temp/FICUSFARE_DADS/FICUSFARE_DADS_2.parquet")

# Add commune code to main  -----------
FICUSFARE_DADS_communes <- read_parquet("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Temp/FICUSFARE_DADS/FICUSFARE_DADS_2.parquet") %>% 
  as.data.table()
FICUSFARE_DADS <- read_parquet("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Temp/FICUSFARE_DADS/FICUSFARE_DADS_1.parquet") %>% 
  as.data.table()

FICUSFARE_DADS[, INSEE_COM := FICUSFARE_DADS_communes$INSEE_COM]


## exclude DOMs and NAs for INSEE_COM
sapply(FICUSFARE_DADS, typeof)
FICUSFARE_DADS[, INSEE_COM := as.character(INSEE_COM)]
FICUSFARE_DADS <- FICUSFARE_DADS[substr(INSEE_COM, 1, 2) != "97"]
FICUSFARE_DADS <- FICUSFARE_DADS[!is.na(INSEE_COM)]

# move column
setcolorder(FICUSFARE_DADS, c("SIREN","ANNEE","SIRET", "INSEE_COM"))
rm(FICUSFARE_DADS_communes)

# gen employment shares, based on hours worked ---------

## check NAs and 0s
FICUSFARE_DADS[, .(
  ETP_na = sum(is.na(ETP)),
  ETP_na_perc = round(100*sum(is.na(ETP))/.N,1),
  ETP_zero = sum(!is.na(ETP) & ETP == 0),
  ETP_zero_perc = round(100*sum(!is.na(ETP) & ETP == 0)/.N,1),
  
  redi200_na = sum(is.na(redi_e200)),
  redi200_na_perc = round(100*sum(is.na(redi_e200))/.N,1),
  redi200_zero = sum(!is.na(redi_e200) & redi_e200 == 0),
  redi200_zero_perc = round(100*sum(!is.na(redi_e200) & redi_e200 == 0)/.N,1)
), by = ANNEE]

# gen emp_share ---------------
FICUSFARE_DADS[, emp_share := ifelse(!is.na(ETP) & redi_e200 > 0, ETP/redi_e200, NA_real_)]
FICUSFARE_DADS[,summary(emp_share)]

# keep valid shares
FICUSFARE_DADS <- FICUSFARE_DADS[!is.na(emp_share) & emp_share >= 0 & emp_share <=1]

# redistribute financial data to all SIRET's (plants) within a SIREN (company)
cols_scaled=c("r004","redi_r216","redi_e001", "redi_e200","redi_r310","b330","CAPISOC","b319")

for (col in cols_scaled) {
  new_col = paste0(col, "_SIRET")
  set(FICUSFARE_DADS, j = new_col, value = FICUSFARE_DADS[[col]]*FICUSFARE_DADS$emp_share)
}

# gen mono-establishment var
siren_count <- unique(FICUSFARE_DADS[,.(SIREN, SIRET)])[,.N,by=SIREN][N == 1, SIREN]
FICUSFARE_DADS[, mono_etab := as.integer(SIREN %in% siren_count)]
FICUSFARE_DADS[,summary(mono_etab)]
rm(siren_count)

# This is the final dataset for analysis. For heterogeneity analysis need to rerun by modifying the variables imported initially? OR: import heterog variables (PCSs) then simply add them to main dataset i am currently using
write_parquet(FICUSFARE_DADS, "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/FICUS_FARE_DADS_2000_to_2020.parquet")
write_parquet(FICUSFARE_DADS, "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/FICUS_FARE_DADS_2000_to_2020_heterogen.parquet")






###################################### OLD

## Add commune code and geo location from external INSEE source---------
FICUSFARE_DADS <- FICUSFARE_DADS[,.(ANNEE, SIRET, SIREN)]


#
geo1 <- fread("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Import/cleaned_geo_sirene_batches/cleaned_geo_sirene_batch_2.csv",
              select = c("siren", "nic", "siret", "longitude", "latitude", "codeCommuneEtablissement", "codeCommune2Etablissement")) %>% 
  rename(SIRET = siret) %>% as.data.table()
geo1[,SIRET := as.character(SIRET)]

FICUSFARE_DADS_geo <- geo1[,.(SIRET,
                              longitude,
                              latitude,
                              codeCommuneEtablissement,
                              codeCommune2Etablissement)][FICUSFARE_DADS, on = "SIRET"]
rm(FICUSFARE_DADS)                                     
FICUSFARE_DADS_geo[is.na(codeCommuneEtablissement) & !is.na(codeCommune2Etablissement), codeCommuneEtablissement := codeCommune2Etablissement]
FICUSFARE_DADS_geo[,codeCommune2Etablissement := NULL]
setnames(FICUSFARE_DADS_geo, "codeCommuneEtablissement", "INSEE_COM")

#
geo_batches <- 2:8
for (i in geo_batches) {
  path <- paste0("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Import/cleaned_geo_sirene_batches/cleaned_geo_sirene_batch_",i,".csv")
  geo <- fread(path, select = c("siret", "longitude", "latitude", "codeCommuneEtablissement", "codeCommune2Etablissement")) %>% 
    rename(SIRET = siret) %>% as.data.table()
  
  geo[,SIRET := as.character(SIRET)]
  geo[,codeCommuneEtablissement := as.character(codeCommuneEtablissement)]
  geo[,codeCommune2Etablissement := as.character(codeCommune2Etablissement)]
  FICUSFARE_DADS_geo[,INSEE_COM := as.character(INSEE_COM)]
  
  FICUSFARE_DADS_geo[geo, on = "SIRET", `:=`(
    INSEE_COM = fifelse(
      is.na(INSEE_COM),
      fifelse(!is.na(i.codeCommuneEtablissement),i.codeCommuneEtablissement, i.codeCommune2Etablissement),
      INSEE_COM
    ),
    latitude = fifelse(is.na(latitude), i.latitude, latitude),
    longitude = fifelse(is.na(longitude), i.longitude, longitude)
  )]
  matched <- FICUSFARE_DADS_geo[!is.na(INSEE_COM), uniqueN(SIRET)]
  cat(sprintf("After batch %d: %s SIRETs have valid INSEE_COM\n", i, format(matched, big.mark = ",")))
  rm(geo)
  gc()
  }

## (old) add from another source (CASD) -------
geo_casd <- fread("//casd.fr/casdfs/LibreAcces/Géolocalisation des établissements du répertoire SIRENE/Stock au 15 janvier 2025/GeolocalisationEtablissement_Sirene_pour_etudes_statistiques_utf8/GeolocalisationEtablissement_Sirene_pour_etudes_statistiques_utf8.csv",
                  select = c("SIRET", "plg_code_commune","y_latitude", "x_longitude"),
                  sep = ";") %>% as.data.table()

geo_casd[,.N, by = siret][,all(N==1)] # check if siret is unique. TRUE, so unique
geo_casd <- geo_casd %>% rename(SIRET = siret)
geo_casd[,SIRET := as.character(SIRET)]

## merge
setkey(FICUSFARE_DADS_geo, SIRET)
FICUSFARE_DADS_geo <- geo_casd %>% 
  .[FICUSFARE_DADS_geo, on = "SIRET"]

# check how many NAs in INSEE code, still many NAs!!!!!
FICUSFARE_DADS_geo[!is.na(INSEE_COM), uniqueN(SIRET)]
FICUSFARE_DADS_geo[!is.na(plg_code_commune), uniqueN(SIRET)]

# check mismatches across soruces
FICUSFARE_DADS_geo[!is.na(INSEE_COM) & !is.na(plg_code_commune) & INSEE_COM != plg_code_commune,
                   unique(.SD), .SDcols = c("SIRET", "plg_code_commune", "INSEE_COM")]


# create final code commune
FICUSFARE_DADS_geo[, final_INSEE_COM := fifelse(
  !is.na(plg_code_commune), plg_code_commune, INSEE_COM
)]

FICUSFARE_DADS_geo[!is.na(final_INSEE_COM), uniqueN(SIRET)]

# attach these vars to other dataset
write_parquet(FICUSFARE_DADS_geo, "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Temp/FICUSFARE_DADS/FICUSFARE_DADS_1_geo.parquet")


