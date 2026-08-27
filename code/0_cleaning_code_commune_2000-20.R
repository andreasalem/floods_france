
#
#
# Script to extract Code Commune from fichiers Etablissements
#
# years 2000-2015 + 2020 <<- this file
# years 2016-2019 <<- py script (since there is no fichiers etablissements for these years, i extract from Postes)
#############################################################



ids<-read.csv("C://Users//Public//Documents//Fontaine//Floods_Shock//1 - Data processing//Clean//liste_SIRET_FARE_FICUS_2000_to_2020.csv")


### define read communes function

varlist_2000<-c("SIRENN", "NICC", "DEP", "REG", "COM", "EFFTOT", "EFFTOT_1")
varlist_2001<-c("sirenn", "nicc", "dep", "reg", "com") 
varlist<-c("SIREN", "NIC", "DEPT", "REGT", "COMT") 

years <-c(2000:2015,2020)


for (year in years) {
  
  dir=paste0("//casd.fr/casdfs/Projets/F1CHOCS/Data/DADS_DADS Etablissements_",year)
  file<-list.files(path = dir ,pattern ="*.sas7bdat", full.names = T)
  
  if (year ==2000){
    dt <- read_sas(file, col_select = varlist_2000) %>% as.data.table()
    setnames(dt, old = c("NICC", "SIRENN", "DEP", "COM", "REG"), new = c("NIC", "SIREN", "DEPT", "COMT", "REGT"))
    # gen SIRET
    dt[, NIC := str_pad(as.character(NIC), 5, pad = "0")]
    dt[, SIREN := str_pad(as.character(SIREN), 9, pad = "0")]
    dt[, SIRET := paste0(SIREN, NIC)]
    
    # gen INSEE_COM
    dt[, DEPT := str_pad(as.character(DEPT),2)]
    dt[, COMT := str_pad(as.character(COMT),3)]
    dt[, INSEE_COM := paste0(DEPT, COMT)]
    dt <- dt[,COMT:= NULL]
    
    # keep first combination of SIRET-codeCommune
    dt <- dt[,.SD[1], by = .(SIRET, INSEE_COM)] 
    dt <- dt[SIREN %in% ids$SIREN]
    
    }
  else if (year == 2001){
    dt <- read_sas(file, col_select = varlist_2001) %>% as.data.table()
    setnames(dt, old = c("nicc", "sirenn", "dep", "com", "reg"), new = c("NIC", "SIREN", "DEPT", "COMT", "REGT"))
    # gen SIRET
    dt[, NIC := str_pad(as.character(NIC), 5, pad = "0")]
    dt[, SIREN := str_pad(as.character(SIREN), 9, pad = "0")]
    dt[, SIRET := paste0(SIREN, NIC)]
    
    # gen INSEE_COM
    dt[, DEPT := str_pad(as.character(DEPT),2)]
    dt[, COMT := str_pad(as.character(COMT),3)]
    dt[, INSEE_COM := paste0(DEPT, COMT)]
    dt <- dt[,COMT:= NULL]
    
    # keep first combination of SIRET-codeCommune
    dt <- dt[,.SD[1], by = .(SIRET, INSEE_COM)] 
    dt <- dt[SIREN %in% ids$SIREN]
    
    }
  else if (year >= 2002){
    dt <- read_sas(file, col_select = varlist) %>% as.data.table()
    # gen SIRET
    dt[, NIC := str_pad(as.character(NIC), 5, pad = "0")]
    dt[, SIREN := str_pad(as.character(SIREN), 9, pad = "0")]
    dt[, SIRET := paste0(SIREN, NIC)]
    
    # gen INSEE_COM
    dt[, INSEE_COM := COMT]
    dt <- dt[,COMT:= NULL]
    
    # keep first combination of SIRET-codeCommune
    dt <- dt[,.SD[1], by = .(SIRET, INSEE_COM)] 
    dt <- dt[SIREN %in% ids$SIREN]
  }
  
  # order
  dt$YEAR <- year
  setcolorder(dt, c("YEAR", "SIREN", "NIC", "SIRET", "INSEE_COM", "DEPT", "REGT"))
  dt <- dt[order(dt$SIRET, dt$INSEE_COM ),]
  
  # save
  write_parquet(dt,paste0("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Temp/DADS/SIRET-codeComune/etablissements_",year,".parquet"))
  
  rm(dt)
  gc()
}

