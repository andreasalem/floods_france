rm(list=ls())
gc()
library(haven)
library(tidyverse)
setwd("C:/Users/Public/Documents/Fontaine/Floods_Shock")

ids<-read.csv("C:\\Users\\Public\\Documents\\Fontaine\\Floods_Shock\\1 - Data processing\\Clean\\liste_SIRET_FARE_FICUS_2000_to_2020.csv")
range(nchar(ids$SIREN))


# Note: Folder FILT1 if FILT == 1 only. Folder FILT1_FILT2 if FILT == 1 and FILT == 2
# Note: for 2009, need to delete the post_national_1_12 (it is the 1/12 sample) before running the second part 
# Pour ne retenir que les postes presents l'annee N (respectivement : l'annee N-1), il faut utiliser la variable FILT (respectivement, FILT_1)
# Le NIC identifie les différents établissements d'une même entreprise, identifiée par son numéro SIREN. On a SIREN + NIC = SIRET.



varlist_2000<-c("SIRENN", "NIC", "APET", "DEP", "REG", "COM", "EQTC","CIPDZ"
                ,"BRUT","ZEMP", "DUREE", "DUREE_1", "NBHEUR","CS2","SEXE","AGE",
                "DEBREMU","FINREMU","DEBREM_1","FINREM_1","FILT","FILT_1") # "efftot","efftot_1" --> can't find; nor "EFF_MOY_ET","EFF_0101_ET","EFF_3112_ET","EFF_3112_ET_1" (read_sas("\\casd.fr\casdfs\Projets\F1CHOCS\Data\DADS_DADS Etablissements_2000\eta00",col_select = c("EFFTOT","EFFTOT_1")))

varlist_2001<-c("sirenn", "nic","apet","dep", "reg", "com", "eqtc","cipdz"
                ,"brut","zemp","duree", "duree_1", "nbheur","cs2","sexe","age",
                "debremu","finremu","debrem_1","finrem_1","filt","filt_1")# "efftot","efftot_1" --> can't find

varlist_2004<-c("SIREN","NIC","APET","EFF_MOY_ET","EFF_0101_ET","EFF_3112_ET","EFF_3112_ET_1","DEPT", "REGT", "COMT","ETP","CPFD"
                ,"S_BRUT","ZEMPT","IDENT_S","DUREE", "DUREE_1", "NBHEUR","CS","SEXE","AGE",
                "DATDEB","DATFIN","DATDEB_1","DATFIN_1","FILT","FILT_1")
varlist_2005<-c("SIREN","NIC","APET","EFF_MOY_ET","EFF_0101_ET","EFF_3112_ET","EFF_3112_ET_1","DEPT", "REGT", "COMT", "CONTRAT_TRAVAIL","ETP","CPFD"
                ,"S_BRUT","ZEMPT","IDENT_S","DUREE", "DUREE_1", "IND_3112","NBHEUR","CS","SEXE","AGE",
                "DATDEB","DATFIN","DATDEB_1","DATFIN_1","FILT","FILT_1")
varlist_2007<-c("SIREN","NIC","APET","EFF_MOY_ET","EFF_0101_ET","EFF_3112_ET","EFF_3112_ET_1","DEPT", "REGT", "COMT", "CONTRAT_TRAVAIL","ETP","CPFD"
                ,"S_BRUT","ZEMPT","IDENT_S","DUREE", "DUREE_1","IND_3112","IND_3112_1","NBHEUR","CS","SEXE","AGE",
                "DATDEB","DATFIN","DATDEB_1","DATFIN_1","FILT","FILT_1")
varlist_2008<-c("SIREN","NIC","A88","APET","EFF_MOY_ET","EFF_0101_ET","EFF_3112_ET","EFF_3112_ET_1","DEPT", "REGT", "COMT", "CONTRAT_TRAVAIL","ETP","CPFD",
                "S_BRUT","ZEMPT","IDENT_S","DUREE", "DUREE_1","IND_3112","IND_3112_1","NBHEUR","CS","SEXE","AGE",
                "DATDEB","DATFIN","DATDEB_1","DATFIN_1","FILT","FILT_1")
varlist<-c("SIREN","NIC","A88","APET","EFF_MOY_ET","EFF_0101_ET","EFF_3112_ET","EFF_3112_ET_1","DEPT","REGT", "COMT", "CONTRAT_TRAVAIL","ETP","CPFD",
           "S_BRUT","ZEMPT","IDENT_S","DUREE", "DUREE_1","IND_3112","IND_3112_1","NBHEUR","PCS","SEXE","AGE",
           "DATDEB","DATFIN","DATDEB_1","DATFIN_1","FILT","FILT_1")
varlist2<-c("SIREN","NIC","A88","APET","EFF_MOY_ET","EFF_0101_ET","EFF_3112_ET","EFF_3112_ET_1","DEPT","REGT", "COMT", "CONTRAT_TRAVAIL","EQTP","CPFD",
            "S_BRUT","ZEMPT","IDENT_S","DUREE", "DUREE_1","IND_3112","IND_3112_1","NBHEUR","PCS","SEXE","AGE",
            "DATDEB","DATFIN","DATDEB_1","DATFIN_1","FILT","FILT_1")

annee_list<-seq(2000,2020,1) # did for 2000-2020 



read_clean<-function(x,year,pth){
  if (year ==2000){
    temp<-read_sas(paste0(pth,x),col_select = varlist_2000)
    temp$A88 <- NA
    temp$EFF_MOY_ET <- NA
    temp$EFF_0101_ET <- NA
    temp$EFF_3112_ET <- NA # efftot --> present in the etablissment files
    temp$EFF_3112_ET_1 <- NA # efftot_1 --> present in the etablissment files
    temp$CONTRAT_TRAVAIL <- NA
    temp$IND_3112 <- NA
    temp$IND_3112_1 <- NA
    temp$IDENT_S <- NA # very strange; but this is missing!
    temp <- temp %>% rename(SIREN = SIRENN, DEPT = DEP, ETP = EQTC, CPFD = CIPDZ,
                            S_BRUT = BRUT, ZEMPT = ZEMP, PCS = CS2, 
                            DATDEB = DEBREMU, DATFIN = FINREMU, DATDEB_1 = DEBREM_1, DATFIN_1 = FINREM_1)
    temp<-temp[which(temp$FILT=="1" | temp$FILT=="2"),] # only the postes that are around in N  
    temp[temp$DUREE %in% 360 | temp$DATFIN %in% 360, "IND_3112"] <- 1
    temp[temp$DATFIN_1 %in% 360, "IND_3112_1"] <- 1
  }
  else if (year == 2001){
    temp<-read_sas(paste0(pth,x),col_select = varlist_2001)
    temp$A88 <- NA
    temp$EFF_MOY_ET <- NA
    temp$EFF_0101_ET <- NA
    temp$EFF_3112_ET <- NA # efftot --> present in the etablissment files
    temp$EFF_3112_ET_1 <- NA # efftot_1 --> present in the etablissment files
    temp$CONTRAT_TRAVAIL <- NA
    temp$IND_3112 <- NA
    temp$IND_3112_1 <- NA
    temp$IDENT_S <- NA # very strange; but this is missing!
    temp <- temp %>% rename(SIREN = sirenn,  NIC = nic, APET = apet, DEPT = dep, ETP = eqtc,CPFD = cipdz,
                            S_BRUT = brut, ZEMPT = zemp, DUREE = duree, DUREE_1 = duree_1, NBHEUR = nbheur, PCS = cs2, SEXE = sexe, AGE = age, 
                            DATDEB = debremu, DATFIN = finremu, DATDEB_1 = debrem_1, DATFIN_1 = finrem_1, FILT = filt, FILT_1 = filt_1)
    temp<-temp[which(temp$FILT=="1" | temp$FILT=="2"),] # only the postes that are around in N  
    temp[temp$DUREE %in% 360 | temp$DATFIN %in% 360, "IND_3112"] <- 1
    temp[temp$DATFIN_1 %in% 360, "IND_3112_1"] <- 1
  }
  else if (year >= 2002 & year <= 2004){
    temp<-read_sas(paste0(pth,x),col_select = varlist_2004)
    temp<-temp %>% rename(PCS=CS)
    temp<-temp[which(temp$FILT=="1" | temp$FILT=="2"),] # only the postes that are around in N   
    temp$A88 <- NA 
    temp$CONTRAT_TRAVAIL <- NA
    temp$IND_3112 <- NA
    temp$IND_3112_1 <- NA
    temp[temp$DUREE %in%  360 | temp$DATFIN %in%  360, "IND_3112"] <- 1 # use %in%  so that it ignores NA 
    temp[temp$DATFIN_1 %in% 360, "IND_3112_1"] <- 1
  }
  else if (year == 2005){
    temp<-read_sas(paste0(pth,x),col_select = varlist_2005)
    temp<-temp %>% rename(PCS=CS)
    temp<-temp[which(temp$FILT=="1" | temp$FILT=="2"),] # only the postes that are around in N   
    temp$A88 <- NA 
    temp$IND_3112_1 <- NA
    temp[temp$DATFIN_1 %in% 360, "IND_3112_1"] <- 1
  }
  else if (year <=2007 & year > 2005){
    temp<-read_sas(paste0(pth,x),col_select = varlist_2007)
    temp<-temp %>% rename(PCS=CS)
    temp<-temp[which(temp$FILT=="1" | temp$FILT=="2"),] # only the postes that are around in N   
    temp$A88 <- NA 
  }
  else if (year==2008){
    temp<-read_sas(paste0(pth,x),col_select = varlist_2008)
    temp<-temp %>% rename(PCS=CS)
    temp<-temp[which(temp$FILT=="1" | temp$FILT=="2"),] # only the postes that are around in N     
  }
  else if (year<=2016 & year > 2008){
    temp<-read_sas(paste0(pth,x),col_select = varlist)
    temp<-temp[which(temp$FILT=="1" | temp$FILT=="2"),] # only the postes that are around in N 
  } 
  else{
    temp<-read_sas(paste0(pth,x),col_select = varlist2)
    temp<-temp[which(temp$FILT=="1" | temp$FILT=="2"),]
    temp <- temp %>% rename(ETP = EQTP) # if needed
  }
  temp$SIREN<-str_pad(as.character(temp$SIREN),9,side="left",pad="0")
  temp$NIC<-str_pad(as.character(temp$NIC),5,side="left",pad="0")
  temp <- temp[(temp$SIREN %in% ids$SIREN),]
  write.table(temp,paste0("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Temp/DADS/FILT1_FILT2/",substr(x,1,8),"_",year))
  rm(temp)
}

for (annee in annee_list) {
  dir= paste0("//casd.fr/casdfs/Projets/F1CHOCS/Data/DADS_DADS Postes_",annee,"/")
  files<-list.files(path = dir ,pattern ="*.sas7bdat")
  if (!length(files)){
    # if nothing in the root, we go to the Région folder
    dir=paste0("//casd.fr/casdfs/Projets/F1CHOCS/Data/DADS_DADS Postes_",annee,"/Régions/")
    files<-list.files(path = dir, pattern ="*.sas7bdat")}
  lapply(files,function(x){
    cat("Reading file:",x)
    read_clean(x,annee,dir)
  })
}


# 2012 and 2013 have separate folders for Ile de France
annee_idf <-seq(2012,2013,1)
for (annee in annee_idf) {
  dir= paste0("//casd.fr/casdfs/Projets/F1CHOCS/Data/DADS_DADS Postes_",annee,"/Ile de France/")
  files<-list.files(path = dir ,pattern ="*.sas7bdat")
  lapply(files,function(x){
    read_clean(x,annee,dir)
  })
}


######################################################
# Now we create the DADS Postes dataset  ------------------
######################################################

rm(list=ls())
gc()
setwd("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Temp/DADS/FILT1_FILT2")
library(tidyverse)
library(data.table)
annee_list<-seq(2002,2003,1) # run for 2000-2020 OK. 



for (annee in annee_list) {
  files<-list.files(pattern=as.character(annee))
  assign("temp",do.call("rbind",lapply(files,function(x){
    dt <- fread(x)
    if ("V1" %in% names(dt)) {
      dt[, V1 := NULL]
      }
    return(dt)
  }))) # appends regional files into single dataframe "temp"
  cat(paste0("Successfully read and merged files for year ", annee, ". Now creating variables...\n"))
  temp$SIREN<-str_pad(as.character(temp$SIREN),9,side="left",pad="0") # if var has less than 9 chars, it adds 0s to the left
  temp$NIC<-str_pad(as.character(temp$NIC),5,side="left",pad="0")
  temp$SIRET<-paste0(temp$SIREN,temp$NIC)
  temp<-temp[!is.na(temp$SIRET),]
  temp <- temp %>% distinct(IDENT_S, SIREN, NIC, .keep_all = TRUE) # remove duplicates from regional files containing workers who live and work in the region. BUT it keeps by default the first one, is this OK?
  temp[which(temp$IND_3112==2),]<-1 # # INDICATEUR DE PRESENCE AU 3112: Deals with "decalage de paie" (av. starting 2009)
  #temp<-temp[which(temp$IND_3112==1),] # Keep those present on 31/12/XXX and no public sector - can't count separatations then 
  #temp<-temp[!is.na(temp$IND_3112),]
  temp<-temp[as.numeric(substr(temp$APET,1,2))<84,]
  temp<-temp[order(temp$SIRET),]
  if (sum(is.na(temp[which(temp$NBHEUR==0),]))>0){temp[which(temp$NBHEUR==0),]<-35} # Forfait   
  # We compute different aggregate by SIRET (different employment concepts, total hours, avg h. wages, then by PCS)
  temp2<-aggregate(temp$IND_3112,by=list(temp$SIRET),FUN=sum,na.rm=TRUE)
  temp2<-setNames(temp2,c("Group.1","EFF_ALL"))
  temp2<-merge(temp2,aggregate(temp$EFF_3112_ET,by=list(temp$SIRET),FUN=mean,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE)
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112"))
  temp2<-merge(temp2,aggregate(temp$NBHEUR,by=list(temp$SIRET),FUN=sum,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE)
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS"))
  temp$SAL_H<-NA
  temp$SAL_H[which(temp$NBHEUR>0)]<-temp$S_BRUT[which(temp$NBHEUR>0)]/temp$NBHEUR[which(temp$NBHEUR>0)]
  temp2<-merge(temp2,aggregate(temp$SAL_H,by=list(temp$SIRET),FUN=mean,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE)
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE"))
  temp2<-merge(temp2,aggregate(temp$S_BRUT,by=list(temp$SIRET),FUN=mean,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE) 
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE"))
  temp2<-merge(temp2,aggregate(temp$ETP,by=list(temp$SIRET),FUN=sum,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE) 
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP"))
  
  temp$HIRES<-0
  temp$HIRES2<-0
  temp$HIRES[which(temp$DATFIN_1<360 | is.na(temp$DATFIN_1))]<-1
  temp$HIRES2[which(temp$IND_3112_1==0)]<-1
  temp$SEPARATIONS<-0
  temp$SEPARATIONS[which(temp$IND_3112==0)]<-1
  temp$SEPARATIONS2<-0
  temp$SEPARATIONS2[which(temp$IND_3112==0 & temp$IND_3112_1==1)]<-1
  temp2<-merge(temp2,aggregate(temp$HIRES,by=list(temp$SIRET),FUN=sum,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE) 
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP","HIRES"))
  temp2<-merge(temp2,aggregate(temp$HIRES2,by=list(temp$SIRET),FUN=sum,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE) 
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP","HIRES","HIRES2"))
  temp2<-merge(temp2,aggregate(temp$SEPARATIONS,by=list(temp$SIRET),FUN=sum,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE) 
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP","HIRES","HIRES2","SEPARATIONS"))  
  temp2<-merge(temp2,aggregate(temp$SEPARATIONS2,by=list(temp$SIRET),FUN=sum,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE) 
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP","HIRES","HIRES2","SEPARATIONS","SEPARATIONS2")) 
  
  temp$CDI<-0
  temp$CDI[which(temp$CONTRAT_TRAVAIL==1)]<-1 
  temp$CDI[which(is.na(temp$CONTRAT_TRAVAIL))]<-NA
  temp$CDD<-0
  temp$CDD[which(temp$CONTRAT_TRAVAIL==2)]<-1
  temp$CDD[which(is.na(temp$CONTRAT_TRAVAIL))]<-NA
  temp$interim<-0
  temp$interim[which(temp$CONTRAT_TRAVAIL==3)]<-1
  temp$interim[which(is.na(temp$CONTRAT_TRAVAIL))]<-NA
  temp$othercontract<-0
  temp$othercontract[which(temp$CONTRAT_TRAVAIL>3)]<-1
  temp$othercontract[which(is.na(temp$CONTRAT_TRAVAI))]<-NA
  temp2<-merge(temp2,aggregate(temp$CDI,by=list(temp$SIRET),FUN=sum,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE) 
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP","HIRES",
                          "HIRES2","SEPARATIONS","SEPARATIONS2","CDI"))
  temp2<-merge(temp2,aggregate(temp$CDD,by=list(temp$SIRET),FUN=sum,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE) 
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP","HIRES",
                          "HIRES2","SEPARATIONS","SEPARATIONS2","CDI","CDD"))
  temp2<-merge(temp2,aggregate(temp$interim,by=list(temp$SIRET),FUN=sum,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE) 
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP","HIRES",
                          "HIRES2","SEPARATIONS","SEPARATIONS2","CDI","CDD","interim"))  
  temp2<-merge(temp2,aggregate(temp$othercontract,by=list(temp$SIRET),FUN=sum,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE) 
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP","HIRES",
                          "HIRES2","SEPARATIONS","SEPARATIONS2","CDI","CDD","interim","othercontract"))  
  temp2<-merge(temp2,aggregate(temp$ZEMPT,by=list(temp$SIRET),FUN=mean,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE) 
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP","HIRES",
                          "HIRES2","SEPARATIONS","SEPARATIONS2","CDI","CDD","interim","othercontract","ZEMPT"))  
  temp2<-merge(temp2,aggregate(temp$EFF_MOY_ET,by=list(temp$SIRET),FUN=mean,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE) 
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP", "HIRES",
                          "HIRES2","SEPARATIONS","SEPARATIONS2","CDI","CDD","interim","othercontract","ZEMPT","EFF_MOY_ET"))
  
  # We count the number of contracts of < 1 month, < 3 months, < 6 months, and also >1 <3, >3 <6, >6 < 1year. However, some durations might be short over the year but actually continues 
  # over the subsequent year, which means that it shouldn't be counted as a short contract. 
  # To try to avoid this problem, we look at total duration over year t and t-1 for the contracts that started less than X days before the end of the t-1 year and that were 
  # still there at the end of the year.
  # To avoid double counting, we count the short contract only over the current year. 
  # That is, we don't count the contracts that start in X days before the end of year t in year t (and don't end before the end of year t), but we count them in t + 1
  # Essentially, either the contract started this year (datdeb > 1) and before X days before the end of the year, or ended this year, or it started last year, less than X days before the end of t-1 
  temp$duree_tot <- rowSums(temp[, c("DUREE", "DUREE_1")], na.rm=T)
  temp$shortjob_1m<-0
  temp$shortjob_1m[which( ((temp$duree_tot<31) & (temp$duree_tot >= 0) & (temp$DUREE<31) & (temp$DUREE >= 0) & (temp$DATDEB < 1) & (temp$DATDEB_1 >= 330) & (temp$DATFIN_1 >= 360) ) |  ((temp$DUREE<31) & (temp$DUREE >= 0) & (temp$DATDEB >= 1) & ((temp$DATDEB < 330) | (temp$DATFIN < 360)) ) )]<-1
  temp$shortjob_3m<-0
  temp$shortjob_3m[which( ((temp$duree_tot<91) & (temp$duree_tot >= 0) & (temp$DUREE<91) & (temp$DUREE >= 0) & (temp$DATDEB < 1) & (temp$DATDEB_1 >= 270) & (temp$DATFIN_1 >= 360) ) |  ((temp$DUREE<91) & (temp$DUREE >= 0) & (temp$DATDEB >= 1) & ((temp$DATDEB < 270) | (temp$DATFIN < 360))  ) )]<-1
  temp$shortjob_6m<-0
  temp$shortjob_6m[which( ((temp$duree_tot<181) & (temp$duree_tot >= 0) & (temp$DUREE<181) & (temp$DUREE >= 0) & (temp$DATDEB < 1) & (temp$DATDEB_1 >= 180) & (temp$DATFIN_1 >= 360) ) |  ((temp$DUREE<181) & (temp$DUREE >= 0) & (temp$DATDEB >= 1) & ((temp$DATDEB < 180) | (temp$DATFIN < 360))  ) )]<-1
  temp$shortjob_1to3m<-0
  temp$shortjob_1to3m[which( ((temp$duree_tot<91) & (temp$duree_tot >= 31) & (temp$DUREE<91) & (temp$DUREE >= 31) & (temp$DATDEB < 1) & (temp$DATDEB_1 >= 270) & (temp$DATFIN_1 >= 360) ) |  ((temp$DUREE<91) & (temp$DUREE >= 31) & (temp$DATDEB >= 1) & ((temp$DATDEB < 270) | (temp$DATFIN < 360))  ) )]<-1
  temp$shortjob_3to6m<-0
  temp$shortjob_3to6m[which( ((temp$duree_tot<181) & (temp$duree_tot >= 91) & (temp$DUREE<181) & (temp$DUREE >= 91) & (temp$DATDEB < 1) & (temp$DATDEB_1 >= 180) & (temp$DATFIN_1 >= 360) ) |  ((temp$DUREE<181) & (temp$DUREE >= 91) & (temp$DATDEB >= 1) & ((temp$DATDEB < 180) | (temp$DATFIN < 360)) ) )]<-1
  temp$shortjob_6to12m<-0
  temp$shortjob_6to12m[which( ((temp$duree_tot<360) & (temp$duree_tot >= 181) & (temp$DUREE<360) & (temp$DUREE >= 181) & (temp$DATDEB < 1) & (temp$DATDEB_1 >= 1) & (temp$DATFIN_1 >= 360) ) |  ((temp$DUREE<360) & (temp$DUREE >= 181) & (temp$DATDEB >= 1) & (temp$DATFIN < 360))  | ((temp$DUREE<390) & (temp$DUREE >360) & (temp$IND_3112_1 == 0)) )]<-1 # Décalage de paie: durée peut aller jusqu'à 390 jours 
  temp$shortjob_more6m<-0
  temp$shortjob_more6m[which(temp$shortjob_6m == 0) ]<-1
  temp$shortjob_more1y<-0
  temp$shortjob_more1y[which( (temp$IND_3112_1 == 1)  & (temp$DUREE>360) )]<-1
  temp2<-merge(temp2,aggregate(temp$shortjob_1m,by=list(temp$SIRET),FUN=sum,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE) 
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP", "HIRES",
                          "HIRES2","SEPARATIONS","SEPARATIONS2","CDI","CDD","interim","othercontract","ZEMPT","EFF_MOY_ET", "SHORTJOB_1M"))
  temp2<-merge(temp2,aggregate(temp$shortjob_3m,by=list(temp$SIRET),FUN=sum,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE) 
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP", "HIRES",
                          "HIRES2","SEPARATIONS","SEPARATIONS2","CDI","CDD","interim","othercontract","ZEMPT","EFF_MOY_ET", "SHORTJOB_1M", "SHORTJOB_3M"))
  temp2<-merge(temp2,aggregate(temp$shortjob_6m,by=list(temp$SIRET),FUN=sum,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE) 
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP", "HIRES",
                          "HIRES2","SEPARATIONS","SEPARATIONS2","CDI","CDD","interim","othercontract","ZEMPT","EFF_MOY_ET", "SHORTJOB_1M", "SHORTJOB_3M", 
                          "SHORTJOB_6M"))
  temp2<-merge(temp2,aggregate(temp$shortjob_1to3m,by=list(temp$SIRET),FUN=sum,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE)
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP", "HIRES",
                          "HIRES2","SEPARATIONS","SEPARATIONS2","CDI","CDD","interim","othercontract","ZEMPT","EFF_MOY_ET", "SHORTJOB_1M", "SHORTJOB_3M", 
                          "SHORTJOB_6M", "SHORTJOB_1TO3M"))
  temp2<-merge(temp2,aggregate(temp$shortjob_3to6m,by=list(temp$SIRET),FUN=sum,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE)
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP", "HIRES",
                          "HIRES2","SEPARATIONS","SEPARATIONS2","CDI","CDD","interim","othercontract","ZEMPT","EFF_MOY_ET", "SHORTJOB_1M", "SHORTJOB_3M", 
                          "SHORTJOB_6M", "SHORTJOB_1TO3M", "SHORTJOB_3TO6M"))
  temp2<-merge(temp2,aggregate(temp$shortjob_6to12m,by=list(temp$SIRET),FUN=sum,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE)
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP", "HIRES",
                          "HIRES2","SEPARATIONS","SEPARATIONS2","CDI","CDD","interim","othercontract","ZEMPT","EFF_MOY_ET", "SHORTJOB_1M", "SHORTJOB_3M", 
                          "SHORTJOB_6M", "SHORTJOB_1TO3M", "SHORTJOB_3TO6M", "SHORTJOB_6TO12M"))
  temp2<-merge(temp2,aggregate(temp$shortjob_more6m,by=list(temp$SIRET),FUN=sum,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE)
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP", "HIRES",
                          "HIRES2","SEPARATIONS","SEPARATIONS2","CDI","CDD","interim","othercontract","ZEMPT","EFF_MOY_ET", "SHORTJOB_1M", "SHORTJOB_3M", 
                          "SHORTJOB_6M", "SHORTJOB_1TO3M", "SHORTJOB_3TO6M", "SHORTJOB_6TO12M", "SHORTJOB_MORE6M"))
  temp2<-merge(temp2,aggregate(temp$shortjob_more1y,by=list(temp$SIRET),FUN=sum,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE)
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP", "HIRES",
                          "HIRES2","SEPARATIONS","SEPARATIONS2","CDI","CDD","interim","othercontract","ZEMPT","EFF_MOY_ET", "SHORTJOB_1M", "SHORTJOB_3M", 
                          "SHORTJOB_6M", "SHORTJOB_1TO3M", "SHORTJOB_3TO6M", "SHORTJOB_6TO12M", "SHORTJOB_MORE6M", "SHORTJOB_MORE1Y"))
  
  
  
  # Average wage for workers still present on December 31st only
  temp4<-temp[which(temp$IND_3112==1),]
  temp4$SAL_H<-NA
  temp4$SAL_H[which(temp4$NBHEUR>0)]<-temp4$S_BRUT[which(temp4$NBHEUR>0)]/temp4$NBHEUR[which(temp4$NBHEUR>0)]
  temp2<-merge(temp2,aggregate(temp4$SAL_H,by=list(temp4$SIRET),FUN=mean,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE)
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP", "HIRES",
                          "HIRES2","SEPARATIONS","SEPARATIONS2","CDI","CDD","interim","othercontract","ZEMPT","EFF_MOY_ET", "SHORTJOB_1M", "SHORTJOB_3M", 
                          "SHORTJOB_6M", "SHORTJOB_1TO3M", "SHORTJOB_3TO6M", "SHORTJOB_6TO12M", "SHORTJOB_MORE6M", "SHORTJOB_MORE1Y", "HWAGE_3112"))
  temp2<-merge(temp2,aggregate(temp4$S_BRUT,by=list(temp4$SIRET),FUN=mean,na.rm=TRUE),by="Group.1",all.x=TRUE,all.y=TRUE) 
  temp2<-setNames(temp2,c("Group.1","EFF_ALL","EFF_3112","HOURS","HWAGE","AVGWAGE", "ETP", "HIRES",
                          "HIRES2","SEPARATIONS","SEPARATIONS2","CDI","CDD","interim","othercontract","ZEMPT","EFF_MOY_ET", "SHORTJOB_1M", "SHORTJOB_3M", 
                          "SHORTJOB_6M", "SHORTJOB_1TO3M", "SHORTJOB_3TO6M", "SHORTJOB_6TO12M", "SHORTJOB_MORE6M", "SHORTJOB_MORE1Y", "HWAGE_3112" , "AVGWAGE_3112"))
  
  
  # By PCS (catÃ©gorie socioprofessionnelle), using first number
  temp$PCS_agg<-substr(temp$PCS,1,2)
  
  #temp %>% group_by(temp$PCS_agg) %>% summarize(count=n()) # display nbr for each category
  #group_PCS<-unique(temp$PCS_agg)
  #group_PCS<-group_PCS[!(substr(group_PCS,1,1) %in% c(NA,"7","8","9"))]
  group_PCS<-c("1","10","11","12","13","2","21","22","23",
               "3","31","32","33","34","35","36","37","38",
               "4","41","42","43","44","45",
               "5","51","52","53","54","55","56",
               "6","61","62","63","64","65","66","67","68","69")
  temp <- temp %>% filter(temp$PCS_agg %in% group_PCS)
  groupx_PCS<-paste0("x.",group_PCS)
  
  # Head count
  temp3<-aggregate(temp$IND_3112,by=list(temp$SIRET,temp$PCS_agg),FUN=sum,na.rm=TRUE)
  temp3$Group.2[which(temp3$Group.2=="")]<-NA
  temp3<-reshape(temp3,timevar="Group.2",idvar="Group.1",direction="wide")
  temp3<-temp3[order(temp3$Group.1),]
  for(var_p in groupx_PCS){
    if(!(var_p %in% names(temp3))) temp3<-temp3 %>% add_column("{var_p}":=NA)
  }
  temp3<-temp3[,order(colnames(temp3))]
  temp3<-setNames(temp3,c("Group.1",paste0("EFF_ALL_",group_PCS)))
  temp2<-merge(temp2,temp3,by="Group.1",all.x=TRUE,all.y=TRUE)  
  # Full time equivalent
  temp3<-aggregate(temp$ETP,by=list(temp$SIRET,temp$PCS_agg),FUN=sum,na.rm=TRUE)
  temp3$Group.2[which(temp3$Group.2=="")]<-NA
  temp3<-reshape(temp3,timevar="Group.2",idvar="Group.1",direction="wide")
  temp3<-temp3[order(temp3$Group.1),]
  for(var_p in groupx_PCS){
    if(!(var_p %in% names(temp3))) temp3<-temp3 %>% add_column("{var_p}":=NA)
  }
  temp3<-temp3[,order(colnames(temp3))]
  temp3<-setNames(temp3,c("Group.1",paste0("ETP_",group_PCS)))
  temp2<-merge(temp2,temp3,by="Group.1",all.x=TRUE,all.y=TRUE)  
  # Hours
  temp3<-aggregate(temp$NBHEUR,by=list(temp$SIRET,temp$PCS_agg),FUN=sum,na.rm=TRUE)
  temp3$Group.2[which(temp3$Group.2=="")]<-NA
  temp3<-reshape(temp3,timevar="Group.2",idvar="Group.1",direction="wide")
  temp3<-temp3[order(temp3$Group.1),]
  for(var_p in groupx_PCS){
    if(!(var_p %in% names(temp3))) temp3<-temp3 %>% add_column("{var_p}":=NA)
  }
  temp3<-temp3[,order(colnames(temp3))]
  temp3<-setNames(temp3,c("Group.1",paste0("HOURS_",group_PCS)))
  temp2<-merge(temp2,temp3,by="Group.1",all.x=TRUE,all.y=TRUE)  
  # Hourly Wage
  temp3<-aggregate(temp$SAL_H,by=list(temp$SIRET,temp$PCS_agg),FUN=mean,na.rm=TRUE)
  temp3$Group.2[which(temp3$Group.2=="")]<-NA
  temp3<-reshape(temp3,timevar="Group.2",idvar="Group.1",direction="wide")
  temp3<-temp3[order(temp3$Group.1),]
  for(var_p in groupx_PCS){
    if(!(var_p %in% names(temp3))) temp3<-temp3 %>% add_column("{var_p}":=NA)
  }
  temp3<-temp3[,order(colnames(temp3))]
  temp3<-setNames(temp3,c("Group.1",paste0("HWAGE_",group_PCS)))
  temp2<-merge(temp2,temp3,by="Group.1",all.x=TRUE,all.y=TRUE) 
  # Wage
  temp3<-aggregate(temp$S_BRUT,by=list(temp$SIRET,temp$PCS_agg),FUN=mean,na.rm=TRUE)
  temp3$Group.2[which(temp3$Group.2=="")]<-NA
  temp3<-reshape(temp3,timevar="Group.2",idvar="Group.1",direction="wide")
  temp3<-temp3[order(temp3$Group.1),]
  for(var_p in groupx_PCS){
    if(!(var_p %in% names(temp3))) temp3<-temp3 %>% add_column("{var_p}":=NA)
  }
  temp3<-temp3[,order(colnames(temp3))]
  temp3<-setNames(temp3,c("Group.1",paste0("AVGWAGE_",group_PCS)))
  temp2<-merge(temp2,temp3,by="Group.1",all.x=TRUE,all.y=TRUE) 
  
  
  temp2$YEAR<-annee
  # First column SIRET, last year  
  names(temp2)[1]<-"SIRET"
  write.table(temp2,paste0("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Temp/DADS/years_all/data","_",annee))
  rm(temp,temp2,temp3, temp4)
}


# convert the yearly databases to parquet  ------------------
files <- list.files("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Temp/DADS/years_all", full.names = T)

for (file in files) {
  df <- fread(file)[,-1, with = F]
  df <- df %>%  mutate(SIREN = substr(SIRET,1,9))
  year <- stringr::str_extract(file, "\\d{4}")
  write_parquet(df, paste0("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Temp/DADS/years_all.parquet/data","_",year,".parquet"))
  rm(df)
  gc()
}

## Creating a DADS (unbalanced) panel ------------------

# Note: years_all folder includes FILT == 1 and FILT == 2 jobs

# run this twice
years <- 2011:2020 # it broke at 2010 (for 2010 needed to convert col SIRET to double)
DADS <- NULL
for (year in years) {
  path <- paste0("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Temp/DADS/years_all.parquet/data_",year, ".parquet")
  df <- read_parquet(path) %>% as.data.table()
  DADS <- rbind(DADS, df, use.names = T, fill = T)
  rm(df)
  gc()
}

# run python script to append the 2! Resulting file is stored in "C:\\Users\\Public\\Documents\\Fontaine\\Floods_Shock\\1 - Data processing\\Clean\\DADS_2000_to_2020_all.parquet"




