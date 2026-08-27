library(data.table)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readxl)
library(tableone)
library(haven)
library(arrow)

###########################################
# Import data
#sample <- read_parquet("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/FICUS_FARE_DADS_GASPAR_2000_to_2020.parquet")
#setDT(sample)

#colnames(sample)
#unique(sample$ANNEE) # 2000-2020

# final sample: remove communes for which I don't have risk index data (however, i need this var only for matching! Should i really remove 1900 communes?)
#communes_to_drop <-unique(sample[is.na(flood_risk_index_RP100), INSEE_COM]) # ... communes to drop
#sample <- sample[!INSEE_COM %in% communes_to_drop]
#write_parquet(sample, "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/final_sample.parquet")



#subsample <- rename(subsample,
#                    Nb_emplois_au_lieu_de_travail      = Nb.d.emplois.au.lieu.de.travail..LT..2021,
#                    Densite_population_historique      = Densité.de.population..historique.depuis.1876..2021,
#                    Creations_entreprises_en_nombre    = Créations.d.entreprises..en.nombre..2022,
#                    Unites_legales_en_nombre           = Unités.légales..en.nombre..2021)

#write_dta(subsample, "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/final_sample.dta")

##########################################
# I take FICUS_FARE_DADS_GASPAR_2000_to_2020.parquet does not contain counts of previous cout correctlw, which is hierarchically the most comprehensive data set of outcomes. I use this to plot the 
# map of the distribution of economic activity



#Summary stats: Table -----------

# EFF_3112 should equal e001 (excluded the latter)
# EFF_MOY_ET shouldequal e200 (excluded the latter)

all_vars = c("mono_etab", "EFF_ALL", "EFF_3112", "EFF_MOY_ET","HOURS","HWAGE","HWAGE_3112",
         "AVGWAGE_3112","AVGWAGE","ETP",
         "r004_SIRET","redi_r216_SIRET","redi_e001_SIRET","redi_e200_SIRET" ,
         "redi_r310_SIRET", "b330_SIRET", "CAPISOC_SIRET", "b319_SIRET",
         "ape_diff", "CDI", "CDD", "interim", "othercontract") 

subsample <- read_parquet("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/subsamples/sample_2010_2019.parquet",
                          col_select = c("SIRET","ANNEE", "INSEE_COM", "flood_dummy", "flood_type", "duration_category", "flood_risk_index_RP100",
                                         all_vars))

subsample <- read_parquet("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/final_sample.parquet",
                          col_select = c("SIRET","ANNEE", "INSEE_COM", "flood_dummy", "flood_type", "duration_category", "flood_risk_index_RP100",
                                         all_vars))

setDT(subsample)
gc()
## main vars  ------
vars <- c("mono_etab", "EFF_ALL", "EFF_3112", "EFF_MOY_ET","HOURS","HWAGE","HWAGE_3112",
           "AVGWAGE_3112","AVGWAGE","ETP", "SEPARATIONS", "SEPARATIONS2", "HIRES", "HIRES2",
           "r004_SIRET","redi_r216_SIRET","redi_e001_SIRET","redi_e200_SIRET" ,
           "redi_r310_SIRET", "b330_SIRET", "CAPISOC_SIRET", "b319_SIRET")

summary_table <- lapply(vars, function(var) {
  x <- subsample[[var]]
  data.table(
    Variable = var,
    Mean = round(mean(x, na.rm = T),2),
    Sd = round(sd(x, na.rm = T),2),
    Count = sum(!is.na(x))
  )
 })

summary_dt <- rbindlist(summary_table)

## add contract type  ------
contract_vars <- c("CDI", "CDD", "interim", "othercontract")

subsample[, total_contract  :=rowSums(.SD, na.rm = T), .SDcols = contract_vars]


typeof(subsample$CDI)

contract_stats <- rbindlist(lapply(contract_vars, function(var) {
  subsample[,paste0("share_", var) := get(var) /total_contract]
  col <- paste0("share_", var)
  x <- subsample[[col]]
  data.table(
    Variable = paste0("Share: ",var),
    Mean = round(mean(x, na.rm = T),2),
    Sd = round(sd(x, na.rm = T),2),
    Count = sum(!is.na(x))
  )
}))

# bind to initial 
summary_dt_extended <- rbindlist(list(summary_dt,contract_stats))

## add industry ------

# crosswalk between ape_diff and NAF 2 (using NAF2, NAF 1, and NAF 1993 codes)
subsample[,.N, by = ape_diff]
unique_APE <-unique(subsample$ape_diff) %>% sort() %>% tibble() %>% setDT()
unique_APE[, ape_diff := as.character(.)]
unique_APE$. <- NULL


crosswalk <- setDT(read_excel("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Temp/data_for_crosswalk_APEs.xlsx", sheet = 1))
crosswalk <- crosswalk[,.(rev1 = `APE en NAF rev1`, rev2 = `APE en NAF rev2`)]
crosswalk <- crosswalk[!grepl("Ensemble", rev1)]
crosswalk_unique <- unique(crosswalk[,.(rev1, rev2)])

ref_table <- setDT(read_excel("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Temp/data_for_crosswalk_APEs.xlsx", sheet = 2))
ref_table <- ref_table[,.(Section, libelle = `Libellé des sections`, Division = `Code Division`, intitule = `Intitulé` )]
ref_table[, Section := Section[nafill(replace(seq_len(.N),is.na(Section), NA),"locf")]]
ref_table[, libelle := libelle[nafill(replace(seq_len(.N),is.na(libelle), NA),"locf")]]

# convert NAF 1 to NAF 2, if resulting (after merge) rev2 = NA; then ape_diff code was not in ver1
unique_APE <- merge(unique_APE, crosswalk_unique, by.x = "ape_diff", by.y = "rev1", all.x = TRUE)
unique_APE[,uniqueN(rev2), by = ape_diff] # some rev1 are associate to multiple rev2, keep one randomly (they are almost always all time within same division)
unique_APE <- unique_APE[, .(rev2=sample(rev2,1)), by = ape_diff]


# check if NAs are present in NAF2; if yes attach
missing <- unique_APE[is.na(rev2), ape_diff]
matches <- missing[missing %in% unique(crosswalk$rev2)]
unique_APE[ape_diff %in% matches, rev2 := ape_diff]


# there are still NAs --> they are from 1993 NAF code. Fill those
missing <- unique_APE[is.na(rev2), ape_diff] %>% tibble() %>% setDT()
missing[, rev93 := as.character(.)]
missing$. <- NULL

crosswalk_93 <- setDT(read_excel("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Temp/data_for_crosswalk_APEs.xlsx", sheet = 3))
crosswalk_93[,(names(crosswalk_93)) := lapply(.SD,function(x)gsub("\\.","",x))]
crosswalk_93 <- crosswalk_93[,.(rev93 = `NAF 1993`, rev1 = `NAF rév. 1 (2003)`)]
crosswalk_93 <- unique(crosswalk_93[,.(rev93, rev1)])
crosswalk_93 <- crosswalk_93[!grepl("Ensemble", rev93)]


# convert NAF 93 to NAF 1 
missing2 <- left_join(missing, crosswalk_93, by = "rev93") %>% tibble() %>% setDT()
missing2 <- unique(missing2[,.(rev93, rev1)])
missing2 <- missing2[, .(rev1=sample(rev1,1)), by = rev93]

# convert NAF 1 to NAF 2
missing2 <- merge(missing2, crosswalk_unique, by.x = "rev1", by.y = "rev1", all.x = TRUE) %>% tibble() %>% setDT()
missing2 <- unique(missing2[,.(rev93, rev2)]) # some codes are assciated with more than 1 rev2
missing2 <- missing2[, .(rev2=sample(rev2,1)), by = rev93]

# attach to main data
setnames(missing2, "rev2", "rev2_93")
unique_APE <- merge(unique_APE, missing2, by.x = "ape_diff", by.y = "rev93", all.x = TRUE)
unique_APE[is.na(rev2), rev2:=rev2_93]
unique_APE$rev2_93 <- NULL

# add division
unique_APE[,Division := substr(rev2, 1, 2)]
unique_APE <- merge(unique_APE, ref_table, by = "Division", all.x = T)

# create further aggregation
unique_APE[, Final_Sector := fifelse(
  Section %in% c("A", "B"), "Agriculture",
  fifelse(Section == "C", "Industries",
  fifelse(Section %in% c("D", "E", "F"), "Construction",
  fifelse(Section %in% c("G", "H", "I"), "Commerces", 
  fifelse(Section %in% c("J", "K", "L","M", "N", "O", "P", "Q", "R", "S", "T", "U"), "Services",NA_character_)))))]

# save
writexl::write_xlsx(unique_APE, "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/crosswalk_APEs.xlsx")
write_dta(unique_APE, "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/crosswalk_APEs.dta")

# add to main dataset (only 27 ape_diff NAs remaining)
subsample <- merge(subsample, unique_APE[,c("Final_Sector", "ape_diff", "rev2", "Section")], by = "ape_diff", all.x = T)


# compute summary stats by sector, relative to EFF_ALL !
total_emp <- sum(subsample$EFF_ALL, na.rm = T)
total_etp <- sum(subsample$ETP, na.rm = T)
total_valueadded <- sum(subsample$r004_SIRET, na.rm = T)
total_wagesalar <- sum(subsample$redi_r216_SIRET, na.rm = T)

ind_stats <- subsample[,.(
  Count = sum(r004_SIRET, na.rm = T),
  Share = round(sum(r004_SIRET, na.rm = T)/total_valueadded, 2),
  Mean = round(mean(r004_SIRET, na.rm = TRUE), 2),
  Sd = round(sd(r004_SIRET, na.rm = TRUE), 2)
), by = Final_Sector] # high mean means large establishments. HIgh mean can have low share, as in a sector you can have high establishment but in total employment is not much



# bind to initial 
summary_dt_extended <- rbindlist(list(summary_dt_extended,ind_stats))

# save table
write_csv(summary_dt_extended, "C:/Users/Public/Documents/Fontaine/Floods_Shock/2 - Data analysis/Tables/Balance/summary_baseline.csv")

# Flood counts ------

# table by year
floods_stats_siret <- subsample[flood_dummy == 1, .N, by=.(ANNEE, duration_category)]
floods_stats_siret <- dcast(floods_stats_siret, ANNEE ~ duration_category,
                            value.var = "N", fill = 0)
floods_stats_siret[,total_floods := rowSums(.SD), .SDcols=c("less than 1 day", "1 to 22 days", "more than 22 days")]

write_csv(floods_stats_siret, "C:/Users/Public/Documents/Fontaine/Floods_Shock/2 - Data analysis/Tables/Flood_counts_SIRET.csv")

# table by exposure
flood_counts <-  subsample[, .(
  total_floods = sum(flood_dummy),
  low = sum(flood_dummy == 1 & duration_category == "less than 1 day"),
  medium = sum(flood_dummy == 1 & duration_category == "1 to 22 days"),
  high = sum(flood_dummy == 1 & duration_category == "more than 22 days"),
  avg_time_between = if(sum(flood_dummy)>1) {
    mean(diff(sort(ANNEE[flood_dummy==1])))
  } else NA_real_
), by = SIRET]

flood_counts <- flood_counts[,.(
  Total_SIRETs = .N,
  low_intensity = sum(low),
  medium_intensity = sum(medium),
  high_intensity = sum(high),
  avg_years_between = mean(avg_time_between, na.rm = T)
), by = total_floods][order(total_floods)]


#flood_events <- unique(subsample[flood_dummy == 1 & duration_category != "less than 1 day", .(SIRET, ANNEE, duration_category)])


# Balance table------
subsample[, first_treat := if (all(flood_dummy == 0)) NA else min(ANNEE[flood_dummy==1]), by = SIRET]
subsample[, ever_treat := ifelse(is.na(first_treat), 0, 1)]


# 1) Baseline sample  (all Control (all years) and all not-yet treated)
sample_pre <- subsample[ANNEE < first_treat | is.na(first_treat)]
rm(subsample)
gc()
#t.test(EFF_ALL ~ 


var_balance = c("mono_etab", "EFF_3112", "EFF_MOY_ET","HOURS", "EFF_ALL", "AVGWAGE",
                "HWAGE","HWAGE_3112","AVGWAGE_3112","ETP", "SEPARATIONS", "SEPARATIONS2", "HIRES", "HIRES2",
                "r004_SIRET","redi_r216_SIRET","redi_e001_SIRET","redi_e200_SIRET" ,
                "redi_r310_SIRET", "b330_SIRET", "CAPISOC_SIRET", "b319_SIRET")

b_table1 <- CreateTableOne(vars = var_balance,
               strata = "ever_treat", data = sample_pre)

write_csv(b_table1, "C:/Users/Public/Documents/Fontaine/Floods_Shock/2 - Data analysis/Tables/Balance/balance_baseline.csv")


# 2) At risk sample  (all Control (all years) and all not yeat treated)
sample_pre[,risk_strata := cut(flood_risk_index_RP100, breaks = quantile(flood_risk_index_RP100, probs= c(0, 1/3, 2/3, 1), na.rm = T),
                           labels = c("low_risk", "medium_risk", "high_risk"), include.lowest = T)]

for (stratum in unique(sample_pre$risk_strata)){
  cat("\n\n=== Risk stratum:", stratum, "===\n")
  subset <- sample_pre[risk_strata == stratum]
  print(t.test(EFF_MOY_ET ~ ever_flooded, data=sample_pre))
}

# 3) Matched sample
library(MatchIt)
install.packages("MatchIt")

## collapse dataset to SIRET level
sample_siret_pre <- sample_pre[,.(
  eff_all = mean(EFF_ALL, na.rm=T),
  etp = mean(ETP, na.rm=T),
  avg_wage = mean(AVGWAGE, na.rm=T),
  h_wage = mean(HWAGE, na.rm=T),
  r216 = mean(redi_r216_SIRET, na.rm=T),
  capisoc = mean(CAPISOC_SIRET, na.rm=T),
  b319 = mean(b319_SIRET, na.rm=T),
  flood_risk = mean(flood_risk_index_RP100, na.rm=T),
  treated = any(ever_treat)
), by = SIRET] 

# A) matching; gen weights
match_out <- matchit(treated ~ eff_all, etp, avg_wage, h_wage, r216, b319, capisoc, flood_risk,
                     data = sample_siret_pre, method = "nearest")

weights <- data.table(SIRET = rownames(match_out$weights),
                      weight = as.numeric(match_out$weights))
# B) IPW
ps_model <- glm(treated  ~ eff_all, etp, avg_wage, h_wage, r216, b319, capisoc, flood_risk,
                data = sample_siret_pre, family = binomial)

sample_siret_pre[, pscore := predict(ps_model, type = "response")]
sample_siret_pre[, weight := ifesle(treated == 1, 1, ps/(1-ps))] # ATT weights

sample <- merge(sample, sample_siret_pre[.,(SIRET, weight)], by = "SIRET", all.x = T)


# Graphs old-----
numeric_vars <- c("mono_etab", "EFF_ALL", "EFF_3112", "EFF_MOY_ET","HOURS","HWAGE","HWAGE_3112","AVGWAGE_3112","AVGWAGE","ETP",
                  "r004_SIRET","redi_r216_SIRET","redi_e001_SIRET","redi_e200_SIRET" ,
                  "redi_r310_SIRET", "b330_SIRET", "CAPISOC_SIRET",   "b319_SIRET")


# check distributions
sample[, lapply(.SD, function(x) list(
  min = min(x, na.rm = T),
  q1 = quantile(x, 0.25, na.rm = T),
  median = median(x, na.rm = T),
  mean = mean(x, na.rm = T),
  q3 = quantile(x, 0.75, na.rm = T),
  max = max(x, na.rm = T),
  na = sum(is.na(x))
)), .SDcols = numeric_vars]

# Boxplots
for (var in numeric_vars) {
  print(
    ggplot(sample, aes_string(y = var)) +
      geom_boxplot(outlier.shape = 16, outlier.size=2) +
      labs(title = paste("Boxplot of", var), y = var) +
      theme_minimal()
  )
}

# Density plots
for (var in numeric_vars) {
  print(
    ggplot(sample, aes_string(y = var)) +
      geom_histogram(bins = 50, fill = "steelblue") +
      labs(title = paste("Histogram of", var), y = var) +
      theme_minimal()
  )
}






