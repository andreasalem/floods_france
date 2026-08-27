gc()
library(pacman)
p_load(tidyverse, data.table, dplyr, arrow, haven,
       fixest, did, fect, panelView, PanelMatch, ggplot2, bacondecomp, didimputation, doParallel, fect,
       ggthemes, rio, geomtextpath, gghighlight, collapse, modelsummary)

# Import sample ------
all_vars = c("SIRET","ANNEE", "ZEMPT","INSEE_COM", "flood_dummy", "flood_type", "cat_nat_code", "duration_category", "flood_risk_index_RP100",
             "total_floods", "floods_last_5y", "floods_last_3y", "floods_last_2y", "floods_last_year_dummy", "years_since_last_flood",
             "mono_etab", "EFF_ALL", "EFF_3112", "EFF_MOY_ET","HOURS","HWAGE","HWAGE_3112",
             "AVGWAGE_3112","AVGWAGE","ETP",
             "r004_SIRET","redi_r216_SIRET","redi_e001_SIRET","redi_e200_SIRET" ,
             "redi_r310_SIRET", "b330_SIRET", "CAPISOC_SIRET", "b319_SIRET",
             "ape_diff", "CDI", "CDD", "interim", "othercontract") 


subsample <- read_parquet("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/subsamples/sample_2010_2019.parquet",
                         col_select = all_vars) %>% setDT()

subsample[, ANNEE := as.numeric(as.character(ANNEE))]



# gen: year of first flood (if never flooded: NA)
subsample[, first_treat := if (all(flood_dummy == 0)) NA_integer_ else min(ANNEE[flood_dummy==1]), by = SIRET]
subsample[, first_treat := as.numeric(first_treat)]

# gen: treat (0 if never treated or not yet treated, 1 if ANNEE >= first_treat)
subsample[, treat := ifelse(is.na(first_treat), 0, ifelse(ANNEE >= first_treat, 1, 0)), by = SIRET]

# gen: ever treated unit dummy
subsample[, ever_treat := ifelse(is.na(first_treat), 0, 1)]

# gen: event time. I.e.: time to treatment
subsample[, time_to_treat := ifelse(ever_treat == 1, ANNEE - first_treat, NA)]

# gen: Cohort
subsample[, Cohort := ifelse(!is.na(first_treat), paste0("Cohort:", first_treat), "Control")]

# gen: risk strata
subsample[,risk_strata := cut(flood_risk_index_RP100, breaks = quantile(flood_risk_index_RP100, probs= seq(0, 1, by = 0.25), na.rm = T),
                           labels = F, include.lowest = T)]


# drop: always treated SIRETs
subsample <- subsample %>% 
  group_by(SIRET) %>% 
  mutate(treatment_mean = mean(flood_dummy, na.rm = TRUE))

subsample <- subsample %>% filter(treatment_mean<1)


# Stacked data creation  ------


df.st <- NULL
target.cohorts <- setdiff(unique(subsample$Cohort), "Control") %>% sort()
k <- 1


for(cohort in target.cohorts){
  
  df.sub <- subsample[which(subsample$Cohort%in%c(cohort,"Control") &
                            abs(subsample$time_to_treat) <= 7),]
  df.sub$stack <- k
  k <- k+1
  df.st <-rbind(df.st, df.sub)
  
  cat(sprintf("Processing cohort %s, matched %d obs.\n", cohort, nrow(df.sub)))
  
  year <- gsub("\\D", "", cohort)
  write_parquet(df.sub, paste0("C:/Users/Public/Documents/Fontaine/Floods_Shock/2 - Data analysis/stacked_data/df_sub_cohort_",year,".parquet"))

  rm(df.sub)
  gc()
  
}


df.st$st_unit <- as.numeric(factor(paste0(df.st$stack, '-',df.st$SIRET)))
df.st$st_year <- as.numeric(factor(paste0(df.st$stack, '-',df.st$ANNEE)))

## some stacks have no control, we remove them and recrate the FE identifiers
df.st <- setDT(df.st) 
df.st[, .N, by = .(treat, stack)] # each stack should have both 1 and 0 !

# check that FE make sense
df.st[, var1 := var(treat), by = st_unit]
df.st[, var2 := var(treat), by = st_year] # there is no variation! For each st_year, treat is not varying. We will remove them


## ATT -----
model.st <- feols(ETP ~ treat | st_unit ,
                  data = df.st, cluster = "st_unit")

print(model.st)

## Event study ------
df.st$treat1 <- as.numeric(df.st$treatment_mean>0)
df.st[which(is.na(df.st$time_to_treat)), 'time_to_treat'] <- 9999

st.est <- feols(ETP~
                  i(time_to_treat, treat1, ref = -1) | st_unit ,
                data = df.st, cluster = "st_unit")

summary(st.est)

st.output <- as.data.frame(st.est$coeftable)
st.output$Time <- c(c(-7:-2), c(0:7)) + 1

#p.st <- esplot(st.output, Period = "Time", Estimate = "Estimate", SE = "Std. Error", xlim = c(-10, 10)) 

event_times <- c(-7:-2, 0:7)
keep_vars <- paste0("time_to_treat::",event_times, ":treat1")

coefplot(st.est,
         keep = keep_vars,
         xlab="Time to Treatment") 


