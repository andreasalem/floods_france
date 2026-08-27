library(pacman)
p_load(tidyverse, data.table, dplyr, arrow, haven,
       fixest, did, fect, panelView, PanelMatch, ggplot2, bacondecomp, didimputation, doParallel, fect, HonestDiD, 
       DIDmultiplegtDYN)

p_load(ggthemes, rio, geomtextpath, gghighlight, collapse, modelsummary)

setwd("C:/Users/Public/Documents/Fontaine/Floods_Shock")
options(scipen = 999)

# Import sample ------
all_vars = c("SIRET","ANNEE", "ZEMPT","INSEE_COM", "flood_dummy", "flood_type", "cat_nat_code", "duration_category", "flood_risk_index_RP100",
             "total_floods", "floods_last_5y", "floods_last_3y", "floods_last_2y", "floods_last_year_dummy", "years_since_last_flood",
             "mono_etab", "EFF_ALL", "EFF_3112", "EFF_MOY_ET","HOURS","HWAGE","HWAGE_3112",
             "AVGWAGE_3112","AVGWAGE","ETP",
             "r004_SIRET","redi_r216_SIRET","redi_e001_SIRET","redi_e200_SIRET" ,
             "redi_r310_SIRET", "b330_SIRET", "CAPISOC_SIRET", "b319_SIRET",
             "ape_diff", "CDI", "CDD", "interim", "othercontract") 


subsample <- read_parquet("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/final_sample.parquet",
                          col_select = all_vars) %>% setDT()

subsample[, ANNEE := as.numeric(as.character(ANNEE))]



##  did2s::event_study -------
mult_est <- event_study(sample,
            yname = "AVGWAGE",
            idname = "SIRET", # must be numeric 
            tname = "ANNEE",
            gname = "first_treat",
            estimator = c("TWFE", "did2s", "did", "sunab"))

plot_event_study(mult_est, horizon = c(-10,10))


# Specification: Treatment switching on-off ------ 

y <- "redi_r310_SIRET" # chiffre d'affaire total
d <- "flood_dummy"
unit <- "SIRET"
time <- "ANNEE"
index <- c("SIRET", "ANNEE")

panelview(Y=y, D=d, index = index, data = subsample,
          xlab = "Time Period", ylab = "SIRET", gridOff = T, 
          by.timing = TRUE, cex.legend = 5, cex.axis = 5,
          cex.main = 10, cex.lab = 5)

# TWFE
model.twfe <- feols(redi_r310_SIRET ~ flood_dummy |SIRET + ANNEE,
                    data=subsample, cluster = "SIRET")
summary(model.twfe)

# dynamic TWFE
subsample <- as.data.frame(subsample)

data.cohort <- get.cohort(subsample, index = index, D=d)
data_cohort$treat <- 0
data_cohort[which(data_cohort$Cohort !='Control'), 'treat'] <- 1
data_cohort[which(is.na(data_cohort$Time_to_Treatment)), 'treat'] <- 0

remove <- intersect(which(is.na(data_cohort$Time_to_Treatment)),
                    which(data_cohort[,d]==1))
if(length(remove)>0){data_cohort <- data_cohort[-remove,]}

data_cohort[which(is.na(data_cohort$Time_to_Treatment)), "Time_to_Treatment"] <- 999

twfe.est <- feols(redi_r310_SIRET ~
                    i(Time_to_Treatment, treat, ref = -1) | SIRET + ANNEE,
                  data = data_cohort, cluster = "SIRET")

twfe.output <- as.data.frame(twfe.est$coeftable[c(1:25),])
twfe.output$Time <- c(c(-16:-2),c(0:9)) + 1

p.twfe <- esplot(twfe.output, Period = 'Time', Estimate = 'Estimate',
                 SE = 'Std. Error', xlim = c(-10,10))


# Variable Creation -------------

## gen: year of first flood (if never flooded: NA)
sample[, first_treat := if (all(flood_dummy == 0)) NA else min(ANNEE[flood_dummy==1]), by = SIRET]

# gen: treat (FALSE if never treated or not yet treated, TRUE if ANNEE >= first_treat)
sample[, treat := ifelse(is.na(first_treat), F, ANNEE >= first_treat)]

# gen: ever treated unit dummy
sample[, ever_treat := ifelse(is.na(first_treat), 0, 1)]

## gen: event time (and omit reference period). I.e.: time to treatment
sample[, time_to_treat := ifelse(ever_treat == 1, ANNEE - first_treat, 0)]


# exclude extreme lags
sample <- sample %>% 
  filter(event_time >= -10, event_time <= 10) %>% 
  mutate(event_time_factor = relevel(event_time_factor, ref = "-1"))

## gen: post flood indicator
sample <- sample %>% 
  mutate(post = ifelse(!is.na(first_treat) & ANNEE >= first_treat,1,0))



# Regression Analysis -------------

## Baseline TWFE estimate -----------
twfe_model <- feols(SEPARATIONS ~ post | SIRET + ANNEE, data = sample)
summary(twfe_model)

## Event-study TWFE estimate -----------
event_model <- feols(
  SEPARATIONS ~ i(event_time_factor, ref = "-1") | SIRET + ANNEE,
  #cluster = ~INSEE,
  data = sample
)
summary(event_model)

# plot
pdf("2 - Data analysis/Figures/TWFE Separations")
iplot(event_model,
      xlab = "Years since first flood",
      ylab = "Coefficients",
      main = "TWFE Event study: Separations",
      ref.line = 0)
dev.off()

## Staggered TWFE estimate -----------


## Callaway and St anna (2021) -----------
# Group-Time ATE
CS_ATT <- att_gt(yname = "redi_r310_SIRET",
                      tname = "ANNEE",
                      idname = "SIRET",
                      gname = "first_treat",
                      data = small_sample,
                      est_method = "dr", # or "ipw
                      control_group = "nevertreated", # or; "nevertreated"
                      allow_unbalanced_panel = T)

# plot
summary(CS_ATT)
agg_effects <- aggte(CS_ATT, type = "dynamic", na.rm = T) # if replace with "group" I see the ATT by year; or but simple
summary(agg_effects)
ggdid(agg_effects)
ggsave("2 - Data analysis/Figures/C&S Separations dynamic.png")

## Sun and Abraham -----------
sample <- sample %>% 
  mutate(event_time2 = ifelse(event_time <= -10, -10,
                              ifelse(event_time >= 10,10, event_time))) 


event_model2 <- feols(
  SEPARATIONS ~ i(event_time2, ever_treat, ref = "-1") | SIRET + ANNEE,
  cluster = ~SIRET,
  data = filter(sample, !is.na(first_treat))
)

# plot
etable(event_model2, file = "2 - Data analysis/Tables/S&A Separations.tex")
pdf("2 - Data analysis/Figures/S&A Separations")
iplot(event_model2,
      xlab = "Years since first flood",
      ylab = "Coefficients",
      main = "Sun & Abraham (2021) Event study: SEPARATIONS",
      ref.line = 0)
dev.off()

colnames(small_sample)
## Stacked DiD -----------

window <- 10

# treated units are such that !is.na(first_treat) = T
stacked_did <- small_sample %>% 
  filter(!is.na(first_treat)) %>% 
  mutate(rel_year = ANNEE - first_treat) %>% 
  filter(rel_year >= -window & rel_year <= window)

stacked_did <- stacked_did %>% 
  mutate(treat_post = ifelse(rel_year >= 0, 1, 0))

stacked_model <- feols(SEPARATIONS ~ treat_post | SIRET + ANNEE, data = stacked_did)
summary(stacked_model)


## Stacked DiD 2 -----------
sample[,risk_strata := cut(flood_risk_index_RP100, breaks = quantile(flood_risk_index_RP100, probs= c(0, 1/3, 2/3, 1), na.rm = T),
                           labels = c("low_risk", "medium_risk", "high_risk"), include.lowest = T)]
sample[, first_treat := if (all(flood_dummy == 0)) NA else min(ANNEE[flood_dummy==1]), by = SIRET]

#
treated_units <- sample[!is.na(first_treat),
                        .(SIRET, first_treat, ZEMPT, risk_strata)] %>% unique()

control_units <- sample[is.na(first_treat),
                        .(SIRET, ZEMPT, INSEE_COM, risk_strata)] %>% unique()
#
matched_control_list <- list()


for(i in 1:nrow(treated_units)) {
  treat_id <- treated_units$SIRET[i]
  treat_zempt <- treated_units$ZEMPT[i]
  treat_risk <- treated_units$risk_strata[i]
  event_year <- treated_units$first_treat[i]
  
  possible_controls <- control_units[ZEMPT == treat_zempt &
                                       risk_strata == treat_risk, SIRET]
  
  pre_years <- (event_year-10):(event_year-1)
  
  control_history <- sample[SIRET %in% possible_controls & ANNEE %in% pre_years,
                            .(flood_history = sum(flood_dummy)), by = SIRET]
  
  matched_control_list[[as.character(treat_id)]] <- data.table(
    treated_siret = treat_id,
    control_siret = possible_controls,
    event_year = event_year
  )
}

# stack them
stacked_panel <- rbindlist(list(), use.names = T)

for(match in matched_control_list){
  event_id <- match$treated_siret[1]
  event_year <- match$event_year[1]
  
  treated_obs <- sample[SIRET == event_id &
                          ANNEE %between% c(event_year-10,event_year+10)]
  treated_obs[, `:=`(event_id = event_id, treated = 1, rel_year = ANNEE - event_year)]
  
  control_obs <- sample[SIRET %in% match$control_siret &
                          ANNEE %between% c(event_year-10,event_year+10)]
  control_obs[, `:=`(event_id = event_id, treated = 0, rel_year = ANNEE - event_year)]
  
  event_stack <- rbindlist(list(treated_obs, control_obse), use.names = TRUE, fill = TRUE)
  stacked_panel <- rbindlist(list(stacked_panel, event_stack), use.names = TRUE, fill = TRUE)
  
  rm(treated_obs, control_obs, event_stack)
  gc()
  
}

# add controls of subsequent floods
# run by severity of floods
# run by sector
# run by PCS



# estimate
model <- feols(AVGWAGE ~ i(rel_year, treated, ref = -1) | SIRET + ANNEE,
               data = stacked_panel, cluster = ~event_id)

iplot(model, xlab = "Years since flood", ylab = "Effect on Outcome", ref.line = 0)


## Erda (2025) -----------
# TWFE with sample restrictions!
feols(SEPARATIONS ~ flood_dummy + flood_exposure_index  | SIRET + ANNEE, data = sample %>%  filter(max_floods_any_6yr < 4)) %>% 
  summary()

lm(SEPARATIONS~flood_dummy + factor(ANNEE), data = sample %>%  filter(max_floods_any_6yr < 4)) %>% 
  summary()

## Fatica et al. (2024) -----------
horizons <- 0:10

### Dynamic IRF ----
models_fatica <- map(horizons,function(h) {
  data_h <- subsample %>% 
    group_by(SIRET) %>% 
    arrange(ANNEE) %>% 
    mutate(y_lead = lead(AVGWAGE, n = h),
           y_lag = lag(AVGWAGE, n = 1),
           delta_y = y_lead - y_lag) %>% 
    ungroup()
  
  feols(delta_y ~ flood_dummy | INSEE_COM + ANNEE, data = data_h)
})

# plot
irf_fatica <- map2_dfr(models_fatica, horizons, ~tidy(.x) %>% 
                         filter(term == "flood_dummy") %>% 
                         mutate(horizon = .y))

ggplot(irf_fatica, aes(x = horizon, y = estimate)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = estimate - 1.96*std.error,
                  ymax = estimate + 1.96*std.error), alpha = 0.2) +
  labs(title = "IRF of Flood dummy on AVGWAGE",
       x = "Years after flood",
       y = "Effect on AVGWAGE") +
  theme_minimal()

### LP-DiD with clean control ----


# filter to only treated or never treated and repeat as above
models_fatica2 <- map(horizons,function(h) {
  data_h <- sample %>% 
    filter(ever_treat == T | flood_dummy == 0) %>% # added this line
    group_by(SIRET) %>% 
    arrange(ANNEE) %>% 
    mutate(y_lead = lead(SEPARATIONS, n = h),
           y_lag = lag(SEPARATIONS, n = 1),
           delta_y = y_lead - y_lag) %>% 
    ungroup()
  
  feols(delta_y ~ flood_dummy + flood_exposure_index | INSEE_COM, data = data_h)
})

# plot
irf_fatica2 <- map2_dfr(models_fatica2, horizons, ~tidy(.x) %>% 
                         filter(term == "flood_dummy") %>% 
                         mutate(horizon = .y))

ggplot(irf_fatica2, aes(x = horizon, y = estimate)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = estimate - 1.96*std.error,
                  ymax = estimate + 1.96*std.error), alpha = 0.2) +
  labs(title = "IRF of Flood dummy on SEPARATIONS",
       x = "Years after flood",
       y = "Effect on SEPARATIONS") +
  theme_minimal()
ggsave("2 - Data analysis/Figures/Fatica et al - Separations.png")


### Augmented IPW ----

# specify propensity score variables
prop_model <- glm(flood_dummy ~ flood_exposure_index,
                  data = sample %>% filter(ANNEE == 2005),
                  family = binomial())

# add to dataset
sample <- sample %>% 
  mutate(p_score = predict(prop_model,newdata = sample, type = "response"))

# estimate
aipw_result <- aipw_did()


## DID imputation (Borusyack et al) -------------
data("df_het", package = "didimputation")
setDT(df_het)

did_imputation(sample, yname = "c(SEPARATIONS, HIRES)", gname = "first_treat",
               tname = "ANNEE", idname = "SIRET", first_stage = ~0 | SIRET + ANNEE)

# SPECIFy DEP VAR
sample_avg <- sample[,
                     .(dep_var = mean(AVGWAGE)),
                     by = .(first_treat, ANNEE)]

gs <- sample[treat == T, unique(first_treat)]

ggplot() +
  geom_line(data = sample_avg, mapping = aes(y = dep_var, x = ANNEE, color = first_treat), size = 1.5) +
  geom_vline(xintercept = gs - 0.5, linetype = "dashed") +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom") +
  labs(y = "Outcome", x = "Year", color = "Treat cohort") +
  scale_y_continuous(expand = expansion(add = .5))

# static ATT
static <- did_imputation(data = sample, yname = "AVGWAGE", gname = "first_treat", tname = "ANNEE",
                         idname = "SIRET")
static

# event study ATTs
es <- did_imputation(data = sample,
                     yname = "AVGWAGE", gname = "first_treat", tname = "ANNEE", idname = "SIRET",
                     horizon = T, pretrends = -5:-1)
es





# Heterogeneity analysis -----------
# sectors
APEs <- unique(sample$ape_diff) %>% as.data.frame() 
APEs <- desc(APEs$.)


# Create risk strata old ------
# 3 ways: decile stratification, coarsened exact matching on risk, propensity score stratification.

# 1
panel <- panel %>% 
  mutate(risk_decile = ntile(flood_exposure_index,10))

# 2
library(cem)
cem_result <- cem(
  treatment = "treated",
  data = "panel",
  drop = c("ANNEE", "first_treat_year", "INSEE_COM"),
  cutpoints = list(flood_exposure_index = c(0, 1, 3, 7, 15, 100)) # need to refine
)

# 3
library(stats)
risk_model <- glm(flood_dummy ~ flood_exposure_index + Population.municipale.2021 + Nombre.d.établissements.employeurs.actifs.au.31.12.2022 + Unités.légales..en.nombre..2021
                  + Densité.de.population..historique.depuis.1876..2021 + Unités.légales.dans.activités.immobilières..en.nombre..2021,
                  data = panel,
                  family = binomial())

panel <- panel %>% 
  mutate(propensity = predict(risk_model, type = "response"),
         risk_strata = ntile(propensity, 5))  # need to refine, i need to build using qll flood data and insee panel

## i use method 1 for now: now need to identify deciles that contain both T and C
valid_deciles <- panel %>% 
  group_by(risk_decile) %>% 
  summarise(n_treated = sum(flood_dummy == 1),
            n_control = sum(flood_dummy == 0)) %>% 
  filter(n_treated > 0 & n_control > 0) %>% 
  pull(risk_decile)

DADS_FARE <- DADS_FARE %>% 
  left_join(panel, by = c("INSEE_COM", "ANNEE"))

## finally run DiD
library(fixest)

feols(profits_log ~ post | siret + ANNEE, data = DADS_FARE)

iplot(
  feols(avgwage ~ sunab(first_treat_year, ANNEE) | siret + ANNEE, data = DADS_FARE))

colnames(panel)

DADS_FARE %>% 
  select(siret, siren, ANNEE, depcom,
         VA, hwage_log, etp_log, avgwage, etp, hires, separations, cdi, cdd, profits_log,
         flood_exposure_index, INSEE_COM, treated, post, event_time)










# for each zone d'emploi, check how many TRI communes/total communes (commmnad is slow, why?) -----------
zemploi <- dataset %>%  
  group_by(Zones.d.emploi.2020) %>% 
  summarise(TRI_communes = sum(TRI == 1, na.rm = T),
            total_communes = n()) %>% 
  as_tibble()

zemploi <- zemploi %>% 
  mutate(perc = round(TRI_communes*100/total_communes,1))


# export table
export_zemploi <- zemploi %>% 
  select(c(1,2,3,5)) %>% 
  arrange(desc(perc)) %>% 
  filter(TRI_communes > 0)

sum(export_zemploi$TRI_communes)
sum(export_zemploi$total_communes)

print(
  xtable(export_zemploi, align = c("l", "l", "r", "r", "r")),
  include.rownames = FALSE,
  floating = FALSE,  # Important to avoid floating tables
  tabular.environment = "longtable"
)


########### NOTES

scale_color_manual(values = c("2000" = "#d2382c", "2001" = "#497eb3", "2002" = "#8e549f", "2003" = "#f29e4C",
                              "2004" = "#5b6f59", "2005" = "#db6ba6", "2006" = "#9c914f", "2007" = "#3b9c9c",
                              "2008" = "#e5b07b", "2009" = "#9d4edd", "2010" = "#ef476f", "2011" = "#118ab2",
                              "2012" = "#06d6a0", "2013" = "#ffd166", "2014" = "#8338ec", "2015" = "#2a9d8f",
                              "2016" = "#ff6f61", "2017" = "#6a994e", "2018" = "#b5838d", "2019" = "#264653","2020" = "#f29e4C"))

colnames(sample)
[1] "SIREN"                                               "ANNEE"                                              
[3] "SIRET"                                               "INSEE_COM"                                          
[5] "ZEMPT"                                               "EFF_ALL"                                            
[7] "EFF_3112"                                            "EFF_MOY_ET"                                         
[9] "HOURS"                                               "HWAGE"                                              
[11] "HWAGE_3112"                                          "AVGWAGE_3112"                                       
[13] "AVGWAGE"                                             "ETP"                                                
[15] "HIRES"                                               "HIRES2"                                             
[17] "SEPARATIONS"                                         "SEPARATIONS2"                                       
[19] "CDI"                                                 "CDD"                                                
[21] "interim"                                             "othercontract"                                      
[23] "SHORTJOB_1M"                                         "SHORTJOB_3M"                                        
[25] "SHORTJOB_6M"                                         "SHORTJOB_1TO3M"                                     
[27] "SHORTJOB_3TO6M"                                      "SHORTJOB_6TO12M"                                    
[29] "SHORTJOB_MORE6M"                                     "SHORTJOB_MORE1Y"                                    
[31] "ape_diff"                                            "depcom"                                             
[33] "r004"                                                "redi_r216"                                          
[35] "redi_e001"                                           "redi_e200"                                          
[37] "redi_r310"                                           "b330"                                               
[39] "CAPISOC"                                             "b319"                                               
[41] "emp_share"                                           "r004_SIRET"                                         
[43] "redi_r216_SIRET"                                     "redi_e001_SIRET"                                    
[45] "redi_e200_SIRET"                                     "redi_r310_SIRET"                                    
[47] "b330_SIRET"                                          "CAPISOC_SIRET"                                      
[49] "b319_SIRET"                                          "mono_etab"                                          
[51] "flood_dummy"                                         "flood_type"                                         
[53] "cat_nat_code"                                        "duration_category"                                  
[55] "total_floods"                                        "floods_last_5y"                                     
[57] "floods_last_3y"                                      "floods_last_2y"                                     
[59] "floods_last_year_dummy"                              "years_since_last_flood"                             
[61] "Nb.d.emplois.au.lieu.de.travail..LT..2021"           "Densité.de.population..historique.depuis.1876..2021"
[63] "Unités.légales..en.nombre..2021"                     "Créations.d.entreprises..en.nombre..2022"           
[65] "percent_flooded_low"                                 "percent_flooded_moderate"                           
[67] "percent_flooded_high"                                "percent_flooded_very_high"                          
[69] "flood_risk_index_RP100"                              "first_treat"  




# exported to STATA
setnames(sample,
         old = c("Nb.d.emplois.au.lieu.de.travail..LT..2021",
                 "Densité.de.population..historique.depuis.1876..2021",
                 "Créations.d.entreprises..en.nombre..2022",
                 "Unités.légales..en.nombre..2021"),
         new = c("Nb_d_emplois_au_lieu_de_travail_","Densit__de_population__historiqu",
                 "Unit_legales__en_nombre__2021","Cr_ations_d_entreprises__en_nomb"))


write_dta(sample, "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/FICUS_FARE_DADS_GASPAR_2000_to_2020.dta")


# random subset of SIRETs, to test packages --------------
uniqueN(sample$SIRET) #3'522'201

set.seed(6939)
small_sample_siret <- sample(unique(sample$SIRET), 50000)
small_sample <- sample[sample$SIRET %in% small_sample_siret,]
setDT(small_sample)
rm(sample, small_sample_siret)


# Variable Creation (small sample) -------------
small_sample[, first_treat := if (all(flood_dummy == 0)) 0 else min(ANNEE[flood_dummy==1]), by = SIRET] #  year of first flood
small_sample[, treat := ifelse(is.na(first_treat), F, ANNEE >= first_treat)] # treat
small_sample[, ever_treat := !is.na(first_treat)] # ever treated

small_sample[, SIRET := as.numeric(SIRET)]

## 



# OLD -----------
library(tidyverse)
library(data.table)
library(haven)
library(dplyr)
library(arrow)
library(fixest)
library(did)
library(did2s)
library(fect)
library(panelView)
library(PanelMatch)
library(ggplot2)
library(bacondecomp)
library(didimputation)
library(doParallel)



setwd("C:/Users/Public/Documents/Fontaine/Floods_Shock")
options(scipen = 999)

# import data 
sample <- read_parquet("C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/final_sample.parquet")

# gen: year of first flood (if never flooded: NA)
sample[, first_treat := if (all(flood_dummy == 0)) 0 else min(ANNEE[flood_dummy==1]), by = SIRET]

y_vars <- c("y", "ETP", "HWAGE", "AVGWAGE", "HOURS",
            "r004_SIRET", "redi_r216_SIRET", "CAPISOC_SIRET", "b319_SIRET", "b330_SIRET")

for (y in y_vars) {
  
  mult_est <- event_study(sample,
                          yname = y,
                          idname = "SIRET", # must be numeric 
                          tname = "ANNEE",
                          gname = "first_treat",
                          estimator = c("all"))
  
  p <- plot_event_study(mult_est, horizon = c(-10,10))
  
  ggsave(filename = paste0("C:/Users/Public/Documents/Fontaine/Floods_Shock/2 - Data analysis/Figures/Event_study/",y,".png"), 
         plot = p, width = 8, height = 6)
  
  rm(p, mult_est)
  gc()
}





