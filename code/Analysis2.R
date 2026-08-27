

library(did)
library(tidyverse)
library(fixest)
library(fastDummies)
library(haven)
library(data.table)
library(arrow)

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

# Var creation  ------
setDT(subsample)
subsample[, SIRET := as.numeric(SIRET)] # for CS, need id as numeric

## gen: year of first flood (if never flooded: NA)
subsample[, first_treat := if (all(flood_dummy == 0)) NA else min(ANNEE[flood_dummy==1]), by = SIRET]
subsample[, first_treat_CS := if (all(flood_dummy == 0)) 0 else min(ANNEE[flood_dummy==1]), by = SIRET] # for CS, need 0 instead of NA for never treated units

# gen: treat (FALSE if never treated or not yet treated, TRUE if ANNEE >= first_treat)
subsample[, treat := ifelse(is.na(first_treat), F, ANNEE >= first_treat)]

# gen: ever treated unit dummy
subsample[, ever_treat := ifelse(is.na(first_treat), 0, 1)]

## gen: event time (and omit reference period). I.e.: time to treatment
subsample[, time_to_treat := ifelse(ever_treat == 1, ANNEE - first_treat, NA)]
subsample[, time_to_treat_TWFE := ifelse(ever_treat == 1, ANNEE - first_treat, 0)] # for TWFE, need to be 0

# gen: risk strata (will do tomorrow)
#subsample[,risk_strata := cut(flood_risk_index_RP100, breaks = quantile(flood_risk_index_RP100, probs= c(0, 1/3, 2/3, 1), na.rm = T),
#                           labels = c("low_risk", "medium_risk", "high_risk"), include.lowest = T)]

#colnames(base_stagg)
#data("base_stagg")
# id = SIRET
# year              = ANNEE
# year_treated      = first_treat
# time_to_treatment = time_to_treat
# treated           = ever_treat


#head(subsample[, c("SIRET", "ANNEE", "flood_dummy", "first_treat", "time_to_treat", "ever_treat")], 100)




# Stacked Dataset------
cohorts <- subsample %>% 
  filter(!is.na(first_treat)) %>% 
  distinct(first_treat) %>% 
  pull()

getdata <- function(j, window) {
  subsample %>% 
    filter(
      first_treat==j |
      first_treat > j + window
    ) %>% 
    filter(
      ANNEE >= j - window &
      ANNEE <= j + window 
    ) %>% 
    mutate(df = j)
}

stacked_data <- map_df(cohorts, ~ getdata(., window = 5)) %>% 
  mutate(
    rel_year = if_else(df == first_treat, time_to_treat, NA_real_)
  ) %>% 
  fastDummies::dummy_cols("rel_year", ignore_na = TRUE) %>% 
  mutate(across(starts_with("rel_year_"), ~ replace_na(.,0)))



# Estimation: -----
y_vars <- c("EFF_MOY_ET", "ETP", "HWAGE", "AVGWAGE", "AVGWAGE_3112", "HOURS",
            "r004_SIRET", "redi_r216_SIRET", "CAPISOC_SIRET", "b319_SIRET", "b330_SIRET", "redi_r310_SIRET")


for (y in y_vars) {

  
## Stacked -------
cat("\nEstimating Stacked DiD. Variable: ", y, "...\n")
  
stacked_result <- feols(
  xpd(..lhs ~ `rel_year_-5` + `rel_year_-4` + `rel_year_-3` + `rel_year_-2` + 
    `rel_year_0` + `rel_year_1` + `rel_year_2` + `rel_year_3` + `rel_year_4` + `rel_year_5` |
    SIRET ^ df + ANNEE ^ df, ..lhs = y),
  data = stacked_data
)


stacked_coeffs <- stacked_result$coefficients
stacked_se <- stacked_result$se

stacked_coeffs <- c(stacked_coeffs[1:4],0,stacked_coeffs[5:10])
stacked_se <- c(stacked_se[1:4],0,stacked_se[5:10])

rm(stacked_result)
gc()

#CS (2021) ------
cat("\nEstimating Callaway and Sant'Anna (2021). Variable: ", y, "...\n")

cs_out <- att_gt(
  yname = y,
  data = subsample,
  gname = "first_treat_CS",
  idname = "SIRET",
  tname = "ANNEE",
  control_group = "nevertreated"
)

cs <- 
  aggte(
    cs_out,
    type = "dynamic",
    min_e = -5,
    max_e = 5,
    bstrap = F,
    cband = F
  )

rm(cs_out)
gc()
#SA (2020) ------
#cat("\nEstimating Sun and Abraham (2020). Variable: ", y, "...\n")

#res_sa20 = feols(xpd(..lhs ~ sunab(first_treat, ANNEE) | SIRET + ANNEE, ..lhs = y), subsample)
 
#sa = tidy(res_sa20)[5:14,] %>% pull(estimate)
#sa = c(sa[1:4], 0, sa[5:10])

#sa_se = tidy(res_sa20)[6:15,] %>% pull(std.error)
#sa_se = c(sa[1:4], 0, sa[5:10])

#rm(res_sa20)
#gc()

# TFWE ------
cat("\nEstimating TWFE. Variable: ", y, "...\n")

tfwe_model = feols(xpd(..lhs ~ i(time_to_treat_TWFE, ever_treat, ref = -1) | SIRET + ANNEE, ..lhs = y),
             cluster = ~SIRET,
             data = subsample)

tfwe_result <- tidy(tfwe_model, conf.int = TRUE) %>% 
  filter(str_detect(term, "time_to_treat_TWFE::")) %>% 
  mutate(
         period = as.numeric(str_extract(term, "-?[0-9]+"))
         ) %>% 
  arrange(period)

twfe_coeffs <- tfwe_result$estimate
twfe_se <- tfwe_result$std.error

twfe_coeffs <- c(twfe_coeffs[1:4],0,twfe_coeffs[5:10])
twfe_se <- c(twfe_se[1:4],0,twfe_se[5:10])

rm(tfwe_model, tfwe_result)
gc()

# Table with all results
compare_df_est = data.frame(
  period = -5:5,
  CS = cs$att.egt,
  #SA = sa,
  Stacked = stacked_coeffs,
  TWFE = twfe_coeffs
)

compare_df_se = data.frame(
  period = -5:5,
  CS = cs$se.egt,
  #SA = sa_se,
  Stacked = stacked_se,
  TWFE = twfe_se
)

rm(cs)
gc()

#
cat("\nSaving plot and table. Variable: ", y, "...\n")

compare_df_longer <- compare_df_est %>% 
  pivot_longer(!period, names_to = "estimator", values_to = "est") %>% 
  full_join(compare_df_se %>% 
              pivot_longer(!period, names_to = "estimator", values_to = "se")) %>% 
  mutate(upper = est + 1.96*se,
         lower = est - 1.96*se)



p <- ggplot(compare_df_longer, aes(x = period, y = est, color = estimator, shape = estimator)) +
  geom_point(size = 2.2, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.4, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = 0, color = "gray40") +
  scale_x_continuous(breaks = seq(min(compare_df_longer$period),max(compare_df_longer$period),1)) +
  labs(
    title = paste0(y, ":"),
    x = "Event Time (Periods since Flood)",
    y = "ATT Estimate and 95% Conf. Int. ",
    color = "Estimator",
    shape = "Estimator"
  ) +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        axis.title =  element_text(face = "bold", size = 12),
        axis.text =  element_text(size = 10))

ggsave(filename = paste0("C:/Users/Public/Documents/Fontaine/Floods_Shock/2 - Data analysis/Figures/Event_study/",y,".png"), 
       plot = p, width = 8, height = 6)

write_csv(compare_df_longer, paste0("C:/Users/Public/Documents/Fontaine/Floods_Shock/2 - Data analysis/Tables/Event_study/",y,".png"))

rm(p, compare_df_longer)
gc()

}






