
#-Script 1-------------------------------------------------------------------------------------- 

#-Load packages to use--------------------------------------------------------------------------
library(mlgts)
library(didehpc)
library(tidyverse)
library(furrr)
library(drat)

#-some functions from Pete's work to make this easier and aligned-------------------------------
output_vars <- function(lower_age = 0:99, upper_age = 1:100){
 #prev vars
   output_prev <- data.frame(type = "prev", lower = lower_age, upper = upper_age) %>%
    dplyr::mutate(name = paste("prev", lower, upper, sep = "_"))
 #clin inc vars 
  output_inc <- data.frame(type = "clin_inc", lower = lower_age, upper = upper_age) %>%
    dplyr::mutate(name = paste("inc", lower, upper, sep = "_"))
  #sev inc vars
  output_sev <- data.frame(type = "sev_inc", lower =lower_age, upper = upper_age) %>%
    dplyr::mutate(name = paste("sev", lower, upper, sep = "_"))
  #proportion population ages vars
  output_prop <- data.frame(type = "prop", lower = lower_age, upper = upper_age) %>%
    dplyr::mutate(name = paste("prop", lower, upper, sep = "_")) 
  #extra vars needed for sense checking etc
  output_cali <- data.frame(type = "prev", lower = 2, upper = 10) %>%
    dplyr::mutate(name = paste("prev", lower, upper, sep = "_"))
  #combine together 
  dplyr::bind_rows(output_cali, output_prev, output_inc, output_sev, output_prop)
}

#-create data-frame to pass to model------------------------------------------------------------ 

#-baseline PfPR2–10 levels----------------------------------------------------------------------
pfpr <- 0.35# #c(0.1, 0.15, 0.2, 0.25, 0.35, 0.45, 0.55, 0.65)

#-seasonality - Highly seasonal reprentative of Fatik in Senegal and Seasonal Upper East in Ghana
seasonality <- c("highly_seasonal","seasonal")#, "Perennial")

#-model parameter draw to run uncertainty over---------------------------------------------------
draw <- 0#c(0:50)

#-intervention modelled--------------------------------------------------------------------------
run1 <- c(#"epi_smc",                   #age based plus smc
         "srtss_4_dose_original_smc",  # 4 dose seasonal with no changes to rtss params plus smc
         "srtss_4_dose_update_smc",    # 4 dose seasonal with increased booster ab titres plus smc
         "srtss_4_dose_param_mod_smc", # 4 dose seasonal with increased booster ab titres plus smc and synergy profiles
         "srtss_5_dose_original_smc", 
         "srtss_5_dose_update_smc",
         "srtss_5_dose_param_mod_smc")

run2 <- c(#"epi",                  # age based vaccination
         "srtss_4_dose_original", # 4 dose seasonal with no changes to rtss params
         "srtss_5_dose_original", # 5 dose seasonal with no changes to rtss params
         "srtss_4_dose_update",   # 4 dose seasonal with increased booster ab titres 
         "srtss_5_dose_update")   # 5 dose seasonal with increased booster ab titres

#-coverage values--------------------------------------------------------------------------------
coverage <- c(0.8)#, 0.5, 0.9)

timing_smc <- c("-2 months", "-1 month", "Optimal", "+1 month", "+2 months")
timing_sv  <- c("-2 months", "-1 month", "Optimal", "+1 month", "+2 months")

#-create initial dataframe-----------------------------------------------------------------------
vaccine_combined <- 
  crossing(run1, pfpr, seasonality, draw, coverage) %>% 
  mutate(smc = 1) %>%
  crossing(timing_smc, timing_sv) %>% 
  rename(run = run1)

vaccine_alone <-
  crossing(run2, pfpr, seasonality, draw, coverage) %>% 
  mutate(smc = 0, timing_smc = NA) %>% 
  crossing(timing_sv)%>% 
  rename(run = run2)

smc <- 
  data.frame(run = "smc", coverage = 0) %>% 
  crossing(pfpr, seasonality, draw) %>% 
  mutate(smc = 1) %>% 
  crossing(timing_smc) %>%
  mutate(timing_sv = NA)

baseline <- 
  data.frame(run = "baseline", coverage = 0) %>% 
  crossing(pfpr, seasonality, draw) %>% 
  mutate(smc = 0, 
         timing_smc = NA, 
         timing_sv = NA)

epi <- 
  data.frame(run = "epi", coverage = 0.8) %>% 
  crossing(pfpr, seasonality, draw) %>%
  mutate(smc = 0, 
         timing_smc = NA, 
         timing_sv = NA)

epi_smc <-
  data.frame(run = "epi_smc", coverage = 0.8) %>% 
  crossing(pfpr, seasonality, draw) %>% 
  mutate(smc = 1) %>% 
  crossing(timing_smc) %>% 
  mutate(timing_sv = NA)

#-add additional run specific parameters---------------------------------------------------------
param_df1 <- 
  bind_rows(baseline, vaccine_alone, vaccine_combined, smc, epi, epi_smc)%>% 
  mutate(epi_pev  = case_when(run == "epi_smc" ~ 1, TRUE ~ 0), 
         mass_pev = case_when(grepl("srtss", run) ~ 1, TRUE ~ 0 ), 
         dose_5   = case_when(grepl("srtss_5_dose",run) ~ 1, TRUE~0)) %>% 
  mutate(srtss_start = case_when(seasonality == "seasonal" & timing_sv == "Optimal"   ~ 5+5/12, 
                                 seasonality == "seasonal" & timing_sv == "+1 month"  ~ 5+6/12, 
                                 seasonality == "seasonal" & timing_sv == "+2 months" ~ 5+7/12, 
                                 seasonality == "seasonal" & timing_sv == "-1 month"  ~ 5+4/12, 
                                 seasonality == "seasonal" & timing_sv == "-2 months" ~ 5+3/12, 
                                 seasonality == "highly_seasonal"& timing_sv == "Optimal"   ~ 5+7/12, 
                                 seasonality == "highly_seasonal"& timing_sv == "+1 month"  ~ 5+8/12, 
                                 seasonality == "highly_seasonal"& timing_sv == "+2 months" ~ 5+9/12, 
                                 seasonality == "highly_seasonal"& timing_sv == "-1 month"  ~ 5+6/12, 
                                 seasonality == "highly_seasonal"& timing_sv == "-2 months" ~ 5+5/12, 
                                 TRUE~0), 
         smc_offset = case_when(seasonality == "seasonal" & timing_smc == "Optimal"   ~ 0, 
                                seasonality == "seasonal" & timing_smc == "+1 month"  ~ 1/12,
                                seasonality == "seasonal" & timing_smc == "+2 months" ~ 2/12,
                                seasonality == "seasonal" & timing_smc == "-1 month"  ~ -1/12,
                                seasonality == "seasonal" & timing_smc == "-2 months" ~ -2/12,
                                seasonality == "highly_seasonal" & timing_smc == "Optimal" ~ 1/12,
                                seasonality == "highly_seasonal" & timing_smc == "+1 month" ~ 2/12,
                                seasonality == "highly_seasonal" & timing_smc == "+2 months" ~ 3/12,
                                seasonality == "highly_seasonal" & timing_smc == "-1 month" ~ 0,
                                seasonality == "highly_seasonal" & timing_smc == "-2 months" ~ -1/12,
                                TRUE~0), 
         smc_file = case_when(grepl("param_mod", run) ~ "22", 
                              TRUE ~ "0")) %>% 
  ungroup()


#-seasonality and vector composition parameters that relate to these generalised settings-------
df1 <- tibble(
  seasonality = seasonality,
  seasonality_coefs = list(
    tibble(seasonal_a0 = 0.284596, seasonal_a1 = -0.317878,
           seasonal_b1 = -0.331361, seasonal_a2 = -0.0017527, 
           seasonal_b2 = 0.293128, seasonal_a3 = 0.116455, 
           seasonal_b3 = -0.0617547),
    tibble(seasonal_a0 = 0.285505, seasonal_a1 = -0.325352, 
           seasonal_b1 = -0.132815, seasonal_a2 = -0.0109352, 
           seasonal_b2 = 0.104675, seasonal_a3 = 0.0779865, 
           seasonal_b3 = -0.013919)),
    #tibble(seasonal_a0 = 1, seasonal_a1 = 0, seasonal_b1 = 0, seasonal_a2 = 0, seasonal_b2 = 0, seasonal_a3 = 0, seasonal_b3 = 0)),
  vectors = list(
    tibble(  prop_gamb_ss = 0.5,
             prop_fun = 0.25,
             prop_arab = 0.25,
             irs_dif_gamb_ss = 0.813,
             irs_dif_fun = 0.813,
             irs_dif_arab = 0.422,
             gamb_ss_Q0 = 0.92,
             gamb_ss_Q_in = 0.97,
             gamb_ss_Q_bed = 0.89,
             fun_Q0 = 0.94,
             fun_Q_in = 0.98,
             fun_Q_bed = 0.9,
             arab_Q0 = 0.71,
             arab_Q_in = 0.96,
             arab_Q_bed = 0.9)),
  output_vars = list(
    tibble(output_vars())
  )
  

)

#-full data frame all elements----------------------------
final_df <- left_join(param_df1, df1, by=c("seasonality"))

#-Median parameter draw data frame------------------------
median_df <- final_df %>% filter(draw == 0) 


# #-run locally to test------------------------------------
# plan(multiprocess, workers = 6)
# system.time({out <- future_pmap(median_df[5,], run_sites, .progress = TRUE)})
# 

