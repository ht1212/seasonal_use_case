
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
pfpr <- c(0.1, 0.15, 0.2, 0.25, 0.35, 0.45, 0.55, 0.65)

#-seasonality - Highly seasonal reprentative of Fatik in Senegal and Seasonal Upper East in Ghana
seasonality <- c("highly_seasonal","seasonal")#, "Perennial")

#-model parameter draw to run uncertainty over---------------------------------------------------
draw <- c(0:50)

#-intervention modelled--------------------------------------------------------------------------
run <- c("epi_smc", 
         "srtss_4_dose_original_smc", "srtss_4_dose_update_smc", "srtss_4_dose_param_mod_smc",
         "srtss_5_dose_original_smc", "srtss_5_dose_update_smc", "srtss_5_dose_param_mod_smc")

#-coverage values--------------------------------------------------------------------------------
coverage <- c(0.8)#, 0.5, 0.9)

#-create initial dataframe-----------------------------------------------------------------------
intervention_df <- crossing(run, pfpr, seasonality, draw, coverage) %>% mutate(smc = 1)
smc             <- data.frame(run = "smc", coverage = 0) %>% crossing(pfpr, seasonality, draw) %>% mutate(smc = 1)
baseline        <- data.frame(run = "baseline", coverage = 0) %>% crossing(pfpr, seasonality, draw) %>% mutate(smc = 0)

#-add additional run specific parameters---------------------------------------------------------
param_df <-
  bind_rows(intervention_df, smc) %>% 
  bind_rows(baseline) %>% 
  mutate(epi_pev = case_when(run == "epi_smc" ~ 1, TRUE ~ 0), 
         mass_pev = case_when(grepl("srtss", run) ~ 1, TRUE ~ 0 ), 
         dose_5   = case_when(grepl("srtss_5_dose",run) ~ 1, TRUE~0)) %>% 
  mutate(srtss_start = case_when(seasonality == "seasonal" ~ 5+4/12, 
                                 seasonality == "highly_seasonal" ~ 5+6/12, 
                                 TRUE~0), 
         smc_offset = case_when(seasonality == "seasonal" ~ 0, 
                                seasonality == "highly_seasonal" ~ 1/12, TRUE~0), 
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
final_df <- left_join(param_df, df1, by=c("seasonality"))

#-Median parameter draw data frame------------------------
median_df <- final_df %>% filter(draw == 0) 

