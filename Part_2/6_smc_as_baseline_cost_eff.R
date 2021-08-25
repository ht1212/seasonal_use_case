#-processing outputs into workable RDS-------------
library(data.table)
library(tidyverse)

#- this script makes the correct cost effectiveness impact estimates relative to 
#- to SMC alone rather than baseline alone 

#-source functions------------------------
source("output_functions.R")

#-variables to pass to output processing function----------------------------------------
baseline <-  data.frame(r = "baseline", cov = 0, dr=0) %>% crossing(pfpr, seasonality) %>%
  select(r, prev = pfpr, seas = seasonality, cov, dr)

x <- median_df %>% select(r = run,
                          prev = pfpr,
                          seas = seasonality,
                          cov  = coverage,
                          dr = draw) %>% bind_rows(baseline)

params <- as.list(x)

#-combine outputs and output single data.frame------------------------------------------
df_combined <- data.frame(rbindlist(purrr::pmap(params, combine_outputs)))

saveRDS(df_combined, "combined_outputs_smc_as_baseline.RDS")
#df_combined <- readRDS("combined_outputs_smc.RDS")

#-life expectancy data------------------------------------------------------------------
le <- read.csv("Q:/who_use_case/life_expectancy.csv")

#-define variables to be used in processing--------------------------------------------
population_size = 100000 #target outputs per 100,000 children 
tx = 0.52                #treatment coverage run in simulations 
scaler = 0.215           #severe case to death modifier
treatment_scaler = 0.5   #treatment modifier
treatment_coverage = 0.52#coverage 

#-process raw outputs - clean and weighting----------------------------------------------------------------------------
process_1 <- 
  df_combined %>% 
  dplyr::select(-contains("prev_2_10")) %>% 
  mutate_at(vars(contains("smooth")), as.numeric) %>% 
  #mutate_if(is.character, as.numeric)
  # keep smoothed output and ignore intervention numbers for now
  dplyr::select(run, pfpr, seasonality, draw, coverage, year, dplyr::contains("smooth")) %>% 
  #-convert from wide to long format
  tidyr::pivot_longer(-c(year, run, pfpr, seasonality, draw, coverage), names_to = "var", values_to = "y") %>%
  #-remove _smooth subscript for neater names
  dplyr::mutate(var = stringr::str_remove(var, "_smooth")) %>%
  #-isolate lower and upper age bounds from variable names
  tidyr::separate(var, into = c("type", "age_lower", "age_upper"), sep = "_", convert = TRUE) %>%
  # Convert back to wide
  tidyr::pivot_wider(id_cols = c(run, pfpr, seasonality, draw, coverage, year, age_lower, age_upper),
                     names_from = type, values_from = y) %>% 
  #-replace -999 / INF/ NA values in output
  dplyr::mutate(prev = ifelse(prev == -999, 0, prev),
                inc = ifelse(inc == -999, 0, inc),
                inc = ifelse(inc == Inf, 0, inc),
                sev = ifelse(sev == -999, 0, sev),
                sev = ifelse(sev == Inf, 0, sev),
                prop = ifelse(prop == -999, 0, prop)) %>%
  tidyr::replace_na(
    list( prev = 0, inc = 0, sev = 0, prop = 0)) %>% 
  #-mortality rate
  mutate(mort = (1-(treatment_scaler * 0)) * scaler * sev) %>% 
  #-convert burden (incidence * proportion aged * population size)
  #-case load is weighted by proprtion in age range in population * totalsize
  mutate(cases            = round(inc * prop * population_size), 
         severe           = round(sev * prop * population_size), 
         hospitalisations = round(severe * tx), 
         deaths           = round(mort * prop * population_size)) %>% 
  #-join with life expectancy
  left_join(le, by=c("age_lower", "age_upper")) %>% 
  #-calculate DALYS
  mutate(dalys      = deaths * life_expectancy + cases * 0.01375342 * 0.211 + severe * 0.04794521 * 0.6,
         disc_dalys = deaths * life_expectancy_discounted + cases * 0.01375342 * 0.211 + severe * 0.04794521 * 0.6) %>% 
  select(-life_expectancy, -life_expectancy_discounted) 


#-baseline runs to calculate cases averted etc from------------------------------------
baseline_runs <- 
  #-keep baseline runs separate----
process_1 %>% 
  #-remove unwanted variables------
select(-prev, -inc, -sev, -mort, -prop, -coverage) %>%
  filter(run == "smc") %>% 
  #-rename baseline variables to then join onto df-----
rename(cases_bl            = cases, 
       severe_bl           = severe, 
       hospitalisations_bl = hospitalisations,
       deaths_bl           = deaths, 
       dalys_bl            = dalys, 
       disc_dalys_bl       = disc_dalys) %>% 
  select(-run)


#-intervention runs averted outputs----------------------------------------------------
vaccine_runs <- 
  process_1 %>% 
  filter(run != "baseline") %>% 
  filter(run != "smc") %>% 
  #-join up with baseline runs
  left_join(baseline_runs, by=c("pfpr", "seasonality", "draw", "year", "age_lower", "age_upper")) %>% 
  #-calculate averted realtive to baseline
  mutate(cases_averted            = cases_bl - cases, 
         severe_averted           = severe_bl - severe,
         hospitalisations_averted = hospitalisations_bl - hospitalisations,
         deaths_averted           = deaths_bl - deaths,
         dalys_averted            = dalys_bl - dalys,
         disc_dalys_averted       = disc_dalys_bl - disc_dalys) %>% 
  #-filter down to the years vaccines implemented 
  filter(year > 0)

#-aggregate over age groupings interested in----------------------------------------
#-per age group summed over 15 years------------------------------------------------
age_groups <- 
  vaccine_runs %>% 
  group_by(run, pfpr, seasonality, coverage, draw, age_lower, age_upper) %>%
  summarise(across(cases:disc_dalys_averted, sum)) %>%
  ungroup()

#-full aggregate all ages---------------------------------------------------------
#-over 15 years-------------------------------------------------------------------
age_all <- 
  vaccine_runs %>% 
  group_by(run, pfpr, seasonality, coverage, draw) %>%
  summarise(across(cases:disc_dalys_averted, sum)) %>%
  ungroup()

#-process intervention outputs----------------------------------------------------
intervention_per_year <- 
  df_combined %>% 
  filter(run != "baseline") %>%  
  group_by(run, pfpr, seasonality, draw, coverage) %>% 
  dplyr::mutate(
    num_vacc_doses = c(0, diff(num_vacc_doses / 3)),
    num_smc        = c(0, diff(num_smc/ 3)), 
    num_trt        = c(0, diff(num_trt /3))) %>% 
  select(run, pfpr, seasonality, draw, coverage, year, num_vacc_doses, num_smc, num_trt) 

#-countrer factual treatment----------------------------------------------------
baseline_treatment <- 
  intervention_per_year %>% 
  ungroup() %>% 
  filter(run == "smc") %>% 
  select(-coverage, -num_vacc_doses,  -run) %>%
  rename(num_trt_bl = num_trt, 
         num_smc_bl = num_smc) 

#-vaccine intervention outputs years-------------------------------------------
# scaler_4dose <- 0.210526316
# scaler_5dose <- 0.173913043
# 
# vaccine_interventions <- 
# intervention_per_year %>% 
#   filter(run != "baseline") %>% 
#   left_join(baseline_treatment, by=c("pfpr", "seasonality", "draw", "year")) %>% 
#   filter(year >0) %>% 
#   #scaler vaules to turn number of doses to number fully vaccinated people-----
#   mutate(scaler = case_when(grepl("5_dose",run) ~  scaler_5dose, 
#                             TRUE ~ scaler_4dose)) %>% 
#   group_by(run, pfpr, seasonality, coverage, draw) %>% 
#   mutate(number_fvp = case_when(grepl("5_dose",run) ~ lag(num_vacc_doses, 2), 
#                              TRUE ~ lag(num_vacc_doses))) %>% 
#   mutate(number_fvp = number_fvp * scaler) %>% 
#   tidyr::replace_na(list(number_fvp = 0) )

dose_scaler <- 0.266666667

vaccine_interventions <-
  intervention_per_year %>%
  filter(run != "baseline") %>% 
  filter(run != "smc") %>% 
  left_join(baseline_treatment, by=c("pfpr", "seasonality", "draw", "year")) %>%
  filter(year >0) %>%
  mutate(number_fvp = num_vacc_doses * dose_scaler)   

#-aggregate over all years----------------------------------------------------
interventions_all <- 
  vaccine_interventions %>% 
  group_by(run, pfpr, seasonality, draw, coverage) %>%
  summarise(across(num_vacc_doses:number_fvp, sum)) %>%
  ungroup()

#-averted per fully vaccinated persons over all ages-------------------------
impact_all <- 
  age_all %>% 
  left_join(interventions_all, by=c("run", "pfpr", "seasonality", "coverage", "draw")) %>% 
  mutate(
    #averted per fully vacc pop 
    cases_averted_per_100000_fvp            = 100000 * (cases_averted / number_fvp),
    severe_averted_per_100000_fvp           = 100000 * (severe_averted / number_fvp),
    hospitalisations_averted_per_100000_fvp = 100000 * (hospitalisations_averted / number_fvp),
    deaths_averted_per_100000_fvp           = 100000 * (deaths_averted / number_fvp),
    #averted per dose
    cases_averted_per_dose             = (cases_averted / num_vacc_doses),
    severe_averted_per_dose            = (severe_averted / num_vacc_doses),
    hospitalisations_averted_per_dose  = (hospitalisations_averted / num_vacc_doses),
    deaths_averted_per_dose            = (deaths_averted / num_vacc_doses)
  ) 

#-averted per fully vaccinated persons age disaggregated-------------------------
impact_age <- 
  age_groups %>% 
  left_join(interventions_all, by=c("run", "pfpr", "seasonality", "coverage", "draw")) %>% 
  mutate(
    #averted per fully vacc pop 
    cases_averted_per_100000_fvp            = 100000 * (cases_averted / number_fvp),
    severe_averted_per_100000_fvp           = 100000 * (severe_averted / number_fvp),
    hospitalisations_averted_per_100000_fvp = 100000 * (hospitalisations_averted / number_fvp),
    deaths_averted_per_100000_fvp           = 100000 * (deaths_averted / number_fvp),
    #averted per dose
    cases_averted_per_dose             = (cases_averted / num_vacc_doses),
    severe_averted_per_dose            = (severe_averted / num_vacc_doses),
    hospitalisations_averted_per_dose  = (hospitalisations_averted / num_vacc_doses),
    deaths_averted_per_dose            = (deaths_averted / num_vacc_doses)
  ) 


#-costing data-----------------------------------------------------------------
cost_per_dose    <- c(2.69,6.52,12.91) #cost per vaccine - think this should include the cost of wastage etc $2,$5,$10 per dose 
delivery_cost    <- c(0.96,1.62,2.67)  #EPI delivery costs 
tx_unit_cost     <- 1.47 
severe_unit_cost <- 22.41 
smc_cost         <-  1.07

cost_df <- expand_grid(cost_per_dose = cost_per_dose, delivery_cost = delivery_cost)

#-age aggregated-----------------------------------------------------------
impact_costs_all <- 
  impact_all %>% 
  merge(cost_df, by = NULL) %>% 
  ungroup() %>% 
  # setting specific costs
  mutate(smc_cost_bl    = num_smc_bl * smc_cost) %>% 
  mutate(vaccine_cost   = num_vacc_doses * (cost_per_dose + delivery_cost),
         smc_cost       = num_smc * smc_cost, 
         #smc_cost_bl    = num_smc_bl * smc_cost, 
         tx_cost        = (num_trt) * tx_unit_cost,
         tx_cost_bl     = (num_trt_bl) * tx_unit_cost,
         severe_cost    = severe * severe_unit_cost,
         severe_cost_bl = severe_bl * severe_unit_cost,
         cost           = vaccine_cost + tx_cost + severe_cost + smc_cost,
         cost_bl        = tx_cost_bl + severe_cost_bl + smc_cost_bl,
         marginal_cost  = cost - cost_bl) %>% 
  # incremental costs 
  mutate(
    icer_case      = marginal_cost / cases_averted,
    icer_daly      = marginal_cost / dalys_averted,
    icer_disc_daly = marginal_cost / disc_dalys_averted
  )

#-age disaggregated-----------------------------------------------------
impact_costs_age_groups <- 
  impact_age %>% 
  merge(cost_df, by = NULL) %>% 
  # setting specific costs
  mutate(smc_cost_bl    = num_smc_bl * smc_cost) %>% 
  mutate(vaccine_cost   = num_vacc_doses * (cost_per_dose + delivery_cost),
         smc_cost       = num_smc * smc_cost, 
         # smc_cost_bl    = num_smc_bl * smc_cost, 
         tx_cost        = (num_trt) * tx_unit_cost,
         tx_cost_bl     = (num_trt_bl) * tx_unit_cost,
         severe_cost    = severe * severe_unit_cost,
         severe_cost_bl = severe_bl * severe_unit_cost,
         cost           = vaccine_cost + tx_cost + severe_cost + smc_cost,
         cost_bl        = tx_cost_bl + severe_cost_bl + smc_cost_bl,
         marginal_cost  = cost - cost_bl) %>% 
  # incremental costs 
  mutate(
    icer_case      = marginal_cost / cases_averted,
    icer_daly      = marginal_cost / dalys_averted,
    icer_disc_daly = marginal_cost / disc_dalys_averted)

#-processing break time save and check-------------------------------------------------------------
saveRDS(impact_costs_all, "impact_costs_all_ages_smc_as_baseline.RDS")
saveRDS(impact_costs_age_groups, "impact_costs_age_groups_smc_as_baseline.RDS")

