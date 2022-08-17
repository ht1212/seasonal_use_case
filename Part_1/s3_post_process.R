#-Post Processing outputs----------------------------------------------------------------
library(data.table)
library(tidyverse)

#-source functions------------------------
source("output_functions.R")

#-variables to pass to output processing function----------------------------------------
x <- median_df %>% select(r = run,
                          prev = pfpr,
                          seas = seasonality,
                          cov  = coverage,
                          dr = draw) #%>% 
#filter(dr %in% c(0, draw_reduction))

params <- as.list(x)

#-combine outputs and output single data.frame------------------------------------------
df_combined <- data.frame(rbindlist(purrr::pmap(params, combine_outputs)))
df_combined <- readRDS("combined_outputs_raw.RDS")

#-define variables to be used in processing--------------------------------------------
population_size = 100000 #target outputs per 100,000 population size 
tx = 0.52                #treatment coverage run in simulations 
scaler = 0.215           #severe case to death modifier
treatment_scaler = 0.5   #treatment modifier
treatment_coverage = 0.52#coverage 

#-process raw outputs - clean and weighting----------------------------------------------------------------------------
process_1 <- 
  df_combined %>% 
  dplyr::select(-contains("prev_2_10")) %>% 
  mutate_at(vars(contains("smooth")), as.numeric) %>% 
  # keep smoothed output and ignore intervention numbers for now
  dplyr::select(run, pfpr, seasonality, draw, coverage, year, dplyr::contains("smooth")) %>% 
  #-convert from wide to long format
  tidyr::pivot_longer(-c(year, run, pfpr, seasonality, draw, coverage), names_to = "var", values_to = "y") %>%
  #-remove _smooth subscript for neater names
  dplyr::mutate(var = stringr::str_remove(var, "_smooth")) %>%
  #-isolate lower and upper age bounds from variable names
  tidyr::separate(var, into = c("type", "age_lower", "age_upper"), sep = "_", convert = TRUE) %>%
  #-convert back to wide
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
  #-case load is weighted by proportion in age range in population * total size
  mutate(cases            = round(inc * prop * population_size), 
         severe           = round(sev * prop * population_size), 
         hospitalisations = round(severe * tx), 
         deaths           = round(mort * prop * population_size)) 


#-baseline runs to calculate cases averted etc from------------------------------------
baseline_runs <- 
  #-keep baseline runs separate----
process_1 %>% 
  #-remove unwanted variables------
select(-prev, -inc, -sev, -mort, -prop, -coverage) %>%
  filter(run == "baseline") %>% 
  #-rename baseline variables to then join onto df-----
rename(cases_bl            = cases, 
       severe_bl           = severe, 
       hospitalisations_bl = hospitalisations,
       deaths_bl           = deaths) %>%
  select(-run)

#-intervention runs averted outputs----------------------------------------------------
vaccine_runs <- 
  process_1 %>% 
  filter(run != "baseline") %>% 
  #-join up with baseline runs
  left_join(baseline_runs, by=c("pfpr", "seasonality", "draw", "year", "age_lower", "age_upper")) %>% 
  #-calculate averted realtive to baseline
  mutate(cases_averted            = cases_bl - cases, 
         severe_averted           = severe_bl - severe,
         hospitalisations_averted = hospitalisations_bl - hospitalisations,
         deaths_averted           = deaths_bl - deaths) %>% 
  #-filter down to the years vaccines implemented 
  filter(year > 0)

#-aggregate over age groupings interested in----------------------------------------
#-per age group summed over 15 years------------------------------------------------
age_groups <- 
  vaccine_runs %>% 
  group_by(run, pfpr, seasonality, coverage, draw, age_lower, age_upper) %>%
  summarise(across(cases:deaths_averted, sum)) %>%
  ungroup()

#-full aggregate all ages---------------------------------------------------------
#-over 15 years-------------------------------------------------------------------
age_all <- 
  vaccine_runs %>% 
  group_by(run, pfpr, seasonality, coverage, draw) %>%
  summarise(across(cases:deaths_averted, sum)) %>%
  ungroup()

#-save outputs--------------------------------------------------------------------
saveRDS(age_all, "impact_all_ages.RDS")
saveRDS(age_groups, "impact_age_groups.RDS")
