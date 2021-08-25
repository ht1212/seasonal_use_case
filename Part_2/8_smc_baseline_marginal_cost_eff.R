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
       deaths_bl           = deaths, 
       dalys_bl            = dalys, 
       disc_dalys_bl       = disc_dalys) %>% 
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
  filter(run == "baseline") %>% 
  select(-coverage, -num_vacc_doses, -num_smc, -run) %>%
  rename(num_trt_bl = num_trt) 

#-vaccine intervention outputs years-------------------------------------------
dose_scaler <- 0.266666667

vaccine_interventions <-
  intervention_per_year %>%
  filter(run != "baseline") %>%
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
  # setting specific costs
  mutate(vaccine_cost   = num_vacc_doses * (cost_per_dose + delivery_cost),
         smc_cost       = num_smc * smc_cost, 
         tx_cost        = (num_trt) * tx_unit_cost,
         tx_cost_bl     = (num_trt_bl) * tx_unit_cost,
         severe_cost    = severe * severe_unit_cost,
         severe_cost_bl = severe_bl * severe_unit_cost,
         cost           = vaccine_cost + tx_cost + severe_cost + smc_cost,
         cost_bl        = tx_cost_bl + severe_cost_bl,
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
  mutate(vaccine_cost   = num_vacc_doses * (cost_per_dose + delivery_cost),
         smc_cost       = num_smc * smc_cost, 
         tx_cost        = (num_trt) * tx_unit_cost,
         tx_cost_bl     = (num_trt_bl) * tx_unit_cost,
         severe_cost    = severe * severe_unit_cost,
         severe_cost_bl = severe_bl * severe_unit_cost,
         cost           = vaccine_cost + tx_cost + severe_cost + smc_cost,
         cost_bl        = tx_cost_bl + severe_cost_bl,
         marginal_cost  = cost - cost_bl) %>% 
  # incremental costs 
  mutate(
    icer_case      = marginal_cost / cases_averted,
    icer_daly      = marginal_cost / dalys_averted,
    icer_disc_daly = marginal_cost / disc_dalys_averted)

smc <- impact_costs_all %>% filter(run == "smc") %>% 
  select(pfpr, seasonality, marginal_cost.smc = marginal_cost, 
         cases_averted.smc = cases_averted, 
         dalys_averted.smc = dalys_averted, 
         disc_dalys_averted.smc = disc_dalys_averted)

vaccine <- impact_costs_all %>% filter(run != "smc") %>% 
  left_join(smc, by=c("pfpr", "seasonality")) %>% 
  mutate(icer_case.smc = (marginal_cost - marginal_cost.smc)/(cases_averted - cases_averted.smc), 
         icer_daly.smc = (marginal_cost - marginal_cost.smc)/(dalys_averted - dalys_averted.smc))

# for the table pfpr 10 - 50 %
# average SV cost per case and DALY averted 
lol <- 
  vaccine %>% 
  filter(run %in% c("srtss_4_dose_original_smc", "srtss_4_dose_update_smc", "srtss_4_dose_param_mod_smc",
                    "srtss_5_dose_original_smc", "srtss_5_dose_update_smc", "srtss_5_dose_param_mod_smc")) %>% 
  filter(pfpr >= 0.1, pfpr <0.5) %>% 
  filter( coverage == 0.8) %>% 
  group_by(cost_per_dose, delivery_cost) %>% 
  summarise(case = mean(icer_case.smc), 
            daly = mean(icer_daly.smc))

# average EPI cost per case and DALY averted 
pol <-
  vaccine %>% 
  filter(run == "epi_smc") %>% 
  filter(pfpr >= 0.1, pfpr <0.5) %>% 
  filter( coverage == 0.8) %>% 
  group_by(cost_per_dose, delivery_cost) %>% 
  summarise(case = mean(icer_case.smc), 
            daly = mean(icer_daly.smc))
