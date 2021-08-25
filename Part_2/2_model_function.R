
#-Run the model function------------------------------------------------------------------------------------------ 
run_sites <- function(run, pfpr, vectors, seasonality, seasonality_coefs, draw, 
                      epi_pev, mass_pev, dose_5, coverage, srtss_start, output_vars, 
                      smc_offset, smc_file){
  
  #-turning the antibody model on and setting boost mu parameter as seasonality we're modelling differently-----
  if(run %in% c("srtss_4_dose_update_smc", "srtss_5_dose_update_smc", "srtss_4_dose_param_mod_smc", "srtss_5_dose_param_mod_smc")){
    rtss <- c("ab_model 1 pev_ab_boost_mu 6.37008")
     } else {
    rtss <- c("ab_model 1 pev_ab_boost_mu 5.56277")
  }
  
  if(run %in% c("srtss_4_dose_param_mod_smc", "srtss_5_dose_param_mod_smc")){
    rtss_params <- c(" pev_Vmax 0.911028 pev_alpha 0.75303 pev_beta 62.8525")
  }else{
    rtss_params <- ""
  }
  
  rtss <- paste0(rtss, rtss_params)
  
  #-EPI RTS,S implementation parameters------------------------------------- 
  epi_rtss <- paste("epi_pev", epi_pev, "epi_age_1 6 epi_age_2 7.5 epi_age_3 9 epi_age_4 27 pev_epi_start 5 pev_boost_coverage 0.8 pev_epi_coverage", coverage)
  
  #-sRTS,S implementation parameters----------------------------------------
  s_rtss <- paste("mass_pev", mass_pev, "mass_pev_start", srtss_start,"mass_pev_coverage", coverage, "mass_pev_age0 0.4166667 mass_pev_age1 1.416667 mass_pev_frequency 1 mass_pev_boost1 1 mass_pev_boost2", dose_5, "mass_pev_boost1_interval 10 mass_pev_boost2_interval 22 mass_pev_boost_coverage 0.8")
  
  #-smc implementation parameters-------------------------------------------
  smc     <- paste("smc_start 5 smc_coverage 0.75 smc_age0 0.25 smc_age1 5")
  smc_add <- paste("num_rounds 4 smc_offset", smc_offset)
  
  #-remaining model options and pasting together----------------------------
  options = paste("recalculate 2 num_people 100000 final_run 20 num_runs 3 itn 0 irs 0 smc 1 output_type 0 drug_cov_0_0 0.26470592 drug_cov_1_0 0.2647059 drug_cov_2_0 0.0000000 drug_cov_3_0 0.0000000",
                  #to match to the baseline prev we're setting
                  rtss,
                  s_rtss, 
                  epi_rtss,
                  smc,
                  "add",
                  smc_add,
                  mlgts::bionomics_string(vectors) )
  
  # create a site file based on the seasonality co-efficients  
  site   <- mlgts::site_create(vectors, seasonality_coefs, total_M=1) 
  
  # set target prevalence 
  site[site$par == "prev", 2] <- pfpr
  site[site$par == "prev_years", 2] <- 2
  
  #-depending on what combined scenario we're looking at will need to look at different model file location 
  mf_loc <- paste0("Q:/who_use_case/validation/model_files/",smc_file,"/model_files")
  
  # Run the model
  name <- paste0(run, "_", seasonality, "_", pfpr, "_", coverage, "_", draw)
  exe  <- paste0("Q:/who_use_case/validation/executable/helpVacc.exe")
  
  model_out <- mlgts::launch(name,
                             site = site, 
                             demog = mlgts::flat_demog, 
                             options = options, 
                             draw = draw,  
                             exe = exe, 
                             mf_loc = mf_loc, 
                             output_vars=output_vars)
  
  # Basic output formatting and adding in run specific variables
  output             <- model_out$output
  output$run         <- run
  output$draw        <- draw
  output$t           <- output$year + 2000
  output$coverage    <- coverage
  output$pfpr        <- pfpr 
  output$seasonality <- seasonality
  
  # isolating annual summaries 
  output <- 
    output %>% 
    ## We -1 here as we are using smoothed outputs (the average over the previous 12 months)
    ## Therefore the smoothed output for time 2010 is essentially the average over 2009 
    dplyr::mutate(year = round(year + (-4 - 1), 1)) %>%   
    # Select the integer years in specified range
    dplyr::filter(year %in% -4:2050)
  
  # Save output
  output_address <- paste0("output/", name, ".csv")
  write.csv(output, output_address, row.names = FALSE)
  
  return(output) 
  
}

