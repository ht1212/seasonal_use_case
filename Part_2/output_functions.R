
#-function to combined all individual outputs to single df for post-processing----------------------------------------

combine_outputs <- function(r, prev, seas, cov, dr){
  run         <- r       #baseline or smc
  pfpr        <- prev    #baseline pfpr levels
  seasonality <- seas    #seasonality
  coverage    <- cov     #coverage value
  draw        <- dr      #model parameter draw
  
 
  #-upload output in and select relevent variables---------------------------------------------------------------------- 
  raw <- read_csv(paste0(here::here(), "/output/", run,"_", seasonality, "_", pfpr, "_", coverage, "_", draw,".csv")) 
  out <- raw
  
  return(out)
  
}


