#some functions from Pete's work to make this easier and aligned----------------------------------------------------

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
