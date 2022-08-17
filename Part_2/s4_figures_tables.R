#-age disaggregated plots------------------------------------------
#-Figure S8--------------------------------------------------------
# Load packages
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)
library(ghibli)
library(scales)
library(LaCroixColoR)

#-load outputs-----------------------------
impact <- readRDS("Part_2/impact_all_ages.RDS")
impact_age <- readRDS("Part_2/impact_age_groups.RDS")

# New facet label names for supp variable
seas.labs <- c("Highly Seasonal", "Seasonal")
names(seas.labs) <- c("highly_seasonal", "seasonal")

# set the colour levels 
color_vals <- c("smc" = "#f0c9e9",
                "epi_smc" = "#FF3200",
                "srtss_4_dose_original_smc" = "#E9A17C", 
                "srtss_4_dose_update_smc" = "#E9E4A6", 
                "srtss_4_dose_param_mod_smc" = "#eab06d",
                "srtss_5_dose_original_smc" = "#1bb6af", 
                "srtss_5_dose_update_smc" = "#0076bb",
                "srtss_5_dose_param_mod_smc" = "#172869")

labels <- c("epi_smc" = "AB-RTS,S model 1 + SMC", 
            "srtss_4_dose_original_smc" = "SV 4-dose model 1 + SMC",
            "srtss_5_dose_original_smc" = "SV 5-dose model 1 + SMC",
            "srtss_4_dose_update_smc" = "SV 4-dose model 2 + SMC",
            "srtss_5_dose_update_smc" = "SV 5-dose model 2 + SMC", 
            "srtss_4_dose_param_mod_smc" = "SV 4-dose + SMC model 3", 
            "srtss_5_dose_param_mod_smc" = "SV 5-dose + SMC model 3", 
            "smc" = "SMC alone")

age_pd <- 
  impact_age %>%
  filter(age_upper <= 20) %>% 
  filter(run %in% c("smc",
                    "epi_smc", 
                    "srtss_4_dose_original_smc", "srtss_4_dose_update_smc", "srtss_4_dose_param_mod_smc",
                    "srtss_5_dose_original_smc", "srtss_5_dose_update_smc", "srtss_5_dose_param_mod_smc")) %>% 
  filter(coverage %in% c(0,0.8), draw==0)

# New facet label names pfpr
pfpr.labs <- c("10%",  "25%", "35%", "55%")
names(pfpr.labs) <- c("0.1", "0.25","0.35","0.55")

age_pd$run <- factor(age_pd$run, levels= c("smc", 
                                           "epi_smc" , 
                                           "srtss_4_dose_original_smc",
                                           "srtss_4_dose_update_smc",
                                           "srtss_4_dose_param_mod_smc",
                                           "srtss_5_dose_original_smc",
                                           "srtss_5_dose_update_smc" ,
                                           "srtss_5_dose_param_mod_smc"))

ca_ageA <- 
  ggplot(age_pd %>% filter(seasonality == "highly_seasonal") %>% filter(pfpr %in% c(0.1, 0.25,0.35,0.55)), 
         aes(x = age_upper, y = cases_averted, fill = run)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #geom_linerange(col = "grey70") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = color_vals, 
                    labels = labels) +
  ylab("") +
  xlab("Age") +
  labs(fill = "Vaccination schedule") +
  facet_grid(pfpr ~ run, scales = "free_y",  labeller = labeller(pfpr = pfpr.labs)) +
  ggtitle("Highly Seasonal - clinical cases averted (per 100,000 population)")+
  theme_bw()+
  theme(strip.text.x = element_blank())

ca_ageA 

ca_ageB <- 
  ggplot(age_pd %>% filter(seasonality == "seasonal") %>% filter(pfpr %in% c(0.1, 0.25,0.35,0.55)), 
         aes(x = age_upper, y = cases_averted, fill = run)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #geom_linerange(col = "grey70") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = color_vals, 
                    labels = labels) + 
  ylab(" ") +
  xlab("Age") +
  labs(fill = "Vaccination schedule") +
  facet_grid(pfpr~run, scales = "free_y",  labeller = labeller(pfpr = pfpr.labs)) +
  ggtitle("Seasonal - clinical cases averted (per 100,000 population)")+
  theme_bw()+
  theme(strip.text.x = element_blank())

ca_ageB 

#-Deaths averted-----------------------------------------------------------------------------------------------
da_ageA <- 
  ggplot(age_pd %>% filter(seasonality == "highly_seasonal") %>% filter(pfpr %in% c(0.1, 0.25,0.35,0.55)), 
         aes(x = age_upper, y = deaths_averted, fill = run)) +
  geom_bar(position=position_dodge(), stat="identity", colour="black") +
  #geom_linerange(col = "grey70") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = color_vals, 
                    labels = labels) + 
  ylab(" ") +
  xlab("Age") +
  labs(fill = "Vaccination schedule") +
  facet_grid(pfpr~run, scales = "free_y",  labeller = labeller(pfpr = pfpr.labs)) +
  ggtitle("Highly Seasonal - deaths averted (per 100,000  population)")+
  theme_bw()+
  theme(strip.text.x = element_blank())

da_ageA 

da_ageB <- 
  ggplot(age_pd %>% filter(seasonality == "seasonal") %>% filter(pfpr %in% c(0.1, 0.25,0.35,0.55)), 
         aes(x = age_upper, y = deaths_averted, fill = run)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #geom_linerange(col = "grey70") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = color_vals, 
                    labels = labels) + 
  ylab(" ") +
  xlab("Age") +
  labs(fill = "Vaccination schedule") +
  facet_grid(pfpr~run, scales = "free_y",  labeller = labeller(pfpr = pfpr.labs)) +
  ggtitle("Seasonal - deaths averted (per 100,000 population)")+
  theme_bw() +
  theme(strip.text.x = element_blank())

da_ageB 

combined_ages <- 
  ca_ageA + ca_ageB + da_ageA + da_ageB + plot_layout(guides = "collect") & theme(legend.position = "bottom", strip.text.x = element_blank())

combined_ages

ggsave("Figure_S8.png",combined_ages, width=19, height=10)

#-Table S13----------------------------------------------------------------------------------------
#-table of cases averted and proportional reductions in cases from baseline-# 
impact_0_5 <- 
  impact_age %>% 
  filter(age_lower <= 5) %>%   
  group_by(run, pfpr, seasonality, coverage, draw) %>%
  summarise(across(cases:deaths_averted, sum)) %>%
  ungroup()

impact_0_5$run <- factor(impact_0_5$run, levels=c("smc", "epi_smc","srtss_4_dose_original_smc", "srtss_4_dose_update_smc", 
                                                  "srtss_4_dose_param_mod_smc", "srtss_5_dose_original_smc", 
                                                  "srtss_5_dose_update_smc", "srtss_5_dose_param_mod_smc"))

table1 <-
  impact_0_5 %>%  
  filter(#coverage %in% c(0.8, 0.0), 
    draw == 0) %>% 
  mutate(#precentage_reductions = round(((cases_averted / cases_bl)*100), digits = 1), 
    #percentage_reductions_deaths = round(((deaths_averted / deaths_bl)*100), digits = 1), 
    pfpr = pfpr * 100, 
    pfpr = paste0(pfpr, "%"), 
    cases_averted = prettyNum(round(cases_averted, digits = -3),big.mark = ",", scientific = FALSE), 
    deaths_averted = prettyNum(round(deaths_averted, digits = -1),big.mark = ",", scientific = FALSE)) %>% 
  select(run, pfpr, seasonality, cases_averted, deaths_averted) #, precentage_reductions, percentage_reductions_deaths) #%>%
#-convert from wide to long format
# tidyr::pivot_longer(-c(run, seasonality, pfpr), names_to = "event", values_to = "y") %>% 
# #-convert back to wide
# tidyr::pivot_wider(id_cols = c(run, seasonality, event),
#                    names_from = pfpr, values_from = y) 

#-lower and upper 
lower_upper <- 
  impact_0_5 %>%  
  filter(#coverage %in% c(0.8, 0.0), 
    draw > 0) %>% 
  mutate(#precentage_reductions = round(((cases_averted / cases_bl)*100), digits = 1), 
    #percentage_reductions_deaths = round(((deaths_averted / deaths_bl)*100), digits = 1), 
    pfpr = pfpr * 100, 
    pfpr = paste0(pfpr, "%"), 
    cases_averted = round(cases_averted, digits = -3), 
    deaths_averted = round(deaths_averted, digits = -1)) %>% 
  group_by(run, pfpr, seasonality) %>% 
  summarise(lower_cases = quantile(cases_averted, probs = 0.025), 
            lower_cases = prettyNum(round(lower_cases, digits=-3), big.mark = ",", scientific=FALSE), 
            upper_cases = quantile(cases_averted, probs = 0.975), 
            upper_cases = prettyNum(round(upper_cases, digits=-3), big.mark = ",", scientific=FALSE),
            lower_deaths = quantile(deaths_averted, probs = 0.025), 
            lower_deaths = prettyNum(round(lower_deaths, digits=-1), big.mark = ",", scientific=FALSE),
            upper_deaths = quantile(deaths_averted, probs = 0.975), 
            upper_deaths = prettyNum(round(upper_deaths, digits=-1), big.mark = ",", scientific=FALSE)) %>% 
  ungroup() %>% 
  select(run, pfpr, seasonality, lower_cases, upper_cases, lower_deaths, upper_deaths) %>%
  unite("infs_CI" ,c("lower_cases", "upper_cases"), sep=" - " ) %>% 
  unite("deaths_CI" ,c("lower_deaths", "upper_deaths"), sep=" - " ) %>% 
  mutate(infs_CI = paste0("(",infs_CI,")"),
         deaths_CI = paste0("(",deaths_CI,")")) 


table_totals <- 
  table1 %>% 
  left_join(lower_upper) %>% 
  unite("infections" ,c("cases_averted", "infs_CI"), sep=" \n " ) %>% 
  unite("deaths" ,c("deaths_averted", "deaths_CI"), sep=" \n " ) %>% 
  #-convert from wide to long format
  tidyr::pivot_longer(-c(run, seasonality, pfpr), names_to = "event", values_to = "y") %>%
  #-convert back to wide
  tidyr::pivot_wider(id_cols = c(run, seasonality, event),
                     names_from = pfpr, values_from = y)


writexl::write_xlsx(table_totals, "table_s13.xlsx")