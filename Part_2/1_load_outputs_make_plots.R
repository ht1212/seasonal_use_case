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
impact     <- readRDS("impact_costs_all_ages.RDS")
impact_age <- readRDS("impact_costs_age_groups.RDS")
impact_0_5 <- impact_age %>% 
  filter(age_lower <= 5) %>%   
  filter(cost_per_dose == 6.52, delivery_cost == 1.62) %>% 
  group_by(run, pfpr, seasonality, coverage, draw) %>%
  summarise(across(cases:disc_dalys_averted, sum)) %>%
  ungroup()

#-impact by transmission level-------------
figure_2_pd <- 
  impact %>%
  filter(coverage %in% c(0.8,0), cost_per_dose == 6.52, delivery_cost == 1.62) %>% 
  filter(pfpr %in% c(0.1,0.25,0.35,0.55))

# New facet label names for supp variable
seas.labs <- c("Highly Seasonal", "Seasonal")
names(seas.labs) <- c("highly_seasonal", "seasonal")

figure_2_pd$run <- factor(figure_2_pd$run, levels=c("smc", "epi_smc","srtss_4_dose_original_smc", "srtss_4_dose_update_smc", 
                                                    "srtss_4_dose_param_mod_smc", "srtss_5_dose_original_smc", 
                                                    "srtss_5_dose_update_smc", "srtss_5_dose_param_mod_smc"))

# set the colour levels 
color_vals <- c("smc" = "#f0c9e9",
                "epi_smc" = "#FF3200",
                "srtss_4_dose_original_smc" = "#E9A17C", 
                "srtss_4_dose_update_smc" = "#E9E4A6", 
                "srtss_4_dose_param_mod_smc" = "#eab06d",
                "srtss_5_dose_original_smc" = "#1bb6af", 
                "srtss_5_dose_update_smc" = "#0076bb",
                "srtss_5_dose_param_mod_smc" = "#172869")
# and labels
labels <- c("epi_smc" = "EPI + SMC", 
            "srtss_4_dose_original_smc" = "SV 4-dose + SMC",
            "srtss_5_dose_original_smc" = "SV 5-dose + SMC",
            "srtss_4_dose_update_smc" = "SV 4-dose - updated booster + SMC",
            "srtss_5_dose_update_smc" = "SV 5-dose - updated booster + SMC", 
            "srtss_4_dose_param_mod_smc" = "SV 4-dose synergy + SMC", 
            "srtss_5_dose_param_mod_smc" = "SV 5-dose synergy + SMC", 
            "smc" = "SMC alone")

#-Impact over all ages------------------------
ca <-  
  ggplot(figure_2_pd, aes(x = factor(pfpr * 100), y = cases_averted, fill=run)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  scale_fill_manual(values = color_vals, 
                    labels = labels) + 
  labs(x = expression(paste(italic(Pf),"PR"[2-10])), 
       y = " ",
       fill="Vaccination schedule",
       parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  #  ylab("Clinical cases averted\n(per 100000 population)") +
  xlab(expression(italic(PfPR[2-10]~(symbol("\045"))))) +
  ggtitle("A - Clinical cases averted per 100,000 population") +
  theme_bw( ) 

ca 

da <-  
  ggplot(figure_2_pd, aes(x = factor(pfpr * 100), y = deaths_averted, fill=run)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  scale_fill_manual(values = color_vals, 
                    labels = labels) + 
  labs(x = expression(paste(italic(Pf),"PR"[2-10])), 
       y = " ",
       fill="Vaccination schedule",
       parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  #  ylab("Deaths averted\n(per 100000 population)") +
  xlab(expression(italic(PfPR[2-10]~(symbol("\045"))))) +
  ggtitle("B - Deaths averted per 100,000 population") +
  theme_bw()

da 

#-impact in children 0-5 only ----------------------------------------------------------------------------------------
figure_0_5 <- 
  impact_0_5 %>%
  #main results coverage 
  filter(coverage %in% c(0.8, 0.0)) %>% 
  #filtering down pfpr to be clearer 
  filter(pfpr %in% c(0.1,0.25,0.35,0.55))

figure_0_5$run <- factor(figure_0_5$run, levels=c("smc", "epi_smc","srtss_4_dose_original_smc", "srtss_4_dose_update_smc", 
                                                    "srtss_4_dose_param_mod_smc", "srtss_5_dose_original_smc", 
                                                    "srtss_5_dose_update_smc", "srtss_5_dose_param_mod_smc"))

#-cases averted chilren 0-5---------------------------- 
ca2 <-  
  ggplot(figure_0_5, aes(x = factor(pfpr * 100), y = cases_averted, fill=run)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  scale_fill_manual(values = color_vals, 
                    labels = labels) + 
  labs(x = expression(paste(italic(Pf),"PR"[2-10])), 
       y = " ",
       fill="Vaccination schedule",
       parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  #  ylab("Clinical cases averted\n(per 100000 population)") +
  xlab(expression(italic(PfPR[2-10]~(symbol("\045"))))) +
  ggtitle("C - Clinical cases averted per 100,000 children 0-5 years") +
  theme_bw( ) 

ca2 

#-deaths averted children 0-5-------------------------------
da2 <-  
  ggplot(figure_0_5, aes(x = factor(pfpr * 100), y = deaths_averted, fill=run)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  scale_fill_manual(values = color_vals, 
                    labels = labels) + 
  labs(x = expression(paste(italic(Pf),"PR"[2-10])), 
       y = " ",
       fill="Vaccination schedule",
       parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  #  ylab("Deaths averted\n(per 100000 population)") +
  xlab(expression(italic(PfPR[2-10]~(symbol("\045"))))) +
  ggtitle("D - Deaths averted per 100,000 children 0-5 years") +
  theme_bw()

da2 

#-finished plots-------------------------------------------
total_pop <- ca + da + plot_layout(guides = "collect") & theme(legend.position = "bottom") 
total_pop

ggsave("Figure_S4.png", total_pop, width=12, height = 4)

children_0_5 <- ca2 + da2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
children_0_5

ggsave("Figure_4.2.png", children_0_5, width=12.2, height = 4.2)

#-numbers for text and tables---------------------------------------------------------------------------------------
epi <- 
  impact_0_5 %>% 
  filter(coverage == 0.0) %>% 
  filter(pfpr >0.05) %>% 
  filter(run == "smc") %>% 
  select(pfpr,
         seasonality, 
         cases_averted_epi = cases_averted, 
         deaths_averted_epi = deaths_averted)

increment <- 
  impact_0_5 %>% 
  filter(coverage == 0.8) %>% 
  filter(pfpr >0.05) %>% 
  filter(run != "smc") %>% 
  select(run, pfpr, seasonality, cases_averted, deaths_averted) %>% 
  left_join(epi, by=c("pfpr", "seasonality")) %>% 
  mutate(incremental_cases = (cases_averted - cases_averted_epi)/ cases_averted_epi,
         incremental_deaths = (deaths_averted - deaths_averted_epi)/ deaths_averted_epi, 
         raw_additional_cases = (cases_averted - cases_averted_epi)) %>% 
  ungroup()


numbers <- 
  increment %>% 
  group_by(run, seasonality) %>% 
  summarise(increment_cases = mean(incremental_cases), 
            increment_deaths = mean(incremental_deaths), 
            raw_additional_cases = mean(raw_additional_cases))

write.csv(numbers, "numbers.csv")

#-table of cases averted and proportional reductions in cases from baseline-# 
table2 <-
  impact_0_5 %>%  
  filter(coverage %in% c(0.8, 0.0), pfpr > 0.05) %>% 
  mutate(precentage_reductions = round(((cases_averted / cases_bl)*100), digits = 1), 
         percentage_reductions_deaths = round(((deaths_averted / deaths_bl)*100), digits = 1), 
         pfpr = pfpr * 100, 
         pfpr = paste0(pfpr, "%")) %>% 
  select(run, pfpr, seasonality, cases_averted,precentage_reductions, deaths_averted, percentage_reductions_deaths) %>% 
  #-convert from wide to long format
  tidyr::pivot_longer(-c(run, seasonality, pfpr), names_to = "event", values_to = "y") %>% 
  #-convert back to wide
  tidyr::pivot_wider(id_cols = c(run, seasonality, event),
                     names_from = pfpr, values_from = y)

write.csv(table2, "Table_2_0_5.csv")

