
# Post processing script for the seasonal rts,s paper 
library(ggplot2)
library(tidyverse)
library(patchwork)
library(LaCroixColoR)
library(scales)

#-load outputs-------------------------------
impact     <- readRDS("impact_costs_all_ages.RDS")
impact_age <- readRDS("impact_costs_age_groups.RDS")
impact_0_5 <- 
  impact_age %>% 
  filter(age_lower <= 5) %>%   
  filter(cost_per_dose == 6.52, delivery_cost == 1.62) %>% 
  group_by(run, pfpr, seasonality, coverage, draw) %>%
  summarise(across(cases:disc_dalys_averted, sum)) %>%
  ungroup()

#-New facet label names for seas variables--
seas.labs <- c("Highly Seasonal", "Seasonal")
names(seas.labs) <- c("highly_seasonal", "seasonal")

#-impact by transmission level-------------------------------------------------------------------------------------
figure_2_pd <- 
  impact %>%
  #main results coverage and costs
  filter(coverage == 0.8, cost_per_dose == 6.52, delivery_cost == 1.62) %>% 
  #filtering down pfpr to be clearer 
  filter(pfpr %in% c(0.1,0.25,0.35,0.55))

#-cases averted per population---------------------------- 
ca <-  
  ggplot(figure_2_pd, aes(x = factor(pfpr * 100), y = cases_averted, fill=run)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  scale_fill_manual(values=lacroix_palette("PeachPear"), 
                    labels=c("epi" = "AB-RTS,S",
                             "srtss_4_dose_original" = "SV-RTS,S 4-dose",
                             "srtss_5_dose_original" = "SV-RTS,S 5-dose",
                             "srtss_4_dose_update" = "SV-RTS,S 4-dose (updated booster)",
                             "srtss_5_dose_update" = "SV-RTS,S 5-dose (updated booster)"))+
  labs(x = expression(paste(italic(Pf),"PR"[2-10])), 
       y = " ",
       fill="Vaccination schedule",
       parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  xlab(expression(italic(PfPR[2-10]~(symbol("\045"))))) +
  ggtitle("A - Clinical cases averted per 100,000 population") +
  theme_bw( ) 

ca 

#-deaths averted per population---------------------------
da <-  
  ggplot(figure_2_pd, aes(x = factor(pfpr * 100), y = deaths_averted, fill=run)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  scale_fill_manual(values=lacroix_palette("PeachPear"), 
                    labels=c("epi" = "AB-RTS,S",
                             "srtss_4_dose_original" = "SV-RTS,S 4-dose",
                             "srtss_5_dose_original" = "SV-RTS,S 5-dose",
                             "srtss_4_dose_update" = "SV-RTS,S 4-dose (updated booster)",
                             "srtss_5_dose_update" = "SV-RTS,S 5-dose (updated booster)"))+
  labs(x = expression(paste(italic(Pf),"PR"[2-10])), 
       y = " ",
       fill="Vaccination schedule",
       parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  xlab(expression(italic(PfPR[2-10]~(symbol("\045"))))) +
  ggtitle("B - Deaths averted per 100,000 population") +
  theme_bw( )

da 

#-impact in children 0-5 only ----------------------------------------------------------------------------------------
figure_0_5 <- 
  impact_0_5 %>%
  #main results coverage 
  filter(coverage == 0.8) %>% 
  #filtering down pfpr to be clearer 
  filter(pfpr %in% c(0.1,0.25,0.35,0.55))

#-cases averted per population---------------------------- 
ca2 <-  
  ggplot(figure_0_5, aes(x = factor(pfpr * 100), y = cases_averted, fill=run)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  scale_fill_manual(values=lacroix_palette("PeachPear"), 
                    labels=c("epi" = "AB-RTS,S",
                             "srtss_4_dose_original" = "SV-RTS,S 4-dose",
                             "srtss_5_dose_original" = "SV-RTS,S 5-dose",
                             "srtss_4_dose_update" = "SV-RTS,S 4-dose (updated booster)",
                             "srtss_5_dose_update" = "SV-RTS,S 5-dose (updated booster)"))+
  labs(x = expression(paste(italic(Pf),"PR"[2-10])), 
       y = " ",
       fill="Vaccination schedule",
       parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  xlab(expression(italic(PfPR[2-10]~(symbol("\045"))))) +
  ggtitle("A - Clinical cases averted per 100,000 children 0-5 years") +
  theme_bw( ) 

ca2 

#-deaths averted per population---------------------------
da2 <-  
  ggplot(figure_0_5, aes(x = factor(pfpr * 100), y = deaths_averted, fill=run)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  scale_fill_manual(values=lacroix_palette("PeachPear"), 
                    labels=c("epi" = "AB-RTS,S",
                             "srtss_4_dose_original" = "SV-RTS,S 4-dose",
                             "srtss_5_dose_original" = "SV-RTS,S 5-dose",
                             "srtss_4_dose_update" = "SV-RTS,S 4-dose (updated booster)",
                             "srtss_5_dose_update" = "SV-RTS,S 5-dose (updated booster)"))+
  labs(x = expression(paste(italic(Pf),"PR"[2-10])), 
       y = " ",
       fill="Vaccination schedule",
       parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  xlab(expression(italic(PfPR[2-10]~(symbol("\045"))))) +
  ggtitle("B - Deaths averted per 100,000 children 0-5 years") +
  theme_bw( )

da2 

#-finished plots-------------------------------------------
total_pop <- ca + da + plot_layout(guides = "collect") & theme(legend.position = "bottom") 
total_pop

ggsave("Figure_S2.png", total_pop, width=12, height = 4)

children_0_5 <- ca2 + da2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
children_0_5

ggsave("Figure_4.png", children_0_5, width=12, height = 4)

#-numbers for text and tables---------------------------------------------------------------------------------------
epi <- 
  impact_0_5 %>% 
  filter(coverage == 0.8) %>% 
  filter(pfpr >0.05) %>% 
  filter(run == "epi") %>% 
  select(pfpr,
         seasonality, 
         cases_averted_epi = cases_averted, 
         deaths_averted_epi = deaths_averted)

increment <- 
  impact_0_5 %>% 
  filter(coverage == 0.8) %>% 
  filter(pfpr >0.05) %>% 
  filter(run != "epi") %>% 
  select(run, pfpr, seasonality, cases_averted, deaths_averted) %>% 
  left_join(epi, by=c("pfpr", "seasonality")) %>% 
  mutate(incremental_cases = (cases_averted - cases_averted_epi)/ cases_averted_epi,
         incremental_deaths = (deaths_averted - deaths_averted_epi)/ deaths_averted_epi) %>% 
  ungroup()


numbers <- 
  increment %>% 
  group_by(run, seasonality) %>% 
  summarise(increment_cases = mean(incremental_cases), 
            increment_deaths = mean(incremental_deaths))
  
# mean(increment$incremental_cases[which(increment$seasonality=="seasonal")])
# mean(increment$incremental_cases[which(increment$seasonality=="highly_seasonal")])
# 
# mean(increment$incremental_deaths[which(increment$seasonality=="seasonal")])
# mean(increment$incremental_deaths[which(increment$seasonality=="highly_seasonal")])
# 
# mean(increment$incremental_cases)
# mean(increment$incremental_deaths)
