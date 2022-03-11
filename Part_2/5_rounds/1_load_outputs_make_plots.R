# Load packages
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)
library(ghibli)
library(scales)
library(LaCroixColoR)
library(cowplot)
library(tidyverse)

#-load outputs-----------------------------
impact     <- readRDS("impact_costs_all_ages_5_rounds.RDS")
impact_age <- readRDS("impact_costs_age_groups_5_rounds.RDS")
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
labels <- c("epi_smc" = "AB-RTS,S + SMC", 
            "srtss_4_dose_original_smc" = "SV-RTS,S 4-dose + SMC",
            "srtss_5_dose_original_smc" = "SV-RTS,S 5-dose + SMC",
            "srtss_4_dose_update_smc" = "SV-RTS,S 4-dose (updated booster) + SMC",
            "srtss_5_dose_update_smc" = "SV-RTS,S 5-dose (updated booster) + SMC", 
            "srtss_4_dose_param_mod_smc" = "SV-RTS,S 4-dose (synergy) + SMC", 
            "srtss_5_dose_param_mod_smc" = "SV-RTS,S 5-dose (synergy) + SMC", 
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

#ggsave("Figure_S5.png", total_pop, width=12, height = 4)

children_0_5 <- ca2 + da2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
children_0_5

#ggsave("Figure_4.2.png", children_0_5, width=12.2, height = 4.2)

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

write.csv(numbers, "numbers_5_rounds.csv")

#-combined plot with the 4 rounds values-# 
numbers_4_rounds <- read_csv("Q:/who_use_case/combined_smc/numbers.csv") 
numbers <- read_csv("numbers_5_rounds.csv")
numbers_5_rounds <- numbers %>% select(run, seasonality, increment_cases_5_rounds = increment_cases, increment_deaths_5_rounds = increment_deaths)

increment_plot <- left_join(numbers_4_rounds, numbers_5_rounds)

increment_plot$run <- factor(increment_plot$run, levels=c("epi_smc",
                                                          "srtss_4_dose_original_smc",
                                                          "srtss_4_dose_update_smc", 
                                                          "srtss_4_dose_param_mod_smc",
                                                          "srtss_5_dose_original_smc",
                                                          "srtss_5_dose_update_smc", 
                                                          "srtss_5_dose_param_mod_smc"))
# and labels
labels <- c("epi_smc" = "EPI + SMC", 
            "srtss_4_dose_original_smc" = "SV 4-dose + SMC",
            "srtss_5_dose_original_smc" = "SV 5-dose + SMC",
            "srtss_4_dose_update_smc" = "SV 4-dose (updated booster) + SMC",
            "srtss_5_dose_update_smc" = "SV 5-dose (updated booster) + SMC", 
            "srtss_4_dose_param_mod_smc" = "SV 4-dose (synergy) + SMC", 
            "srtss_5_dose_param_mod_smc" = "SV 5-dose (synergy) + SMC", 
            "smc" = "SMC alone")

p2 <- 
  ggplot(increment_plot) +
  geom_segment( aes(x=increment_cases*100, xend=increment_cases_5_rounds*100, y=run, yend=run), color="grey") +
  geom_point( aes(x=increment_cases*100, y=run, col="4 cycles"), size=3 ) +
  geom_point( aes(x=increment_cases_5_rounds*100, y=run, col="5 cycles"), size=3 ) +
  scale_color_manual(values = c("4 cycles" = "#9E0031", 
                               "5 cycles" = "#EE8434")) +
  scale_y_discrete(labels = labels) +
  xlab("Incremental impact (%)") +
  ylab("Vaccination schedule") +
  labs(col = "SMC Monthly Cycles") +
  theme_bw(12) +
  theme(legend.position = "bottom") +
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) 

p2
  
#-cases averted plots-------------
impact_4_rounds <- 
  readRDS("Q:/who_use_case/combined_smc/impact_costs_age_groups.RDS")%>% 
  filter(age_lower <= 5) %>%   
  filter(cost_per_dose == 6.52, delivery_cost == 1.62) %>% 
  group_by(run, pfpr, seasonality, coverage, draw) %>%
  summarise(across(cases:disc_dalys_averted, sum)) %>%
  ungroup() %>% 
  select(run, pfpr, seasonality, coverage, draw, cases_averted_4_rounds = cases_averted) %>% 
  filter(pfpr >0.05)#, seasonality == "seasonal")

impact_5_rounds <- impact_0_5 %>% 
  select(run, pfpr, seasonality, coverage, draw, cases_averted_5_rounds = cases_averted)

plot_averted <- left_join(impact_4_rounds, impact_5_rounds) %>%   filter(coverage == 0.8) 

plot_averted$run <- factor(plot_averted$run, levels=c("epi_smc",
                                                          "srtss_4_dose_original_smc",
                                                          "srtss_4_dose_update_smc", 
                                                          "srtss_4_dose_param_mod_smc",
                                                          "srtss_5_dose_original_smc",
                                                          "srtss_5_dose_update_smc", 
                                                          "srtss_5_dose_param_mod_smc"))

# New facet label names for supp variable
pfpr.labs <- c("10%", 
               "25%", 
               "35%", 
               "55%")
names(pfpr.labs) <- c(0.1,0.25,0.35,0.55)


prevlabs <- c(
  `0.1` = expression(paste(italic(Pf),"PR"[2-10]," = 10%")),
  `0.25` = expression(paste(italic(Pf),"PR"[2-10]," = 25%")),
  `0.35` = expression(paste(italic(Pf),"PR"[2-10]," = 35%")),
  `0.55` = expression(paste(italic(Pf),"PR"[2-10]," = 55%"))
)

p1 <- 
  ggplot(plot_averted %>% filter(pfpr %in% c(0.1,0.25,0.35,0.55))) +
  geom_segment( aes(x=cases_averted_4_rounds, xend=cases_averted_5_rounds, y=run, yend=run), color="grey") +
  geom_point( aes(x=cases_averted_4_rounds, y=run, col="4 cycles"), size=3 ) +
  geom_point( aes(x=cases_averted_5_rounds, y=run, col="5 cycles"), size=3) +
  scale_color_manual(values = c("4 cycles" = "#9E0031", 
                                "5 cycles" = "#EE8434")) +
  scale_y_discrete(labels = labels) +
  xlab("Clinical cases averted per 100,000 children 0-5 years") +
  ylab("Vaccination schedule") +
  labs(col = "SMC Monthly Cycles") +
  theme_bw(12) +
  theme(legend.position = "bottom") +
  facet_wrap(seasonality  ~ pfpr, labeller = labeller(seasonality = seas.labs, 
                                                      pfpr = pfpr.labs), scales = "free_x", nrow=2)

p1

#-Seasonal and Highly Seasonal plots separate-# 
s1 <- ggplot(plot_averted %>% filter(pfpr %in% c(0.1,0.25,0.35,0.55), seasonality == "seasonal")) +
  geom_segment( aes(x=cases_averted_4_rounds, xend=cases_averted_5_rounds, y=run, yend=run), color="grey") +
  geom_point( aes(x=cases_averted_4_rounds, y=run, col="4 cycles"), size=3 ) +
  geom_point( aes(x=cases_averted_5_rounds, y=run, col="5 cycles"), size=3) +
  scale_color_manual(values = c("4 cycles" = "#9E0031", 
                                "5 cycles" = "#EE8434")) +
  scale_y_discrete(labels = labels) +
  scale_x_continuous(labels=comma) +
  xlab("Clinical cases averted per 100,000 children 0-5 years") +
  ylab("Vaccination schedule") +
  labs(col = "SMC Monthly Cycles") +
  theme_bw(12) +
  theme(legend.position = "None", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(seasonality  ~ pfpr, labeller = labeller(seasonality = seas.labs, 
                                                      pfpr = pfpr.labs), scales = "free_x", nrow=1)

s1

s2 <-  
  ggplot(increment_plot %>% filter(seasonality == "seasonal")) +
  geom_segment( aes(x=increment_cases*100, xend=increment_cases_5_rounds*100, y=run, yend=run), color="grey") +
  geom_point( aes(x=increment_cases*100, y=run, col="4 cycles"), size=3 ) +
  geom_point( aes(x=increment_cases_5_rounds*100, y=run, col="5 cycles"), size=3 ) +
  scale_color_manual(values = c("4 cycles" = "#9E0031", 
                                "5 cycles" = "#EE8434")) +
  scale_y_discrete(labels = labels) +
  xlab("Incremental impact (%)") +
  ylab("Vaccination schedule") +
  labs(col = "SMC Monthly Cycles") +
  theme_bw(12) +
  theme(legend.position = "right", 
        plot.margin=unit(c(0.2,4,1,1),"cm")) +
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) 

s2


plot_grid(s1, s2, align = "v",axis = "l", nrow = 2, labels = c("A", "B"), rel_heights = c(1,1.2), label_fontface = "plain")

ggsave("Figure_6.png", width=10, height=7, dpi=600)

#-do the same for the highly seasonal for the supplement---------------------------------------------------------------
hs1 <- ggplot(plot_averted %>% filter(pfpr %in% c(0.1,0.25,0.35,0.55), seasonality == "highly_seasonal")) +
  geom_segment( aes(x=cases_averted_4_rounds, xend=cases_averted_5_rounds, y=run, yend=run), color="grey") +
  geom_point( aes(x=cases_averted_4_rounds, y=run, col="4 cycles"), size=3 ) +
  geom_point( aes(x=cases_averted_5_rounds, y=run, col="5 cycles"), size=3) +
  scale_color_manual(values = c("4 cycles" = "#9E0031", 
                                "5 cycles" = "#EE8434")) +
  scale_y_discrete(labels = labels) +
  scale_x_continuous(labels=comma) +
  xlab("Clinical cases averted per 100,000 children 0-5 years") +
  ylab("Vaccination schedule") +
  labs(col = "SMC Monthly Cycles") +
  theme_bw(12) +
  theme(legend.position = "None", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(seasonality  ~ pfpr, labeller = labeller(seasonality = seas.labs, 
                                                      pfpr = pfpr.labs), scales = "free_x", nrow=1)

hs1

hs2 <-  
  ggplot(increment_plot %>% filter(seasonality == "highly_seasonal")) +
  geom_segment( aes(x=increment_cases*100, xend=increment_cases_5_rounds*100, y=run, yend=run), color="grey") +
  geom_point( aes(x=increment_cases*100, y=run, col="4 cycles"), size=3 ) +
  geom_point( aes(x=increment_cases_5_rounds*100, y=run, col="5 cycles"), size=3 ) +
  scale_color_manual(values = c("4 cycles" = "#9E0031", 
                                "5 cycles" = "#EE8434")) +
  scale_y_discrete(labels = labels) +
  xlab("Incremental impact (%)") +
  ylab("Vaccination schedule") +
  labs(col = "SMC Monthly Cycles") +
  theme_bw(12) +
  theme(legend.position = "right", 
        plot.margin=unit(c(0.2,4,1,1),"cm")) +
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) 

hs2


plot_grid(hs1, hs2, align = "v",axis = "l", nrow = 2, labels = c("A", "B"), rel_heights = c(1,1.2), label_fontface = "plain")

ggsave("Figure_S6.png", width=10, height=7, dpi=600)
