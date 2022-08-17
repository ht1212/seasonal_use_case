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
impact     <- readRDS("Part_2/5_rounds/impact_costs_all_ages_5_rounds.RDS")
impact_age <- readRDS("Part_2/5_rounds/impact_costs_age_groups_5_rounds.RDS")
impact_0_5 <- impact_age %>% 
  filter(age_lower <= 5) %>%   
  filter(cost_per_dose == 6.52, delivery_cost == 1.62) %>% 
  group_by(run, pfpr, seasonality, coverage, draw) %>%
  summarise(across(cases:disc_dalys_averted, sum)) %>%
  ungroup()

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
numbers_4_rounds <- read_csv("Part_2/5_rounds/numbers_4_rounds.csv") 
numbers <- read_csv("Part_2/5_rounds/numbers_5_rounds.csv")
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
labels <- c("epi_smc" = "AB-RTS,S model 1 + SMC", 
            "srtss_4_dose_original_smc" = "SV 4-dose model 1 + SMC",
            "srtss_5_dose_original_smc" = "SV 5-dose model 1 + SMC",
            "srtss_4_dose_update_smc" = "SV 4-dose model 2 + SMC",
            "srtss_5_dose_update_smc" = "SV 5-dose model 2 + SMC", 
            "srtss_4_dose_param_mod_smc" = "SV 4-dose + SMC model 3", 
            "srtss_5_dose_param_mod_smc" = "SV 5-dose + SMC model 3", 
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
  readRDS("Part_2/impact_age_groups.RDS")%>% 
  filter(age_lower <= 5) %>%   
  filter(draw==0) %>% 
  filter(coverage == 0.8) %>% 
  group_by(run, pfpr, seasonality, coverage, draw) %>%
  summarise(across(cases:deaths_averted, sum)) %>%
  ungroup() %>% 
  select(run, pfpr, seasonality, coverage, draw, cases_averted_4_rounds = cases_averted) #%>% 
#filter(pfpr >0.05)#, seasonality == "seasonal")

impact_5_rounds <- impact_0_5 %>% 
  select(run, pfpr, seasonality, coverage, draw, cases_averted_5_rounds = cases_averted)

plot_averted <- left_join(impact_4_rounds, impact_5_rounds) # %>%   filter(coverage == 0.8) 

plot_averted$run <- factor(plot_averted$run, levels=c("epi_smc",
                                                      "srtss_4_dose_original_smc",
                                                      "srtss_4_dose_update_smc", 
                                                      "srtss_4_dose_param_mod_smc",
                                                      "srtss_5_dose_original_smc",
                                                      "srtss_5_dose_update_smc", 
                                                      "srtss_5_dose_param_mod_smc"))

# New facet label names for supp variable
pfpr.labs <- c("10%", 
               #"25%", 
               "35%", 
               "65%")
names(pfpr.labs) <- c(0.1,0.35,0.65)


prevlabs <- c(
  `0.1` = expression(paste(italic(Pf),"PR"[2-10]," = 10%")),
  # `0.25` = expression(paste(italic(Pf),"PR"[2-10]," = 25%")),
  `0.35` = expression(paste(italic(Pf),"PR"[2-10]," = 35%")),
  `0.65` = expression(paste(italic(Pf),"PR"[2-10]," = 65%"))
)

p1 <- 
  ggplot(plot_averted %>% filter(pfpr %in% c(0.1,0.35,0.65))) +
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
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  facet_wrap(seasonality  ~ pfpr, labeller = labeller(seasonality = seas.labs, 
                                                      pfpr = pfpr.labs), scales = "free_x", nrow=2) 

p1


cowplot::plot_grid(p1, p2, 
                   align = "v",
                   axis = "l",
                   nrow = 2, 
                   labels = c("A", "B"),
                   rel_heights = c(1,0.7), 
                   label_fontface = "plain")

ggsave("Figure_s9.png", width=9, height=8.5, dpi=600)

