# Load packages
library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)
library(ghibli)
library(scales)
library(LaCroixColoR)

#-load outputs-----------------------------
impact     <- readRDS("impact_all_ages.RDS")
impact_age <- readRDS("impact_age_groups.RDS")
impact_0_5 <- impact_age %>% 
  filter(age_lower <= 5) %>%   
  group_by(run, pfpr, seasonality, srtss_start, smc_offset, timing_sv, timing_smc) %>%
  summarise(across(cases:disc_dalys_averted, sum)) %>%
  ungroup()

# New facet label names for supp variable
seas.labs <- c("Highly Seasonal", "Seasonal")
names(seas.labs) <- c("highly_seasonal", "seasonal")

run.labs <- c("SMC",
              "SV-RTS,S 4-dose", 
              "SV-RTS,S 4-dose (updated booster)",
              "SV-RTS,S 5-dose",
              "SV-RTS,S",
              "AB-RTS,S + SMC", 
              "SV-RTS,S + SMC")

names(run.labs) <- c("smc", 
                     "srtss_4_dose_original",
                     "srtss_4_dose_update", 
                     "srtss_5_dose_original", 
                     "srtss_5_dose_update", 
                     "epi_smc", 
                     "srtss_5_dose_param_mod_smc")


# and labels
labels <- c("epi_smc" = "AB-RTS,S + SMC", 
            "srtss_4_dose_original_smc" = "SV-RTS,S 4-dose + SMC",
            "srtss_5_dose_original_smc" = "SV-RTS,S 5-dose + SMC",
            "srtss_4_dose_update_smc" = "SV-RTS,S 4-dose (updated booster) + SMC",
            "srtss_5_dose_update_smc" = "SV-RTS,S 5-dose (updated booster) + SMC", 
            "srtss_4_dose_param_mod_smc" = "SV-RTS,S 4-dose (synergy) + SMC", 
            "srtss_5_dose_param_mod_smc" = "SV-RTS,S 5-dose (synergy) + SMC", 
            "smc" = "SMC alone")

single_interventions <- 
  impact_0_5 %>% 
  filter(run %in% c("smc", "srtss_5_dose_update")) %>% 
  mutate(mistiming = case_when(is.na(timing_smc) ~ timing_sv, 
                               is.na(timing_sv) ~ timing_smc))
  
single_interventions$mistiming <- factor(single_interventions$mistiming, levels=c("-2 months", "-1 month", "Optimal", "+1 month", "+2 months"))

#-Impact by degree of mistiming for single interventions----------------------------
p1 <-  
  ggplot(single_interventions, aes(x = mistiming, y = cases_averted)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black", fill="dodgerblue", alpha=0.5) +
  scale_y_continuous(labels = comma) + 
  facet_wrap(run  ~ seasonality, labeller = labeller(seasonality = seas.labs, 
                                                     run = run.labs), nrow=1, scales = "free_y") +
  ylab("Clinical cases averted\n(per 100,000 children 0-5)") +
  xlab("Intervention delivery relative to peak in transmission") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p1 

#-impact of SMC changes when combined with EPI------------------------------------- 
epi_smc_time <- 
  impact_0_5 %>% 
  filter(run %in% c("smc", "epi_smc")) %>% 
  mutate(mistiming = case_when(is.na(timing_smc) ~ timing_sv, 
                               is.na(timing_sv) ~ timing_smc))

epi_smc_time$mistiming <- factor(epi_smc_time$mistiming, levels=c("-2 months", "-1 month", "Optimal", "+1 month", "+2 months"))


#-Impact by degree of mistiming for single interventions----------------------------
p2 <-  
  ggplot(epi_smc_time, aes(x = mistiming, y = cases_averted, fill=run)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black", alpha=0.6, show.legend = F) +
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = c("smc" = "#f0c9e9",
                               "epi_smc" = "#FF3200"))+
  facet_wrap(seasonality ~ run, labeller = labeller(seasonality = seas.labs, run=run.labs), nrow=1,scales = "free_y") +
  ylab("Clinical cases averted\n(per 100,000 children 0-5)") +
  xlab("SMC delivery relative to peak in transmission") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 

#-SV and SMC combined impact plot--------------------------------------------------
sv_smc_time <- 
  impact_0_5 %>% 
  filter(run %in% c("srtss_5_dose_param_mod_smc")) 

smc.alone <- single_interventions %>% filter(run == "smc")

sv_smc_time <- bind_rows(sv_smc_time, smc.alone)

sv_smc_time$timing_sv  <- factor(sv_smc_time$timing_sv, levels=c("-2 months", "-1 month", "Optimal", "+1 month", "+2 months"))
sv_smc_time$timing_smc <- factor(sv_smc_time$timing_smc, levels=c("-2 months", "-1 month", "Optimal", "+1 month", "+2 months"))

sv_smc_time$run <- factor(sv_smc_time$run, levels=c("srtss_5_dose_param_mod_smc", "smc"))

p3 <- ggplot(sv_smc_time, aes(x = timing_smc, y = cases_averted, fill=timing_sv)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black", alpha=0.6, show.legend = T) +
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = c("-2 months"  = "#FE785C", 
                               "-1 month"  = "#9F3620", 
                               "Optimal"   = "#200B06",
                               "+1 month"  = "#9F3620", 
                               "+2 months" = "#FE785C"), 
                    na.value = "#f0c9e9")+
  facet_wrap(seasonality  ~ run, labeller = labeller(seasonality = seas.labs, 
                                                     run=run.labs), scales = "free", nrow = 1) +
  ylab("Clinical cases averted\n(per 100,000 children 0-5)") +
  xlab("SMC delivery relative to peak in transmission") +
  labs(fill = "SV delivery")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
p3

p1 / p3 /p2 + plot_annotation(tag_levels = "A")

ggsave("Figure_6.png", width=11, height=9, dpi=600)
#p1 / p2















#-numbers for text and tables---------------------------------------------------------------------------------------
smc <- 
  impact_0_5 %>% 
  filter(coverage == 0.0) %>% 
  filter(pfpr >0.05) %>% 
  filter(run == "smc") %>% 
  select(pfpr,
         seasonality, 
         cases_averted_smc = cases_averted, 
         deaths_averted_smc = deaths_averted)

increment <- 
  impact_0_5 %>% 
  filter(coverage == 0.8) %>% 
  filter(pfpr >0.05) %>% 
  filter(run != "smc") %>% 
  select(run, pfpr, seasonality, cases_averted, deaths_averted) %>% 
  left_join(smc, by=c("pfpr", "seasonality")) %>% 
  mutate(incremental_cases = (cases_averted - cases_averted_smc)/ cases_averted_smc,
         incremental_deaths = (deaths_averted - deaths_averted_smc)/ deaths_averted_smc) %>% 
  ungroup()


numbers <- 
  increment %>% 
  group_by(run, seasonality) %>% 
  summarise(increment_cases = mean(incremental_cases), 
            increment_deaths = mean(incremental_deaths))

write.csv(numbers, "numbers_5_rounds.csv")

#-combined plot with the 4 rounds values-# 
numbers_4_rounds <- read_csv("Q:/who_use_case/combined_smc/numbers.csv") %>% filter(seasonality == "seasonal")
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
            "srtss_4_dose_update_smc" = "SV 4-dose  updated booster + SMC",
            "srtss_5_dose_update_smc" = "SV 5-dose  updated booster + SMC", 
            "srtss_4_dose_param_mod_smc" = "SV 4-dose synergy + SMC", 
            "srtss_5_dose_param_mod_smc" = "SV 5-dose synergy + SMC", 
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
  filter(pfpr >0.05, seasonality == "seasonal")

impact_5_rounds <- impact_0_5 %>% 
  select(run, pfpr, seasonality, coverage, draw, cases_averted_5_rounds = cases_averted)

plot_averted <- left_join(impact_4_rounds, impact_5_rounds) %>%   filter(coverage == 0.8) 

p1 <- 
  ggplot(plot_averted) +
  geom_segment( aes(x=cases_averted_4_rounds, xend=cases_averted_5_rounds, y=run, yend=run), color="grey") +
  geom_point( aes(x=cases_averted_4_rounds, y=run, col="4 cycles"), size=3 ) +
  geom_point( aes(x=cases_averted_5_rounds, y=run, col="5 cycles"), size=3 ) +
  scale_color_manual(values = c("4 cycles" = "#9E0031", 
                                "5 cycles" = "#EE8434")) +
  scale_y_discrete(labels = labels) +
  xlab(" ") +
  ylab("Vaccination schedule") +
  labs(col = "SMC Monthly Cycles") +
  theme_bw(12) +
  theme(legend.position = "bottom") +
  facet_wrap(seasonality  ~ pfpr, labeller = labeller(seasonality = seas.labs), scales = "free_x") 

p1
