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
impact     <- readRDS("Part_1/mistimings/impact_all_ages.RDS")
impact_age <- readRDS("Part_1/mistimings/impact_age_groups.RDS")
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
                     "srtss_5_dose_update_smc")


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
  scale_y_continuous(labels = comma, limits=c(0,230000)) + 
  facet_wrap(run  ~ seasonality, labeller = labeller(seasonality = seas.labs, 
                                                     run = run.labs), nrow=1) +
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
  ggplot(epi_smc_time %>% filter(run == "epi_smc"), aes(x = mistiming, y = cases_averted, fill=run)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black", alpha=0.6, show.legend = F) +
  scale_y_continuous(labels = comma, limits=c(0,230000)) + 
  scale_fill_manual(values = c("smc" = "#f0c9e9",
                               "epi_smc" = "#FF3200"))+
  facet_wrap(seasonality ~ run, labeller = labeller(seasonality = seas.labs, run=run.labs), nrow=1) +
  ylab("Clinical cases averted\n(per 100,000 children 0-5)") +
  xlab("SMC delivery relative to peak in transmission") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 

#-SV and SMC combined impact plot--------------------------------------------------
sv_smc_time <- 
  impact_0_5 %>% 
  filter(run %in% c("srtss_5_dose_update_smc")) 

smc.alone <- single_interventions %>% filter(run == "smc")

sv_smc_time <- bind_rows(sv_smc_time, smc.alone)

sv_smc_time$timing_sv  <- factor(sv_smc_time$timing_sv, levels=c("-2 months", "-1 month", "Optimal", "+1 month", "+2 months"))
sv_smc_time$timing_smc <- factor(sv_smc_time$timing_smc, levels=c("-2 months", "-1 month", "Optimal", "+1 month", "+2 months"))

sv_smc_time$run <- factor(sv_smc_time$run, levels=c("srtss_5_dose_update_smc", "smc"))

p3 <- ggplot(sv_smc_time %>% filter(run == "srtss_5_dose_update_smc"), aes(x = timing_smc, y = cases_averted, fill=timing_sv)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black", alpha=0.6, show.legend = T) +
  scale_y_continuous(labels = comma, limits=c(0,230000)) + 
  scale_fill_manual(values = c("-2 months"  = "#FE785C", 
                               "-1 month"  = "#9F3620", 
                               "Optimal"   = "#200B06",
                               "+1 month"  = "#9F3620", 
                               "+2 months" = "#FE785C"), 
                    na.value = "#f0c9e9")+
  facet_wrap(seasonality  ~ run, labeller = labeller(seasonality = seas.labs, 
                                                     run=run.labs), nrow = 1) +
  ylab("Clinical cases averted\n(per 100,000 children 0-5)") +
  xlab("SMC delivery relative to peak in transmission") +
  labs(fill = "SV delivery")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3

p1 / (p3 + p2) + plot_annotation(tag_levels = "A")

ggsave("Figure_s10.png", width=11, height=7, dpi=600)
#p1 / p2


# reviwer comments testing 
optimal <- 
  single_interventions %>% 
  filter(timing_smc == "Optimal" | timing_sv == "Optimal") %>% 
  select(run, pfpr, seasonality, timing_smc, timing_sv, 
         cases_averted_optimal = cases_averted)


not <- single_interventions %>% 
  filter(timing_smc != "Optimal" | timing_sv != "Optimal") %>% 
  left_join(optimal, by=c("run", "pfpr", "seasonality")) %>% 
  mutate(reductions =  cases_averted - cases_averted_optimal, 
         point_change = cases_averted / cases_averted_optimal)


top <- 
  ggplot(not, aes(x = mistiming, y = point_change)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black", alpha=0.5) +
  scale_y_continuous(labels = comma) + 
  facet_wrap(run  ~ seasonality, labeller = labeller(seasonality = seas.labs, 
                                                     run = run.labs), nrow=1) +
  ylab(" ") +
  xlab("Intervention delivery relative to peak in transmission") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#-row b------------------------------------------------------------------------- 
optimal_b <- 
  impact_0_5 %>% 
  filter(run %in% c("srtss_5_dose_update_smc")) %>% 
  filter(timing_sv == "Optimal") %>% 
  select(run, pfpr, seasonality, timing_smc, timing_sv, 
         cases_averted_optimal = cases_averted) %>% 
  filter(timing_smc == "Optimal")

not_b <- 
  impact_0_5 %>% 
  filter(run %in% c("srtss_5_dose_update_smc")) %>% 
  filter(timing_sv == "Optimal") %>% 
  filter(timing_smc != "Optimal") %>% 
  left_join(optimal_b, by=c("run", "pfpr", "seasonality", "timing_sv")) %>% 
  mutate(reductions =  cases_averted - cases_averted_optimal, 
         point_change = cases_averted / cases_averted_optimal)

not_b$timing_smc.x <- factor(not_b$timing_smc.x, levels=c("-2 months", "-1 month", "Optimal", "+1 month", "+2 months"))

ggplot(not_b, aes(x = timing_smc.x, y = point_change)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black",  fill="grey", alpha=0.6, show.legend = T) +
  scale_y_continuous(labels = comma) + 
  facet_wrap(run  ~ seasonality, labeller = labeller(seasonality = seas.labs, 
                                                     run = run.labs), nrow=1) +
  ylab(" ") +
  xlab("SMC delivery relative to peak in transmission") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#-c with epi--------------------------
optimal_c <- 
  impact_0_5 %>% 
  filter(run %in% c("epi_smc")) %>% 
  select(run, pfpr, seasonality, timing_smc,  
         cases_averted_optimal = cases_averted) %>% 
  filter(timing_smc == "Optimal")

not_c <- 
  impact_0_5 %>% 
  filter(run %in% c("epi_smc")) %>% 
  filter(timing_smc != "Optimal") %>% 
  left_join(optimal_c, by=c("run", "pfpr", "seasonality")) %>% 
  mutate(reductions =  cases_averted - cases_averted_optimal, 
         point_change = cases_averted / cases_averted_optimal)

not_c$timing_smc.x <- factor(not_c$timing_smc.x, levels=c("-2 months", "-1 month", "Optimal", "+1 month", "+2 months"))

ggplot(not_c, aes(x = timing_smc.x, y = point_change)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black",  fill="grey", alpha=0.6, show.legend = T) +
  scale_y_continuous(labels = comma) + 
  facet_wrap(run  ~ seasonality, labeller = labeller(seasonality = seas.labs, 
                                                     run = run.labs), nrow=1) +
  ylab(" ") +
  xlab("SMC delivery relative to peak in transmission") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bottom_row <- bind_rows(not_b, not_c)
bottom_row$timing_smc.x <- factor(bottom_row$timing_smc.x, levels=c("-2 months", "-1 month", "Optimal", "+1 month", "+2 months"))

bottom <- 
  ggplot(bottom_row, aes(x = timing_smc.x, y = point_change)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black", alpha=0.6, show.legend = F) +
  scale_y_continuous(labels = comma) + 
  facet_wrap(run  ~ seasonality, labeller = labeller(seasonality = seas.labs, 
                                                     run = run.labs), nrow=1) +
  ylab(" ") +
  xlab("SMC delivery relative to peak in transmission") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4 <- ggplot(data.frame(l = "Fold change reduction in case burden relative to optimally timed intervetnions", 
                        x = 1, y = 1)) +
  geom_text(aes(x, y, label = l), angle = 90) + 
  theme_void() +
  coord_cartesian(clip = "off")

p4 + (top / bottom) + plot_annotation(tag_levels = list(c(" ", "A", "B"))) + plot_layout(widths = c(1, 25))

#grid::grid.draw(grid::textGrob("Fold change reduction in case burden relative to optimally timed intervetnions", x = 0.025, rot = 90))

ggsave("figure_s11.png", width=11, height=7, dpi=600)

