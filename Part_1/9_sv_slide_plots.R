
#-Impact figures test 1---------------------
# Load packages
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)
library(ghibli)
library(scales)
library(LaCroixColoR)

#-load the dataframes from each separate analysis--------------- 
non_smc_impact <- readRDS("impact_costs_all_ages.RDS")
smc_impact <- readRDS("combined_smc/impact_costs_all_ages.RDS")

#-New facet label names for supp variable----------------------
seas.labs <- c("Highly Seasonal", "Seasonal")
names(seas.labs) <- c("highly_seasonal", "seasonal")

#-combine data frames together--------------------------------- 
impact <- 
  bind_rows(non_smc_impact, smc_impact) %>% 
  filter(run %in% c("smc", "srtss_4_dose_original", "srtss_5_dose_original", "srtss_4_dose_update",
                    "srtss_5_dose_update",
                    "srtss_4_dose_original_smc", "srtss_4_dose_update_smc", 
                    "srtss_4_dose_param_mod_smc", "srtss_5_dose_original_smc", 
                    "srtss_5_dose_update_smc", "srtss_5_dose_param_mod_smc")) %>% 
  mutate(delivery = case_when(run == "smc" ~ "smc",                       
                              run == "srtss_4_dose_original" ~ "sv_4_dose", 
                              run == "srtss_4_dose_update" ~ "sv_4_dose", 
                              run == "srtss_5_dose_original" ~ "sv_5_dose", 
                              run == "srtss_5_dose_update" ~ "sv_5_dose", 
                              run == "srtss_4_dose_original_smc" ~ "sv_4_dose_smc", 
                              run == "srtss_4_dose_update_smc" ~ "sv_4_dose_smc", 
                              run == "srtss_4_dose_param_mod_smc" ~ "sv_4_dose_smc", 
                              run == "srtss_5_dose_original_smc" ~ "sv_5_dose_smc", 
                              run == "srtss_5_dose_update_smc" ~ "sv_5_dose_smc", 
                              run == "srtss_5_dose_param_mod_smc" ~ "sv_5_dose_smc")) %>% 
  group_by(seasonality, pfpr, coverage, draw, cost_per_dose, delivery_cost, delivery) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

#-impact by transmission level---------------------------------
figure_2_pd <- 
  impact %>%
  filter(coverage %in% c(0.8,0), cost_per_dose == 6.52, delivery_cost == 1.62) %>% 
  # filtering down pfpr to be clearer 
  filter(pfpr %in% c(0.1,0.25,0.35,0.55))

# cases averted per 100,000 population-------------------------

figure_2_pd$delivery <- factor(figure_2_pd$delivery, levels=c("sv_4_dose", "sv_5_dose", "smc", "sv_4_dose_smc", "sv_5_dose_smc"))

ca <-  
  ggplot(figure_2_pd, aes(x = factor(pfpr * 100), y = cases_averted, fill=delivery)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  scale_fill_manual(values=lacroix_palette("MurePepino"),
                    labels=c("sv_4_dose" = "SV 4-dose",
                             "sv_5_dose" = "SV 5-dose",
                             "smc" = "SMC",
                             "sv_4_dose_smc" = "SV 4-dose + SMC", 
                             "sv_5_dose_smc" = "SV 5-dose + SMC"))+
  labs(y = " ", fill=" ",  parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  #  ylab("Clinical cases averted\n(per 100000 population)") +
  xlab(expression(PfPR[2-10]~(symbol("\045")))) +
  ggtitle("A - Clinical cases averted per 100,000 population") +
  theme_bw( ) 

ca 

da <-  
  ggplot(figure_2_pd, aes(x = factor(pfpr * 100), y = deaths_averted, fill=delivery)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  scale_fill_manual(values=lacroix_palette("MurePepino"),
                    labels=c("sv_4_dose" = "SV 4-dose",
                             "sv_5_dose" = "SV 5-dose",
                             "smc" = "SMC",
                             "sv_4_dose_smc" = "SV 4-dose + SMC", 
                             "sv_5_dose_smc" = "SV 5-dose + SMC"))+
  labs(y = "", 
       fill=" ",
       parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  #  ylab("Deaths averted\n(per 100000 population)") +
  xlab(expression(PfPR[2-10]~(symbol("\045")))) +
  ggtitle("B - Deaths averted per 100,000 population") +
  theme_bw( )

da 

#combine together 
fig <- ca / da +  plot_layout(guides = "collect") & theme(legend.position = "bottom") 

fig

ggsave("sv_only_plot.png", fig, width=7, height = 7)


#-cost effectiveness data- 
# average EPI cost per case and DALY averted 
pol <-
  impact %>% 
  filter(delivery %in% c("sv_4_dose", "sv_5_dose")) %>% 
  filter(pfpr > 0.1, pfpr < 0.5) %>% 
  filter( coverage == 0.8) %>% 
  group_by(cost_per_dose, delivery_cost) %>% 
  summarise(case = mean(icer_case), 
            daly = mean(icer_daly))

# when combined with smc
lol <-
  impact %>% 
  filter(delivery %in% c("sv_4_dose_smc", "sv_5_dose_smc")) %>% 
  filter(pfpr >= 0.1, pfpr < 0.5) %>% 
  filter(coverage == 0.8) %>% 
  group_by(cost_per_dose, delivery_cost) %>% 
  summarise(case = mean(icer_case), 
            daly = mean(icer_daly))

