
#-coverage sensitivity------------------------------------------------------------------------

#-plot coverage on the x and cases averted on the y - aggregated over all settings------------
impact <- readRDS("impact_costs_all_ages.RDS")
impact_age <- readRDS("impact_costs_age_groups.RDS")

# New facet label names for supp variable
seas.labs <- c("Highly Seasonal", "Seasonal")
names(seas.labs) <- c("highly_seasonal", "seasonal")

#-impact by coverage level - aggregated over pfpr for diff seasonality -------------
figure_2_pd <- 
  impact %>%
  filter(cost_per_dose == 6.52, delivery_cost == 1.62) %>% 
  group_by(run, coverage, draw, seasonality) %>% 
  summarise(mean_cases_averted = mean(cases_averted), 
            mean_deaths_averted = mean(deaths_averted), 
            mean_cases_fvp = mean(cases_averted_per_100000_fvp), 
            mean_deaths_fvp = mean(deaths_averted_per_100000_fvp))

figure_2_pd$coverage <- as.character(figure_2_pd$coverage)

#-plot- 
A <- 
  ggplot(figure_2_pd, 
         aes(x=coverage, y=mean_cases_fvp, fill=run)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=lacroix_palette("PeachPear"), 
                    labels=c("epi" = "EPI",
                             "srtss_4_dose_original" = "SV 4-dose",
                             "srtss_5_dose_original" = "SV 5-dose",
                             "srtss_4_dose_update" = "SV 4-dose - updated booster",
                             "srtss_5_dose_update" = "SV 5-dose - updated booster"))+
  labs(x = "Primary series coverage", 
       y = " ",
       fill="Vaccination schedule",
       parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  ggtitle("Clinical cases averted per 100,000 FVP") +
  theme_bw( ) 

A

B <- 
  ggplot(figure_2_pd, 
       aes(x=coverage, y=mean_cases_averted, fill=run)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=lacroix_palette("PeachPear"), 
                    labels=c("epi" = "EPI",
                             "srtss_4_dose_original" = "SV 4-dose",
                             "srtss_5_dose_original" = "SV 5-dose",
                             "srtss_4_dose_update" = "SV 4-dose - updated booster",
                             "srtss_5_dose_update" = "SV 5-dose - updated booster"))+
  labs(x = "Primary series coverage", 
       y = " ",
       fill="Vaccination schedule",
       parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  ggtitle("Clinical cases averted per 100,000 population") +
  theme_bw( ) 

B

C <- 
ggplot(figure_2_pd, 
       aes(x=coverage, y=mean_deaths_fvp, fill=run)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=lacroix_palette("PeachPear"), 
                    labels=c("epi" = "EPI",
                             "srtss_4_dose_original" = "SV 4-dose",
                             "srtss_5_dose_original" = "SV 5-dose",
                             "srtss_4_dose_update" = "SV 4-dose - updated booster",
                             "srtss_5_dose_update" = "SV 5-dose - updated booster"))+
  labs(x = "Primary series coverage", 
       y = " ",
       fill="Vaccination schedule",
       parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  ggtitle("Deaths averted per 100,000 FVP") +
  theme_bw( ) 

C

D <- 
  ggplot(figure_2_pd, 
         aes(x=coverage, y=mean_deaths_averted, fill=run)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=lacroix_palette("PeachPear"), 
                    labels=c("epi" = "EPI",
                             "srtss_4_dose_original" = "SV 4-dose",
                             "srtss_5_dose_original" = "SV 5-dose",
                             "srtss_4_dose_update" = "SV 4-dose - updated booster",
                             "srtss_5_dose_update" = "SV 5-dose - updated booster"))+
  labs(x = "Primary series coverage", 
       y = " ",
       fill="Vaccination schedule",
       parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  ggtitle("Deaths averted per 100,000 population") +
  theme_bw( ) 

D

A + B + C + D + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("coverage_annex.png", width=14, height=8)
