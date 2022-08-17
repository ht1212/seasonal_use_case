
#-script to make Figure 3 and Figure 4 and Table S12-----------------------------
library(ggplot2)
library(tidyverse)
library(patchwork)
library(LaCroixColoR)
library(scales)

#-load outputs-------------------------------
impact     <- readRDS("Part_1/impact_all_ages.RDS")
impact_age <- readRDS("Part_1/impact_age_groups.RDS")

impact_0_5 <- 
  impact_age %>% 
  filter(age_lower <= 5) %>%   
  group_by(run, pfpr, seasonality, coverage, draw) %>%
  summarise(across(cases:disc_dalys_averted, sum)) %>%
  ungroup() %>% 
  filter(pfpr %in% c(0.1,0.25,0.35,0.55))

#-New facet label names for seas variables--
seas.labs <- c("Highly Seasonal", "Seasonal")
names(seas.labs) <- c("highly_seasonal", "seasonal")

#-colour values------------------------------
colour_values = c("epi" = "#FF3200", 
                  "srtss_4_dose_original" = "#E9A17C", 
                  "srtss_4_dose_update" = "#E9E4A6", 
                  "srtss_5_dose_original" = "#1BB6AF", 
                  "srtss_5_dose_update" = "#0076BB") 


uncertainty_intervals_for_plots <- 
  impact_0_5 %>% 
  filter(draw > 0) %>% 
  group_by(run, pfpr, seasonality, coverage) %>% 
  summarise(lower_cases = quantile(cases_averted, probs = 0.025), 
            upper_cases = quantile(cases_averted, probs = 0.975), 
            lower_deaths = quantile(deaths_averted, probs = 0.025), 
            upper_deaths = quantile(deaths_averted, probs = 0.975), 
            med_cases = quantile(cases_averted, probs = 0.5), 
            med_deaths = quantile(deaths_averted, probs = 0.5))

cases_averted <- 
  ggplot(impact_0_5 %>% filter(draw == 0),
         mapping=aes(x = factor(pfpr * 100), y = cases_averted, fill=run)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black", alpha=0.8) +
  geom_errorbar(data = uncertainty_intervals_for_plots,
                mapping = aes(x = factor(pfpr * 100), ymin = lower_cases, ymax = upper_cases, y=med_cases, group=run),
                width=0.4, 
                position=position_dodge(0.9), 
                stat = "identity") +
  scale_fill_manual(values=colour_values, 
                    labels=c("epi" = "AB-RTS,S model 1",
                             "srtss_4_dose_original" = "SV-RTS,S 4-dose model 1",
                             "srtss_5_dose_original" = "SV-RTS,S 5-dose model 1",
                             "srtss_4_dose_update" = "SV-RTS,S 4-dose model 2",
                             "srtss_5_dose_update" = "SV-RTS,S 5-dose model 2"))+
  labs(x = expression(paste(italic(Pf),"PR"[2-10])), 
       y = " ",
       fill="Vaccination schedule",
       parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  xlab(expression(italic(PfPR[2-10]~(symbol("\045"))))) +
  ggtitle("A - Clinical cases averted per 100,000 children 0-5 years") +
  theme_bw() +
  theme(legend.position = "bottom", 
        title = element_text(size=10), 
        legend.text = element_text(size=9))


cases_averted 

deaths_averted <- 
  ggplot(impact_0_5 %>% filter(draw == 0),
         mapping=aes(x = factor(pfpr * 100), y = deaths_averted, fill=run)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black", alpha=0.8) +
  geom_errorbar(data = uncertainty_intervals_for_plots,
                mapping = aes(x = factor(pfpr * 100), ymin = lower_deaths, ymax = upper_deaths, y=med_deaths, group=run),
                width=0.4, 
                position=position_dodge(0.9), 
                stat = "identity") +
  scale_fill_manual(values=colour_values, 
                    labels=c("epi" = "AB-RTS,S model 1",
                             "srtss_4_dose_original" = "SV-RTS,S 4-dose model 1",
                             "srtss_5_dose_original" = "SV-RTS,S 5-dose model 1",
                             "srtss_4_dose_update" = "SV-RTS,S 4-dose model 2",
                             "srtss_5_dose_update" = "SV-RTS,S 5-dose model 2"))+
  labs(x = expression(paste(italic(Pf),"PR"[2-10])), 
       y = " ",
       fill="Vaccination schedule",
       parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  xlab(expression(italic(PfPR[2-10]~(symbol("\045"))))) +
  ggtitle("B - Deaths averted per 100,000 children 0-5 years") +
  theme_bw() +
  theme(legend.position = "bottom", 
        title = element_text(size=10), 
        legend.text = element_text(size=9))

deaths_averted

legend_b <- cowplot::get_legend(
  cases_averted + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

top <- cowplot::plot_grid(cases_averted + theme(legend.position="none"),
                          deaths_averted + theme(legend.position="none") )

row1 <- cowplot::plot_grid(top, legend_b, ncol = 1, rel_heights = c(1, .1))

#-add in the + SMC scenario--------------------------------------------------------
# need to make sure these model runs and outputs have been processed 

#-load outputs-------------------------------
impact_smc <- readRDS("Part_2/impact_all_ages.RDS")
impact_age_smc <- readRDS("Part_2/ch5_impact_age_groups.RDS")

impact_0_5_smc <- 
  impact_age_smc %>% 
  filter(age_lower <= 5) %>%   
  group_by(run, pfpr, seasonality, coverage, draw) %>%
  summarise(across(cases:deaths_averted, sum)) %>%
  ungroup() %>% 
  filter(pfpr %in% c(0.1,0.25,0.35,0.55))

impact_0_5_smc$run <- factor(impact_0_5_smc$run, levels=c("smc", "epi_smc","srtss_4_dose_original_smc", "srtss_4_dose_update_smc", 
                                                          "srtss_4_dose_param_mod_smc", "srtss_5_dose_original_smc", 
                                                          "srtss_5_dose_update_smc", "srtss_5_dose_param_mod_smc"))
# set the colour levels 
color_vals_smc <- c("smc" = "#f0c9e9",
                    "epi_smc" = "#FF3200",
                    "srtss_4_dose_original_smc" = "#E9A17C", 
                    "srtss_4_dose_update_smc" = "#E9E4A6", 
                    "srtss_4_dose_param_mod_smc" = "#eab06d",
                    "srtss_5_dose_original_smc" = "#1bb6af", 
                    "srtss_5_dose_update_smc" = "#0076bb",
                    "srtss_5_dose_param_mod_smc" = "#172869")


# and labels
labels_smc <- c("epi_smc" = "AB-RTS,S model 1+ SMC", 
                "srtss_4_dose_original_smc" = "SV 4-dose model 1 + SMC",
                "srtss_5_dose_original_smc" = "SV 5-dose model 1+ SMC",
                "srtss_4_dose_update_smc" = "SV 4-dose model 2 + SMC",
                "srtss_5_dose_update_smc" = "SV 5-dose model 2 + SMC", 
                "srtss_4_dose_param_mod_smc" = "SV 4-dose + SMC model 3", 
                "srtss_5_dose_param_mod_smc" = "SV 5-dose + SMC model 3", 
                "smc" = "SMC alone")

uncertainty_intervals_smc <- 
  impact_0_5_smc %>% 
  filter(draw > 0) %>% 
  group_by(run, pfpr, seasonality, coverage) %>% 
  summarise(lower_cases = quantile(cases_averted, probs = 0.025), 
            upper_cases = quantile(cases_averted, probs = 0.975), 
            lower_deaths = quantile(deaths_averted, probs = 0.025), 
            upper_deaths = quantile(deaths_averted, probs = 0.975), 
            med_cases = quantile(cases_averted, probs = 0.5), 
            med_deaths = quantile(deaths_averted, probs = 0.5))

#-plot bar chart fig results 1-----------------  
cases_averted2 <- 
  ggplot(impact_0_5_smc %>% filter(draw == 0),
         mapping=aes(x = factor(pfpr * 100), y = cases_averted, fill=run)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black", alpha=0.8) +
  geom_errorbar(data = uncertainty_intervals_smc,
                mapping = aes(x = factor(pfpr * 100), ymin = lower_cases, ymax = upper_cases, y=med_cases, group=run),
                width=0.4, 
                position=position_dodge(0.9), 
                stat = "identity") +
  scale_fill_manual(values=color_vals_smc, 
                    labels=labels_smc) +
  labs(x = expression(paste(italic(Pf),"PR"[2-10])), 
       y = " ",
       fill="Vaccination schedule",
       parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  xlab(expression(italic(PfPR[2-10]~(symbol("\045"))))) +
  ggtitle("C - Clinical cases averted per 100,000 children 0-5 years") +
  theme_bw() +
  theme(legend.position = "bottom", 
        title = element_text(size=10), 
        legend.text = element_text(size=9))

cases_averted2 

deaths_averted2 <- 
  ggplot(impact_0_5_smc %>% filter(draw == 0),
         mapping=aes(x = factor(pfpr * 100), y = deaths_averted, fill=run)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black", alpha=0.8) +
  geom_errorbar(data = uncertainty_intervals_smc,
                mapping = aes(x = factor(pfpr * 100), ymin = lower_deaths, ymax = upper_deaths, y=med_deaths, group=run),
                width=0.4, 
                position=position_dodge(0.9), 
                stat = "identity") +
  scale_fill_manual(values=color_vals_smc, 
                    labels=labels_smc) +
  labs(x = expression(paste(italic(Pf),"PR"[2-10])), 
       y = " ",
       fill="Vaccination schedule",
       parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  xlab(expression(italic(PfPR[2-10]~(symbol("\045"))))) +
  ggtitle("D - Deaths averted per 100,000 children 0-5 years") +
  theme_bw() +
  theme(legend.position = "bottom", 
        title = element_text(size=10), 
        legend.text = element_text(size=9))

deaths_averted2

legend_c <- cowplot::get_legend(
  cases_averted2 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

top2 <- cowplot::plot_grid(cases_averted2 + theme(legend.position="none"),
                           deaths_averted2 + theme(legend.position="none") )

row2 <- cowplot::plot_grid(top2, legend_c, ncol = 1, rel_heights = c(1, .2))

cowplot::plot_grid(row1, row2, nrow=2, rel_heights = c(0.9,1), align = "hv")

#ggsave("revisions_impact_plot_error_bars.png", dpi=600, width=12, height=8)

ggsave("Figure_3.pdf", dpi=600, width=12, height=8)

#-Age disaggregated outputs-----------------------------------------------------------------
age_disagg <- 
  impact_age %>%
  filter(age_upper <= 20) %>% 
  filter(pfpr %in% c(0.1,0.25,0.35,0.55))

age_disagg_limits <- 
  impact_age %>% 
  filter(age_upper <= 20) %>% 
  filter(draw >0) %>% 
  group_by(run, pfpr, seasonality, age_lower, age_upper) %>% 
  summarise(lower_cases = quantile(cases_averted, probs = 0.025), 
            upper_cases = quantile(cases_averted, probs = 0.975), 
            lower_deaths = quantile(deaths_averted, probs = 0.025), 
            upper_deaths = quantile(deaths_averted, probs = 0.975), 
            med_cases = quantile(cases_averted, probs = 0.5), 
            med_deaths = quantile(deaths_averted, probs = 0.5))%>% 
  filter(pfpr %in% c(0.1,0.25,0.35,0.55))

# New facet label names pfpr
pfpr.labs <- c("10%", "25%", "35%", "55%")
names(pfpr.labs) <- c("0.1", "0.25","0.35", "0.55")

# New facet label names for supp variable
run.labs <- c("AB-RTS,S model 1", "SV 4-dose model 1","SV 5-dose model 1", "SV 4-dose model 2", "SV 5-dose model 2")
names(run.labs) <- c("epi", "srtss_4_dose_original", "srtss_5_dose_original", "srtss_4_dose_update", "srtss_5_dose_update")


casesA <- 
  ggplot(age_disagg %>% filter(seasonality == "highly_seasonal", draw == 0 ), 
         aes(x = age_upper, y = cases_averted, fill = run)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black", alpha = 0.8) +
  geom_errorbar(data = age_disagg_limits %>% filter(seasonality == "highly_seasonal"),
                mapping =  aes(x = age_upper, y = med_cases, ymin = lower_cases, ymax = upper_cases),
                width=0.4, 
                position=position_dodge(0.9), 
                stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values=colour_values, 
                    labels=c("epi" = "AB-RTS,S model 1",
                             "srtss_4_dose_original" = "SV-RTS,S 4-dose model 1",
                             "srtss_5_dose_original" = "SV-RTS,S 5-dose model 1",
                             "srtss_4_dose_update" = "SV-RTS,S 4-dose model 2",
                             "srtss_5_dose_update" = "SV-RTS,S 5-dose model 2"))+
  ylab(" ") +
  xlab("Age") +
  labs(fill = "Vaccination schedule") +
  facet_grid(pfpr~run, scales = "free_y",  labeller = labeller(pfpr = pfpr.labs, run = run.labs)) +
  ggtitle("Highly Seasonal - clinical cases averted (per 100,000 population)")+
  theme_bw()

casesA 

casesB <- 
  ggplot(age_disagg %>% filter(seasonality == "seasonal", draw == 0), 
         aes(x = age_upper, y = cases_averted, fill = run)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black", alpha = 0.8) +
  geom_errorbar(data = age_disagg_limits %>% filter(seasonality == "seasonal"),
                mapping =  aes(x = age_upper, y = med_cases, ymin = lower_cases, ymax = upper_cases),
                width=0.4, 
                position=position_dodge(0.9), 
                stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values=colour_values, 
                    labels=c("epi" = "AB-RTS,S model 1",
                             "srtss_4_dose_original" = "SV-RTS,S 4-dose model 1",
                             "srtss_5_dose_original" = "SV-RTS,S 5-dose model 1",
                             "srtss_4_dose_update" = "SV-RTS,S 4-dose model 2",
                             "srtss_5_dose_update" = "SV-RTS,S 5-dose model 2"))+
  ylab(" ") +
  xlab("Age") +
  labs(fill = "Vaccination schedule") +
  facet_grid(pfpr~run, scales = "free_y",  labeller = labeller(pfpr = pfpr.labs, run = run.labs)) +
  ggtitle("Seasonal - clinical cases averted (per 100,000 population)")+
  theme_bw()+
  theme(legend.position = "none")

casesB 

deathsA <-  
  ggplot(age_disagg %>% filter(seasonality == "highly_seasonal", draw == 0), 
         aes(x = age_upper, y = deaths_averted, fill = run)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black", alpha=0.8) +
  geom_errorbar(data = age_disagg_limits %>% filter(seasonality == "highly_seasonal"),
                mapping =  aes(x = age_upper, y = med_deaths, ymin = lower_deaths, ymax = upper_deaths),
                width=0.4, 
                position=position_dodge(0.9), 
                stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values=colour_values, 
                    labels=c("epi" = "AB-RTS,S model 1",
                             "srtss_4_dose_original" = "SV-RTS,S 4-dose model 1",
                             "srtss_5_dose_original" = "SV-RTS,S 5-dose model 1",
                             "srtss_4_dose_update" = "SV-RTS,S 4-dose model 2",
                             "srtss_5_dose_update" = "SV-RTS,S 5-dose model 2"))+
  ylab(" ") +
  xlab("Age") +
  labs(fill = "Vaccination schedule") +
  facet_grid(pfpr~run, scales = "free_y",  labeller = labeller(pfpr = pfpr.labs, run = run.labs)) +
  ggtitle("Highly Seasonal - deaths averted (per 100,000 population)")+
  theme_bw()+
  theme(legend.position = "none")

deathsA

deathsB <-  
  ggplot(age_disagg %>% filter(seasonality == "seasonal", draw == 0), 
         aes(x = age_upper, y = deaths_averted, fill = run)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black", alpha=0.8) +
  geom_errorbar(data = age_disagg_limits %>% filter(seasonality == "seasonal"),
                mapping =  aes(x = age_upper, y = med_deaths, ymin = lower_deaths, ymax = upper_deaths),
                width=0.4, 
                position=position_dodge(0.9), 
                stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values=colour_values, 
                    labels=c("epi" = "AB-RTS,S model 1",
                             "srtss_4_dose_original" = "SV-RTS,S 4-dose model 1",
                             "srtss_5_dose_original" = "SV-RTS,S 5-dose model 1",
                             "srtss_4_dose_update" = "SV-RTS,S 4-dose model 2",
                             "srtss_5_dose_update" = "SV-RTS,S 5-dose model 2"))+
  ylab(" ") +
  xlab("Age") +
  labs(fill = "Vaccination schedule") +
  facet_grid(pfpr~run, scales = "free_y",  labeller = labeller(pfpr = pfpr.labs, run = run.labs)) +
  ggtitle("Seasonal - deaths averted (per 100,000 population)")+
  theme_bw() +
  theme(legend.position = "none")

deathsB

casesA + casesB + deathsA + deathsB + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom", strip.text.x = element_blank(), plot.title = element_text(size=11))

#ggsave("revisions_age_based_error_bars.png", width=16, height=9, dpi=600)

ggsave("Figure_4.pdf", width=16, height=9, dpi=600)

#-Table S12------------------------------------------------------------------------------------------------
#-table of cases averted from baseline-# 
table1 <-
  impact_0_5 %>%  
  filter(coverage %in% c(0.8, 0.0), 
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

#write.csv(table1, "ch5_results_table_1.csv")

#-lower and upper 
lower_upper <- 
  impact_0_5 %>%  
  filter(coverage %in% c(0.8, 0.0), 
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


writexl::write_xlsx(table_totals, "table_s12.xlsx")