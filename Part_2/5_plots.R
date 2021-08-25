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

#-load outputs-----------------------------
impact <- readRDS("impact_costs_all_ages.RDS")
impact_age <- readRDS("impact_costs_age_groups.RDS")

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

labels <- c("epi_smc" = "EPI + SMC", 
            "srtss_4_dose_original_smc" = "SV 4-dose + SMC",
            "srtss_5_dose_original_smc" = "SV 5-dose + SMC",
            "srtss_4_dose_update_smc" = "SV 4-dose - updated booster + SMC",
            "srtss_5_dose_update_smc" = "SV 5-dose - updated booster + SMC", 
            "srtss_4_dose_param_mod_smc" = "SV 4-dose synergy + SMC", 
            "srtss_5_dose_param_mod_smc" = "SV 5-dose synergy + SMC", 
            "smc" = "SMC alone")

#-plot impact averted-----------------------
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
  
ca_fvp <- 
    ggplot(figure_2_pd %>% filter(run != "smc"), 
           aes(x = factor(pfpr * 100), y = cases_averted_per_100000_fvp, fill=run)) +
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
    # ylab("Clinical cases averted\n(per 100,000 fully vaccinated children)") +
    xlab(expression(italic(PfPR[2-10]~(symbol("\045"))))) +
    ggtitle("B - Clinical cases averted (per 100,000 fully vaccinated children)") +
    theme_bw( )
  
ca_fvp
  
averted <- ca / ca_fvp + plot_layout(guides = "collect")

averted
  
ggsave("cases_averted_1.png", averted, width=14, height = 8.5)
  

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
  
da_fvp <- 
    ggplot(figure_2_pd %>% filter(run !="smc"), aes(x = factor(pfpr * 100), y = deaths_averted_per_100000_fvp, fill=run)) +
    geom_hline(yintercept = 0) +
    geom_bar(position=position_dodge(), stat="identity",colour="black") +
    scale_fill_ghibli_d("MononokeMedium", direction = -1, 
                        labels=c("epi_smc" = "EPI + SMC", 
                                 "srtss_4_dose_original_smc" = "SV 4-dose + SMC",
                                 "srtss_5_dose_original_smc" = "SV 5-dose + SMC",
                                 "srtss_4_dose_update_smc" = "SV 4-dose - updated booster + SMC",
                                 "srtss_5_dose_update_smc" = "SV 5-dose - updated booster + SMC", 
                                 "srtss_4_dose_param_mod_smc" = "SV 4-dose synergy + SMC", 
                                 "srtss_5_dose_param_mod_smc" = "SV 5-dose synergy + SMC")) + 
    labs(x = expression(paste(italic(Pf),"PR"[2-10])), 
         y = " ",
         fill="Vaccination schedule",
         parse=TRUE) + 
    scale_y_continuous(labels = comma) + 
    facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
    # ylab("Deaths averted\n(per 100,000 fully vaccinated children)") +
    xlab(expression(PfPr[2-10]~(symbol("\045")))) +
    ggtitle("B - Deaths averted (per 100,000 fully vaccinated children)") +
    theme_bw( )
  
da_fvp
 
report_fig <- ca / da + plot_layout(guides = "collect") & theme(legend.position = "right")
  
report_fig
  
ggsave("report_fig_main.png", report_fig)#, width=8, height = 5)
  

#-age disaggregated plots------------------------------------------
age_pd <- 
    impact_age %>%
    filter(age_upper <= 20) %>% 
    filter(run %in% c("smc",
                      "epi_smc", 
                      "srtss_4_dose_original_smc", "srtss_4_dose_update_smc", "srtss_4_dose_param_mod_smc",
                      "srtss_5_dose_original_smc", "srtss_5_dose_update_smc", "srtss_5_dose_param_mod_smc")) %>% 
    filter(coverage %in% c(0,0.8), cost_per_dose == 6.52, delivery_cost == 1.62)
  
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
  
#ggsave("ca_age_hs.png",ca_ageA, width=12, height=6)
  
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
  
#ggsave("ca_age_s.png",ca_ageB, width=12, height=6)
  
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
  
#ggsave("da_age_hs.png",da_ageA, width=12, height=6)
  
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
  
ggsave("combined_ages.png",combined_ages, width=19, height=10)
  
