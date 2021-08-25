
#-Impact figures----------------------------
# Load packages
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(patchwork)
library(ghibli)
library(scales)
library(LaCroixColoR)

#-load outputs-------------------------------
impact <- readRDS("impact_costs_all_ages.RDS")
impact_age <- readRDS("impact_costs_age_groups.RDS")

#-for presentation plots--------------------
saveRDS(impact, "non_smc_impact.RDS")
saveRDS(impact_age, "non_smc_impact_age.RDS")

#-New facet label names for sease variables--
seas.labs <- c("Highly Seasonal", "Seasonal")
names(seas.labs) <- c("highly_seasonal", "seasonal")

#-impact by transmission level--------------
figure_2_pd <- 
  impact %>%
  #main results coverage and costs
  filter(coverage == 0.8, cost_per_dose == 6.52, delivery_cost == 1.62) %>% 
  #filtering down pfpr to be clearer 
  filter(pfpr %in% c(0.1,0.25,0.35,0.55))

#-plot impact averted-----------------------

#-per population---------------------------- 
ca <-  
  ggplot(figure_2_pd, aes(x = factor(pfpr * 100), y = cases_averted, fill=run)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  scale_fill_manual(values=lacroix_palette("PeachPear"), 
                    labels=c("epi" = "EPI",
                             "srtss_4_dose_original" = "SV 4-dose",
                             "srtss_5_dose_original" = "SV 5-dose",
                             "srtss_4_dose_update" = "SV 4-dose - updated booster",
                             "srtss_5_dose_update" = "SV 5-dose - updated booster"))+
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

#-per 100,000 fully vaccinated people----------
ca_fvp <- 
  ggplot(figure_2_pd, aes(x = factor(pfpr * 100), y = cases_averted_per_100000_fvp, fill=run)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  scale_fill_manual(values=lacroix_palette("PeachPear"), 
                    labels=c("epi" = "EPI",
                             "srtss_4_dose_original" = "SV 4-dose",
                             "srtss_5_dose_original" = "SV 5-dose",
                             "srtss_4_dose_update" = "SV 4-dose - updated booster",
                             "srtss_5_dose_update" = "SV 5-dose - updated booster"))+
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

averted <- ca / ca_fvp  + plot_layout(guides = "collect") & theme(legend.position = "bottom")

averted

#ggsave("cases_averted_cov_0.9.png", averted, width=9.4, height = 7.3)

#-deaths averted per population--------------
da <-  
  ggplot(figure_2_pd, aes(x = factor(pfpr * 100), y = deaths_averted, fill=run)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  scale_fill_manual(values=lacroix_palette("PeachPear"), 
                    labels=c("epi" = "EPI",
                             "srtss_4_dose_original" = "SV 4-dose",
                             "srtss_5_dose_original" = "SV 5-dose",
                             "srtss_4_dose_update" = "SV 4-dose - updated booster",
                             "srtss_5_dose_update" = "SV 5-dose - updated booster"))+
  labs(x = expression(paste(italic(Pf),"PR"[2-10])), 
       y = " ",
       fill="Vaccination schedule",
       parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  xlab(expression(italic(PfPR[2-10]~(symbol("\045"))))) +
  ggtitle("C - Deaths averted per 100,000 population") +
  theme_bw( )

da 

#-deaths averted per 100,000 fully vaccinated-------
da_fvp <- 
  ggplot(figure_2_pd, aes(x = factor(pfpr * 100), y = deaths_averted_per_100000_fvp, fill=run)) +
  geom_hline(yintercept = 0) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  scale_fill_manual(values=lacroix_palette("PeachPear"), 
                    labels=c("epi" = "EPI",
                             "srtss_4_dose_original" = "SV 4-dose",
                             "srtss_5_dose_original" = "SV 5-dose",
                             "srtss_4_dose_update" = "SV 4-dose - updated booster",
                             "srtss_5_dose_update" = "SV 5-dose - updated booster"))+
  labs(x = expression(paste(italic(Pf),"PR"[2-10])), 
       y = " ",
       fill="Vaccination schedule",
       parse=TRUE) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(seasonality  ~ ., labeller = labeller(seasonality = seas.labs)) +
  xlab(expression(italic(PfPR[2-10]~(symbol("\045"))))) +
  ggtitle("D - Deaths averted (per 100,000 fully vaccinated children)") +
  theme_bw( )

da_fvp

#-combining outputs into single figure
main_fig <- ca + ca_fvp + da + da_fvp + plot_layout(guides = "collect") & theme(legend.position = "bottom") 

main_fig

ggsave("combined_averted.png", main_fig, width=12, height = 7)


#-cost per case and per daly averted--------------------------------------
figure_4_pd <-
  impact %>%
  filter(coverage == 0.8, delivery_cost == 1.62)

cp_plot <- function(x, title, ylab, ylimit, y){
  ggplot(x, aes(x = pfpr*100, y = {{y}}, col=run)) +
   # geom_ribbon(fill = run) +
    geom_line(size=0.8, alpha=0.8) +
    ylab(ylab) +
    xlab(expression(PfPr[2-10]~(symbol("\045")))) +
    labs(col="Vaccine schedule")+
    ylim(-10, ylimit) +
    theme_bw() +
    scale_color_manual(values=lacroix_palette("PeachPear"), 
                      labels=c("epi" = "EPI",
                               "srtss_4_dose_original" = "SV 4-dose",
                               "srtss_5_dose_original" = "SV 5-dose",
                               "srtss_4_dose_update" = "SV 4-dose - updated booster",
                               "srtss_5_dose_update" = "SV 5-dose - updated booster"))+
    facet_wrap(seasonality ~ ., labeller = labeller(seasonality = seas.labs)) +
    ggtitle(title)
}

cpc1 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 2.69), title = "$2", ylab = "Cost per clinical case averted ($US)", ylimit = 350, icer_case)
cpc2 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 6.52), title = "$5", ylab = "Cost per clinical case averted ($US)", ylimit = 350, icer_case)
cpc3 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 12.91), title = "$10", ylab = "Cost per clinical case averted ($US)", ylimit = 350, icer_case)

cp_plot <- function(x, title, ylab, ylimit, y){
  ggplot(x, aes(x = pfpr*100, y = {{y}}, col=run)) +
    # geom_ribbon(fill = run) +
    geom_line(size=0.8, alpha=0.8) +
    ylab(ylab) +
    xlab(expression(PfPr[2-10]~(symbol("\045")))) +
    ylim(-10, ylimit) +
    labs(col="Vaccine schedule")+
    theme_bw() +
    scale_color_manual(values=lacroix_palette("PeachPear"), 
                       labels=c("epi" = "EPI",
                                "srtss_4_dose_original" = "SV 4-dose",
                                "srtss_5_dose_original" = "SV 5-dose",
                                "srtss_4_dose_update" = "SV 4-dose - updated booster",
                                "srtss_5_dose_update" = "SV 5-dose - updated booster"))+
    facet_wrap(seasonality ~ ., labeller = labeller(seasonality = seas.labs)) +
    ggtitle(title)
}

cpd1 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 2.69), title = "$2", ylab = "Cost per DALY averted ($US)", ylimit = 650, icer_daly)
cpd2 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 6.52), title = "$5", ylab = "Cost per DALY averted ($US)", ylimit = 650, icer_daly)
cpd3 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 12.91), title = "$10", ylab = "Cost per DALY averted ($US)", ylimit = 650, icer_daly)

cp_plots <- (cpc1 | cpc2 | cpc3) / (cpd1 | cpd2 | cpd3) + plot_layout(guides = "collect")

cp_plots 

ggsave("cost_per_case_daly_averted.png", cp_plots, height = 6, width = 12)


#-age disaggregated plots------------------------------------------
age_pd <- 
  impact_age %>%
  filter(age_upper <= 20, coverage == 0.8, cost_per_dose == 6.52, delivery_cost == 1.62) 

# New facet label names pfpr
pfpr.labs <- c("10%",  "25%", "35%", "55%")
names(pfpr.labs) <- c("0.1", "0.25","0.35","0.55")

# New facet label names for supp variable
run.labs <- c("EPI", "SV 4-dose","SV 5-dose", "SV 4-dose - updated booster", "SV 5-dose - updated booster")
names(run.labs) <- c("epi", "srtss_4_dose_original", "srtss_5_dose_original", "srtss_4_dose_update", "srtss_5_dose_update")


ca_ageA <- 
  ggplot(age_pd %>% filter(seasonality == "highly_seasonal") %>% filter(pfpr %in% c(0.1, 0.25,0.35,0.55)), 
         aes(x = age_upper, y = cases_averted, fill = run)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #geom_linerange(col = "grey70") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values=lacroix_palette("PeachPear"), 
                    labels=c("epi" = "EPI",
                             "srtss_4_dose_original" = "SV 4-dose",
                             "srtss_5_dose_original" = "SV 5-dose",
                             "srtss_4_dose_update" = "SV 4-dose - updated booster",
                             "srtss_5_dose_update" = "SV 5-dose - updated booster"))+
  ylab(" ") +
  xlab("Age") +
  labs(fill = "Vaccination schedule") +
  facet_grid(pfpr~run, scales = "free_y",  labeller = labeller(pfpr = pfpr.labs, run = run.labs)) +
  ggtitle("Highly Seasonal - clinical cases averted (per 100,000 population)")+
  theme_bw()

ca_ageA 

#ggsave("ca_age_hs.png",ca_ageA, width=12, height=6)

ca_ageB <- 
  ggplot(age_pd %>% filter(seasonality == "seasonal")  %>% filter(pfpr %in% c(0.1, 0.25,0.35,0.55)), 
         aes(x = age_upper, y = cases_averted, fill = run)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #geom_linerange(col = "grey70") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values=lacroix_palette("PeachPear"), 
                    labels=c("epi" = "EPI",
                             "srtss_4_dose_original" = "SV 4-dose",
                             "srtss_5_dose_original" = "SV 5-dose",
                             "srtss_4_dose_update" = "SV 4-dose - updated booster",
                             "srtss_5_dose_update" = "SV 5-dose - updated booster"))+
  ylab(" ") +
  xlab("Age") +
  labs(fill = "Vaccination schedule") +
  facet_grid(pfpr~run, scales = "free_y",  labeller = labeller(pfpr = pfpr.labs, run = run.labs)) +
  ggtitle("Seasonal - clinical cases averted (per 100,000 population)")+
  theme_bw()

ca_ageB 

#ggsave("ca_age_s.png",ca_ageB, width=12, height=6)

#-Deaths averted------------------------------------------------------------------------------------------------
da_ageA <- 
  ggplot(age_pd %>% filter(seasonality == "highly_seasonal") %>% filter(pfpr %in% c(0.1, 0.25,0.35,0.55)), 
         aes(x = age_upper, y = deaths_averted, fill = run)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #geom_linerange(col = "grey70") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values=lacroix_palette("PeachPear"), 
                    labels=c("epi" = "EPI",
                             "srtss_4_dose_original" = "SV 4-dose",
                             "srtss_5_dose_original" = "SV 5-dose",
                             "srtss_4_dose_update" = "SV 4-dose - updated booster",
                             "srtss_5_dose_update" = "SV 5-dose - updated booster"))+
  ylab(" ") +
  xlab("Age") +
  labs(fill = "Vaccination schedule") +
  facet_grid(pfpr~run, scales = "free_y",  labeller = labeller(pfpr = pfpr.labs, run = run.labs)) +
  ggtitle("Highly Seasonal - deaths averted (per 100,000 population)")+
  theme_bw()

da_ageA 

#ggsave("da_age_hs.png",da_ageA, width=12, height=6)

da_ageB <- 
  ggplot(age_pd %>% filter(seasonality == "seasonal") %>% filter(pfpr %in% c(0.1, 0.25,0.35,0.55)), 
         aes(x = age_upper, y = deaths_averted, fill = run)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #geom_linerange(col = "grey70") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values=lacroix_palette("PeachPear"), 
                    labels=c("epi" = "EPI",
                             "srtss_4_dose_original" = "SV 4-dose",
                             "srtss_5_dose_original" = "SV 5-dose",
                             "srtss_4_dose_update" = "SV 4-dose - updated booster",
                             "srtss_5_dose_update" = "SV 5-dose - updated booster"))+
  ylab(" ") +
  xlab("Age") +
  labs(fill = "Vaccination schedule") +
  facet_grid(pfpr~run, scales = "free_y",  labeller = labeller(pfpr = pfpr.labs, run = run.labs)) +
  ggtitle("Seasonal - deaths averted (per 100,000 population)")+
  theme_bw()

da_ageB 

#ggsave("da_age_s.png",da_ageB, width=12, height=6)

combined_ages <- 
  ca_ageA + ca_ageB + da_ageA + da_ageB + plot_layout(guides = "collect") & theme(legend.position = "bottom", strip.text.x = element_blank())

combined_ages

ggsave("combined_ages.png",combined_ages, width=16, height=9)


