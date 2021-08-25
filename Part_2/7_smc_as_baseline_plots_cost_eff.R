
#- cost effectiveness analysis----------------------------------------------
#- smc as baseline comparator arm ------------------------------------------

#-load outputs-----------------------------
impact <- readRDS("impact_costs_all_ages_smc_as_baseline.RDS")

# New facet label names for supp variable
seas.labs <- c("Highly Seasonal", "Seasonal")
names(seas.labs) <- c("highly_seasonal", "seasonal")

# set the colour levels 
color_vals <- c("epi_smc" = "#FF3200",
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
            "srtss_5_dose_param_mod_smc" = "SV 5-dose synergy + SMC")

#-cost per case and per daly averted--------------------------------------
figure_4_pd <-
  impact %>%
  filter(coverage %in% c(0.8), delivery_cost == 1.62) #smc and vaccine keeping

cp_plot <- function(x, title, ylab, ylimit, y){
  ggplot(x, aes(x = pfpr*100, y = {{y}}, col=run)) +
    # geom_ribbon(fill = run) +
    geom_line(size=0.8) +
    ylab(ylab) +
    xlab(expression(italic(PfPR[2-10]~(symbol("\045"))))) +
    labs(col="Vaccine schedule")+
    ylim(-10, ylimit) +
  #  scale_x_continuous(limits=c(5,65)) +
    theme_bw() +
    scale_color_manual(values=color_vals, labels = labels)+
    facet_wrap(seasonality ~ ., labeller = labeller(seasonality = seas.labs)) +
    ggtitle(title)
}

cp_plot_daly <- function(x, title, ylab, ylimit, y){
  ggplot(x, aes(x = pfpr*100, y = {{y}}, col=run)) +
    # geom_ribbon(fill = run) +
    geom_line(size=0.8) +
    ylab(ylab) +
    xlab(expression(italic(PfPR[2-10]~(symbol("\045"))))) +
    ylim(-10, ylimit) +
 #   scale_x_continuous(limits=c(5,65)) +
    labs(col="Vaccine schedule")+
    theme_bw() +
    scale_color_manual(values=color_vals,labels = labels) +
    facet_wrap(seasonality ~ ., labeller = labeller(seasonality = seas.labs)) +
    ggtitle(title)
}

cpc1 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 2.69), title = "$2", ylab = "Cost per clinical case averted ($US)", ylimit = 500, icer_case)
cpc2 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 6.52), title = "$5", ylab = "Cost per clinical case averted ($US)", ylimit = 500, icer_case)
cpc3 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 12.91), title = "$10", ylab = "Cost per clinical case averted ($US)", ylimit = 500, icer_case)

cpd1 <- cp_plot_daly(x = filter(figure_4_pd, cost_per_dose == 2.69), title = "$2", ylab = "Cost per DALY averted ($US)", ylimit = 1000, icer_daly)
cpd2 <- cp_plot_daly(x = filter(figure_4_pd, cost_per_dose == 6.52), title = "$5", ylab = "Cost per DALY averted ($US)", ylimit = 1000, icer_daly)
cpd3 <- cp_plot_daly(x = filter(figure_4_pd, cost_per_dose == 12.91), title = "$10", ylab = "Cost per DALY averted ($US)", ylimit = 1000, icer_daly)

cp_plots <- (cpc1 | cpc2 | cpc3) / (cpd1 | cpd2 | cpd3) + plot_layout(guides = "collect") + 
  plot_annotation(title = "B") &
  theme(legend.position = "bottom")

cp_plots

ggsave("cost_per_case_daly_averted_relative_to_smc.png", cp_plots, height = 7, width = 10)

# only $5 per dose for main text 
cpc2 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 6.52), title = "$5", ylab = "Cost per clinical case averted ($US)", ylimit = 300, icer_case)
cpd2 <- cp_plot_daly(x = filter(figure_4_pd, cost_per_dose == 6.52), title = "$5", ylab = "Cost per DALY averted ($US)", ylimit = 800, icer_daly)

plog <- 
  cpc2 + cpd2 + plot_layout(guides = "collect") +
  plot_annotation(title = "B") &
  theme(legend.position = "bottom")

plog 

ggsave("cost_per_case_daly_averted_5_relative_to_SMC.png", plog, height = 4.25, width = 10)


# for the table pfpr 10 - 50 %
# average SV cost per case and DALY averted 
lol <- 
  impact %>% 
  filter(run %in% c("srtss_4_dose_original_smc", "srtss_4_dose_update_smc", "srtss_4_dose_param_mod_smc",
                    "srtss_5_dose_original_smc", "srtss_5_dose_update_smc", "srtss_5_dose_param_mod_smc")) %>% 
  filter(pfpr >= 0.1, pfpr <0.5) %>% 
  filter( coverage == 0.8) %>% 
  group_by(cost_per_dose, delivery_cost, run) %>% 
  summarise(case = mean(icer_case), 
            daly = mean(icer_daly))

# average EPI cost per case and DALY averted 
pol <-
  impact %>% 
  filter(run == "epi_smc") %>% 
  filter(pfpr >= 0.1, pfpr <0.5) %>% 
  filter( coverage == 0.8) %>% 
  group_by(cost_per_dose, delivery_cost, run) %>% 
  summarise(case = mean(icer_case), 
            daly = mean(icer_daly))

