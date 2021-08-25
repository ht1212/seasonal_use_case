
#-cost per case and per daly averted--------------------------------------
figure_4_pd <-
  impact %>%
  filter(coverage == 0.8, delivery_cost == 1.62)

cp_plot <- function(x, title, ylab, ylimit, y){
  ggplot(x, aes(x = pfpr*100, y = {{y}}, col=run)) +
    # geom_ribbon(fill = run) +
    geom_line(size=0.8) +
    ylab(ylab) +
    xlab(expression(italic(PfPR[2-10]~(symbol("\045"))))) +
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

cp_plot_daly <- function(x, title, ylab, ylimit, y){
  ggplot(x, aes(x = pfpr*100, y = {{y}}, col=run)) +
    # geom_ribbon(fill = run) +
    geom_line(size=0.8) +
    ylab(ylab) +
    xlab(expression(italic(PfPR[2-10]~(symbol("\045"))))) +
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

cpc1 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 2.69), title = "$2", ylab = "Cost per clinical case averted ($US)", ylimit = 350, icer_case)
cpc2 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 6.52), title = "$5", ylab = "Cost per clinical case averted ($US)", ylimit = 350, icer_case)
cpc3 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 12.91), title = "$10", ylab = "Cost per clinical case averted ($US)", ylimit = 350, icer_case)

cpd1 <- cp_plot_daly(x = filter(figure_4_pd, cost_per_dose == 2.69), title = "$2", ylab = "Cost per DALY averted ($US)", ylimit = 650, icer_daly)
cpd2 <- cp_plot_daly(x = filter(figure_4_pd, cost_per_dose == 6.52), title = "$5", ylab = "Cost per DALY averted ($US)", ylimit = 650, icer_daly)
cpd3 <- cp_plot_daly(x = filter(figure_4_pd, cost_per_dose == 12.91), title = "$10", ylab = "Cost per DALY averted ($US)", ylimit = 650,icer_daly)

cp_plots <- (cpc1 | cpc2 | cpc3) / (cpd1 | cpd2 | cpd3) + plot_layout(guides = "collect") +
  plot_annotation(title = "A") &
  theme(legend.position = "bottom")

cp_plots 

ggsave("cost_per_case_daly_averted.png", cp_plots, height = 7, width = 10)

# only $5 per dose for main text 
cpc2 <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 6.52), title = "$5", ylab = "Cost per clinical case averted ($US)", ylimit = 200, icer_case)
cpd2 <- cp_plot_daly(x = filter(figure_4_pd, cost_per_dose == 6.52), title = "$5", ylab = "Cost per DALY averted ($US)", ylimit = 500, icer_daly)

plog <- 
  cpc2 + cpd2 + plot_layout(guides = "collect") +
  plot_annotation(title = "A") &
  theme(legend.position = "bottom")

plog 

ggsave("cost_per_case_daly_averted_5.png", plog, height = 4, width = 10)

# average SV cost per case and DALY averted 
lol <- 
  impact %>% 
  filter(run != "epi") %>% 
  filter(pfpr >= 0.1, pfpr < 0.5) %>% 
  filter( coverage == 0.8) %>% 
  group_by(cost_per_dose, delivery_cost, run) %>% 
  summarise(case = mean(icer_case), 
            daly = mean(icer_daly))

# average EPI cost per case and DALY averted 
pol <-
  impact %>% 
  filter(run == "epi") %>% 
  filter(pfpr >= 0.1, pfpr < 0.5) %>% 
  filter( coverage == 0.8) %>% 
  group_by(cost_per_dose, delivery_cost, run) %>% 
  summarise(case = mean(icer_case), 
            daly = mean(icer_daly))



#-lower delivery cost---------
figure_4_pd <-
  impact %>%
  filter(coverage == 0.8, delivery_cost == 0.96)

cpc1l <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 2.69), title = "$2", ylab = "Cost per clinical case averted ($US)", ylimit = 350, icer_case)
cpc2l <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 6.52), title = "$5", ylab = "Cost per clinical case averted ($US)", ylimit = 350, icer_case)
cpc3l <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 12.91), title = "$10", ylab = "Cost per clinical case averted ($US)", ylimit = 350, icer_case)


cpd1l <- cp_plot_daly(x = filter(figure_4_pd, cost_per_dose == 2.69), title = "$2", ylab = "Cost per DALY averted ($US)", ylimit = 650, icer_daly)
cpd2l <- cp_plot_daly(x = filter(figure_4_pd, cost_per_dose == 6.52), title = "$5", ylab = "Cost per DALY averted ($US)", ylimit = 650, icer_daly)
cpd3l <- cp_plot_daly(x = filter(figure_4_pd, cost_per_dose == 12.91), title = "$10", ylab = "Cost per DALY averted ($US)", ylimit = 650, icer_daly)

cp_plotsl <- (cpc1l | cpc2l | cpc3l) / (cpd1l | cpd2l | cpd3l) + plot_layout(guides = "collect")

cp_plotsl 

ggsave("cost_per_case_daly_averted_lower.png", cp_plotsl, height = 6, width = 12)

#-upper delivery cost----------
figure_4_pd <-
  impact %>%
  filter(coverage == 0.8, delivery_cost == 2.67)

cpc1h <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 2.69), title = "$2", ylab = "Cost per clinical case averted ($US)", ylimit = 350, icer_case)
cpc2h <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 6.52), title = "$5", ylab = "Cost per clinical case averted ($US)", ylimit = 350, icer_case)
cpc3h <- cp_plot(x = filter(figure_4_pd, cost_per_dose == 12.91), title = "$10", ylab = "Cost per clinical case averted ($US)", ylimit = 350, icer_case)


cpd1h <- cp_plot_daly(x = filter(figure_4_pd, cost_per_dose == 2.69), title = "$2", ylab = "Cost per DALY averted ($US)", ylimit = 650, icer_daly)
cpd2h <- cp_plot_daly(x = filter(figure_4_pd, cost_per_dose == 6.52), title = "$5", ylab = "Cost per DALY averted ($US)", ylimit = 650, icer_daly)
cpd3h <- cp_plot_daly(x = filter(figure_4_pd, cost_per_dose == 12.91), title = "$10", ylab = "Cost per DALY averted ($US)", ylimit = 650, icer_daly)

cp_plotsh <- (cpc1h | cpc2h | cpc3h) / (cpd1h | cpd2h | cpd3h) + plot_layout(guides = "collect")

cp_plotsh 

ggsave("cost_per_case_daly_averted_higher.png", cp_plotsh, height = 6, width = 12)

#-Testing % increases in delivery costs of seasonal vaccination only--------------------------------
# cost_per_dose <- c(2.69,6.52,12.91) #cost per vaccine - think this should include the cost of wastage etc $2,$5,$10 per dose 
# delivery_cost <- c(0.96, 1.62 * 5, 2.67)  #scaled delivery costs 
# 
# tx_unit_cost  <- 1.47 
# severe_unit_cost <- 22.41 
# 
# cost_df <- expand_grid(cost_per_dose = cost_per_dose, delivery_cost = delivery_cost)
# 
# #-age aggregated-----------------------------------------------------------
# impact_costs_sv_increase <- 
#   impact_all %>% 
#   filter(run != "epi") %>% 
#   merge(cost_df) %>% 
#   # setting specific costs
#   mutate(vaccine_cost   = num_vacc_doses * (cost_per_dose + delivery_cost),
#          tx_cost        = (num_trt) * tx_unit_cost,
#          tx_cost_bl     = (num_trt_bl) * tx_unit_cost,
#          severe_cost    = hospitalisations * severe_unit_cost,
#          severe_cost_bl = hospitalisations_bl * severe_unit_cost,
#          cost           = vaccine_cost + tx_cost + severe_cost,
#          cost_bl        = tx_cost_bl + severe_cost_bl,
#          marginal_cost  = cost - cost_bl) %>% 
#   # incremental costs 
#   mutate(
#     icer_case      = marginal_cost / cases_averted,
#     icer_daly      = marginal_cost / dalys_averted,
#     icer_disc_daly = marginal_cost / disc_dalys_averted
#   )
# 
# # epi 
# epi <- 
#   figure_4_pd %>%
#   filter(run == "epi") %>% 
#   filter(coverage == 0.8, delivery_cost == 1.62)
# 
# # sv 
# sv <- 
#   impact_costs_sv_increase %>% 
#   filter(coverage == 0.8, delivery_cost == 1.62 * 5)
# 
# increase <- 
#   bind_rows(epi, sv)
# 
# cpc1h <- cp_plot(x = filter(increase, cost_per_dose == 2.69), title = "$2", ylab = "Cost per clinical case averted ($US)", ylimit = 350, icer_case)
# cpc2h <- cp_plot(x = filter(increase, cost_per_dose == 6.52), title = "$5", ylab = "Cost per clinical case averted ($US)", ylimit = 350, icer_case)
# cpc3h <- cp_plot(x = filter(increase, cost_per_dose == 12.91), title = "$10", ylab = "Cost per clinical case averted ($US)", ylimit = 350, icer_case)
# 
# cpc1h <- cp_plot_daly(x = filter(increase, cost_per_dose == 2.69), title = "$2", ylab = "Cost per clinical DALY averted ($US)", ylimit = 550, icer_daly)
# cpc2h <- cp_plot_daly(x = filter(increase, cost_per_dose == 6.52), title = "$5", ylab = "Cost per clinical DALY averted ($US)", ylimit = 550, icer_daly)
# cpc3h <- cp_plot_daly(x = filter(increase, cost_per_dose == 12.91), title = "$10", ylab = "Cost per clinical DALY averted ($US)", ylimit = 550, icer_daly)
# 
# cp_plotsh <- (cpc1h | cpc2h | cpc3h) / (cpd1h | cpd2h | cpd3h) + plot_layout(guides = "collect")
# 
# cp_plotsh 
