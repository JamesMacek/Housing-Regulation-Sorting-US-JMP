#PACKAGES
library(sf)
library(haven) #Reading stata.dta files 
library(dplyr)
library(readr)
library(collapse)
library(stringr)
library(labelled)
library(ggplot2)
library(patchwork) #combining plots
library(estimatr)

#gam package (generalized additive models) for Fact1
library(mgcv)
library(gratia) #for helping to plot these

options(scipen = 5) #limit scientific

#Date created: August 26th, 2022. 
#This file does robustness checks for facts.

#importing data
load(file = "DataV2/US_Data/Output/Constructed_Block_V2.Rdata")
load(file = "DataV2/US_Data/Output/CBSA_med_house_price.Rdata")
load(file = "DataV2/US_Data/Output/CBSA_quantiles.Rdata")
load(file = "DataV2/US_Data/Output/CBSA_quantiles_dens.Rdata")

#
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(citywt = 1/n())

#Formulas 
controls <- "+ demeaned_median_bage + demeaned_household_size + demeaned_car_share + demeaned_family_share + demeaned_car_transport_share + 
               demeaned_public_transport_share + demeaned_avg_travel_time + 
               demeaned_perf_arts_dens + demeaned_spec_sports_dens + 
               demeaned_casino_dens + demeaned_rec_act_dens + demeaned_prop_park_area_tract + 
               demeaned_count_tri_facilities + demeaned_stops_per_sqmile + demeaned_frestaurant_dens + 
               demeaned_fastfood_dens + demeaned_coffee_dens + demeaned_bar_dens + 
               demeaned_perennial_snow + demeaned_deciduous_forest + demeaned_evergreen_forest +
               demeaned_mixed_forest + demeaned_shrubs + demeaned_herbaceous + demeaned_woody_wetlands + 
               demeaned_herbaceous_wetlands"

excluded_controls_income_formula <- as.formula('demeaned_log_Income ~ s(rank_density_CBSA, k = 5, bs = "cr")')

included_controls_income_formula <- as.formula(paste('demeaned_log_Income ~ s(rank_density_CBSA, k = 5, bs = "cr")', controls, sep = " "))

excluded_controls_stringency_formula <- as.formula('demeaned_stringency ~ s(rank_density_CBSA, k = 5, bs = "cr")')

included_controls_stringency_formula <- as.formula(paste('demeaned_stringency ~ s(rank_density_CBSA, k = 5, bs = "cr")', controls, sep = " "))

#Robustness 1: Different sample definitions of cities
reg_t10 <- gam(formula = included_controls_income_formula,
               data = US_BLOCK[US_BLOCK$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),])

reg_b10 <- gam(formula = included_controls_income_formula,
               data = US_BLOCK[US_BLOCK$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),])

t10_smooth <- smooth_estimates(reg_t10, n = 1000) %>%
  add_confint()  #from gratia, extracts gam estimates + add confidence intervals
b10_smooth <- smooth_estimates(reg_b10, n = 1000) %>%
  add_confint()

ggplot() +
  geom_ribbon(data = t10_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
  geom_line(data = t10_smooth, aes(x = rank_density_CBSA, y = est, color = 'Top 10%')) + 
  
  geom_ribbon(data = b10_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = b10_smooth, aes(x = rank_density_CBSA, y = est, color = 'Bot 10%')) + 
  scale_colour_manual(name="Sample", values = c("red","blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Log Average Income (demeaned by MSA, residualized by controls)") +
  ggtitle("Top10/Bot10")

#Top50/Bot50
reg_t50<- gam(formula = included_controls_income_formula,
               data = US_BLOCK[US_BLOCK$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["50.0%"]),])

reg_b50 <- gam(formula = included_controls_income_formula,
               data = US_BLOCK[US_BLOCK$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["50.0%"]),])

t50_smooth <- smooth_estimates(reg_t50, n = 1000) %>%
  add_confint()  #from gratia, extracts gam estimates + add confidence intervals
b50_smooth <- smooth_estimates(reg_b50, n = 1000) %>%
  add_confint()

ggplot() +
  geom_ribbon(data = t50_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
  geom_line(data = t50_smooth, aes(x = rank_density_CBSA, y = est, color = 'Top 50%')) + 
  
  geom_ribbon(data = b50_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = b50_smooth, aes(x = rank_density_CBSA, y = est, color = 'Bot 50%')) + 
  scale_colour_manual(name="Sample", values = c("red","blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Log Average Income (demeaned by MSA, residualized by controls)") +
  ggtitle("Top50/Bot50")

#Repeating for stringency
reg_t10 <- gam(formula = excluded_controls_stringency_formula,
               data = US_BLOCK[US_BLOCK$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),])

reg_b10 <- gam(formula = excluded_controls_stringency_formula,
               data = US_BLOCK[US_BLOCK$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),])

t10_smooth <- smooth_estimates(reg_t10, n = 1000) %>%
  add_confint()  #from gratia, extracts gam estimates + add confidence intervals
b10_smooth <- smooth_estimates(reg_b10, n = 1000) %>%
  add_confint()

ggplot() +
  geom_ribbon(data = t10_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
  geom_line(data = t10_smooth, aes(x = rank_density_CBSA, y = est, color = 'Top 10%')) + 
  
  geom_ribbon(data = b10_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = b10_smooth, aes(x = rank_density_CBSA, y = est, color = 'Bot 10%')) + 
  scale_colour_manual(name="Sample", values = c("red","blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Income Stringency of Density Restrictions (demeaned by MSA)") +
  ggtitle("Top10/Bot10")

#Top50/Bot50
reg_t50<- gam(formula = excluded_controls_stringency_formula,
              data = US_BLOCK[US_BLOCK$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["50.0%"]),])

reg_b50 <- gam(formula = excluded_controls_stringency_formula,
               data = US_BLOCK[US_BLOCK$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["50.0%"]),])

t50_smooth <- smooth_estimates(reg_t50, n = 1000) %>%
  add_confint()  #from gratia, extracts gam estimates + add confidence intervals
b50_smooth <- smooth_estimates(reg_b50, n = 1000) %>%
  add_confint()

ggplot() +
  geom_ribbon(data = t50_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
  geom_line(data = t50_smooth, aes(x = rank_density_CBSA, y = est, color = 'Top 50%')) + 
  
  geom_ribbon(data = b50_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = b50_smooth, aes(x = rank_density_CBSA, y = est, color = 'Bot 50%')) + 
  scale_colour_manual(name="Sample", values = c("red","blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Income Stringency of Density Restrictions (demeaned by MSA)") +
  ggtitle("Top50/Bot50")

#With controls (still holds)
reg_t50<- gam(formula = included_controls_stringency_formula,
              data = US_BLOCK[US_BLOCK$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["50.0%"]),])

reg_b50 <- gam(formula = included_controls_stringency_formula,
               data = US_BLOCK[US_BLOCK$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["50.0%"]),])

t50_smooth <- smooth_estimates(reg_t50, n = 1000) %>%
  add_confint()  #from gratia, extracts gam estimates + add confidence intervals
b50_smooth <- smooth_estimates(reg_b50, n = 1000) %>%
  add_confint()

ggplot() +
  geom_ribbon(data = t50_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
  geom_line(data = t50_smooth, aes(x = rank_density_CBSA, y = est, color = 'Top 50%')) + 
  
  geom_ribbon(data = b50_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = b50_smooth, aes(x = rank_density_CBSA, y = est, color = 'Bot 50%')) + 
  scale_colour_manual(name="Sample", values = c("red","blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  labs(y = paste0("Income Stringency of Density Restrictions", "\n", "(demeaned by MSA, residualized by controls)")) +
  ggtitle("Top50/Bot50")


#Robustness 2: Alternative definitions of weights. 

reg_t25 <- gam(formula = included_controls_income_formula,
               data = US_BLOCK[US_BLOCK$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),],
               weights = citywt)

reg_b25 <- gam(formula = included_controls_income_formula,
               data = US_BLOCK[US_BLOCK$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),],
               weights = citywt)

#Plotting manually with GGPlot2 (Extracting partially linear plot using gratia package)
t25_smooth <- smooth_estimates(reg_t25, n = 1000) %>%
  add_confint()  #from gratia, extracts gam estimates + add confidence intervals
b25_smooth <- smooth_estimates(reg_b25, n = 1000) %>%
  add_confint()

#Plotting these residualized plots
ggplot() +
  geom_ribbon(data = t25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
  geom_line(data = t25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Top 25%')) + 
  
  geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = b25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Bot 25%')) + 
  scale_colour_manual(name="Sample", values = c("red","blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Log Average Income (demeaned by MSA, residualized by controls)") + 
  ggtitle("Cities weighted equally")

#Linear regressions 
linear_income_formula <- as.formula('demeaned_log_Income ~ rank_density_CBSA + 
                                        demeaned_median_bage + 
                                        demeaned_household_size + demeaned_car_share + demeaned_family_share + 
                                        demeaned_car_transport_share + demeaned_public_transport_share + 
                                        demeaned_avg_travel_time + demeaned_perf_arts_dens + demeaned_spec_sports_dens + 
                                        demeaned_casino_dens + demeaned_rec_act_dens + demeaned_prop_park_area_tract + 
                                        demeaned_count_tri_facilities + demeaned_stops_per_sqmile + 
                                        demeaned_frestaurant_dens + demeaned_fastfood_dens + demeaned_coffee_dens +
                                        demeaned_bar_dens')
linear_stringency_formula <- as.formula('demeaned_stringency ~ rank_density_CBSA + 
                                        demeaned_median_bage + 
                                        demeaned_household_size + demeaned_car_share + demeaned_family_share + 
                                        demeaned_car_transport_share + demeaned_public_transport_share + 
                                        demeaned_avg_travel_time + demeaned_perf_arts_dens + demeaned_spec_sports_dens + 
                                        demeaned_casino_dens + demeaned_rec_act_dens + demeaned_prop_park_area_tract + 
                                        demeaned_count_tri_facilities + demeaned_stops_per_sqmile + 
                                        demeaned_frestaurant_dens + demeaned_fastfood_dens + demeaned_coffee_dens +
                                        demeaned_bar_dens')

summary(lm_robust(formula = linear_income_formula, data = 
                    US_BLOCK[US_BLOCK$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]))
summary(lm_robust(formula = linear_income_formula, data = 
                    US_BLOCK[US_BLOCK$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]))

summary(lm_robust(formula = linear_stringency_formula, data = 
                    US_BLOCK[US_BLOCK$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]))
summary(lm_robust(formula = linear_stringency_formula, data = 
                    US_BLOCK[US_BLOCK$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]))


#Using distance to CBD
excluded_controls_income_formula_CBD <- as.formula('demeaned_log_Income ~ s(rank_inv_D2CBD, k = 5, bs = "cr")')

included_controls_income_formula_CBD <- as.formula('demeaned_log_Income ~ s(rank_inv_D2CBD, k = 5, bs = "cr") + 
                                        demeaned_median_bage + 
                                        demeaned_household_size + demeaned_car_share + demeaned_family_share + 
                                        demeaned_car_transport_share + demeaned_public_transport_share + 
                                        demeaned_avg_travel_time + demeaned_perf_arts_dens + demeaned_spec_sports_dens + 
                                        demeaned_casino_dens + demeaned_rec_act_dens + demeaned_prop_park_area_tract + 
                                        demeaned_count_tri_facilities + demeaned_stops_per_sqmile + 
                                        demeaned_frestaurant_dens + demeaned_fastfood_dens + demeaned_coffee_dens +
                                        demeaned_bar_dens')

excluded_controls_stringency_formula_CBD <- as.formula('demeaned_stringency ~ s(rank_inv_D2CBD, k = 5, bs = "cr")')

included_controls_stringency_formula_CBD <- as.formula('demeaned_stringency ~ s(rank_inv_D2CBD, k = 5, bs = "cr") + 
                                        demeaned_median_bage + 
                                        demeaned_household_size + demeaned_car_share + demeaned_family_share + 
                                        demeaned_car_transport_share + demeaned_public_transport_share + 
                                        demeaned_avg_travel_time + demeaned_perf_arts_dens + demeaned_spec_sports_dens + 
                                        demeaned_casino_dens + demeaned_rec_act_dens + demeaned_prop_park_area_tract + 
                                        demeaned_count_tri_facilities + demeaned_stops_per_sqmile + 
                                        demeaned_frestaurant_dens + demeaned_fastfood_dens + demeaned_coffee_dens +
                                        demeaned_bar_dens')

reg_t25 <- gam(formula = included_controls_income_formula_CBD,
               data = US_BLOCK[US_BLOCK$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),],
               weights = citywt)

reg_b25 <- gam(formula = included_controls_income_formula_CBD,
               data = US_BLOCK[US_BLOCK$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),],
               weights = citywt)

#Plotting manually with GGPlot2 (Extracting partially linear plot using gratia package)
t25_smooth <- smooth_estimates(reg_t25, n = 1000) %>%
  add_confint()  #from gratia, extracts gam estimates + add confidence intervals
b25_smooth <- smooth_estimates(reg_b25, n = 1000) %>%
  add_confint()

#Plotting these residualized plots
CBD_income.plot <- ggplot() +
  geom_ribbon(data = t25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_inv_D2CBD),alpha = 0.2) +
  geom_line(data = t25_smooth, aes(x = rank_inv_D2CBD, y = est, color = 'Top 25%')) + 
  
  geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_inv_D2CBD), alpha = 0.2) +
  geom_line(data = b25_smooth, aes(x = rank_inv_D2CBD, y = est, color = 'Bot 25%')) + 
  scale_colour_manual(name="Sample", values = c("red","blue")) + 
  xlab("Ranked Inverse Distance to CBD (Block Group level)") +
  ylab("Log Average Income (demeaned by MSA, residualized by controls)") +
  ggtitle("Panel A")

reg_t25 <- gam(formula = excluded_controls_stringency_formula_CBD,
               data = US_BLOCK[US_BLOCK$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),],
               weights = citywt)

reg_b25 <- gam(formula = excluded_controls_stringency_formula_CBD,
               data = US_BLOCK[US_BLOCK$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),],
               weights = citywt)

#Plotting manually with GGPlot2 (Extracting partially linear plot using gratia package)
t25_smooth <- smooth_estimates(reg_t25, n = 1000) %>%
  add_confint()  #from gratia, extracts gam estimates + add confidence intervals
b25_smooth <- smooth_estimates(reg_b25, n = 1000) %>%
  add_confint()

#Plotting these residualized plots
CBD_stringency.plot <- ggplot() +
  geom_ribbon(data = t25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_inv_D2CBD),alpha = 0.2) +
  geom_line(data = t25_smooth, aes(x = rank_inv_D2CBD, y = est, color = 'Top 25%')) + 
  
  geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_inv_D2CBD), alpha = 0.2) +
  geom_line(data = b25_smooth, aes(x = rank_inv_D2CBD, y = est, color = 'Bot 25%')) + 
  scale_colour_manual(name="Sample", values = c("red","blue")) + 
  xlab("Ranked Inverse Distance to CBD (Block Group level)") +
  ylab("Income Stringency of Density Restrictions (demeaned by MSA)") +
  ggtitle("Panel B")

CBD_income.plot + CBD_stringency.plot + plot_layout(guides = "collect") & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
ggsave("DataV2/US_Data/Output/CBD_combined.png", width = 24, height = 15, units = "cm")


#Lastly, CBD distance without residualization
reg_t25 <- gam(formula = included_controls_stringency_formula_CBD,
               data = US_BLOCK[US_BLOCK$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),],
               weights = citywt)

reg_b25 <- gam(formula = included_controls_stringency_formula_CBD,
               data = US_BLOCK[US_BLOCK$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),],
               weights = citywt)

#Plotting manually with GGPlot2 (Extracting partially linear plot using gratia package)
t25_smooth <- smooth_estimates(reg_t25, n = 1000) %>%
  add_confint()  #from gratia, extracts gam estimates + add confidence intervals
b25_smooth <- smooth_estimates(reg_b25, n = 1000) %>%
  add_confint()

#Plotting these residualized plots
 ggplot() +
  geom_ribbon(data = t25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_inv_D2CBD),alpha = 0.2) +
  geom_line(data = t25_smooth, aes(x = rank_inv_D2CBD, y = est, color = 'Top 25%')) + 
  
  geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_inv_D2CBD), alpha = 0.2) +
  geom_line(data = b25_smooth, aes(x = rank_inv_D2CBD, y = est, color = 'Bot 25%')) + 
  scale_colour_manual(name="Sample", values = c("red","blue")) + 
  xlab("Ranked Inverse Distance to CBD (Block Group level)") +
  ylab("Income Stringency of Density Restrictions (demeaned by MSA)") #same idea. 

 
#Robustness 3: Replacing sample definition with density____________
 #INCOME
 reg_t25 <- gam(formula = included_controls_income_formula,
                data = US_BLOCK[US_BLOCK$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]),])
 
 reg_b25 <- gam(formula = included_controls_income_formula,
                data = US_BLOCK[US_BLOCK$City_housing_density < as.numeric(quantile_CBSA_dens["25.0%"]),])
 
 #Plotting manually with GGPlot2 (Extracting partially linear plot using gratia package)
 t25_smooth <- smooth_estimates(reg_t25, n = 1000) %>%
   add_confint()  #from gratia, extracts gam estimates + add confidence intervals
 b25_smooth <- smooth_estimates(reg_b25, n = 1000) %>%
   add_confint()
 
 #Plotting these residualized plots
 ggplot() +
   geom_ribbon(data = t25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
   geom_line(data = t25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Top 25% Density')) + 
   
   geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
   geom_line(data = b25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Bot 25% Density')) + 
   scale_colour_manual(name="Sample", values = c("red","blue")) + 
   xlab("Ranked housing unit density (Block Group level)") +
   ylab("Log Average Income (demeaned by MSA, residualized by controls)") + 
   ggtitle("Density Sample Definition") #Holds for by and large most of the data!
 
 #STRINGENCY
 reg_t25 <- gam(formula = excluded_controls_stringency_formula,
                data = US_BLOCK[US_BLOCK$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]),])
 
 reg_b25 <- gam(formula = excluded_controls_stringency_formula,
                data = US_BLOCK[US_BLOCK$City_housing_density < as.numeric(quantile_CBSA_dens["25.0%"]),])
 
 #Plotting manually with GGPlot2 (Extracting partially linear plot using gratia package)
 t25_smooth <- smooth_estimates(reg_t25, n = 1000) %>%
   add_confint()  #from gratia, extracts gam estimates + add confidence intervals
 b25_smooth <- smooth_estimates(reg_b25, n = 1000) %>%
   add_confint()
 
 #Plotting these residualized plots
 ggplot() +
   geom_ribbon(data = t25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
   geom_line(data = t25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Top 25% Density')) + 
   
   geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
   geom_line(data = b25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Bot 25% Density')) + 
   scale_colour_manual(name="Sample", values = c("red","blue")) + 
   xlab("Ranked housing unit density (Block Group level)") +
   ylab("Income Stringency of Density Restrictions (demeaned by MSA)") + 
   ggtitle("Density Sample Definition") #Holds for by and large most of the data!