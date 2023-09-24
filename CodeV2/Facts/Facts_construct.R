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

#gam package (generalized additive models) for Fact1 and Fact2
library(mgcv)
library(gratia) #for helping to plot these

options(scipen = 5) #limit scientific

#Date created: August 26th, 2022
#requires output from a whole host of programs-- namely Merge_stringency.R and NaNDA_Construct.R

#importing data
load(file = "DataV2/US_Data/Output/Constructed_Block_V2.Rdata")
load(file = "DataV2/US_Data/Output/CBSA_quantiles.Rdata")
load(file = "DataV2/US_Data/Output/CBSA_quantiles_dens.Rdata")
load(file = "DataV2/US_Data/Output/CBSA_med_house_price.Rdata")
load(file = "DataV2/US_Data/Output/CBSA_quantiles_pop.Rdata")

#Formulas to use for regressions
controls <- "+ demeaned_median_bage + demeaned_household_size + demeaned_car_share + demeaned_family_share + demeaned_car_transport_share + 
               demeaned_public_transport_share + demeaned_avg_travel_time + 
               demeaned_perf_arts_dens + demeaned_spec_sports_dens + 
               demeaned_casino_dens + demeaned_rec_act_dens + demeaned_prop_park_area_tract + 
               demeaned_count_tri_facilities + demeaned_stops_per_sqmile + demeaned_frestaurant_dens + 
               demeaned_fastfood_dens + demeaned_coffee_dens + demeaned_bar_dens + 
               demeaned_perennial_snow + demeaned_deciduous_forest + demeaned_evergreen_forest +
               demeaned_mixed_forest + demeaned_shrubs + demeaned_herbaceous + demeaned_woody_wetlands + 
               demeaned_herbaceous_wetlands + rank_inv_D2CBD" #baseline set of controls

excluded_controls_income_formula <- as.formula('demeaned_log_Income ~ s(rank_density_CBSA, k = 5, bs = "cr")')

included_controls_income_formula <- as.formula(paste('demeaned_log_Income ~ s(rank_density_CBSA, k = 5, bs = "cr")', controls, sep = " "))

excluded_controls_stringency_formula <- as.formula('demeaned_stringency ~ s(rank_density_CBSA, k = 5, bs = "cr")')

included_controls_stringency_formula <- as.formula(paste('demeaned_stringency ~ s(rank_density_CBSA, k = 5, bs = "cr")', controls, sep = " "))


#Sample definitions
top <- US_BLOCK[US_BLOCK$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]) & 
                     US_BLOCK$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]
bot <- US_BLOCK[US_BLOCK$City_housing_density < as.numeric(quantile_CBSA_dens["75.0%"]) |
                US_BLOCK$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["75.0%"]),]
                                        

#Fact1: "slightly stronger income sorting on density in dense (large) cities"
reg_t25 <- gam(formula = excluded_controls_income_formula,
               data = top) #use high smoothing penalty gamma for this application, though it doesn't matter much. 

reg_b25 <- gam(formula = excluded_controls_income_formula,
               data = bot) 

#Plotting manually with GGPlot2 (Extracting partially linear plot using gratia package)
t25_smooth <- smooth_estimates(reg_t25, n = 1000) %>%
  add_confint()  #from gratia, extracts gam estimates + add confidence intervals
b25_smooth <- smooth_estimates(reg_b25, n = 1000) %>% #unconditional confidence intervals not working for some reason. i.e. arg unconditional = TRUE does not work
  add_confint()

#Plotting
Income.plot <- ggplot() +
  geom_ribbon(data = t25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
  geom_line(data = t25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Top 25%')) + 
  
  geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = b25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Bot 75%')) + 
  scale_colour_manual(name="Sample", values = c("red","blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Log Average Income (demeaned by MSA)") + 
  ggtitle("Panel A")
ggsave("DataV2/US_Data/Output/income.png", plot = Income.plot, width = 24, height = 15, units = "cm")


#Estimating linear model after residualizing other variables (this is temporary-- can estimate a partial linear model later)
#Estimating partial linear model (i.e. residualizing by building age) (i.e. using globally fitted parameter)
reg_t25 <- gam(formula = included_controls_income_formula,
               data = top)

reg_b25 <- gam(formula = included_controls_income_formula,
               data = bot)

#Plotting manually with GGPlot2 (Extracting partially linear plot using gratia package)
t25_smooth <- smooth_estimates(reg_t25, n = 1000) %>%
                    add_confint()  #from gratia, extracts gam estimates + add confidence intervals
b25_smooth <- smooth_estimates(reg_b25, n = 1000) %>%
                    add_confint()

#Plotting these residualized plots
Income_resid.plot <- ggplot() +
  geom_ribbon(data = t25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
  geom_line(data = t25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Top 25%')) + 
  
  geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = b25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Bot 75%')) + 
  scale_colour_manual(name="Sample", values = c("red","blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Log Average Income (demeaned by MSA, residualized by controls)") +
  ggtitle("Panel B")
  ggsave("DataV2/US_Data/Output/income_residualized.png", plot = Income_resid.plot, width = 24, height = 15, units = "cm")

#Combining residualized plots
Income.plot + Income_resid.plot + plot_layout(guides = "collect") & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
ggsave("DataV2/US_Data/Output/income_combined.png", width = 24, height = 15, units = "cm")

#Using a linear regression to check relative slopes in income
linear_income_formula <- as.formula(paste('demeaned_log_Income ~ rank_density_CBSA', controls, sep = " "))

summary(lm_robust(formula = linear_income_formula, data = top))
summary(lm_robust(formula = linear_income_formula, data = bot)) #Same idea. 


#Fact 2: "Variation explained by the prices of minimal lots"
  reg_t25 <- gam(formula = excluded_controls_stringency_formula,
                 data = top)
  
  reg_b25 <- gam(formula = excluded_controls_stringency_formula,
                 data = bot)
  
  #Plotting manually with GGPlot2 (Extracting partially linear plot using gratia package)
  t25_smooth <- smooth_estimates(reg_t25, n = 1000) %>%
    add_confint()  #from gratia, extracts gam estimates + add confidence intervals
  b25_smooth <- smooth_estimates(reg_b25, n = 1000) %>%
    add_confint()
  
  #Plotting these residualized plots
 IncomeStringency.plot <- ggplot() +
    geom_ribbon(data = t25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
    geom_line(data = t25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Top 25%')) + 
    
    geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
    geom_line(data = b25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Bot 75%')) + 
    scale_colour_manual(name="Sample", values = c("red","blue")) + 
    xlab("Ranked housing unit density (Block Group level)") +
    ylab("Income Stringency of Density Restrictions (demeaned by MSA)") +
    ggtitle("Panel A")
    ggsave("DataV2/US_Data/Output/income_stringency.png", width = 24, height = 15, units = "cm")
  
  
  
  reg_t25 <- gam(formula = included_controls_stringency_formula,
                 data = top)
  
  reg_b25 <- gam(formula = included_controls_stringency_formula,
                 data = bot)
  
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
    geom_line(data = b25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Bot 75%')) + 
    scale_colour_manual(name="Sample", values = c("red","blue")) + 
    xlab("Ranked housing unit density (Block Group level)") +
    labs(y = paste0("Income Stringency of Density Restrictions", "\n", "(demeaned by MSA, residualized by controls)")) +
    ggtitle("Panel A")
    ggsave("DataV2/US_Data/Output/income_stringency_residualized.png", width = 24, height = 15, units = "cm")
    
    
    #Checking if the same holds for formula with physical density restriction (units/acre)
    excluded_controls_density_formula <- as.formula('demeaned_densrestriction ~ s(rank_density_CBSA, k = 5, bs = "cr")')
    
    reg_t25 <- gam(formula = excluded_controls_density_formula,
                   data = top)
    
    reg_b25 <- gam(formula = excluded_controls_density_formula,
                   data = bot)
    
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
      geom_line(data = b25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Bot 75%')) + 
      scale_colour_manual(name="Sample", values = c("red","blue")) + 
      xlab("Ranked housing unit density (Block Group level)") +
      labs(y = paste0("Density Restriction (Acres/Unit)", "\n", "(demeaned by MSA)")) +
      theme(axis.title = element_text(size = 15))
    ggsave("DataV2/US_Data/Output/income_density.png", width = 24, height = 15, units = "cm")
    #NOTE: SAME HOLDS INCLUDING CONTROLS! This suggests a completely opposite framework! (remember--these density restrictions do not take into account fraciton of zoned land)


#Replicating negative income sorting on density plots allowing for average intercept to differ
t25d_int <- US_BLOCK[US_BLOCK$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]) & 
                  US_BLOCK$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]
b25d_int <- US_BLOCK[US_BLOCK$City_housing_density < as.numeric(quantile_CBSA_dens["50.0%"]) & 
                  US_BLOCK$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["50.0%"]),]

#Regressions allowing for average intercept to vary
reg_t25 <- gam(excluded_controls_income_formula,
               data = t25d_int)

reg_b25 <- gam(excluded_controls_income_formula,
               data = b25d_int)

#Plotting manually with GGPlot2 (Extracting partially linear plot using gratia package)
t25_smooth <- smooth_estimates(reg_t25, n = 1000) %>%
  add_confint()  #from gratia, extracts gam estimates + add confidence intervals
t25_smooth$lower_ci <- t25_smooth$lower_ci + mean(log(t25d_int$Average_income), na.rm = TRUE) - mean(log(b25d_int$Average_income), na.rm = TRUE) #add differences in intercept
t25_smooth$upper_ci <- t25_smooth$upper_ci + mean(log(t25d_int$Average_income), na.rm = TRUE) - mean(log(b25d_int$Average_income), na.rm = TRUE) #add differences in intercept
t25_smooth$est <- t25_smooth$est + mean(log(t25d_int$Average_income), na.rm = TRUE) - mean(log(b25d_int$Average_income), na.rm = TRUE) #add differences in intercept

b25_smooth <- smooth_estimates(reg_b25, n = 1000) %>%
  add_confint()

#Plotting these residualized plots
ggplot() +
  geom_ribbon(data = t25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = t25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Top 25%')) + 
  
  geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = b25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Bot 75%')) + 
  scale_colour_manual(name="Sample", values = c("red","blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Log Average Income (demeaned by MSA)") +
  ggtitle("intercept varies by sample")


#Regressions allowing for average intercept to vary: Demeaned stringency
reg_t25 <- gam(excluded_controls_stringency_formula,
               data = t25d_int)

reg_b25 <- gam(excluded_controls_stringency_formula,
               data = b25d_int)

#Plotting manually with GGPlot2 (Extracting partially linear plot using gratia package)
t25_smooth <- smooth_estimates(reg_t25, n = 1000) %>%
  add_confint()  #from gratia, extracts gam estimates + add confidence intervals
t25_smooth$lower_ci <- t25_smooth$lower_ci + mean(t25d_int$IncomeStringency, na.rm = TRUE) - mean(b25d_int$IncomeStringency, na.rm = TRUE) #add differences in intercept
t25_smooth$upper_ci <- t25_smooth$upper_ci + mean(t25d_int$IncomeStringency, na.rm = TRUE) - mean(b25d_int$IncomeStringency, na.rm = TRUE) #add differences in intercept
t25_smooth$est <- t25_smooth$est + mean(t25d_int$IncomeStringency, na.rm = TRUE) - mean(b25d_int$IncomeStringency, na.rm = TRUE) #add differences in intercept

b25_smooth <- smooth_estimates(reg_b25, n = 1000) %>%
  add_confint()

#Plotting these residualized plots
IncomeStringency_intercept.plot <- ggplot() +
  geom_ribbon(data = t25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = t25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Top 25%')) + 
  
  geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = b25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Bot 75%')) + 
  scale_colour_manual(name="Sample", values = c("red","blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  labs(y = paste0("Income Stringency of Density Restrictions", "\n", "(demeaned by MSA, intercept varies by sample)")) +
  ggtitle("Panel B")

IncomeStringency.plot + IncomeStringency_intercept.plot + plot_layout(guides = "collect") & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
ggsave("DataV2/US_Data/Output/incomestringency_combined.png", width = 24, height = 15, units = "cm")

rm(list = ls())
