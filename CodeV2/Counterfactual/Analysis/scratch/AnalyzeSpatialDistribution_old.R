#Date created: Feb 18th, 2023

#This file shows what happens in the counterfactual in terms of the spatial distribution of activity.
#Connects counterfactual output to the facts. 

library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)
library(ggplot2)
library(collapse)
library(mgcv)
library(gratia) #for helping to plot these
library(patchwork) #combining plots

options(scipen = 5) #limit scientific

#PARAMETERS
source("CodeV2/Counterfactual/Parameters/GlobalParameters.R")

#FUNCTIONS
source("CodeV2/Counterfactual/Functions/Solve_Equilibrium_Functions_FullDereg.R")
source("CodeV2/Counterfactual/Functions/Analysis_Functions.R")

#Consumption adjustment factors
consumptionAdjustment <- read_dta("DataV2/Counterfactuals/Calibration_Output/consumption_AdjustmentFactor.dta")

#Importing all files from Counterfactual_Output (Endogenous amenities and exogenous amenities)
load("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/Eq_Objects_Full_EndoAmen_TRUE_bySkill_FALSE.RData")
Ct_Amenities <- Equilibrium_objects
rm(Equilibrium_objects)

load("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/Eq_Objects_Full_EndoAmen_FALSE_bySkill_FALSE.RData")
Ct_NoAmenities <- Equilibrium_objects
rm(Equilibrium_objects)


load("DataV2/Counterfactuals/Init_eq.RData")
Init_eq <- Master
rm(Master)

#Importing city density distribution
load(file = "DataV2/US_Data/Output/CBSA_quantiles_dens.Rdata")
load(file = "DataV2/US_Data/Output/CBSA_quantiles.Rdata")


#demeaning income
Init_eq <- Init_eq %>% group_by(CBSA) %>% mutate(demeaned_log_Income = log(Avg_income) - mean(log(Avg_income), na.rm = TRUE))
Ct_Amenities <- Ct_Amenities %>% group_by(CBSA) %>% mutate(demeaned_log_Income = log(Avg_income) - mean(log(Avg_income), na.rm = TRUE))
Ct_NoAmenities <- Ct_NoAmenities %>% group_by(CBSA) %>% mutate(demeaned_log_Income = log(Avg_income) - mean(log(Avg_income), na.rm = TRUE))

#New ranking of relative density
population_Ct <- rep(0, nrow(Ct_Amenities))
population_Init <- rep(0, nrow(Init_eq))
population_CtNoAm <- rep(0, nrow(Ct_NoAmenities))
for (incomeType in 1:7) {
  population_Ct <- population_Ct + Ct_Amenities[[paste0("Population_type_", incomeType)]]
  population_Init <- population_Init + Init_eq[[paste0("Population_type_", incomeType)]]
  population_CtNoAm <- population_CtNoAm + Ct_NoAmenities[[paste0("Population_type_", incomeType)]]
  
}

Ct_Amenities["Housing_density"] <- population_Ct/Init_eq$ALAND
Init_eq["Housing_density"] <- population_Init/Init_eq$ALAND
Ct_NoAmenities["Housing_density"] <- population_CtNoAm/Init_eq$ALAND

rm(population_Ct, population_Init)

Ct_Amenities["rank_density_CBSA"] <- Init_eq$rank_density_CBSA
Ct_NoAmenities["rank_density_CBSA"] <- Init_eq$rank_density_CBSA

excluded_controls_income_formula <- as.formula('demeaned_log_Income ~ s(rank_density_CBSA, k = 5, bs = "cr")')

#Original graph
reg_t25 <- gam(formula = excluded_controls_income_formula,
               data = Ct_Amenities[Init_eq$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]) &
                                    Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]) #use high smoothing penalty gamma for this application, though it doesn't matter much. 

reg_b25 <- gam(formula = excluded_controls_income_formula,
               data = Ct_Amenities[Init_eq$City_housing_density < as.numeric(quantile_CBSA_dens["75.0%"]) |
                                     Init_eq$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["75.0%"]),]) 

#Plotting manually with GGPlot2 (Extracting partially linear plot using gratia package)
t25_smooth <- smooth_estimates(reg_t25, n = 1000) %>%
  add_confint()  #from gratia, extracts gam estimates + add confidence intervals
b25_smooth <- smooth_estimates(reg_b25, n = 1000) %>% #unconditional confidence intervals not working for some reason. i.e. arg unconditional = TRUE does not work
  add_confint()

#Plotting
Income.plot.initial <- ggplot() +
  geom_ribbon(data = t25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
  geom_line(data = t25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Top 25%')) + 
  
  geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = b25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Bot 75%')) + 
  scale_colour_manual(name="Sample", values = c("red","blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Log Average Income (demeaned by MSA)") + 
  ggtitle("Panel A: Counterfactual Equilibrium")

#new graoh
reg_t25 <- gam(formula = excluded_controls_income_formula,
               data = Init_eq[Init_eq$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]) &
                              Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]) #use high smoothing penalty gamma for this application, though it doesn't matter much. 

reg_b25 <- gam(formula = excluded_controls_income_formula,
               data = Init_eq[Init_eq$City_housing_density < as.numeric(quantile_CBSA_dens["75.0%"]) |
                                Init_eq$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["75.0%"]),]) 

#Plotting manually with GGPlot2 (Extracting partially linear plot using gratia package)
t25_smooth <- smooth_estimates(reg_t25, n = 1000) %>%
  add_confint()  #from gratia, extracts gam estimates + add confidence intervals
b25_smooth <- smooth_estimates(reg_b25, n = 1000) %>% #unconditional confidence intervals not working for some reason. i.e. arg unconditional = TRUE does not work
  add_confint()

#Plotting
Income.plot.new <- ggplot() +
  geom_ribbon(data = t25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
  geom_line(data = t25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Top 25%')) + 
  
  geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = b25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Bot 75%')) + 
  scale_colour_manual(name="Sample", values = c("red","blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Log Average Income (demeaned by MSA)") + 
  ggtitle("Panel B: Initial Equilibrium")

Income.plot.initial + Income.plot.new + plot_layout(guides = "collect") & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
ggsave("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/IncomeDensityGradCtfl.png", width = 24, height = 15, units = "cm") 

summary(lm(demeaned_log_Income ~ rank_density_CBSA, data = Ct_Amenities[Init_eq$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]) &
                                                                          Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]))
summary(lm(demeaned_log_Income ~ rank_density_CBSA, data = Ct_Amenities[Init_eq$City_housing_density < as.numeric(quantile_CBSA_dens["75.0%"]) |
                                                                          Init_eq$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["75.0%"]),]))

summary(lm(demeaned_log_Income ~ rank_density_CBSA, data = Init_eq[Init_eq$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]) &
                                                                     Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]))
summary(lm(demeaned_log_Income ~ rank_density_CBSA, data = Init_eq[Init_eq$City_housing_density < as.numeric(quantile_CBSA_dens["75.0%"]) |
                                                                     Init_eq$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["75.0%"]),]))



#Alternate graph 
reg_t25 <- gam(formula = excluded_controls_income_formula,
               data = Init_eq[Init_eq$City_housing_density < as.numeric(quantile_CBSA_dens["75.0%"]) |
                                Init_eq$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["75.0%"]),]) #use high smoothing penalty gamma for this application, though it doesn't matter much. 

reg_b25 <- gam(formula = excluded_controls_income_formula,
               data = Ct_Amenities[Init_eq$City_housing_density < as.numeric(quantile_CBSA_dens["75.0%"]) |
                                     Init_eq$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["75.0%"]),]) 

#Plotting manually with GGPlot2 (Extracting partially linear plot using gratia package)
t25_smooth <- smooth_estimates(reg_t25, n = 1000) %>%
  add_confint()  #from gratia, extracts gam estimates + add confidence intervals
b25_smooth <- smooth_estimates(reg_b25, n = 1000) %>% #unconditional confidence intervals not working for some reason. i.e. arg unconditional = TRUE does not work
  add_confint()

#Plotting
Income.plot.initial <- ggplot() +
  geom_ribbon(data = t25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
  geom_line(data = t25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Initial Equilibrium')) + 
  
  geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = b25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Counterfactual')) + 
  scale_colour_manual(name="Sample", values = c("red","blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Log Average Income (demeaned by MSA)") + 
  ggtitle("Panel A: Bottom 75%")

#new graoh
reg_t25 <- gam(formula = excluded_controls_income_formula,
               data = Init_eq[Init_eq$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]) &
                                Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]) #use high smoothing penalty gamma for this application, though it doesn't matter much. 

reg_b25 <- gam(formula = excluded_controls_income_formula,
               data = Ct_Amenities[Init_eq$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]) &
                                     Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]) 

#Plotting manually with GGPlot2 (Extracting partially linear plot using gratia package)
t25_smooth <- smooth_estimates(reg_t25, n = 1000) %>%
  add_confint()  #from gratia, extracts gam estimates + add confidence intervals
b25_smooth <- smooth_estimates(reg_b25, n = 1000) %>% #unconditional confidence intervals not working for some reason. i.e. arg unconditional = TRUE does not work
  add_confint()

#Plotting
Income.plot.new <- ggplot() +
  geom_ribbon(data = t25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
  geom_line(data = t25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Initial equlibrium')) + 
  
  geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = b25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Counterfactual')) + 
  scale_colour_manual(name="Sample", values = c("red","blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Log Average Income (demeaned by MSA)") + 
  ggtitle("Panel B: Top 25%")

Income.plot.initial + Income.plot.new + plot_layout(guides = "collect") & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
ggsave("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/IncomeDensityGradCtfl_alternate.png", width = 24, height = 15, units = "cm") 



#Question: How can we relate these facts to changes in amenity exposure/ housing supply elasticity exposure?

#Lets look at overall changes in density gradients
Ct_Amenities <- Ct_Amenities %>% group_by(CBSA) %>% mutate(demeaned_Housing_density = Housing_density/mean(Housing_density, na.rm = TRUE))

Init_eq <- Init_eq %>% group_by(CBSA) %>% mutate(demeaned_Housing_density = Housing_density/mean(Housing_density, na.rm = TRUE))

Ct_NoAmenities <- Ct_NoAmenities %>% group_by(CBSA) %>% mutate(demeaned_Housing_density = Housing_density/mean(Housing_density, na.rm = TRUE))

reg_Ct <- gam(formula = demeaned_Housing_density ~  s(rank_density_CBSA, k = 5, bs = "cr"),
           data = Ct_Amenities)
reg_Init <- gam(formula = demeaned_Housing_density ~  s(rank_density_CBSA, k = 5, bs = "cr"),
              data = Init_eq)

Ct_smooth <- smooth_estimates(reg_Ct, n = 1000) %>%
  add_confint()  #from gratia, extracts gam estimates + add confidence intervals
Init_smooth <- smooth_estimates(reg_Init, n = 1000) %>% #unconditional confidence intervals not working for some reason. i.e. arg unconditional = TRUE does not work
  add_confint()

ggplot() +
  geom_ribbon(data = Ct_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
  geom_line(data = Ct_smooth, aes(x = rank_density_CBSA, y = est, color = 'Counterfactual')) + 
  
  geom_ribbon(data = Init_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = Init_smooth, aes(x = rank_density_CBSA, y = est, color = 'Initial Equilibrium')) +
  
  scale_colour_manual(name="Sample", values = c("orange","turquoise")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Housing unit density (demeaned by MSA)") 
  ggsave("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/DensityGradCtfl.png", width = 24, height = 15, units = "cm")
    
#Splitting up these changes by sample corrected to have similar total populations
  reg_Ct <- gam(formula = demeaned_Housing_density ~  s(rank_density_CBSA, k = 5, bs = "cr"),
                data = Ct_Amenities[Init_eq$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]) &
                                    Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),])
  reg_Init <- gam(formula = demeaned_Housing_density ~  s(rank_density_CBSA, k = 5, bs = "cr"),
                  data = Init_eq[Init_eq$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]) &
                                 Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),])
  Ct_smooth <- smooth_estimates(reg_Ct, n = 1000) %>%
    add_confint()  #from gratia, extracts gam estimates + add confidence intervals
  Init_smooth <- smooth_estimates(reg_Init, n = 1000) %>% #unconditional confidence intervals not working for some reason. i.e. arg unconditional = TRUE does not work
    add_confint()
  
plotb85 <- ggplot() +
    geom_ribbon(data = Ct_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
    geom_line(data = Ct_smooth, aes(x = rank_density_CBSA, y = est, color = "Counterfactual")) + 
    
    geom_ribbon(data = Init_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
    geom_line(data = Init_smooth, aes(x = rank_density_CBSA, y = est, color = "Initial Equilibrium")) +
    scale_colour_manual(name="Sample", values = c("orange","turquoise")) + 
    xlab("Ranked housing unit density (Block Group level)") +
    ylab("Housing unit density (demeaned by MSA)") + 
    ggtitle("Panel B: Top 75%")
  
  #Splitting up these changes by top and bot 25%
  reg_Ct <- gam(formula = demeaned_Housing_density ~  s(rank_density_CBSA, k = 5, bs = "cr"),
                data = Ct_Amenities[Init_eq$City_housing_density < as.numeric(quantile_CBSA_dens["75.0%"]) |
                                    Init_eq$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["75.0%"]),])
  reg_Init <- gam(formula = demeaned_Housing_density ~  s(rank_density_CBSA, k = 5, bs = "cr"),
                  data = Init_eq[Init_eq$City_housing_density < as.numeric(quantile_CBSA_dens["75.0%"]) |
                                 Init_eq$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["75.0%"]),])
  
  Ct_smooth <- smooth_estimates(reg_Ct, n = 1000) %>%
    add_confint()  #from gratia, extracts gam estimates + add confidence intervals
  Init_smooth <- smooth_estimates(reg_Init, n = 1000) %>% #unconditional confidence intervals not working for some reason. i.e. arg unconditional = TRUE does not work
    add_confint()
  
plott15 <- ggplot() +
    geom_ribbon(data = Ct_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
    geom_line(data = Ct_smooth, aes(x = rank_density_CBSA, y = est, color = 'Counterfactual')) + 
    
    geom_ribbon(data = Init_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
    geom_line(data = Init_smooth, aes(x = rank_density_CBSA, y = est, color = 'Initial Equilibrium')) +
    scale_colour_manual(name="Sample", values = c("orange","turquoise")) + 
    xlab("Ranked housing unit density (Block Group level)") +
    ylab("Housing unit density (demeaned by MSA)") + 
    ggtitle("Panel A: Bottom 75%")

plott15 + plotb85 + plot_layout(guides = "collect") & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
ggsave("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/DensityGradCtfl_breakdown.png", width = 24, height = 15, units = "cm") 



#Checking to see if amenity exposure increases by more than top 15% and bot 85% of sample

total_population <- rep(NA, 7)
for (i in 1:7) {
  total_population[i] <- Ct_Amenities[[paste0("Total_Population_type_", i)]][1]
}

Change_t15 <- rep(NA, 7)
Change_b85 <- rep(NA, 7)

for (i in 1:7) {
  
  Change_t15[i] <- (getAmenityExposure(Master_data = Ct_Amenities[Init_eq$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]) &
                                                                    Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], 
                                       bySkill = FALSE, incomeType = i)/getAmenityExposure(Master_data = Init_eq[Init_eq$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]) &
                                                                                                                 Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),],
                                                                                           bySkill = FALSE, incomeType = i))
  Change_b85[i] <- (getAmenityExposure(Master_data = Ct_Amenities[Init_eq$City_housing_density < as.numeric(quantile_CBSA_dens["75.0%"]) |
                                                                           Init_eq$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["75.0%"]),],
                                                     bySkill = FALSE, incomeType = i)/getAmenityExposure(Master_data = Init_eq[Init_eq$City_housing_density < as.numeric(quantile_CBSA_dens["75.0%"]) |
                                                                                                                               Init_eq$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["75.0%"]),],
                                                                                                         bySkill = FALSE, incomeType = i))

}

Change_t15 <- (Change_t15%*%total_population)/(sum(total_population))
Change_b85 <- (Change_b85%*%total_population)/(sum(total_population)) #25% larger changes in amenity exposure.

#The relative difference in amenity exposure across the t15 and b85 is
print(paste0("The relative difference in amenity exposure across the b75 and b75 is ", 100*(Change_b85 - Change_t15)/(Change_t15 - 1), " percent"))
#25%.   


rm(list = ls())