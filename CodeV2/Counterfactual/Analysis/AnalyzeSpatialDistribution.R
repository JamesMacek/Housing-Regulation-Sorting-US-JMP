#Date created: Feb 18th, 2023

#This file shows what happens in the counterfactual in terms of the spatial distribution of activity (primarily within cities)
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
source("CodeV2/Counterfactual/Functions/Solve_Equilibrium_Functions_FullDereg_current.R")
source("CodeV2/Counterfactual/Functions/Analysis_Functions.R")

#_______________________________________________________________________________ PRELIMINARIES

#SPECIFY BASELINE COUNTERFACTUAL OUTPUT
BASELINE_SPECIFICATION <- list(pref = "SG", bySkill_to_pass = FALSE) #estimating omega will not work for bySkill == True at current codebase

if (BASELINE_SPECIFICATION$bySkill == TRUE) {
  
  skillVector <-  c("College", "NoCollege")
  skillName <- c("College_", "NoCollege_") 
  
}else{
  
  skillVector <- c("Pooled")
  skillName <- c("")
  
}


#Importing all files from Counterfactual_Output (Endogenous amenities)
load(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", "Eq_Objects_FULL", 
            "_EndoAmen_", TRUE, 
            "_EndoProd_", FALSE,
            "_bySkill_", BASELINE_SPECIFICATION$bySkill_to_pass,
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
Ct_Amenities <- Equilibrium_objects
rm(Equilibrium_objects)

#Importing all files from Counterfactual_Output (Exogenous amenities)
load(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", "Eq_Objects_FULL", 
            "_EndoAmen_", FALSE, 
            "_EndoProd_", FALSE,
            "_bySkill_", BASELINE_SPECIFICATION$bySkill_to_pass,
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
Ct_NoAmenities <- Equilibrium_objects
rm(Equilibrium_objects)



#Importing master data for comparison to equilibrium
load(paste0("DataV2/Counterfactuals/Init_eq_", 
            BASELINE_SPECIFICATION$bySkill_to_pass, 
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
Init_eq <- Master
rm(Master)


#Consumption adjustment factors for baseline spec
load(paste0("DataV2/Counterfactuals/Calibration_output/consumption_AdjustmentFactor_bySkill",
            BASELINE_SPECIFICATION$bySkill_to_pass,
            "_pref_", BASELINE_SPECIFICATION$pref, ".Rdata"))
#Final land for res
Init_eq["final_land_for_res"] <- Init_eq$land_regulated + Init_eq$land_unregulated
Ct_Amenities["ALAND"] <- Init_eq$ALAND #official landmass from census shapefiles
Ct_NoAmenities["ALAND"] <- Init_eq$ALAND


#__________________________________________________________________________________________________

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

#Households in neighborhood 
for (skill in skillVector) {
  name_of_skill <- skillName[which(skill == skillVector)]
  for (incomeType in 1:7) {
    population_Ct <- population_Ct + Ct_Amenities[[paste0("Population_type_", name_of_skill, incomeType)]]
    population_Init <- population_Init + Init_eq[[paste0("Population_type_", name_of_skill,  incomeType)]]
    population_CtNoAm <- population_CtNoAm + Ct_NoAmenities[[paste0("Population_type_", name_of_skill, incomeType)]]
  
  }
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
ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/IncomeDensityGradCtfl",
              BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
              BASELINE_SPECIFICATION$pref, ".png"),
              width = 24, height = 15, units = "cm") 


#CHECKING GRADIENT VALUES!!!!!
print("COUNTERFACTUAL AT BASELINE GRADIENTS")
print("\n")
print( summary(lm(demeaned_log_Income ~ rank_density_CBSA, data = Ct_Amenities[Init_eq$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]) &
                                                                          Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),])) )
print( summary(lm(demeaned_log_Income ~ rank_density_CBSA, data = Ct_Amenities[Init_eq$City_housing_density < as.numeric(quantile_CBSA_dens["75.0%"]) |
                                                                          Init_eq$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["75.0%"]),])) )
print("INITIAL AT BASELINE GRADIENTS")
print("\n")
print( summary(lm(demeaned_log_Income ~ rank_density_CBSA, data = Init_eq[Init_eq$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]) &
                                                                     Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),])) )
print( summary(lm(demeaned_log_Income ~ rank_density_CBSA, data = Init_eq[Init_eq$City_housing_density < as.numeric(quantile_CBSA_dens["75.0%"]) |
                                                                     Init_eq$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["75.0%"]),])) ) 
#All of the differences are gone in current model!


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
  scale_colour_manual(name="Sample", values = c("yellow","purple")) + 
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
  geom_line(data = t25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Initial Equilibrium')) + 
  
  geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = b25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Counterfactual')) + 
  scale_colour_manual(name="Sample", values = c("yellow","purple")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Log Average Income (demeaned by MSA)") + 
  ggtitle("Panel B: Top 25%")

Income.plot.initial + Income.plot.new + plot_layout(guides = "collect") & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/IncomeDensityGradCtfl_alternate",
               BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
               BASELINE_SPECIFICATION$pref, ".png"),
                  width = 24, height = 15, units = "cm") 


#Changes in Density Gradients
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
  
  scale_colour_manual(name="Sample", values = c("yellow","purple")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Housing unit density (demeaned by MSA)") 
  ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/DensityGradCtfl",
                BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                BASELINE_SPECIFICATION$pref, ".png"), width = 24, height = 15, units = "cm")
    
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
    ggtitle("Panel B: Top 25%")
  
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
ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/DensityGradCtfl_breakdown",
              BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
              BASELINE_SPECIFICATION$pref, ".png"), width = 24, height = 15, units = "cm")



#LAST PART: ANALYZE SEGREGATION PATTERNS ____________________________________________________________________________________________________

#Analyzing spatial distribution of income within cities....

#only works if BySkill == FALSE.


if (BASELINE_SPECIFICATION$bySkill == FALSE) {

  #What happens to segregation == variance of average income across neighborhoods?
  print(paste0("Neighborhood variance in log average income increases by ",
              100*(var(log(Ct_Amenities$Avg_income)) - var(log(Init_eq$Avg_income)))/var(log(Init_eq$Avg_income)), " percent.")) #Variance in log income increases in the counterfactual. Why? 

  #Average within-city segregation (using only within-city variation)
  print(paste0("Neighborhood variance in log average income increases by ",
             100*(var(Ct_Amenities$demeaned_log_Income) - var(Init_eq$demeaned_log_Income))/var(Init_eq$demeaned_log_Income), " percent for within-city variation.")) #Variance in log income increases in the counterfactual. Why?
  #Q: Why does segregation increase after imposing the counterfactual?

  #What about superstar cities?
  print(paste0("Neighborhood variance in log average income increases by ",
             100*(var(Ct_Amenities[Init_eq$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]) &
                                     Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$demeaned_log_Income) - 
                      var(Init_eq[Init_eq$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]) &
                                    Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$demeaned_log_Income))/
                      var(Init_eq[Init_eq$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]) &
                                    Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$demeaned_log_Income), " percent for within-city variation in superstar cities.")) 
                      #This number is more like 27% in superstar cities. How is this possible if segregation is to be reduced -- at least with respect to density?

  #Correlation between income growth and initial income.
  #Correlation between initial stringency and incomes
  print(paste0("The correlation between initial stringency and income changes in the cross section is " ,
             cor(Init_eq$IncomeStringency_cl, log(Ct_Amenities$Avg_income) - log(Init_eq$Avg_income), use = "complete.obs") ))
  #Correlation between initial stringency and incomes
  Init_eq <- Init_eq %>% group_by(CBSA) %>% mutate(demeaned_stringency = IncomeStringency_cl - mean(IncomeStringency_cl, na.rm = TRUE))
  print(paste0("The correlation between initial stringency and income changes in the cross section on within-city variation is " ,
             cor(Init_eq$demeaned_stringency, Ct_Amenities$demeaned_log_Income - Init_eq$demeaned_log_Income, use = "complete.obs") )) #Either way, reasonably string correlation between stringency and income changes.

  print(paste0("The correlation between initial stringency and initial income is " ,
             cor(Init_eq$IncomeStringency_cl, log(Init_eq$Avg_income), use = "complete.obs") )) #Either way, 41% correlation between initial stringency and initial income, which is quite interesting. 

  print(paste0("The correlation between initial stringency and income AFTER deregulation is " ,
             cor(Init_eq$IncomeStringency_cl, log(Ct_Amenities$Avg_income), use = "complete.obs") )) #Correlation between initial stringency and income after has been reduced to 0.1299! 
                                                                                                     #It's still positive, yet residential segregation increases??? 


  print(paste0("The correlation between initial income and income changes in the cross section on within-city variation is " ,
             cor(log(Init_eq$Avg_income),log(Ct_Amenities$Avg_income) - log(Init_eq$Avg_income), use = "complete.obs") )) #Noisy income changes. Is this why "segregation" increases?

  #Amenities -- what's the correlation?
  Init_eq["low_amenity_ex"] <- (Ct_Amenities$exogenous_Amenity_1 + Ct_Amenities$exogenous_Amenity_2 + Ct_Amenities$exogenous_Amenity_3)
  Init_eq["high_amenity_ex"] <- (Ct_Amenities$exogenous_Amenity_5 + Ct_Amenities$exogenous_Amenity_6 + Ct_Amenities$exogenous_Amenity_7)
  Init_eq["relative_amenity_high_ex"] <- log(Init_eq$high_amenity_ex/Init_eq$low_amenity_ex) #top 2 quantiles - bot 2 quantiles. 

  temp_df <- Init_eq[Init_eq$high_amenity_ex > 0 & Init_eq$low_amenity_ex > 0,]

  temp_df <- temp_df %>% group_by(CBSA) %>% mutate(demeaned_rel_amenity = relative_amenity_high_ex - mean(relative_amenity_high_ex, na.rm = TRUE))  

 
  #However, stringency positively correlated with relatively higher fundamental amenity values for rich...?
  #How is this commesurate with decreasing segregation?
  print("Within and Across city correlation between income stringency and relative fundamental amenities for rich")
  print(summary(lm(relative_amenity_high_ex ~ IncomeStringency_cl, data = temp_df) ) ) 
  print(summary(lm(demeaned_rel_amenity ~ demeaned_stringency, data = temp_df) ) ) 
  
  
  #What about actual amenity-adjusted values? Clearly these exogenous amenities might overcome the fact that income sorting is so strong.
    print("Within and Across city correlation between income stringency and relative fundamental amenities for rich")
  Init_eq["low_amenity"] <- (Init_eq$Amenity_1 + Init_eq$Amenity_2 + Init_eq$Amenity_3)
  Init_eq["high_amenity"] <- (Init_eq$Amenity_5 + Init_eq$Amenity_6 + Init_eq$Amenity_7)
  Init_eq["relative_amenity_high"] <- log(Init_eq$high_amenity/Init_eq$low_amenity) #top 2 quantiles - bot 2 quantiles. 
  
  temp_df <- Init_eq[Init_eq$high_amenity > 0 & Init_eq$low_amenity > 0,]
  
  temp_df <- temp_df %>% group_by(CBSA) %>% mutate(demeaned_rel_amenity = relative_amenity_high - mean(relative_amenity_high, na.rm = TRUE))  
  
  print("Within and Across city correlation between income stringency and initial amenities for rich")
  print(summary(lm(relative_amenity_high ~ IncomeStringency_cl, data = temp_df) ) ) 
  print(summary(lm(demeaned_rel_amenity ~ demeaned_stringency, data = temp_df) ) ) 
  
  
  #What are other measures of segregation? For example the difference in quantiles between 10-90? Really rich vs really poor neighborhoods?
  #Check quantiles of other 
  
  
  #Check q75 - q25
  print("The difference between q75 and q25 from Initial to Cftl is...")
  print(quantile(log(Init_eq$Avg_income), probs = 0.75, na.rm = TRUE) -  quantile(log(Init_eq$Avg_income), probs = 0.25, na.rm = TRUE))
  print(quantile(log(Ct_Amenities$Avg_income), probs = 0.75, na.rm = TRUE) -  quantile(log(Ct_Amenities$Avg_income), probs = 0.25, na.rm = TRUE))
  
  print("The difference between q90 and q10 from Initial to Cftl is...")
  print(quantile(log(Init_eq$Avg_income), probs = 0.9, na.rm = TRUE) -  quantile(log(Init_eq$Avg_income), probs = 0.1, na.rm = TRUE))
  print(quantile(log(Ct_Amenities$Avg_income), probs = 0.9, na.rm = TRUE) -  quantile(log(Ct_Amenities$Avg_income), probs = 0.1, na.rm = TRUE)) #90-10 difference increases
  
  print("The difference between q99 and q1 from Initial to Cftl is...")
  print(quantile(log(Init_eq$Avg_income), probs = 0.99, na.rm = TRUE) -  quantile(log(Init_eq$Avg_income), probs = 0.01, na.rm = TRUE))
  print(quantile(log(Ct_Amenities$Avg_income), probs = 0.99, na.rm = TRUE) -  quantile(log(Ct_Amenities$Avg_income), probs = 0.01, na.rm = TRUE)) #99-1 difference increases by even more!!!
  
  #By all accounts, segregation increases, which is very interesting. 
  
  #plot neighborhood income distributions
  ggplot() + 
    geom_histogram(data = Init_eq, aes(Avg_income), fill = "red", alpha = 0.2) + 
    geom_histogram(data = Ct_Amenities, aes(Avg_income), fill = "blue", alpha = 0.2) #Segregation increasing, blue clearly has much higher variance
  #Hard to reconcile with results above that turn toward declining segregation.
  
  Init_eq["IncomeStringency_rescaled"] <- Init_eq$IncomeStringency_cl/1000000
  reg <- gam(formula = log(Avg_income) ~ s(IncomeStringency_rescaled, k = 5, bs = "cr"), data = Init_eq)
  reg <- smooth_estimates(reg, n = 1000) %>% add_confint()
  ggplot() +
    geom_ribbon(data = reg, aes(ymin = lower_ci, ymax = upper_ci, x = IncomeStringency_rescaled), alpha = 0.2) +
    geom_line(data = reg, aes(x = IncomeStringency_rescaled, y = est)) +
    ylab("log Average income (Rescaled so average == 0)") + 
    xlab("Stringency (value of a minimal lot * fraction of block group population in regulated structures, Millions of USD)")
  
  #Doing the same with within-city variation
  reg <- gam(formula = demeaned_log_Income ~ s(demeaned_stringency, k = 5, bs = "cr"), data = Init_eq)
  reg <- smooth_estimates(reg, n = 1000) %>% add_confint()
  ggplot() +
    geom_ribbon(data = reg, aes(ymin = lower_ci, ymax = upper_ci, x = demeaned_stringency), alpha = 0.2) +
    geom_line(data = reg, aes(x = demeaned_stringency, y = est))
    #Its... U shaped... holy shit
  
  
  #Relative amenitites, within-city variation
  reg <- gam(formula = demeaned_rel_amenity ~ s(demeaned_stringency, k = 3, bs = "cr"), data = temp_df)
  reg <- smooth_estimates(reg, n = 1000) %>% add_confint()
  ggplot() +
    geom_ribbon(data = reg, aes(ymin = lower_ci, ymax = upper_ci, x = demeaned_stringency), alpha = 0.2) +
    geom_line(data = reg, aes(x = demeaned_stringency, y = est))
  
  #Linear reg
  print(summary(lm(demeaned_log_Income ~ demeaned_stringency, data = Init_eq))) #Positive relationship

  #This is the reason.... what's happening in the tails. Stringency is noisy here
  
}

rm(list = ls())