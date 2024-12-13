#Date created: Feb 18th, 2023

#This file shows what happens in the complete deregulation counterfactual in terms of the spatial distribution of activity (primarily within cities)
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
source("CodeV2/Counterfactual/Functions/Analysis_Functions.R")
source("CodeV2/Facts/Functions/Facts_functions.R") #Plotting link of functions to facts

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
Ct_Amenities <- Equilibrium_objects_output
rm(Equilibrium_objects)

#Importing all files from Counterfactual_Output (Exogenous amenities)
load(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", "Eq_Objects_FULL", 
            "_EndoAmen_", FALSE, 
            "_EndoProd_", FALSE,
            "_bySkill_", BASELINE_SPECIFICATION$bySkill_to_pass,
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
Ct_NoAmenities <- Equilibrium_objects_output
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
#Setting NA to 0 (as we do before)
Init_eq$IncomeStringency_cl[is.na(Init_eq$IncomeStringency_cl)]  <- 0

#demeaning income
Init_eq <- Init_eq %>% group_by(CBSA) %>% mutate(demeaned_log_Income = log(Avg_income) - mean(log(Avg_income), na.rm = TRUE))
Ct_Amenities <- Ct_Amenities %>% group_by(CBSA) %>% mutate(demeaned_log_Income = log(Avg_income) - mean(log(Avg_income), na.rm = TRUE))
Ct_NoAmenities <- Ct_NoAmenities %>% group_by(CBSA) %>% mutate(demeaned_log_Income = log(Avg_income) - mean(log(Avg_income), na.rm = TRUE))

#New ranking of relative density
population_Ct <- rep(0, nrow(Ct_Amenities))
population_Init <- rep(0, nrow(Init_eq))
population_CtNoAm <- rep(0, nrow(Ct_NoAmenities))

#Populations (and their equilibrium response)
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

#Retain initial CBSA density ranking and CBD distance ranking
Ct_Amenities["rank_density_CBSA"] <- Init_eq$rank_density_CBSA
Ct_NoAmenities["rank_density_CBSA"] <- Init_eq$rank_density_CBSA
Ct_Amenities["rank_inv_D2CBD"] <- Init_eq$rank_inv_D2CBD
Ct_NoAmenities["rank_inv_D2CBD"] <- Init_eq$rank_inv_D2CBD


#Formula for income
excluded_controls_income_formula <- as.formula('demeaned_log_Income ~ s(rank_density_CBSA, k = 1, bs = "cr") + rank_inv_D2CBD')


#__________________________________________________________________________________________________

#Importing quantiles from empirical facts
#Quantiles of city distributions on various statistics for robustness
for (qtile in c("", "_dens", "_pop", "_wage")) {
  load(file = paste0("DataV2/US_Data/Output/CBSA_quantiles", qtile, ".Rdata"))
}

#Baseline Sample definitions of superstar cities based on productivity
top_init <- Init_eq[Init_eq$PooledWage > as.numeric(quantile_CBSA_wage["75.0%"]),]
bot_init <- Init_eq[Init_eq$PooledWage < as.numeric(quantile_CBSA_wage["75.0%"]),]

top_ct <- Ct_Amenities[Ct_Amenities$PooledWage > as.numeric(quantile_CBSA_wage["75.0%"]),]
bot_ct <- Ct_Amenities[Ct_Amenities$PooledWage < as.numeric(quantile_CBSA_wage["75.0%"]),]

baselineSampleNames =  list("Superstar" = "Top 25% \n Productivity",
                            "nonSuperstar" = "All other cities") #Passes to flexibleEstimation function 
#__________________________________________________________________________________________________


#___________________________
# START FIGURES HERE
#___________________________

  #1. Replicating Fact 1 under counterfactual equilibrium
Income.plot.new <- flexibleEstimation(Dataframe_list = list("Superstar" = top_ct,
                                                                "nonSuperstar" = bot_ct),
                                          formula = excluded_controls_income_formula,
                                          SampleNames = baselineSampleNames) +
                       xlab("Density quantiles (Block Group level)") +
                       ylab("Log Average Income (demeaned by MSA)") + ggtitle("Panel B: Counterfactual Equilibrium") +
                       theme_gray(base_size = 15) & 
                       theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))



  #Plotting initial for reference
Income.plot.initial <- flexibleEstimation(Dataframe_list = list("Superstar" = top_init,
                                                                "nonSuperstar" = bot_init),
                                          formula = excluded_controls_income_formula,
                                          SampleNames = baselineSampleNames) +
                                     xlab("Density quantiles (Block Group level)") +
                                     ylab("Log Average Income (demeaned by MSA)") + ggtitle("Panel A: Initial Equilibrium") +
                                     theme_gray(base_size = 15) & 
                                     theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

Income.plot.initial + Income.plot.new + plot_layout(guides = "collect") & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/IncomeDensityGradCtfl",
              BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
              BASELINE_SPECIFICATION$pref, ".png"),
              width = 24, height = 15, units = "cm") 


#CHECKING GRADIENT VALUES!!!!!
print("COUNTERFACTUAL AT BASELINE GRADIENTS")
print("\n")
print( summary(lm(demeaned_log_Income ~ rank_density_CBSA + rank_inv_D2CBD, data = top_ct) ))
print( summary(lm(demeaned_log_Income ~ rank_density_CBSA + rank_inv_D2CBD, data = bot_ct) )) #Same level!
print("INITIAL AT BASELINE GRADIENTS")
print("\n")
print( summary(lm(demeaned_log_Income ~ rank_density_CBSA + rank_inv_D2CBD, data = top_init) ))
print( summary(lm(demeaned_log_Income ~ rank_density_CBSA + rank_inv_D2CBD, data = bot_init) )) 

#_______________________________________________________________
# Alternate graph that compares baseline to counterfactual _____
#_______________________________________________________________
reg_t25 <- gam(formula = excluded_controls_income_formula,
               data = bot_init) #use high smoothing penalty gamma for this application, though it doesn't matter much. 

reg_b25 <- gam(formula = excluded_controls_income_formula,
               data = bot_ct) 

#Plotting manually with GGPlot2 (Extracting partially linear plot using gratia package)
t25_smooth <- smooth_estimates(reg_t25, n = 1000) %>%
  add_confint()  #from gratia, extracts gam estimates + add confidence intervals
b25_smooth <- smooth_estimates(reg_b25, n = 1000) %>% #unconditional confidence intervals not working for some reason. i.e. arg unconditional = TRUE does not work
  add_confint()

#plot bottom
Income.plot.initial_forPres <- ggplot() +
  geom_ribbon(data = t25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
  geom_line(data = t25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Initial Equilibrium')) + 
  
  geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = b25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Counterfactual')) + 
  scale_colour_manual(name="Sample", values = c("yellow","purple")) + 
  xlab("Density quantile (Block Group level)") +
  ylab("Log Average Income (demeaned by MSA)") +
  ggtitle("Panel B: \n All other cities") + theme_gray(base_size = 15) & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

#Repeating for high productivity cities
reg_t25 <- gam(formula = excluded_controls_income_formula,
               data = top_init) #use high smoothing penalty gamma for this application, though it doesn't matter much. 

reg_b25 <- gam(formula = excluded_controls_income_formula,
               data = top_ct) 

#Plotting manually with GGPlot2 (Extracting partially linear plot using gratia package)
t25_smooth <- smooth_estimates(reg_t25, n = 1000) %>%
  add_confint()  #from gratia, extracts gam estimates + add confidence intervals
b25_smooth <- smooth_estimates(reg_b25, n = 1000) %>% #unconditional confidence intervals not working for some reason. i.e. arg unconditional = TRUE does not work
  add_confint()

#Plot top
Income.plot.new_forPres <- ggplot() +
  geom_ribbon(data = t25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
  geom_line(data = t25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Initial Equilibrium')) + 
  
  geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
  geom_line(data = b25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Counterfactual')) + 
  scale_colour_manual(name="Sample", values = c("yellow","purple")) + 
  xlab("Density quantile (Block Group level)") +
  ylab("Log Average Income (demeaned by MSA)") +
  ggtitle("Panel A: \n Top 25% Productivity") + theme_gray(base_size = 15) & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

#Plotting poster plot
Income.plot.new_forPres + Income.plot.initial_forPres + plot_layout(guides = "collect") & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/IncomeDensityGradCtfl_alternate_",
              BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
              BASELINE_SPECIFICATION$pref, ".png"),
                  width = 24, height = 15, units = "cm") 



#______________________________________________________________
#__ Changes in Density Gradients_______________________________
#______________________________________________________________


#Demeaning housing density in levels...
Ct_Amenities <- Ct_Amenities %>% group_by(CBSA) %>% mutate(demeaned_Housing_density = Housing_density/mean(Housing_density, na.rm = TRUE))
Init_eq <- Init_eq %>% group_by(CBSA) %>% mutate(demeaned_Housing_density = Housing_density/mean(Housing_density, na.rm = TRUE))
Ct_NoAmenities <- Ct_NoAmenities %>% group_by(CBSA) %>% mutate(demeaned_Housing_density = Housing_density/mean(Housing_density, na.rm = TRUE))


#Running regressions

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

#_____________________________________________________________________________________________________________________________________
#PART 2: ANALYZE SEGREGATION PATTERNS ____________________________________________________________________________________________________
#_____________________________________________________________________________________________________________________________________

#Correlation between initial stringency and incomes
print(paste0("The correlation between initial stringency and log income changes in the cross section is (in levels) is " ,
             cor(Init_eq[Init_eq$IncomeStringency_model_rents < regulation_censoring,]$IncomeStringency_cl, 
                 log(Ct_Amenities[Init_eq$IncomeStringency_model_rents < regulation_censoring,]$Avg_income) - 
                 log(Init_eq[Init_eq$IncomeStringency_model_rents < regulation_censoring,]$Avg_income), use = "complete.obs") ))
#Note: we account for the fact that the income stringency measure was censored in the model at regulation_censoring (otherwise, correlation greatley reduced because of outlier sensitivity)
#   Alternatively, log correlation higher because of outlier limitation

#Correlation between initial income and income changes
print(paste0("The correlation between initial income and income changes in the cross section " ,
             cor(log(Init_eq$Avg_income),log(Ct_Amenities$Avg_income) - log(Init_eq$Avg_income), use = "complete.obs") )) #Noisy income changes. Is this why "segregation" increases?


#Folloiwng code only works if bySkill == FALSE
#______________________________________________________________________________________________________
if (BASELINE_SPECIFICATION$bySkill == FALSE) {
  
  #Creating stringency measures (note--we use log stringency here which drops many zeros. Similar results hold for levels, though worse fit)
  #If we did the same exercise in levels, conclusions would be even more stark.
  Init_eq["log_IncomeStringency_cl"] <- log(Init_eq$IncomeStringency_cl)
  Init_eq$log_IncomeStringency_cl[is.infinite(Init_eq$log_IncomeStringency_cl)] <- NA
  Ct_Amenities["log_IncomeStringency_cl"] <- Init_eq$log_IncomeStringency_cl
  Init_eq <- Init_eq %>% group_by(CBSA) %>% mutate(demeaned_log_stringency = log_IncomeStringency_cl - mean(log_IncomeStringency_cl, na.rm = TRUE))
  Ct_Amenities["demeaned_log_stringency"] <- Init_eq$demeaned_log_stringency
  
  #Correlations between income and initial stringency before and after deregulation; tells us something about
  #whether MLS is optimally targeting certain neighborhoods...
  
  print("Within and Across city correlation between log(income stringency) and income AFTER DEREGULATION")
  print(summary(lm(log(Avg_income) ~ log_IncomeStringency_cl, data = Ct_Amenities) ) ) 
  print(summary(lm(demeaned_log_Income ~ demeaned_log_stringency, data = Ct_Amenities) ) )
  
  print("AND BEFORE DEREGULATION...")
  print(summary(lm(log(Avg_income) ~ log_IncomeStringency_cl, data = Init_eq) ) ) 
  print(summary(lm(demeaned_log_Income ~ demeaned_log_stringency, data = Init_eq) ) )
  
  #Putting these regressions into a graph
  ggplot() + 
                        geom_point(data = Init_eq, aes(x = log_IncomeStringency_cl, y = log(Avg_income)), color = "red", size = 0.25, alpha = 0.1) +
                        geom_point(data = Ct_Amenities, aes(x = log_IncomeStringency_cl, y = log(Avg_income)), color = "blue", size = 0.25, alpha = 0.05) +
                        geom_smooth(data = Init_eq, aes(x = log_IncomeStringency_cl, y = log(Avg_income), color = "Initial"), method = "lm") +
                        geom_smooth(data = Ct_Amenities, aes(x = log_IncomeStringency_cl, y = log(Avg_income), color = "Counterfactual"), method = "lm") +
                        scale_colour_manual(name="Equilibrium", values = c("royalblue4", "red4")) + 
                        xlab("log Lot Size Stringency (from Initial Equilibrium)") + 
                        ylab("log Average Income") + 
                        coord_cartesian(xlim = c(min(Init_eq[Init_eq$log_IncomeStringency_cl > quantile(Init_eq$log_IncomeStringency_cl, 
                                                                                                        probs = c(0.01), na.rm = TRUE),]$log_IncomeStringency_cl, na.rm = TRUE),
                                        
                                                 max(Init_eq[Init_eq$log_IncomeStringency_cl < quantile(Init_eq$log_IncomeStringency_cl, 
                                                                                                            probs = c(0.99), na.rm = TRUE),]$log_IncomeStringency_cl, na.rm = TRUE)),
                                        ylim = c(9.5, 13)) #Cut off x axis at 1th and 99th percentile for plot, same with y
  
  ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/IncomeStringencyCorrelation_aftdereg_",
                                      BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                                      BASELINE_SPECIFICATION$pref, ".png"), width = 24, height = 15, units = "cm")
  
  
  
  #plot neighborhood income distributions
  ggplot() + 
    geom_histogram(data = Init_eq, aes(x = Avg_income/1000), fill = "red", alpha = 0.2) + 
    geom_histogram(data = Ct_Amenities, aes(x = Avg_income/1000), fill = "blue", alpha = 0.2) +
    xlab("Average Income (Block Group, $1000's)") +
    ylab("Counts") +
    ggtitle("Neighborhood Income distribution (counterfactual is blue)")
  ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/NeighborhoodIncomeDist_",
                  BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                  BASELINE_SPECIFICATION$pref, ".png"), width = 24, height = 15, units = "cm")
  
}

rm(list = ls())