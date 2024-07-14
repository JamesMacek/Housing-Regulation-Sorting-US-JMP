#PACKAGES
library(sf)
library(haven) #Reading stata.dta files 
library(dplyr)
library(readr)
library(collapse)
library(stringr)
library(labelled)
library(estimatr)
library(here) #for maps api
library(purrr) #for openStreetMap
library(viridis) #for colors

#Plotting
library(ggplot2)
library(patchwork) #combining plots

#gam package (generalized additive models) for Fact1 and Fact2
library(mgcv)
library(gratia) #for helping to plot these

options(scipen = 5) #limit scientific

#Date created: August 26th, 2022
#requires output from a whole host of programs-- namely Merge_stringency.R and NaNDA_Construct.R

#LOGS:

#_______PARAMETERS & FUNCTIONS_________________________________________________________________________________
source("CodeV2/Facts/Parameters/Facts_parameters.R")
source("CodeV2/Facts/Functions/Facts_functions.R")
source("CodeV2/Counterfactual/Functions/OpenStreetMapping_Functions.R")
#______________________________________________________________________________________________________________

#importing data
load(file = "DataV2/US_Data/Output/CBSA_med_house_price.Rdata")
load(file = "DataV2/US_Data/Output/Constructed_Block_V2.Rdata")

#Converting density to square miles...
US_BLOCK$Housing_density <- US_BLOCK$Housing_density*640


#Quantiles of city distributions on various statistics for robustness
for (qtile in c("", "_dens", "_pop", "_wage")) {
  load(file = paste0("DataV2/US_Data/Output/CBSA_quantiles", qtile, ".Rdata"))
}

#Formulas to use for regressions
controls <-   "demeaned_median_bage + demeaned_household_size + demeaned_car_share + demeaned_family_share + dm_car_transport_share + 
               dm_public_transport_share + demeaned_avg_travel_time + demeaned_white_share + 
               demeaned_perf_arts_dens + demeaned_spec_sports_dens + 
               demeaned_casino_dens + demeaned_rec_act_dens + demeaned_prop_park_area_tract + 
               demeaned_count_tri_facilities + demeaned_stops_per_sqmile + demeaned_frestaurant_dens + 
               demeaned_fastfood_dens + demeaned_coffee_dens + demeaned_bar_dens + 
               demeaned_perennial_snow + demeaned_deciduous_forest + demeaned_evergreen_forest +
               demeaned_mixed_forest + demeaned_shrubs + demeaned_herbaceous + demeaned_woody_wetlands + 
               demeaned_herbaceous_wetlands + rank_inv_D2CBD" #baseline set of controls

#BASELINE FORMULAS for GAM models
excluded_controls_income_formula <- as.formula('demeaned_log_Income ~ s(rank_density_CBSA, k = 1, bs = "cr") + rank_inv_D2CBD')
included_controls_income_formula <- as.formula(paste('demeaned_log_Income ~ s(rank_density_CBSA, k = 1, bs = "cr") + ', controls, sep = " ")) 
excluded_controls_stringency_formula <- as.formula('demeaned_stringency ~ s(rank_density_CBSA, k = 1, bs = "cr") + rank_inv_D2CBD')
included_controls_stringency_formula <- as.formula(paste('demeaned_stringency ~ s(rank_density_CBSA, k = 1, bs = "cr") + ', controls, sep = " ")) 

#Additional variables for analysis
US_BLOCK["log_Average_income"] <- log(US_BLOCK$Average_income)
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_LandValueDensity = (LandValueDensity_matched - mean(LandValueDensity_matched, na.rm = TRUE))/1000000) #in millions

#Baseline Sample definitions of superstar cities based on productivity
top <- US_BLOCK[US_BLOCK$PooledWage > as.numeric(quantile_CBSA_wage["75.0%"]),]
bot <- US_BLOCK[US_BLOCK$PooledWage < as.numeric(quantile_CBSA_wage["75.0%"]),]

baselineSampleNames =  list("Superstar" = "Top 25% \n Productivity",
                            "nonSuperstar" = "All other cities") #Passes to flexibleEstimation function 
                                        
#_____________________________________________________________________________________________
#Fact1: " Slightly Stronger income sorting on density in dense (large) cities "
#_____________________________________________________________________________________________
 
#Baseline regression of income on density, 
Income.plot <- flexibleEstimation(Dataframe_list = list("Superstar" = top,
                                                        "nonSuperstar" = bot),
                                  formula = excluded_controls_income_formula,
                                  SampleNames = baselineSampleNames  ) +
                    xlab("Density quantiles (Block Group level)") +
                    ylab("Log Average Income (demeaned by MSA)") +  
                    theme_gray(base_size = 15) & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
ggsave("DataV2/US_Data/Output/income.png", plot = Income.plot, 
        width = 24, height = 15, units = "cm")


#Residualized versions of these plots...
Income_resid.plot <- flexibleEstimation(Dataframe_list = list("Superstar" = top,
                                                               "nonSuperstar" = bot),
                                        formula = included_controls_income_formula,
                                        SampleNames = baselineSampleNames ) +
  xlab("Density quantiles (Block Group level)") +
  ylab("Log Average Income \n (demeaned by MSA, residualized by controls)") +  
  theme_gray(base_size = 15) & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
ggsave("DataV2/US_Data/Output/income_residualized.png", plot = Income_resid.plot, 
       width = 24, height = 15, units = "cm")

#Combining residualized plots for paper
(Income.plot + ggtitle("Panel A")) + (Income_resid.plot + ggtitle("Panel B")) + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
ggsave("DataV2/US_Data/Output/income_combined.png", width = 24, height = 15, units = "cm")


#_______CHECKING LINEAR REGRESSION RESULTS DIRECTLY____________________________________________
#Using a linear regression to check relative slopes in income
linear_income_formula <- as.formula(paste('demeaned_log_Income ~ rank_density_CBSA'))
print(summary(lm_robust(formula = linear_income_formula, data = top)))
print(summary(lm_robust(formula = linear_income_formula, data = bot)))

linear_income_formula <- as.formula(paste('demeaned_log_Income ~ rank_density_CBSA + ', controls, sep = " "))
print(summary(lm_robust(formula = linear_income_formula, data = top)))
print(summary(lm_robust(formula = linear_income_formula, data = bot)))

#_________________________________________________________________________________
# Fact2: "This variation is explained by the prices of minimal lots"
#_________________________________________________________________________________

Stringency.plot <- flexibleEstimation(Dataframe_list = list("Superstar" = top,
                                                            "nonSuperstar" = bot),
                                      formula = excluded_controls_stringency_formula,
                                      SampleNames = baselineSampleNames   ) +
                  xlab("Density quantiles (Block Group level)") +
                  ylab("Regulatory Stringency (demeaned by MSA)") + 
                  theme_gray(base_size = 15) & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
ggsave("DataV2/US_Data/Output/income_stringency.png", plot = Stringency.plot, width = 24, height = 15, units = "cm")
  
  
Stringency_resid.plot <- flexibleEstimation(Dataframe_list = list("Superstar" = top,
                                                                  "nonSuperstar" = bot),
                                            formula = included_controls_stringency_formula,
                                            SampleNames = baselineSampleNames   ) +
                         xlab("Density quantiles (Block Group level)") +
                         ylab(paste0("Regulatory Stringency", " \n ", "(demeaned by MSA, residualized by controls)")) & 
                         theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
ggsave("DataV2/US_Data/Output/income_stringency_residualized.png", plot = Stringency_resid.plot, width = 24, height = 15, units = "cm")

  #Decomposing variation into three components:
  
  #1) Physical density restriction (units/acre)
    excluded_controls_density_formula <- as.formula('demeaned_densrestriction ~ s(rank_density_CBSA, k = 5, bs = "cr") + rank_inv_D2CBD')
    
    flexibleEstimation(Dataframe_list = list("Superstar" = top,
                                             "nonSuperstar" = bot),
                       formula = excluded_controls_density_formula,
                       SampleNames = baselineSampleNames   ) +
      xlab("Ranked housing unit density (Block Group level)") +
      ylab("Density Restriction (Acres/Unit) (demeaned by MSA)") + theme_gray(base_size = 15) & 
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
ggsave("DataV2/US_Data/Output/income_stringency_physLotSize.png", width = 24, height = 15, units = "cm")
  
  #2) Land Values

    excluded_controls_density_formula <- as.formula('demeaned_LandValueDensity ~ s(rank_density_CBSA, k = 5, bs = "cr") + rank_inv_D2CBD')
    flexibleEstimation(Dataframe_list = list("Superstar" = top,
                                           "nonSuperstar" = bot),
                       formula = excluded_controls_density_formula,
                       SampleNames = baselineSampleNames   ) +
    xlab("Ranked housing unit density (Block Group level)") +
    ylab("Land value ($1m/acre) (demeaned by MSA)") + theme_gray(base_size = 15) & 
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  ggsave("DataV2/US_Data/Output/income_stringency_LandValues.png", width = 24, height = 15, units = "cm")

  #3) Regulated housing unit shares
  
    excluded_controls_density_formula <- as.formula('demeaned_regulated_housing_share ~ s(rank_density_CBSA, k = 5, bs = "cr") + rank_inv_D2CBD')
    flexibleEstimation(Dataframe_list = list("Superstar" = top,
                                             "nonSuperstar" = bot),
                       formula = excluded_controls_density_formula,
                       SampleNames = baselineSampleNames   ) +
      xlab("Ranked housing unit density (Block Group level)") +
      ylab("Share of housing units in Regulated Structures (demeaned by MSA)") + theme_gray(base_size = 15) & 
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  ggsave("DataV2/US_Data/Output/income_stringency_RegUnitShare.png", width = 24, height = 15, units = "cm")
  
    
#______________________________________________________________________________________________
# Replicating negative income sorting on density plots allowing for average intercept to differ
#______________________________________________________________________________________________

#Plotting these residualized plots, storing...
      flexibleEstimation(Dataframe_list = list("Superstar" = top,
                                         "nonSuperstar" = bot),
                         formula = excluded_controls_income_formula,
                         SampleNames = baselineSampleNames,
                         interceptDifference = "log_Average_income") + 
                        xlab("Ranked housing unit density (Block Group level)") +
                        ylab(paste0("Log Average Income", "\n", "(demeaned by MSA, intercept varies by sample)")) +
                        theme_gray(base_size = 15) &
                        theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
ggsave("DataV2/US_Data/Output/income_int.png", width = 24, height = 15, units = "cm")


#______________________________________________________________________________________________
# Regressions allowing for average intercept to vary: Demeaned stringency
#______________________________________________________________________________________________
Stringency_intercept.plot <- flexibleEstimation(Dataframe_list = list("Superstar" = top,
                                                                      "nonSuperstar" = bot),
                                                formula = excluded_controls_stringency_formula,
                                                SampleNames = baselineSampleNames,
                                                interceptDifference = "IncomeStringency_cl") + 
                             xlab("Density quantiles (Block Group level)") +
        labs(y = paste0("Regulatory Stringency", "\n", "(demeaned by MSA, intercept varies by sample)")) +
        ggtitle("Panel B") + theme_gray(base_size = 15)

#Combined plot for paper 
Stringency.plot + ggtitle("Panel A") + Stringency_intercept.plot + plot_layout(guides = "collect") & 
                  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
ggsave("DataV2/US_Data/Output/incomestringency_combined.png", width = 24, height = 15, units = "cm")


#________________RAW REGRESSIONS OF INCOME ON STRINGENCY________________________
print("Raw regressions of income on stringency (linear specification")
print(summary(lm( Average_income ~ IncomeStringency_cl, data = US_BLOCK))) #almost uncorrelated!

print("Raw regression removing neighborhoods where minimal lot costs more than 1 million a year -- no wealth channel")
print(summary(lm( Average_income ~ IncomeStringency_cl, data = US_BLOCK[US_BLOCK$IncomeStringency_cl < 2500000,])))

#__________________________________________________________________________________


#Read shpfile:
  rm(top, bot)
  blkgeo <- st_read("DataV2/US_Data/Shapefiles/US_blck_grp_2020.shp") %>% 
                    mutate(State = as.numeric(STATEFP), County = as.numeric(COUNTYFP), Tract = as.numeric(TRACTCE),
                           BlockGroup = as.numeric(BLKGRPCE)) %>%
                    select(State, County, Tract, BlockGroup)
  US_BLOCK <- inner_join(blkgeo, US_BLOCK)
  rm(blkgeo)
  US_BLOCK <- st_transform(US_BLOCK, 4326) #to lat/lon coordinates system
  
  
  #To do: measure income density gradients, regress on productivity. 
    #todo: avoid cities where we observe no MLS 
  US_CBSA_2010_c["Income_dens_gradient"] <- rep(NA, nrow(US_CBSA_2010_c))
  for (CBSA in sort(unique(US_BLOCK$CBSA_NAME))) {
   US_CBSA_2010_c$Income_dens_gradient[US_CBSA_2010_c$CBSA_NAME == CBSA] <- 
    lm(demeaned_log_Income ~ rank_density_CBSA + rank_inv_D2CBD, data = US_BLOCK[US_BLOCK$CBSA_NAME == CBSA,])$coefficients["rank_density_CBSA"]
    
  }
  
  #Regression -- unit wage increase associated with -1.25 differences in income-density gradient. Pretty large!
  print(summary(lm_robust(formula = Income_dens_gradient ~ PooledWage, data = US_CBSA_2010_c)))
  
  


  

#__________________________________________________________________________________________

rm(list = ls())
