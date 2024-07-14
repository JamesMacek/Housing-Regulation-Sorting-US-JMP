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

#_______PARAMETERS______________________________________________________________
source("CodeV2/Facts/Parameters/Facts_parameters.R")
source("CodeV2/Facts/Functions/Facts_functions.R")
source("CodeV2/Counterfactual/Functions/OpenStreetMapping_Functions.R")
#_______________________________________________________________________________


#importing data
load(file = "DataV2/US_Data/Output/CBSA_med_house_price.Rdata")
load(file = "DataV2/US_Data/Output/Constructed_Block_V2.Rdata")

#Quantiles of city distributions on various statistics for robustness
for (qtile in c("", "_dens", "_pop", "_wage")) {
  load(file = paste0("DataV2/US_Data/Output/CBSA_quantiles", qtile, ".Rdata"))
}

#City weights (weight observations by 1/city size in terms of # block groups)
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(citywt = 1/n())

#Weighted by number of households
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(household_wt = Housing_density*(LAND_AREA))

#Converting density from acres to square miles..
US_BLOCK$Housing_density <- US_BLOCK$Housing_density*640


#Quantiles of city distributions on various statistics for robustness
for (qtile in c("", "_dens", "_pop", "_wage")) {
  load(file = paste0("DataV2/US_Data/Output/CBSA_quantiles", qtile, ".Rdata"))
}




#Formulas for estimation
controls_noCBD <- "+ demeaned_median_bage + demeaned_household_size + demeaned_car_share + demeaned_family_share + dm_car_transport_share + 
               dm_public_transport_share + demeaned_avg_travel_time + demeaned_white_share +
               demeaned_perf_arts_dens + demeaned_spec_sports_dens + 
               demeaned_casino_dens + demeaned_rec_act_dens + demeaned_prop_park_area_tract + 
               demeaned_count_tri_facilities + demeaned_stops_per_sqmile + demeaned_frestaurant_dens + 
               demeaned_fastfood_dens + demeaned_coffee_dens + demeaned_bar_dens + 
               demeaned_perennial_snow + demeaned_deciduous_forest + demeaned_evergreen_forest +
               demeaned_mixed_forest + demeaned_shrubs + demeaned_herbaceous + demeaned_woody_wetlands + 
               demeaned_herbaceous_wetlands"

controls <- paste0(controls_noCBD, " + rank_inv_D2CBD") #baseline controls

controls_demo <- paste0(controls, "demeaned_college_share") #additional demographic controls --results are robust to controlling college share, though attenuated

#baseline excluded controls, control for distance to CBD
excluded_controls_income_formula <- as.formula('demeaned_log_Income ~ s(rank_density_CBSA, k = 1, bs = "cr") + rank_inv_D2CBD')
included_controls_income_formula <- as.formula(paste('demeaned_log_Income ~ s(rank_density_CBSA, k = 1, bs = "cr")', controls, sep = " ")) 
excluded_controls_stringency_formula <- as.formula('demeaned_stringency ~ s(rank_density_CBSA, k = 1, bs = "cr") + rank_inv_D2CBD')
included_controls_stringency_formula <- as.formula(paste('demeaned_stringency ~ s(rank_density_CBSA, k = 1, bs = "cr")', controls, sep = " ")) 

#Additional variables
US_BLOCK["log_Average_income"] <- log(US_BLOCK$Average_income)
#Demeaning housing unit density by MSA
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_log_Housing_density = log(Housing_density) - mean(log(Housing_density), na.rm = TRUE))


#Datasets (various for robustness)
top <- US_BLOCK[US_BLOCK$PooledWage > as.numeric(quantile_CBSA_wage["75.0%"]),]
bot <- US_BLOCK[US_BLOCK$PooledWage < as.numeric(quantile_CBSA_wage["75.0%"]),]

baselineSampleNames =  list("Superstar" = "Top 25% \n Productivity",
                            "nonSuperstar" = "All other cities") #Passes to flexibleEstimation function 

#_______________________________________________________________________________________________
#Robustness 1: Different sample definitions of cities -- top 10 and 50%

#Loop over alternative defintions
for (alt_defn in c(50, 90)) { #50th and 90th quartiles
  
  #Alternative definitions of the income plot
  flexibleEstimation(Dataframe_list = list("Superstar" =    US_BLOCK[ US_BLOCK$PooledWage > as.numeric(quantile_CBSA_wage[paste0(alt_defn, ".0%")]), ],
                                           "nonSuperstar" = US_BLOCK[ US_BLOCK$PooledWage < as.numeric(quantile_CBSA_wage[paste0(alt_defn, ".0%")]), ]),
                     formula = included_controls_income_formula,
                     SampleNames = list("Superstar" = paste0("Top ", 100 - alt_defn, "%", " \n Productivity"),
                                        "nonSuperstar" = "All other cities")   ) +
    xlab("Ranked housing unit density (Block Group level)") +
    ylab("Log Average Income (demeaned by MSA, residualized by controls)") + 
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  ggsave(paste0("DataV2/US_Data/Output/Robustness/income_t", 100 - alt_defn ,"_residualized.png"), 
         width = 24, height = 15, units = "cm")
  
  #Stringency plot
  flexibleEstimation(Dataframe_list = list("Superstar" = US_BLOCK[US_BLOCK$PooledWage > as.numeric(quantile_CBSA_wage[paste0(alt_defn, ".0%")]), ],
                                           "nonSuperstar" = US_BLOCK[US_BLOCK$PooledWage < as.numeric(quantile_CBSA_wage[paste0(alt_defn, ".0%")]), ]),
                     formula = included_controls_stringency_formula,
                     SampleNames = list("Superstar" = paste0("Top ", 100 - alt_defn, "%", " \n Productivity"),
                                        "nonSuperstar" = "All other cities")   ) +
    xlab("Ranked housing unit density (Block Group level)") +
    ylab(paste0("Regulatory Stringency", " \n ", "(demeaned by MSA, residualized by controls)")) + 
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  ggsave(paste0("DataV2/US_Data/Output/Robustness/stringency_t", 100 - alt_defn ,"_residualized.png"), 
         width = 24, height = 15, units = "cm")
  
  
  #Alternative definitions of the income plot
  flexibleEstimation(Dataframe_list = list("Superstar" =    US_BLOCK[ US_BLOCK$PooledWage > as.numeric(quantile_CBSA_wage[paste0(alt_defn, ".0%")]), ],
                                           "nonSuperstar" = US_BLOCK[ US_BLOCK$PooledWage < as.numeric(quantile_CBSA_wage[paste0(alt_defn, ".0%")]), ]),
                     formula = excluded_controls_income_formula,
                     SampleNames = list("Superstar" = paste0("Top ", 100 - alt_defn, "%", " \n Productivity"),
                                        "nonSuperstar" = "All other cities")   ) +
    xlab("Ranked housing unit density (Block Group level)") +
    ylab("Log Average Income (demeaned by MSA)") + 
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  ggsave(paste0("DataV2/US_Data/Output/Robustness/income_t", 100 - alt_defn ,".png"), 
         width = 24, height = 15, units = "cm")
  
  #Stringency plot
  flexibleEstimation(Dataframe_list = list("Superstar" = US_BLOCK[US_BLOCK$PooledWage > as.numeric(quantile_CBSA_wage[paste0(alt_defn, ".0%")]), ],
                                           "nonSuperstar" = US_BLOCK[US_BLOCK$PooledWage < as.numeric(quantile_CBSA_wage[paste0(alt_defn, ".0%")]), ]),
                     formula = excluded_controls_stringency_formula,
                     SampleNames = list("Superstar" = paste0("Top ", 100 - alt_defn, "%", " \n Productivity"),
                                        "nonSuperstar" = "All other cities")   ) +
    xlab("Ranked housing unit density (Block Group level)") +
    ylab(paste0("Regulatory Stringency", " \n ", "(demeaned by MSA)")) + 
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
  ggsave(paste0("DataV2/US_Data/Output/Robustness/stringency_t", 100 - alt_defn ,".png"), 
         width = 24, height = 15, units = "cm")
}

#___________________________________________________________________________________________________________________

#Robustness 2: Alternative definitions of weights, cities weighted equally, block groups weighted by population

  flexibleEstimation(Dataframe_list = list("Superstar" = top,
                                           "nonSuperstar" = bot),
                                           formula = included_controls_income_formula,
                                           SampleNames = baselineSampleNames,
                                           weights = "city") +
  xlab("Ranked housing unit density (Block Group level)") +
  ylab(paste0("log Average Income", " \n ", "(demeaned by MSA, residualized by controls)")) + 
  ggtitle("Cities weighted evenly") & 
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 
ggsave("DataV2/US_Data/Output/Robustness/income_citywt.png",  width = 24, height = 15, units = "cm")

  flexibleEstimation(Dataframe_list = list("Superstar" = top,
                                           "nonSuperstar" = bot),
                     formula = included_controls_stringency_formula,
                     SampleNames = baselineSampleNames,
                     weights = "city") +
  xlab("Ranked housing unit density (Block Group level)") +
  ylab(paste0("Regulatory Stringency", " \n ", "(demeaned by MSA, residualized by controls)")) + 
  ggtitle("Cities weighted evenly") & 
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 
ggsave("DataV2/US_Data/Output/Robustness/stringency_citywt.png",  width = 24, height = 15, units = "cm")

flexibleEstimation(Dataframe_list = list("Superstar" = top,
                                         "nonSuperstar" = bot),
                   formula = included_controls_income_formula,
                   SampleNames = baselineSampleNames,
                   weights = "household") +
  xlab("Ranked housing unit density (Block Group level)") +
  ylab(paste0("log Average Income", " \n ", "(demeaned by MSA, residualized by controls)")) + 
  ggtitle("Block groups weighted by # households") & 
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 
ggsave("DataV2/US_Data/Output/Robustness/income_householdwt.png",  width = 24, height = 15, units = "cm")

flexibleEstimation(Dataframe_list = list("Superstar" = top,
                                         "nonSuperstar" = bot),
                   formula = included_controls_stringency_formula,
                   SampleNames = baselineSampleNames,
                   weights = "household") +
  xlab("Ranked housing unit density (Block Group level)") +
  ylab(paste0("Regulatory Stringency", " \n ", "(demeaned by MSA, residualized by controls)")) + 
  ggtitle("Block groups weighted by # households") & 
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 
ggsave("DataV2/US_Data/Output/Robustness/stringency_householdwt.png",  width = 24, height = 15, units = "cm")

  #To do: weight by block group population. (# of households)


#________________________________________________________________________________________
#Using distance to CBD AS ROBUSTNESS CHECK
#________________________________________________________________________________________
excluded_controls_income_formula_CBD <- as.formula('demeaned_log_Income ~ s(rank_inv_D2CBD, k = 1, bs = "cr")')
included_controls_income_formula_CBD <- as.formula(paste0('demeaned_log_Income ~ s(rank_inv_D2CBD, k = 1, bs = "cr") ', controls_noCBD))
excluded_controls_stringency_formula_CBD <- as.formula('demeaned_stringency ~ s(rank_inv_D2CBD, k = 1, bs = "cr")')
included_controls_stringency_formula_CBD <- as.formula(paste0('demeaned_stringency ~ s(rank_inv_D2CBD, k = 1, bs = "cr") ', controls_noCBD))


CBD_income.plot <-  flexibleEstimation(Dataframe_list = list("Superstar" = top,
                                                             "nonSuperstar" = bot),
                                       formula = included_controls_income_formula_CBD,
                                       SampleNames = baselineSampleNames,
                                       CBD = TRUE) +
                    xlab("Ranked Inverse Distance to CBD (Block Group level)") +
                    ylab(paste0("log Average Income", " \n ", "(demeaned by MSA, residualized by controls)")) + 
                    ggtitle("Panel A") & 
                    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 

CBD_stringency.plot <-  flexibleEstimation(Dataframe_list = list("Superstar" = top,
                                                             "nonSuperstar" = bot),
                                       formula = included_controls_stringency_formula_CBD,
                                       SampleNames = baselineSampleNames,
                                       CBD = TRUE) +
                        xlab("Ranked Inverse Distance to CBD (Block Group level)") +
                        ylab(paste0("Regulatory Stringency", " \n ", "(demeaned by MSA, residualized by controls)")) + 
                        ggtitle("Panel B") & 
                        theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 

CBD_income.plot + CBD_stringency.plot + plot_layout(guides = "collect") & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
ggsave("DataV2/US_Data/Output/Robustness/CBD_combined.png", width = 24, height = 15, units = "cm")

#___________________________________________________________________
#___IMPUTING NAs in IncomeStringency_cl as zeros, as in the model___
#___________________________________________________________________

#new dataframes with imputations... 
top_ext <- top
bot_ext <- bot
top_ext$IncomeStringency_cl[is.na(top_ext$IncomeStringency_cl)] <- 0
bot_ext$IncomeStringency_cl[is.na(bot_ext$IncomeStringency_cl)] <- 0

#Plotting
    flexibleEstimation(Dataframe_list = list("Superstar" = top_ext,
                                             "nonSuperstar" = bot_ext),
                       formula = excluded_controls_stringency_formula,
                       SampleNames = baselineSampleNames) +
     xlab("Ranked housing unit density (Block Group level)") + 
     ylab("Regulatory stringency (demeaned by MSA)") +
     ggtitle("Imputed stringency measures")  & 
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 
    ggsave("DataV2/US_Data/Output/Robustness/income_stringency_imputed0.png", width = 24, height = 15, units = "cm")
    
    
    flexibleEstimation(Dataframe_list = list("Superstar" = top_ext,
                                             "nonSuperstar" = bot_ext),
                       formula = included_controls_stringency_formula,
                       SampleNames = baselineSampleNames) +
      xlab("Ranked housing unit density (Block Group level)") + 
      ylab(paste0("Regulatory Stringency", " \n ", "(demeaned by MSA, residualized by controls)")) +
      ggtitle("Imputed stringency measures")  & 
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 
    ggsave("DataV2/US_Data/Output/Robustness/income_stringency_residualized_imputed0.png", width = 24, height = 15, units = "cm")
  
    rm(top_ext, bot_ext)
#________________________________________________________________________________
 #PART 4: Robustness to alternative clustering measures...
#________________________________________________________________________________
 #Importing all clustering definitions in from our dataset. 
 #Listing files
 filelist <- list.files("DataV2/CoreLogic/Output")
 e.filelist <- filelist[which(grepl("Regulation_measurements", filelist, fixed = TRUE) == TRUE)] #extracting desired files
 rm(filelist)
 
 for (file in e.filelist) {
 
  stringencyName <- paste0("IS", 
                           sub("Regulation_measurements_", 
                               "", file)) #Name of our variable
  print(stringencyName)
  
  defn_excluded_controls_stringency_formula <- as.formula(paste(stringencyName, '~ s(rank_density_CBSA, k = 1, bs = "cr") + rank_inv_D2CBD'))
  
  #Plotting: 
  flexibleEstimation(Dataframe_list = list("Superstar" = top,
                                           "nonSuperstar" = bot),
                     formula = defn_excluded_controls_stringency_formula,
                     SampleNames = baselineSampleNames) +
    xlab("Ranked housing unit density (Block Group level)") +
    ylab(paste0("Regulatory Stringency (demeaned by MSA)")) & 
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 
  ggsave(paste0("DataV2/US_Data/Output/Robustness/", stringencyName, ".png"), width = 24, height = 15, units = "cm")
    
 }
 
 
 
 #Also holds in logs (not comparing quantiles)
 #_________________________________
 # Plotting with demeaned Housing density on y axis, only concerning shaving off top and bottom tertile (these are outliers)
    quantile_demeaned_hdens <- quantile(US_BLOCK$demeaned_log_Housing_density, probs = seq(0, 1, 0.05))
    flexibleEstimation(Dataframe_list = list("Superstar" = top[top$demeaned_log_Housing_density < quantile_demeaned_hdens["95%"] &
                                                               top$demeaned_log_Housing_density > quantile_demeaned_hdens["5%"], ], #Shaving off top/bot 5% of data because these are concentrated in select cities
                                             "nonSuperstar" = bot[bot$demeaned_log_Housing_density < quantile_demeaned_hdens["95%"] &
                                                                  bot$demeaned_log_Housing_density > quantile_demeaned_hdens["5%"], ]),
                       formula = as.formula(paste0('demeaned_log_Income ~ s(demeaned_log_Housing_density, k = 1, bs = "cr") + rank_inv_D2CBD')),
                       density_quantiles = FALSE, #no density quantiles
                       SampleNames = baselineSampleNames) +
                 xlab("Log Housing unit density (demeaned by MSA)") +
                 ylab("Log Average Income (demeaned by MSA)") + theme_gray(base_size = 15) & 
                 theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))  
    ggsave("DataV2/US_Data/Output/income_noquantile.png", 
           width = 24, height = 15, units = "cm")
 # 
 #_________________________________
 
 
 #ADDITIONAL ROBUSTNESS CHECKS___________________________________________________________
 
 #________________________F1 ALTERNATIVE DEFINITIONS OF SUPERSTAR CITIES_____________________

  #Top 25%/bottom 75% on price alone
  flexibleEstimation(Dataframe_list = list("Superstar" = US_BLOCK[US_BLOCK$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),],
                                          "nonSuperstar" = US_BLOCK[US_BLOCK$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["75.0%"]),]),
                    formula = excluded_controls_income_formula,
                    SampleNames = list("Superstar" = paste0("Top 25%", " \n Housing prices"),
                                       "nonSuperstar" = "All other cities")   ) +
   xlab("Ranked housing unit density (Block Group level)") +
   ylab("Log Average Income (demeaned by MSA)") + 
   theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + 
   ggtitle("Housing price definition")
   ggsave("DataV2/US_Data/Output/Robustness/income_price.png", width = 24, height = 15, units = "cm")
   
   
  #Density
  flexibleEstimation(Dataframe_list = list("Superstar" = US_BLOCK[US_BLOCK$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]),],
                                            "nonSuperstar" = US_BLOCK[US_BLOCK$City_housing_density < as.numeric(quantile_CBSA_dens["75.0%"]),]),
                      formula = excluded_controls_income_formula,
                      SampleNames = list("Superstar" = paste0("Top 25%", " \n Housing density"),
                                         "nonSuperstar" = "All other cities")   ) +
     xlab("Ranked housing unit density (Block Group level)") +
     ylab("Log Average Income (demeaned by MSA)") + 
     theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + 
     ggtitle("Housing density definition")
   ggsave("DataV2/US_Data/Output/Robustness/income_dens.png", width = 24, height = 15, units = "cm") 
 
  #Productivity
  flexibleEstimation(Dataframe_list = list("Superstar" = US_BLOCK[US_BLOCK$PooledWage > as.numeric(quantile_CBSA_wage["75.0%"]),],
                                            "nonSuperstar" = US_BLOCK[US_BLOCK$PooledWage < as.numeric(quantile_CBSA_wage["75.0%"]),]),
                      formula = excluded_controls_income_formula,
                      SampleNames = list("Superstar" = paste0("Top 25%", " \n Productivity"),
                                         "nonSuperstar" = "All other cities")   ) +
     xlab("Ranked housing unit density (Block Group level)") +
     ylab("Log Average Income (demeaned by MSA)") + 
     theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + 
     ggtitle("Productivity definition")
   ggsave("DataV2/US_Data/Output/Robustness/income_wage.png", width = 24, height = 15, units = "cm")  
   
   
  #Repeating for stringency
   flexibleEstimation(Dataframe_list = list("Superstar" = US_BLOCK[US_BLOCK$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),],
                                            "nonSuperstar" = US_BLOCK[US_BLOCK$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["75.0%"]),]),
                      formula = excluded_controls_stringency_formula,
                      SampleNames = list("Superstar" = paste0("Top 25%", " \n Housing prices"),
                                         "nonSuperstar" = "All other cities")   ) +
     xlab("Ranked housing unit density (Block Group level)") +
     ylab("Regulatory Stringency (demeaned by MSA)") + 
     theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + 
     ggtitle("Housing price definition")
   ggsave("DataV2/US_Data/Output/Robustness/stringency_price.png", width = 24, height = 15, units = "cm")   
   
   
   #Density
   flexibleEstimation(Dataframe_list = list("Superstar" = US_BLOCK[US_BLOCK$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"]),],
                                            "nonSuperstar" = US_BLOCK[US_BLOCK$City_housing_density < as.numeric(quantile_CBSA_dens["75.0%"]),]),
                      formula = excluded_controls_stringency_formula,
                      SampleNames = list("Superstar" = paste0("Top 25%", " \n Housing density"),
                                         "nonSuperstar" = "All other cities")   ) +
     xlab("Ranked housing unit density (Block Group level)") +
     ylab("Regulatory Stringency (demeaned by MSA)") + 
     theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + 
     ggtitle("Housing density definition")
   ggsave("DataV2/US_Data/Output/Robustness/stringency_dens.png", width = 24, height = 15, units = "cm")   
    
   #Wages
   flexibleEstimation(Dataframe_list = list("Superstar" =  US_BLOCK[US_BLOCK$PooledWage > as.numeric(quantile_CBSA_wage["75.0%"]),],
                                            "nonSuperstar" = US_BLOCK[US_BLOCK$PooledWage < as.numeric(quantile_CBSA_wage["75.0%"]),]),
                      formula = excluded_controls_stringency_formula,
                      SampleNames = list("Superstar" = paste0("Top 25%", " \n Productivity"),
                                         "nonSuperstar" = "All other cities")   ) +
     xlab("Ranked housing unit density (Block Group level)") +
     ylab("Regulatory Stringency (demeaned by MSA)") + 
     theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + 
     ggtitle("Productivity definition")
   ggsave("DataV2/US_Data/Output/Robustness/stringency_wage.png", width = 24, height = 15, units = "cm")   

   #All robust!
 
 
 
 #_____________________________________
 # Using 2010 measures but retain same density ranking in 2020. 
 #_____________________________________
 excluded_controls_income_formula_hist <- as.formula('demeaned_log_Income_hist ~ s(rank_density_CBSA, k = 1, bs = "cr") + rank_inv_D2CBD') 
 #retain density ranking from 2020 -- also robust to historical density rankings
 
   flexibleEstimation(Dataframe_list = list("Superstar" =  top,
                                          "nonSuperstar" = bot),
                      formula =  excluded_controls_income_formula_hist,
                      SampleNames = baselineSampleNames) +
   xlab("Ranked housing unit density (Block Group level)") +
   ylab("log Average Income (demeaned by MSA)") + 
   ggtitle("Using 2008-2012 ACS sample") +
    theme_gray(base_size = 15) &
   theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
   ggsave("DataV2/US_Data/Output/Robustness/income_2010.png", width = 24, height = 15, units = "cm")   
   
   
 
   
   
  #_________________________________
  # Robustness: Censoring locations with greater than 500 people/sqm
  #_________________________________ 

   flexibleEstimation(Dataframe_list = list("Superstar" = top[top$Housing_density > Censor_density,],
                                            "nonSuperstar" = bot[bot$Housing_density > Censor_density,]),
                      formula =  excluded_controls_income_formula,
                      SampleNames = baselineSampleNames) +
     xlab("Ranked housing unit density (Block Group level)") +
     ylab("log Average Income (demeaned by MSA)") +
     ggtitle("Density greater than 500 households/square mile") +
     theme_gray(base_size = 15) &
     theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))  
   ggsave("DataV2/US_Data/Output/Robustness/income_density_censored.png", width = 24, height = 15, units = "cm")   
 
 remove(list = ls())