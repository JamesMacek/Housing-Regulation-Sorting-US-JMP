#Date created: Feb 7th, 2023

#This file measures changes in aggregate productivity across locations. 

library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)
library(ggplot2)
library(collapse)
library(vtable) #for easy conditional tables


#PARAMETERS
source("CodeV2/Counterfactual/Parameters/GlobalParameters.R")

#FUNCTIONS
source("CodeV2/Counterfactual/Functions/Solve_Equilibrium_Functions_FullDereg_current.R")
source("CodeV2/Counterfactual/Functions/Analysis_Functions.R")


#_______________________________________________________________________________ PRELIMINARIES

#SPECIFY BASELINE COUNTERFACTUAL OUTPUT
BASELINE_SPECIFICATION <- list(pref = "SG", bySkill_to_pass = FALSE) #SET bySkill_to_pass == FALSE for correct table

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

#Importing quantiles that define superstar cities
load(file = "DataV2/US_Data/Output/CBSA_quantiles_dens.Rdata")
load(file = "DataV2/US_Data/Output/CBSA_quantiles.Rdata")

##_________________________________________________________________________________
#Collecting this all in a data frame and outputting in Latex table for saving later
#__________________________________________________________________________________
Table_toOutput <- data.frame(matrix(nrow = 0, ncol = 5))
colnames(Table_toOutput) <- c("End. Amenities", "End. Productivity", "Education", 
                              "Prod. Growth", "Prod. Growth, no income sorting")
rows_to_output <- list() #list of rows to put into table for appending


#Start logs
sink("DataV2/Counterfactuals/logs/AnalyzeLabourProductivity.txt")

#_______________________________________________________________________________
#PART 1: 
#Checking changes in aggregate labour productivity across equilibria
print(paste0("The difference in labour productivity after deregulation is ",
             100*((getAggregateProductivity(Ct_Amenities) - getAggregateProductivity(Init_eq))/ 
                    getAggregateProductivity(Init_eq)), " percent.")) #Not high like H & M!
rows_to_output[[1]] <- c("Yes", "No", "No",  paste0(round(100*((getAggregateProductivity(Ct_Amenities) - getAggregateProductivity(Init_eq))/ 
                                                                   getAggregateProductivity(Init_eq)), 2), "%" )) #row to output to table


#When amenities are exogenous 
print(paste0("The difference in labour productivity after deregulation is ",
             100*((getAggregateProductivity(Ct_NoAmenities) - getAggregateProductivity(Init_eq))/ 
                    getAggregateProductivity(Init_eq)), " percent for No Endogenous Amenities.")) #Slightly higher
rows_to_output[[2]] <- c("No", "No", "No", paste0(round(100*((getAggregateProductivity(Ct_NoAmenities) - getAggregateProductivity(Init_eq))/ 
                                                              getAggregateProductivity(Init_eq)), 2), "%" ))


#Setting 
AcrossCityAnalysis <- Init_eq %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, City_housing_density, CBSA_med_house_value, IncomeStringency_cl)
AcrossCityAnalysis$IncomeStringency_cl[is.na(AcrossCityAnalysis$IncomeStringency_cl)] <- 0 #replacing IncomeStringency_cl = 0 like we did in the model

#Delta Average types, populations
AcrossCityAnalysis["pDelta_AvgType"] <- 100*(getCityAverageType(Ct_Amenities)/getCityAverageType(Init_eq) - 1)
AcrossCityAnalysis["pDelta_Pop"] <- 100*(getCityTotalPop(Ct_Amenities)/getCityTotalPop(Init_eq) - 1)

#Initial city wages
AcrossCityAnalysis["PooledWage"] <- Init_eq$PooledWage #We take pooled wage as initial city.

#Initial city population
AcrossCityAnalysis["Init_City_Population"] <- getCityTotalPop(Init_eq)

AcrossCityAnalysis["SuperStar"] <- rep(0, nrow(AcrossCityAnalysis))
AcrossCityAnalysis$SuperStar[AcrossCityAnalysis$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) &
                               AcrossCityAnalysis$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"])] <- 1 #superstar dummy


AcrossCityAnalysis <- collap(AcrossCityAnalysis, pDelta_AvgType + pDelta_Pop + PooledWage + IncomeStringency_cl + 
                            City_housing_density + CBSA_med_house_value + Init_City_Population + SuperStar ~ CBSA + CBSA_NAME)

#Checking correlation between pop growth, type growth and wages
print("Correlation with population flows and wages")
print(cor.test(AcrossCityAnalysis$PooledWage, AcrossCityAnalysis$pDelta_Pop, method = "pearson"))
print(cor.test(AcrossCityAnalysis$PooledWage, AcrossCityAnalysis$pDelta_AvgType, method = "pearson"))

#With housing prices
print("Correlation with population flows and initial housing prices")
print(cor.test(AcrossCityAnalysis$CBSA_med_house_value, AcrossCityAnalysis$pDelta_Pop, method = "pearson"))
print(cor.test(AcrossCityAnalysis$CBSA_med_house_value, AcrossCityAnalysis$pDelta_AvgType, method = "pearson"))

#And measures of stringency
print("Correlation with population flows and stringency")
print(cor.test(AcrossCityAnalysis$IncomeStringency_cl, AcrossCityAnalysis$pDelta_Pop, method = "pearson")) #80% correlation here 
print(cor.test(AcrossCityAnalysis$IncomeStringency_cl, AcrossCityAnalysis$pDelta_AvgType, method = "pearson"))


#Something weird happening with current dataframe formatting. Coercing back to original data frame
AcrossCityAnalysis <- data.frame(AcrossCityAnalysis)

#Creating productivity quartiles to better capture variation on plot
AcrossCityAnalysis <- AcrossCityAnalysis %>% mutate(PooledWage_quartiles = 
                                                                 cut(PooledWage, breaks = unique(quantile(PooledWage, seq(0, 1, 0.25))), 
                                                                 label = FALSE))
#NA in lowest prod. city belongs in lowest quartile
AcrossCityAnalysis$PooledWage_quartiles[is.na(AcrossCityAnalysis$PooledWage_quartiles)] <- 1

#Calculating mean productivity by quartile
AcrossCityAnalysis <- AcrossCityAnalysis %>% group_by(PooledWage_quartiles) %>% 
                                             mutate(PooledWage_quartiles_center = round(mean(PooledWage), 2)) %>% ungroup()

#____________________________________________
# Plot income sorting and movement for text
#____________________________________________
mainPlot <- ggplot() +      
                      geom_point(data = AcrossCityAnalysis, 
                                 aes(x = pDelta_Pop, y = pDelta_AvgType, color = PooledWage_quartiles_center, size = Init_City_Population/1000000), alpha = 0.45) +
                      geom_smooth(method = "lm") +
                      geom_text(data = AcrossCityAnalysis[AcrossCityAnalysis$SuperStar == 1,], check_overlap = T, size = 4.5, nudge_y = 1,
                                aes(x = pDelta_Pop, y = pDelta_AvgType, label = CBSA_NAME)) + 
                      scale_color_gradient(low = "blue", high = "red", name = "Productivity \n (binned quartiles)") +
                      xlab("Growth rate in number of households (percent)") + 
                      ylab("Growth rate in average household skill (percent)") +
                      coord_cartesian(clip = "off") + 
                      labs(color = "Productivity", size = "Households \n (millions)") + 
                      theme_gray(base_size = 18) + theme(legend.position = "bottom", 
                                                         plot.title = element_text(hjust = 0.5),
                                                         legend.key.size = unit(1, units = "cm")) #Expand legend sizes

ggsave(plot = mainPlot,
       filename = paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/IncomeSortingMovement_bySkill_", 
               BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", BASELINE_SPECIFICATION$pref, ".png"), 
       width = 35, height = 21, units = "cm") 





#___________________________
#
# Variance decomposition for text
#_______________________________________________________________________________________________________________________________________________
#_________________________________________________________________________
#What would labour productivity be if no income sorting occured?
#Change in aggregate labour productivity = !pop growth by city weighted by output shares! (this is assuming everyone makes the same income)

output_shares <- data.frame(getCityOutputShares(Init_eq)) #calculate CBSA output shares to capture agg productivity changes
colnames(output_shares) <- c("CBSA_NAME", "OutputShares") 
AcrossCityAnalysis <- left_join(AcrossCityAnalysis, output_shares, by = c("CBSA_NAME"))
LabProdGrowth_noincomeSorting <- sum(((AcrossCityAnalysis$pDelta_Pop + 100)*as.numeric(AcrossCityAnalysis$OutputShares))) - 100 #4x higher 
print(paste0("Aggregate productivity growth would have been ", LabProdGrowth_noincomeSorting, " percent if there was no income sorting."))

#Inputing into rows
rows_to_output[[1]] <- c(rows_to_output[[1]], paste0(round(LabProdGrowth_noincomeSorting, digits = 2), "%"))

#_____________________________________________________
#Doing the same statistic for no endogenous amenities
#_____________________________________________________
Ct_NoAmenities["pDelta_Pop"] <- 100*(getCityTotalPop(Ct_NoAmenities)/getCityTotalPop(Init_eq) - 1)
Ct_NoAmenities <- collap(Ct_NoAmenities, pDelta_Pop ~ CBSA + CBSA_NAME)
Ct_NoAmenities  <- left_join(Ct_NoAmenities, output_shares, by = c("CBSA_NAME"))
LabProdGrowth_noincomeSorting <- sum(((Ct_NoAmenities$pDelta_Pop + 100)*as.numeric(Ct_NoAmenities$OutputShares))) - 100 #4x higher 

#Inputing into rows
rows_to_output[[2]] <- c(rows_to_output[[2]], paste0(round(LabProdGrowth_noincomeSorting, digits = 2), "%"))

#____________________________________________________________________________________________________________________
#ROBUSTNESS: CHECK OTHER DATASET TYPES
#____________________________________________________________________________________________________________________
#Endogenous productivity at baseeline specification, no endogenous productivity
load(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", "Eq_Objects_FULL", 
            "_EndoAmen_", TRUE, 
            "_EndoProd_", TRUE,
            "_bySkill_", FALSE,
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))

Ct_Amenities <- Equilibrium_objects
rm(Equilibrium_objects)
load(paste0("DataV2/Counterfactuals/Init_eq_", 
            FALSE, 
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
Init_eq <- Master
rm(Master)

print(paste0("The difference in labour productivity after deregulation is ",
             100*((getAggregateProductivity(Ct_Amenities) - getAggregateProductivity(Init_eq))/ 
                    getAggregateProductivity(Init_eq)), " percent FOR ENDOGENOUS PRODUCTIVITY.")) #Slight decrease in aggregate labour productivity

rows_to_output[[3]] <- c("Yes", "Yes", "No", paste0(round(100*((getAggregateProductivity(Ct_Amenities) - getAggregateProductivity(Init_eq))/ 
                                                                                       getAggregateProductivity(Init_eq)), 2), "%" ))

#Checking what aggregate productivity would be if we muted income sorting
Ct_Amenities["pDelta_Pop"] <- (getCityTotalPop(Ct_Amenities)/getCityTotalPop(Init_eq)) #getting population growth rates (+ 1)
#Getting wage growth rates
Ct_Amenities["WageGrowth"] <- Ct_Amenities$PooledWage/Init_eq$PooledWage
Ct_Amenities <- collap(Ct_Amenities, pDelta_Pop + WageGrowth ~ CBSA + CBSA_NAME)
Ct_Amenities  <- left_join(Ct_Amenities, output_shares, by = c("CBSA_NAME"))

#Calculate total productivity growth
LabProdGrowth_noincomeSorting <- 100*(sum((Ct_Amenities$pDelta_Pop*Ct_Amenities$WageGrowth*as.numeric(Ct_Amenities$OutputShares))) - 1) #4x higher
#Inputting into rows
rows_to_output[[3]] <- c(rows_to_output[[3]], paste0(round(LabProdGrowth_noincomeSorting, digits = 2), "%"))


#___________________________________________
#BySkill version:  
load(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", "Eq_Objects_FULL", 
            "_EndoAmen_", TRUE, #NOTE: WE only do bySkill == False with exogenous amenities 
            "_EndoProd_", FALSE,
            "_bySkill_", TRUE,
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))

Ct_Amenities <- Equilibrium_objects
rm(Equilibrium_objects)

load(paste0("DataV2/Counterfactuals/Init_eq_", 
            TRUE, 
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
Init_eq <- Master
rm(Master)

skillVector <-  c("College", "NoCollege")
skillName <- c("College_", "NoCollege_") 
print(paste0("The difference in labour productivity after deregulation is ",
             100*((getAggregateProductivity(Ct_Amenities) - getAggregateProductivity(Init_eq))/ 
                    getAggregateProductivity(Init_eq)), " percent FOR BYSKILL == TRUE."))

rows_to_output[[4]] <- c("Yes", "No", "Yes", paste0(round(100*((getAggregateProductivity(Ct_Amenities) - getAggregateProductivity(Init_eq))/ 
                                                                 getAggregateProductivity(Init_eq)), 2), "%" ))

#Checking what aggregate productivity would be if we muted income sorting
Ct_Amenities["pDelta_Pop"] <- (getCityTotalPop(Ct_Amenities)/getCityTotalPop(Init_eq)) #getting population growth rates (+ 1)

#Since wages does not adjust when there are no composition changes, no need to calculate wage growth
Ct_Amenities <- collap(Ct_Amenities, pDelta_Pop ~ CBSA + CBSA_NAME)
Ct_Amenities  <- left_join(Ct_Amenities, output_shares, by = c("CBSA_NAME"))

#Calculate total productivity growth
LabProdGrowth_noincomeSorting <- 100*(sum((Ct_Amenities$pDelta_Pop*as.numeric(Ct_Amenities$OutputShares))) - 1) 
#Inputting into rows
rows_to_output[[4]] <- c(rows_to_output[[4]], paste0(round(LabProdGrowth_noincomeSorting, digits = 2), "%"))


#_____________________________________________
#BySkill version with endogenous productivity:  
load(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", "Eq_Objects_FULL", 
            "_EndoAmen_", TRUE, #NOTE: WE only do bySkill == False with exogenous amenities 
            "_EndoProd_", TRUE,
            "_bySkill_", TRUE,
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))

Ct_Amenities <- Equilibrium_objects
rm(Equilibrium_objects)

skillVector <-  c("College", "NoCollege")
skillName <- c("College_", "NoCollege_") 
print(paste0("The difference in labour productivity after deregulation is ",
             100*((getAggregateProductivity(Ct_Amenities) - getAggregateProductivity(Init_eq))/ 
                    getAggregateProductivity(Init_eq)), " percent FOR BYSKILL == TRUE and ENDOGENOUS PRODUCTIVITY."))

rows_to_output[[5]] <- c("Yes", "Yes", "Yes", paste0(round(100*((getAggregateProductivity(Ct_Amenities) - getAggregateProductivity(Init_eq))/ 
                                                                 getAggregateProductivity(Init_eq)), 2), "%" ))

#Checking what aggregate productivity would be if we muted income sorting 
#(Note: this is tricky because uniform city growth changes wages in counterfactual, which differentially affect different education levels, which causes income sorting)
#What we do is assume wages are unchanged, which hardly matters because productivity changes are so small (in the ballpark of a maximum of -0.5-2%).

Ct_Amenities["pDelta_Pop"] <- (getCityTotalPop(Ct_Amenities)/getCityTotalPop(Init_eq)) #getting population growth rates (+ 1)

#Since wages does not adjust when there are no composition changes, no need to calculate wage growth
Ct_Amenities <- collap(Ct_Amenities, pDelta_Pop ~ CBSA + CBSA_NAME)
Ct_Amenities  <- left_join(Ct_Amenities, output_shares, by = c("CBSA_NAME"))

#Calculate total productivity growth
LabProdGrowth_noincomeSorting <- 100*(sum((Ct_Amenities$pDelta_Pop*as.numeric(Ct_Amenities$OutputShares))) - 1)
#Inputting into rows
rows_to_output[[5]] <- c(rows_to_output[[5]], paste0(round(LabProdGrowth_noincomeSorting, digits = 2), "%"))

#_______________________________________________________
#Putting this all in the table in text

for (row in 1:5) {
  Table_toOutput[row, ] <- rows_to_output[[row]]
  
}

#Vtable output
vtable::dftoLaTeX(Table_toOutput, 
                  file = "DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/LabourProductivity_difCtfls.tex")



#________________________________________________________________________________________________________________________________________________
# Contribution of movement of each income type to aggregate productivity using decomposition.
#________________________________________________________________________________________________________________________________________________



#________________

#End logs
sink()
rm(list = ls())