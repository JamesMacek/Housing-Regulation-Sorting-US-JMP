#Date created: Feb 6th, 2023

#This file measures welfare for the full deregulation exercise. 

library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)
library(ggplot2)
library(forcats)


#PARAMETERS
source("CodeV2/Counterfactual/Parameters/GlobalParameters.R")

#FUNCTIONS
source("CodeV2/Counterfactual/Functions/Analysis_Functions.R")


#_______________________________________________________________________________ PRELIMINARIES

#SPECIFY BASELINE COUNTERFACTUAL OUTPUT
BASELINE_SPECIFICATION <- list(pref = "SG", bySkill_to_pass = FALSE)
#Change specification here to get welfare statistics for different specifications.

if (BASELINE_SPECIFICATION$bySkill == TRUE) {
  
  skillVector <-  c("College", "NoCollege")
  skillName <- c("College_", "NoCollege_") 
  
}else{
  
  skillVector <- c("Pooled")
  skillName <- c("")
  
}

if (BASELINE_SPECIFICATION$pref == "CD") {
  demandParameters_to_pass <- c(beta, 0)
}
if (BASELINE_SPECIFICATION$pref == "SG") {
  demandParameters_to_pass <- c(beta_StGeary, min_hReq)
}


#Importing all files from Counterfactual_Output (Endogenous amenities)
load(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", "Eq_Objects_FULL", 
            "_EndoAmen_", TRUE, 
            "_EndoProd_", FALSE,
            "_bySkill_", BASELINE_SPECIFICATION$bySkill_to_pass,
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
Ct_Amenities <- Equilibrium_objects_output
rm(Equilibrium_objects_output)

#Importing all files from Counterfactual_Output (Exogenous amenities)
load(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", "Eq_Objects_FULL", 
            "_EndoAmen_", FALSE, 
            "_EndoProd_", FALSE,
            "_bySkill_", BASELINE_SPECIFICATION$bySkill_to_pass,
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
Ct_NoAmenities <- Equilibrium_objects_output
rm(Equilibrium_objects_output)



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

#Final land for residential use 
Init_eq["final_land_for_res"] <- Init_eq$land_regulated + Init_eq$land_unregulated
Ct_Amenities["ALAND"] <- Init_eq$ALAND #official landmass from census shapefiles
Ct_NoAmenities["ALAND"] <- Init_eq$ALAND


#_____________________________________________________ START FILE HERE ___________________________________________________________


#___________________________________________________________________________________________________________________________________
#PART 1: analyzing the welfare of landlords by asking about the change in the average value of land across all locations.
#___________________________________________________________________________________________________________________________________

#Calculating initial land values by zone
Init_eq["LandValAmenities"] <- (1/(1 + Ct_Amenities$HS_Elasticity_imputed))*(Ct_Amenities$housingPrice^(Ct_Amenities$HS_Elasticity_imputed + 1))*Ct_Amenities$lambda
Init_eq["LandValNoAmenities"] <- (1/(1 + Ct_Amenities$HS_Elasticity_imputed))*(Ct_NoAmenities$housingPrice^(Ct_Amenities$HS_Elasticity_imputed + 1))*Ct_Amenities$lambda

#Calculating initial land values by zone
Init_eq["LandValInitEq_regulated"] <- (1/(1 + Init_eq$HS_Elasticity_imputed))*(Init_eq$price_regulated^(Init_eq$HS_Elasticity_imputed + 1))*Init_eq$lambda 
Init_eq["LandValInitEq_unregulated"] <- (1/(1 + Init_eq$HS_Elasticity_imputed))*(Init_eq$price_unregulated^(Init_eq$HS_Elasticity_imputed + 1))*Init_eq$lambda #land type weighted land values per acre.

#Total land value in initial neighborhood (note: we include the fixed land's share of income in production for housing)
Init_eq["Total_land_values_initial"] <- (Init_eq$land_regulated*Init_eq$LandValInitEq_regulated + Init_eq$land_unregulated*Init_eq$LandValInitEq_unregulated) #land type weighted land values per acre.
#Share of land in production                            #Total expenditure on housing services in the neighborhood

#Calculating growth at neighborhood level (log differences in land value weighted growth)
Init_eq["LandValGrowth"] <- log( ((Init_eq$land_regulated*Init_eq$LandValInitEq_regulated)/(Init_eq$Total_land_values_initial))* #weights
                                   (Init_eq$LandValAmenities/Init_eq$LandValInitEq_regulated) + 
                                   
                                   ((Init_eq$land_unregulated*Init_eq$LandValInitEq_unregulated)/(Init_eq$Total_land_values_initial))* #weights
                                   (Init_eq$LandValAmenities/Init_eq$LandValInitEq_unregulated)  )

Init_eq["LandValGrowthNoAm"] <- log( ((Init_eq$land_regulated*Init_eq$LandValInitEq_regulated)/(Init_eq$Total_land_values_initial))* #weights
                                       (Init_eq$LandValNoAmenities/Init_eq$LandValInitEq_regulated) + 
                                       
                                       ((Init_eq$land_unregulated*Init_eq$LandValInitEq_unregulated)/(Init_eq$Total_land_values_initial))* #weights
                                       (Init_eq$LandValNoAmenities/Init_eq$LandValInitEq_unregulated)  )


#Storing average growth rate in land values, weighted by final_land_for_res
growthRate_landval <- weighted.mean(exp(Init_eq$LandValGrowth), w = Init_eq$Total_land_values_initial) - 1
growthRate_landval_NoAm <- weighted.mean(exp(Init_eq$LandValGrowthNoAm), w = Init_eq$Total_land_values_initial) - 1

#Printing changes in land value for an average block group
print(paste0("The national growth rate in land values is ", growthRate_landval*100, " percent.")) #large losses to landowners.
print(paste0("The national growth rate in land values is exogenous amenities at baseline is ", growthRate_landval_NoAm*100, " percent."))


#Populations split if all effectively one zone
for (skill_to_pass in skillVector) {
  name_of_skill <- skillName[which(skill_to_pass == skillVector)]
  for (i in 1:7) {
    
  
    #Imputing Init_eq zonal population in locations where there are not multiple zones (these are NA in original data; does not matter)
    for (zone in c(1,2)) {
      
      Init_eq[[paste0("Population_type_", name_of_skill, i, "_z", zone)]][is.na(Init_eq[[paste0("Population_type_", name_of_skill, i, "_z", zone)]])] <-  
        (1/2)*Init_eq[[paste0("Population_type_", name_of_skill, i)]][ is.na(Init_eq[[paste0("Population_type_", name_of_skill, i, "_z", zone)]]) ] 
      
      
    }
    
  }
}
