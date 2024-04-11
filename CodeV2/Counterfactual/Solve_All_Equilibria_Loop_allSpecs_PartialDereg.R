#Date created: Feb 5th, 2023

#This file solves for all possible equilibria in our model under full deregulation. 
#This calls Solve_Current_Equilibrium....R for each parameterization of the model we want to solve. 

library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)
library(stringr)

#_______________________________________________________________________________________________________________________________________
#MODEL 1: HALVE MINIMUM LOT SIZES IN SFran counterfactual

#FILEPATH FOR SOLUTION .R FILE
solver <- "CodeV2/Counterfactual/Functions/Solve_Current_Equilibrium_allSpecs_PartialDeregulation.R" #Use different solver file for different results... 

#PARAMETER GUI__________________________________________________________________
EquilibriumType <- list() #List to read which equilibrium to solve for 

#MUTUALLY EXCLUSIVE PARAMETERS LIMITING MOBILITY
EquilibriumType["Full"] <- TRUE #Set to true/1 to solve for full counterfactual
EquilibriumType["WithinCityMobility"] <- FALSE
EquilibriumType["NoMobility"] <- FALSE


#Set to TRUE if we are to use bySkill version of equilibrium.
EquilibriumType["bySkill"] <- FALSE #BySkill
EquilibriumType["StoneGeary"] <- TRUE #SG preferences
EquilibriumType["EndogenousAmenities"] <- TRUE 
EquilibriumType["EndogenousProductivity"] <- FALSE #Baseline, no endogenous productivity
EquilibriumType["NoFundamentals"] <- FALSE #use observed fundamental amenities

if (EquilibriumType$StoneGeary == TRUE) {
  pref <- "SG"
}else{
  pref <- "CD"
}

#Import initial equilibrium to create vector of unit density restrictions
uDR_df <- read_dta(paste0("DataV2/Counterfactuals/Master_post_calibration_bySkill", EquilibriumType$bySkill, "_pref_", pref, "_amenities.dta")) %>%
          select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, IncomeStringency_model_rents)

IncomeStringency_ctfl <- ifelse(test = (uDR_df$CBSA_NAME == "San Francisco-Oakland-Hayward, CA"),       #Cutting value of minimal lot in 2 -- a bit more than eliminating SF zoning
                                          yes = 1/2,                                                    # 
                                          no = 1)*
                                          uDR_df$IncomeStringency_model_rents
rm(uDR_df)
#This vector is passed to solution file

#Outputted file name for this counterfactual
FileOutputName <- "SanFrancisco_ReZoning"
#Run solution
source(solver)

#_______________________________________________________________________________
#Turning off endogenous amenities to isolate role of them for this counterfactual
EquilibriumType["EndogenousAmenities"] <- FALSE

if (EquilibriumType$StoneGeary == TRUE) {
  pref <- "SG"
}else{
  pref <- "CD"
}

#Outputted file name for this counterfactual
FileOutputName <- "SanFrancisco_ReZoning"
#Run solution
source(solver)



#____________________________________________________________________
# TEMPORARY: Multiple city deregulation exercises in full GE
#____________________________________________________________________

EquilibriumType["EndogenousAmenities"] <- TRUE #Type back to endogenous amenities.

cities <- c("Washington-Arlington-Alexandria, DC-VA-MD-WV", #Additional Superstars
            "Denver-Aurora-Lakewood, CO",
            "Tampa-St. Petersburg-Clearwater, FL", #Non-Superstars
            "San Antonio-New Braunfels, TX",
            "Rochester, NY")

cityNames <- c("Washington", "Denver", "Tampa", "SanAntonio", "Rochester")

#Start loop over other cities counterfactuals:  3 non-superstars, 3 superstars
for (city in cities) {
  
  #Reinstating preference identifier
  if (EquilibriumType$StoneGeary == TRUE) {
    pref <- "SG"
  }else{
    pref <- "CD"
  }

#Import initial equilibrium to create vector of unit density restrictions
  uDR_df <- read_dta(paste0("DataV2/Counterfactuals/Master_post_calibration_bySkill", EquilibriumType$bySkill, "_pref_", pref, "_amenities.dta")) %>%
    select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, IncomeStringency_model_rents)

  IncomeStringency_ctfl <- ifelse(test = (uDR_df$CBSA_NAME == city),       #Cutting value of minimal lot in 2 -- a bit more than eliminating SF zoning
                                  yes = 1/2,                                                    # 
                                  no = 1)*uDR_df$IncomeStringency_model_rents

  #Outputted file name for this counterfactual
  FileOutputName <- paste0(cityNames[which(city == cities)], "_ReZoning")
  #Run solution
  source(solver)
  

}

#__________________________________________
#Doing the same without endogenous amenities
#____________________________________________
EquilibriumType["EndogenousAmenities"] <- FALSE

cities <- c("Washington-Arlington-Alexandria, DC-VA-MD-WV", #Additional Superstars
            "Denver-Aurora-Lakewood, CO",
            "Tampa-St. Petersburg-Clearwater, FL", #Non-Superstars
            "San Antonio-New Braunfels, TX",
            "Rochester, NY")

cityNames <- c("Washington", "Denver", "Tampa", "SanAntonio", "Rochester")


for (city in cities) {
  
  if (EquilibriumType$StoneGeary == TRUE) {
    pref <- "SG"
  }else{
    pref <- "CD"
  }
  
  #Import initial equilibrium to create vector of unit density restrictions
  uDR_df <- read_dta(paste0("DataV2/Counterfactuals/Master_post_calibration_bySkill", EquilibriumType$bySkill, "_pref_", pref, "_amenities.dta")) %>%
    select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, IncomeStringency_model_rents)
  
  IncomeStringency_ctfl <- ifelse(test = (uDR_df$CBSA_NAME == city),       #Cutting value of minimal lot in 2 -- a bit more than eliminating SF zoning
                                  yes = 1/2,                                                    # 
                                  no = 1)*uDR_df$IncomeStringency_model_rents
  
  #Outputted file name for this counterfactual
  FileOutputName <- paste0(cityNames[which(city == cities)], "_ReZoning")
  #Run solution
  source(solver)
  

}



