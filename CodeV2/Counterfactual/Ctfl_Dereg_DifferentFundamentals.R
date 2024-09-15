#Date created: Feb 5th, 2023

#This file solves for all possible equilibria in our model under deregulation with no fundamentals.
#This calls Solve_Current_Equilibrium....R for each parameterization of the model we want to solve. 

library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)
library(stringr)

#_______________________________________________________________________________________________________________________________________
#FILEPATH FOR SOLUTION .R FILE
solver <- "CodeV2/Counterfactual/Functions/Solve_Current_Equilibrium_allSpecs_vectorized.R" #Use different solver file for different results... 

#PARAMETER GUI__________________________________________________________________
EquilibriumType <- list() #List to read which equilibrium to solve for 
check_init_eq <- 0 #Must exist in memory, only for debugging

#MUTUALLY EXCLUSIVE PARAMETERS LIMITING MOBILITY
EquilibriumType["Full"] <- TRUE #Set to true/1 to solve for full counterfactual
EquilibriumType["WithinCityMobility"] <- FALSE
EquilibriumType["NoMobility"] <- FALSE
EquilibriumType["SocialOpt_gridSearch"] <- FALSE


#Set to TRUE if we are to use bySkill version of equilibrium.
EquilibriumType["bySkill"] <- FALSE #BySkill
EquilibriumType["StoneGeary"] <- TRUE #SG preferences.
EquilibriumType["EndogenousAmenities"] <- TRUE 
EquilibriumType["EndogenousProductivity"] <- FALSE #Baseline, no endogenous productivity


#______________________________________________________________________________________________________
#_______________PART 2: Deregulation with no income sorting fundamentals_______________________________
#______________________________________________________________________________________________________
#PASS NO FUNDAMENTALS OBJECT FOR THIS ANALYSIS. 
EquilibriumType["NoFundamentals"] <- TRUE


#Import initial equilibrium to create vector of unit density restrictions
uDR_df <- read_dta(paste0("DataV2/Counterfactuals/Master_post_calibration_bySkill", EquilibriumType$bySkill, "_pref_", "SG", "_amenities.dta")) %>%
          select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, IncomeStringency_model_rents)

IncomeStringency_ctfl  <- uDR_df$IncomeStringency_model_rents #Initial values of a minimal lot, calibrated to initial equilibrium with no variation in location fundamentals
rm(uDR_df)

#Finding equilibrium at current levels of regulation with no amenity fundamentals...
EquilibriumType["Partial_Dereg"] <- TRUE

#Run solution
solveEquilibrium() 

#______________________________________________________________________________________________________
#Now, doing Full Deregulation Counterfactual___________________________________________________________
#______________________________________________________________________________________________________
EquilibriumType["Partial_Dereg"] <- FALSE

solveEquilibrium()  #Full deregulation equilibrium with no fundamental amenities -- 
#note: there should be much less income sorting (only minor amounts coming from housing pref; non-homothetic preferences). 








