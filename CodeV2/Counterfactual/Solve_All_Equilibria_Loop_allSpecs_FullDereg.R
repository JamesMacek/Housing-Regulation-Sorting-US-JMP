#Date created: Feb 5th, 2023

#This file solves for all possible equilibria in our model under full deregulation. 
#This calls Solve_Current_Equilibrium....R for each parameterization of the model we want to solve, and creates an 
#equilibrium output file. 

library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)
library(stringr)
library(compiler)

#Function to solve equilibrium
source("CodeV2/Counterfactual/Functions/Solve_Current_Equilibrium_allSpecs_vectorized.R")  

#PARAMETER GUI__________________________________________________________________
EquilibriumType <- list() #List to read which equilibrium to solve for 

#CHECK INITIAL EQUILIBRIUM
check_init_eq <- 0 #Set to 0 to solve for counterfactual, set to 1 to check if population calculation matches initial equilibrium. It does -- population error is at machine precision. 

#MUTUALLY EXCLUSIVE PARAMETERS
EquilibriumType["Full"] <- TRUE #Set to true/1 to solve for full counterfactual, no limit to mobility
EquilibriumType["WithinCityMobility"] <- FALSE
EquilibriumType["NoMobility"] <- FALSE

# Specify complete deregulation
EquilibriumType["Partial_Dereg"] <- FALSE #Specify Partial == FALSE for complete deregulation
EquilibriumType["SocialOpt_gridSearch"] <- FALSE

#Set to TRUE if we are to use bySkill version of equilibrium.
EquilibriumType["bySkill"] <- FALSE #BySkill
EquilibriumType["StoneGeary"] <- TRUE #Baseline, Stone geary preferences 
EquilibriumType["EndogenousAmenities"] <- TRUE
EquilibriumType["EndogenousProductivity"] <- FALSE #Baseline, no endogenous productivity
EquilibriumType["NoFundamentals"] <- FALSE #use observed fundamental amenities

#MODEL 1: BASELINE 
solveEquilibrium() 

#_______________________________________________________________________________________________________________________________________
#MODEL 2: FULL MOBILITY, EXOGENOUS AMENITIES 
EquilibriumType["EndogenousAmenities"] <- FALSE

#Run solution
solveEquilibrium() 

#______________________________________________________
#MODEL 3: ENDOGENOUS AMENITIES  + PRODUCTIVITY
EquilibriumType["EndogenousAmenities"] <- TRUE
EquilibriumType["EndogenousProductivity"] <- TRUE 

#Run solution
solveEquilibrium() 

#______________________________________________________
#MODEL 4: BASELINE BY SKILL (EDUCATION) -- TURN OF ENDOGENOUS AMENITIES.
EquilibriumType["EndogenousProductivity"] <- FALSE
EquilibriumType["EndogenousAmenities"] <- FALSE 
EquilibriumType["bySkill"] <- TRUE

#Run solution
solveEquilibrium() 

#_____________________________________________________________
#MODEL 5: BASELINE BY SKILL --  ENDOGENOUS AMENITIES.
EquilibriumType["EndogenousProductivity"] <- FALSE
EquilibriumType["EndogenousAmenities"] <- TRUE
EquilibriumType["bySkill"] <- TRUE

#Run solution
solveEquilibrium() 

#__________________________________________________________________________
#MODEL 6: BASELINE BY SKILL --  ENDOGENOUS AMENITIES + PRODUCTIVITY (skill biased agglomeration)
EquilibriumType["bySkill"] <- TRUE
EquilibriumType["EndogenousAmenities"] <- TRUE
EquilibriumType["EndogenousProductivity"] <- TRUE

solveEquilibrium() 



