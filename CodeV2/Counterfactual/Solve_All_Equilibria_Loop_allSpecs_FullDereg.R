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
#MODEL 1: FULL MOBILITY, ENDOGENOUS AMENITIES -- BASELINE SPECIFICATION

#FILEPATH FOR SOLUTION .R FILE
solver <- "CodeV2/Counterfactual/Solve_Current_Equilibrium_allSpecs_FullDereg.R" #Use different solver file for different results... 

#PARAMETER GUI__________________________________________________________________
EquilibriumType <- list() #List to read which equilibrium to solve for 

#CHECK INITIAL EQUILIBRIUM
check_init_eq <- 0 #Set to 0 to solve for counterfactual, set to 1 to check if population calculation matches initial equilibrium. It does.

#MUTUALLY EXCLUSIVE PARAMETERS
EquilibriumType["Full"] <- TRUE #Set to true/1 to solve for full counterfactual
EquilibriumType["WithinCityMobility"] <- FALSE
EquilibriumType["NoMobility"] <- FALSE


#Set to TRUE if we are to use bySkill version of equilibrium.
EquilibriumType["bySkill"] <- FALSE #BySkill
EquilibriumType["StoneGeary"] <- TRUE #Baseline, Stone geary preferences 
EquilibriumType["EndogenousAmenities"] <- TRUE
EquilibriumType["EndogenousProductivity"] <- FALSE #Baseline, no endogenous productivity


#Run solution
source(solver)

#_______________________________________________________________________________________________________________________________________
#MODEL 2: FULL MOBILITY, EXOGENOUS AMENITIES 
EquilibriumType["EndogenousAmenities"] <- FALSE

#Run solution
source(solver)

#______________________________________________________
#MODEL 3: WITHIN/C MOBILITY, ENDOGENOUS AMENITIES 
EquilibriumType["WithinCityMobility"] <- TRUE
EquilibriumType["Full"] <- FALSE
EquilibriumType["EndogenousAmenities"] <- TRUE


#Run solution
source(solver)

#______________________________________________________
#MODEL 4: NO MOBILITY, ENDOGENOUS AMENITIES 
EquilibriumType["WithinCityMobility"] <- FALSE
EquilibriumType["NoMobility"] <- TRUE

#Run solution
source(solver)

#______________________________________________________
#MODEL 5: ENDOGENOUS AMENITIES  + PRODUCTIVITY
EquilibriumType["NoMobility"] <- FALSE
EquilibriumType["Full"] <- TRUE
EquilibriumType["EndogenousProductivity"] <- TRUE 

#Run solution
source(solver)

#______________________________________________________
#MODEL 6: BASELINE BY SKILL -- TURN OF ENDOGENOUS AMENITIES.
EquilibriumType["EndogenousProductivity"] <- FALSE
EquilibriumType["EndogenousAmenities"] <- FALSE 
EquilibriumType["bySkill"] <- TRUE

#Run solution
source(solver)

#MODEL 7: BASELINE BY SKILL --  ENDOGENOUS AMENITIES.
EquilibriumType["EndogenousProductivity"] <- FALSE
EquilibriumType["EndogenousAmenities"] <- TRUE
EquilibriumType["bySkill"] <- TRUE

#Run solution
source(solver)


