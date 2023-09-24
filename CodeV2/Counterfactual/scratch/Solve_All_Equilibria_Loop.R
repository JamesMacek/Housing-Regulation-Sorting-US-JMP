#Date created: Feb 5th, 2023

#This file solves for all possible equilibria in our model under full deregulation. 
#This calls Solve_Current_Equilibrium.R for each version of the model we want to solve. 

library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)
library(stringr)

#_______________________________________________________________________________________________________________________________________
#MODEL 1: FULL MOBILITY, ENDOGENOUS AMENITIES

#PARAMETERS
source("CodeV2/Counterfactual/Parameters/GlobalParameters.R")

#_____________________________________________________________

#FUNCTIONS______________________________________________________________
source("CodeV2/Counterfactual/Functions/scratch/Solve_Equilibrium_Functions.R")

#FILEPATH FOR SOLUTION .R FILE
solver <- "CodeV2/Counterfactual/scratch/Solve_Current_Equilibrium.R" #Use different solver file for different results... 

#PARAMETER GUI__________________________________________________________________
EquilibriumType <- list() #List to read which equilibrium to solve for 

#CHECK INITIAL EQUILIBRIUM
check_init_eq <- 0 #Set to 0 to solve for counterfactual, set to 1 to check if population calculation matches initial equilibrium. It does.

#MUTUALLY EXCLUSIVE PARAMETERS
EquilibriumType["Full"] <- TRUE #Set to true/1 to solve for full counterfactual
EquilibriumType["WithinCityMobility"] <- FALSE
EquilibriumType["NoMobility"] <- FALSE

#Set to TRUE if we are to use bySkill version of equilibrium. 
EquilibriumType["bySkill"] <- FALSE
EquilibriumType["EndogenousAmenities"] <- TRUE
EquilibriumType["EndogenousProductivity"] <- FALSE

#Run solution
source(solver)
#_______________________________________________________________________________________________________________________________________
#MODEL 2: FULL MOBILITY, EXOGENOUS AMENITIES

#PARAMETERS
source("CodeV2/Counterfactual/Parameters/GlobalParameters.R")

#_____________________________________________________________

#FUNCTIONS______________________________________________________________
source("CodeV2/Counterfactual/Functions/Solve_Equilibrium_Functions.R")

#PARAMETER GUI__________________________________________________________________
EquilibriumType <- list() #List to read which equilibrium to solve for 

#CHECK INITIAL EQUILIBRIUM
check_init_eq <- 0 #Set to 0 to solve for counterfactual, set to 1 to check if population calculation matches initial equilibrium. It does.


#MUTUALLY EXCLUSIVE PARAMETERS
EquilibriumType["Full"] <- TRUE #Set to true/1 to solve for full counterfactual
EquilibriumType["WithinCityMobility"] <- FALSE
EquilibriumType["NoMobility"] <- FALSE

#Set to TRUE if we are to use bySkill version of equilibrium.
EquilibriumType["bySkill"] <- FALSE
EquilibriumType["EndogenousAmenities"] <- FALSE
EquilibriumType["EndogenousProductivity"] <- FALSE


#Run solution
source(solver)
#_______________________________________________________________________________________________________________________________________
#MODEL 3: WITHIN CITY MOBILITY, ENDOGENOUS AMENITIES

#PARAMETERS
source("CodeV2/Counterfactual/Parameters/GlobalParameters.R")

#_____________________________________________________________

#FUNCTIONS______________________________________________________________
source("CodeV2/Counterfactual/Functions/Solve_Equilibrium_Functions.R")

#PARAMETER GUI__________________________________________________________________
EquilibriumType <- list() #List to read which equilibrium to solve for 

#CHECK INITIAL EQUILIBRIUM
check_init_eq <- 0 #Set to 0 to solve for counterfactual, set to 1 to check if population calculation matches initial equilibrium. It does.


#MUTUALLY EXCLUSIVE PARAMETERS
EquilibriumType["Full"] <- FALSE #Set to true/1 to solve for full counterfactual
EquilibriumType["WithinCityMobility"] <- TRUE
EquilibriumType["NoMobility"] <- FALSE

#Set to TRUE if we are to use bySkill version of equilibrium.
EquilibriumType["bySkill"] <- FALSE
EquilibriumType["EndogenousAmenities"] <- TRUE
EquilibriumType["EndogenousProductivity"] <- FALSE


#Run solution
source(solver)
#_______________________________________________________________________________________________________________________________________
#MODEL 4: NO MOBILITY, ENDOGENOUS AMENITIES

#PARAMETERS
source("CodeV2/Counterfactual/Parameters/GlobalParameters.R")

#_____________________________________________________________

#FUNCTIONS______________________________________________________________
source("CodeV2/Counterfactual/Functions/Solve_Equilibrium_Functions.R")

#PARAMETER GUI__________________________________________________________________
EquilibriumType <- list() #List to read which equilibrium to solve for 

#CHECK INITIAL EQUILIBRIUM
check_init_eq <- 0 #Set to 0 to solve for counterfactual, set to 1 to check if population calculation matches initial equilibrium. It does.


#MUTUALLY EXCLUSIVE PARAMETERS
EquilibriumType["Full"] <- FALSE #Set to true/1 to solve for full counterfactual
EquilibriumType["WithinCityMobility"] <- FALSE
EquilibriumType["NoMobility"] <- TRUE

#Set to TRUE if we are to use bySkill version of equilibrium.
EquilibriumType["bySkill"] <- FALSE
EquilibriumType["EndogenousAmenities"] <- TRUE
EquilibriumType["EndogenousProductivity"] <- FALSE


#Run solution
source(solver)
#____________________________________________________
#MODEL 5: FULL MOBILITY, ENDOGENOUS AMENITIES, ENDOGENOUS PRODUCTIVITY

#PARAMETERS
source("CodeV2/Counterfactual/Parameters/GlobalParameters.R")

#_____________________________________________________________

#FUNCTIONS______________________________________________________________
source("CodeV2/Counterfactual/Functions/Solve_Equilibrium_Functions.R")

#PARAMETER GUI__________________________________________________________________
EquilibriumType <- list() #List to read which equilibrium to solve for 

#CHECK INITIAL EQUILIBRIUM
check_init_eq <- 0 #Set to 0 to solve for counterfactual, set to 1 to check if population calculation matches initial equilibrium. It does.


#MUTUALLY EXCLUSIVE PARAMETERS
EquilibriumType["Full"] <- TRUE #Set to true/1 to solve for full counterfactual
EquilibriumType["WithinCityMobility"] <- FALSE
EquilibriumType["NoMobility"] <- FALSE

#Set to TRUE if we are to use bySkill version of equilibrium.
EquilibriumType["bySkill"] <- FALSE
EquilibriumType["EndogenousAmenities"] <- TRUE
EquilibriumType["EndogenousProductivity"] <- TRUE


#Run solution
source(solver)
