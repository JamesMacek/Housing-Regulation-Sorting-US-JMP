#This file figures out the welfare effects of policies that better target the neighborhood income distribution. 
#


#Packages
library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)
library(compiler)

#FILEPATH FOR SOLUTION .R FILE
source("CodeV2/Counterfactual/Functions/Solve_Current_Equilibrium_allSpecs_vectorized.R")
source("CodeV2/Calibrate/Parameters/GlobalParameters.R") #Get regulation censoring parameter here. 

#PARAMETER GUI__________________________________________________________________
EquilibriumType <- list() #List to read which equilibrium to solve for 

#Specify partial deregulation exercise, uses slower algorithm
EquilibriumType["Partial_Dereg"] <- TRUE
EquilibriumType["SocialOpt_gridSearch"] <- TRUE
check_init_eq <- 0 #Must exist in memory, only for debugging

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


if (EquilibriumType$StoneGeary == FALSE | EquilibriumType$bySkill == TRUE) {
  error("Won't work for non-baseline objects right now")
}


  #Name vectors for counterfactual
  F1 <- ""


  #Import initial equilibrium to create vector of unit density restrictions
  uDR_df <- read_dta(paste0("DataV2/Counterfactuals/Master_post_calibration_bySkill", EquilibriumType$bySkill, "_pref_", "SG", "_amenities.dta")) %>%
            select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, IncomeStringency_model_rents, regulated_housingUnit_share)

  #Importing income data of neighborhoods absent regulation 
  load(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", "Eq_Objects_FULL", 
              "_EndoAmen_", TRUE, 
              "_EndoProd_", FALSE,
              "_bySkill_",  FALSE,
              "_pref_SG", ".RData"))
  uDR_df$Income_vector <- Equilibrium_objects_output$Avg_income #Average income after complete deregulation

  # Create "Amenity score": which is just average neighborhood income assuming amenity value shares are population shares
  # Measures relative fundamental amenity value for rich, which we use in this file to target regulation
  
  #Total exogenous amenity values...
  Equilibrium_objects_output["tot_amenity_value"] <- rep(0, nrow(Equilibrium_objects_output))
  for (incomeType in 1:7) {
    Equilibrium_objects_output["tot_amenity_value"] <- Equilibrium_objects_output[["tot_amenity_value"]] + Equilibrium_objects_output[[paste0("exogenous_Amenity_", incomeType)]]
    
  }
  
  # Expected exogenous amenity score
  uDR_df["exogenousAmenity_score"] <- rep(0, nrow(Equilibrium_objects_output))
  for (incomeType in 1:7) {
    uDR_df["exogenousAmenity_score"] <-    uDR_df[["exogenousAmenity_score"]] + (Equilibrium_objects_output[[paste0("exogenous_Amenity_", incomeType)]]*
                                                                                                                    Equilibrium_objects_output[[paste0("ability_grp", incomeType)]]/
                                                                                                                    Equilibrium_objects_output[["tot_amenity_value"]])
  
  }
  rm(Equilibrium_objects_output)
  
  
  #Checking correlation between income and exogenous amenity score
  print(summary(lm(log(uDR_df$Income_vector) ~ log(uDR_df[["exogenousAmenity_score"]])))) #30% R^2! 
                #Note: this is because of non-homothetic preferences -- other reasons for income sorting absent regulation
  print(summary(lm(log(IncomeStringency_model_rents*regulated_housingUnit_share) ~ log(exogenousAmenity_score),
                   data = uDR_df[uDR_df$IncomeStringency_model_rents*uDR_df$regulated_housingUnit_share > 0,]))) #only 7% R^2, pretty large, all things considered--but noisy.

  #Now, permute regulation across space so that it retains same spatial distribution, but is now ordered based on this "exogenous amenity score".
  
  #Sort uDR_df based on income absent regulation
  uDR_df_sorted <- uDR_df %>% arrange(exogenousAmenity_score)
  
  #Now, put in IncomeStringency_sorted
  uDR_df_sorted["IncomeStringency_counterfactual"] <- as.vector( arrange(select(uDR_df, IncomeStringency_model_rents), IncomeStringency_model_rents) )
  
  #Merging back to uDR_df to retain original order to pass to equilibrium solver
  uDR_df <- left_join(uDR_df, select(uDR_df_sorted, State, County, Tract, BlockGroup, IncomeStringency_counterfactual),
                      by = c("State", "County", "Tract", "BlockGroup"))

  #Saving permuted regulation levels
  write_dta(uDR_df, "DataV2/Counterfactuals/Counterfactual_Output/OptimalPolicy/Permuted_regulation.dta")
  rm(uDR_df_sorted)
  


# #########################################################################################################  
# ############## Solve equilibrium ########################################################################
# #########################################################################################################
    
    #TEMPORARY TEST
    F2 <- "TargetFundAmenity"
  
    #Creating counterfactual parameter vector to pass to file
    IncomeStringency_ctfl <- uDR_df$IncomeStringency_counterfactual
  
    solveEquilibrium() #Call equilibrium solution
    
  
    rm(list = ls())



