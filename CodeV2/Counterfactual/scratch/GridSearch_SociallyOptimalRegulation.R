  #This file performs a grid search to find better MLS policies using simple reduced-parameter rule. 
  #Packages
  library(dplyr)
  library(haven)
  library(labelled)
  library(readr)
  library(rlang)
  library(compiler)
  library(doParallel)
  
  #FILEPATH FOR SOLUTION .R FILE
  source("CodeV2/Counterfactual/Functions/Solve_Current_Equilibrium_allSpecs_vectorized.R")
  
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
  
  #Idea: consider policy space as follows:
      #New R(i) = F1 * (income)^(F2) * R(i)^(1 - F2) : F1 controls the average level of regulation, F2 in [0, 1] controls the relative weighting of income when targeting regulation
                            #Note: rescale income in this calculation so that it has same mean R(i), gives F1 roughly same interpretation as before. 
  
  
  #Create search grids here
    F1_grid = seq(0, 1, by = 0.1)[seq(0, 1, by = 0.1) != 1] #10 equilibria less than current regulation levels. We know that it should be lower given complete deregulation exercise
    F2_grid = seq(0, 1, by = 0.1)[seq(0, 1, by = 0.1) != 0] #Create same grid; Note: F2 must be within
    
  
  #________________________________________________________________________________________________________________________
  # PART 1: Ask if regulation better targeted the neighborhood income distribution, what would that look like? 
  #________________________________________________________________________________________________________________________
    
    #Instead; construct fundamental amenity measure...
    
    
    #Import initial equilibrium to create vector of unit density restrictions
      uDR_df <- read_dta(paste0("DataV2/Counterfactuals/Master_post_calibration_bySkill", EquilibriumType$bySkill, "_pref_", "SG", "_amenities.dta")) %>%
                 select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, IncomeStringency_model_rents)
    
    #Importing income data of neighborhoods absent regulation 
    load(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", "Eq_Objects_FULL", 
                "_EndoAmen_", TRUE, 
                "_EndoProd_", FALSE,
                "_bySkill_",  FALSE,
                "_pref_SG", ".RData"))
    
    # Creating expected fundamental amenity value, which measures how large amenity values are for rich people; 
    # This score is used to target regulation. 
     
    
    rm(Equilibrium_objects_output)
    
    
    
    #Store sample wide mean regulation in memory
    mean_regulation_baseline <- mean(uDR_df$IncomeStringency_model_rents)
    
    # Grid search over policy space assuming control over F2, holding F1 == 1 (fixed)
    ncores  <- detectCores() - 1
    registerDoParallel(ncores)
    
    foreach(F2 = F2_grid) %dopar% { 
              
              
          #Set F1 to one for this exercise
          F1 <- 1
          
          #Creating counterfactual parameter vector
          IncomeStringency_ctfl <- (F2)*Income_vector + (1-F2)*uDR_df$IncomeStringency_model_rents
          
          #Censoring at censor value and 0
            
          #Rescaling so that mean regulation is the same at baseline (to control for level effects from the weighted average)
          IncomeStringency_ctfl <- IncomeStringency_ctfl*(mean_regulation_baseline/mean(IncomeStringency_ctfl))
          
          solveEquilibrium() #Call equilibrium solution
          
          return(NA)
          
        
              
            }
    
    #closing cluster
    stopImplicitCluster()
    
  #____________________________________________________________________________________________________________________________________
  # PART 2: After completely targeting regulation to income distributions, ask how much lower average regulation must be to be optimal?
  #____________________________________________________________________________________________________________________________________
    
    ##Registering doParallel
    ncores  <- detectCores() - 1
  
    #Setting up number of cores (we want to conserve ram as this program is reasonably ram intensive)
    registerDoParallel(ncores)
    
    #Import initial equilibrium to create vector of unit density restrictions
        uDR_df <- read_dta(paste0("DataV2/Counterfactuals/Master_post_calibration_bySkill", EquilibriumType$bySkill, "_pref_", "SG", "_amenities.dta")) %>%
                           select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, IncomeStringency_model_rents)
        
    #Importing income data of neighborhoods absent regulation 
        load(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", "Eq_Objects_FULL", 
                    "_EndoAmen_", TRUE, 
                    "_EndoProd_", FALSE,
                    "_bySkill_",  FALSE,
                    "_pref_SG", ".RData"))
        Income_vector <- Equilibrium_objects_output$Avg_income #preserve income level
        rm(Equilibrium_objects_output)
        
        #Sample-wide mean regulation...
        mean_regulation_baseline <- mean(uDR_df$IncomeStringency_model_rents)
            
    foreach(F1 = F1_grid) %dopar% { 
            
            #Set F2 == 1 for this exercise
            F2 <- 1
            
            #Creating counterfactual parameter vector
            IncomeStringency_ctfl <- (F2)*Income_vector + (1-F2)*uDR_df$IncomeStringency_model_rents #take linear combination of these 
            
            #Rescaling so that mean regulation is the same at baseline (to control for level effects)
            IncomeStringency_ctfl <- IncomeStringency_ctfl*(mean_regulation_baseline/mean(IncomeStringency_ctfl))
            
            #Rescaling final result down by F1,
            IncomeStringency_ctfl <- F1*IncomeStringency_ctfl #Scaling down by F1
            
            rm(uDR_df)
          
            #Call solve equilibrium function
            solveEquilibrium()
              
            return(NA)
            
          }
    
    #closing cluster
    stopImplicitCluster()

    rm(list = ls())
    
    
    
    
    
  