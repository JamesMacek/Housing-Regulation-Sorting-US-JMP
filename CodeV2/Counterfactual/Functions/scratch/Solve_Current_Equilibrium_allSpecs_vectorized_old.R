
#function wrapper 
solveEquilibrium <- function() {

#Date created: Feb 5th, 2023

#This file solves for current set of equilibria under full deregulation. Cleans up code from previous file.

library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)
library(collapse)
library(compiler)


#Importing functions
source("CodeV2/Counterfactual/Functions/Solve_Equilibrium_Functions_vectorized.R", local = TRUE)
source("CodeV2/Counterfactual/Parameters/GlobalParameters.R", local = TRUE) #source script to local environemnt within solveEquilibrium


#__________________________________________________________________________________
#   Checking legality of current parameterization of the model.____________________
#__________________________________________________________________________________
#Equilibrium types in vector
EquilibriumType_vec <- c()
for (type in c("Full", "WithinCityMobility", "NoMobility")) {
  EquilibriumType_vec[type] <- EquilibriumType[[type]]
}


#Checking at most one is true, and at least one is true
if (length(which(EquilibriumType_vec == TRUE)) > 1) {
  stop("ERROR: ONLY ONE TYPE OF EQUILIBRIUM CAN BE CALCULATED AT A TIME")
  
}

if (length(which(EquilibriumType_vec == TRUE)) < 1) {
  stop("ERROR: NO EQUILIBRIUM TYPES SPECIFIED")
  
}

if (length(which(EquilibriumType_vec == TRUE)) + length(which(EquilibriumType_vec == FALSE)) != 3) {
  stop("ERROR: ILLEGAL CHARACTERS IN EQUILIBRIUM TYPE LIST")
  
}


CurrentVersion <- names(EquilibriumType_vec)[which(EquilibriumType_vec == TRUE)]
rm(EquilibriumType_vec)

#________________________________ Setting Up All Objects _______________________________________________________

  #Initializing skill names 
  if (EquilibriumType$bySkill == TRUE) {
  
    skillVector <-  c("College", "NoCollege")
    skillName <- c("College_", "NoCollege_") 
  
  }else{
  
    skillVector <- c("Pooled")
    skillName <- c("")
  
  }
  
  #Preferences
  if (EquilibriumType$StoneGeary == TRUE) {
    pref <- "SG"
    demandParameters_to_pass <- c(beta_StGeary, min_hReq) #for getConsumptionValue
  }else{
    pref <- "CD"
    demandParameters_to_pass <- c(beta, 0) #for getConsumptionValue
  }

  #Zone ID's; (whether) complete deregulation or not
  if (EquilibriumType$Partial_Dereg == TRUE) {
    zoneList <- c(1, 2)
  }else{
    zoneList <- c("Merged")
  }

  #Import Master Dataset + consumption adjustment factors
  load(paste0("DataV2/Counterfactuals/Calibration_Output/consumption_AdjustmentFactor_bySkill", EquilibriumType$bySkill, "_pref_", pref, ".Rdata"))
  Master <- read_dta(paste0("DataV2/Counterfactuals/Master_post_calibration_bySkill", EquilibriumType$bySkill, "_pref_", pref, "_amenities.dta"))
  
  #Importing fundamental productivities at city level
  City_prod <- read_dta("DataV2/Counterfactuals/Calibration_Output/City_Productivity.dta") %>% select(CBSA, starts_with("Productivity"))
  
  #Matching productivity to master dataset
  Master <- left_join(Master, City_prod, by = c("CBSA"))
  rm(City_prod)

  #Inverse city weights for calculation
  Master <- Master %>% group_by(CBSA) %>% mutate(inverse_city_weights = 1/n())
  
  #Calculating total calibrated land in block group
  Master["final_land_for_res"] <- Master$land_regulated + Master$land_unregulated
  
  #Creating data frame with equilibrium objects that will be updated
  Equilibrium_objects <- list()
  
  #Neighborhood income, ability_grp, city wages
  Equilibrium_objects[["ability_grp"]] <- as.matrix(select(ungroup(Master), contains("ability_grp")))
  
  for (skill in skillVector) {
    Equilibrium_objects[["Wage"]][[skill]] <- matrix(Master[[paste0(skill, "Wage")]], 
                                                     nrow = nrow(Master), ncol = 7) #replicate nrow(Master) x 7 matrix
  }
  
  #Initial populations (not by zone)
  for (skill in skillVector) {
      
      if (EquilibriumType$bySkill == FALSE) {
        Equilibrium_objects[["Population"]][[skill]] <- as.matrix( Master[, paste0("Population_type_", 1:7)] )
      }else{
        Equilibrium_objects[["Population"]][[skill]] <- as.matrix( Master[, paste0("Population_type_", skill, "_", 1:7)] )
                                                                         
      }
  }
  
  
  #City populations, total city populations
  Equilibrium_objects[["City_Population"]] <- getCityPopulations_byType(Equilibrium_objects)
  Equilibrium_objects[["Total_City_Population"]] <- getTotalCityPopulations(Equilibrium_objects)
  
  #Total populations in general (fixed everywhere)
  for (skill in skillVector) {
    Equilibrium_objects[["Total_Population"]][[skill]] <- matrix(colSums(Equilibrium_objects[["Population"]][[skill]]), nrow = nrow(Master), ncol = 7, byrow = TRUE)
  }
                                                      
  #Incomes
  Equilibrium_objects[["Avg_income"]] <- getAvgIncome(Eq_objects = Equilibrium_objects)
  
  #Amenities 
  for (skill in skillVector) {
    if (EquilibriumType$bySkill == FALSE) {
      Equilibrium_objects[["Amenity"]][[skill]] <- as.matrix( Master[, paste0("Amenity_", 1:7)] )
    }else{
      Equilibrium_objects[["Amenity"]][[skill]] <- as.matrix( Master[, paste0("Amenity_", skill, "_", 1:7)] )
      
    }
  }
  
  #Land mass 
  if (EquilibriumType$Partial_Dereg == TRUE) {
    Equilibrium_objects[["Land"]][[1]] <- Master$land_regulated #If partial dereg, split land into different zones
    Equilibrium_objects[["Land"]][[2]] <- Master$land_unregulated
  }else{
    Equilibrium_objects[["Land"]][["Merged"]] <- Master$final_land_for_res #If full dereg, aggregate across zones
  }
  
  #Regulated housingUnit shares at initial equilibrium, only required if Partial_Dereg.
  if (EquilibriumType$Partial_Dereg == TRUE) { 
    
    Equilibrium_objects[["init_unit_share"]][[1]] <- Master$regulated_housingUnit_share
    Equilibrium_objects[["init_unit_share"]][[1]][Master$Regulation_code == 0] <- 0
    
    
    Equilibrium_objects[["init_unit_share"]][[2]] <- 1 - Master$regulated_housingUnit_share
    Equilibrium_objects[["init_unit_share"]][[2]][Master$Regulation_code == 0] <- 1 
    #If neighborhood has no regulation, set unit shares to be 1 in unregulated zone. 
    
  }
  
  #Initial city productivity if bySkill == TRUE 
  if (EquilibriumType$bySkill == TRUE) {
    for (skill in skillVector) {
      Equilibrium_objects[["skillProd"]][[skill]] <- Master[[paste0("Productivity_", skill)]]
    }
  }
  
  
  #All other policy-invariant parameters are kept in master dataframe....
  #Storing Master for counterfactuals surrounding the initial equilibrium
  
  Master["Avg_income"] <- Equilibrium_objects["Avg_income"]
  
  for (skill in skillVector) {
    
    name_of_skill <- skillName[which(skill == skillVector)]
    Master[paste0("Total_Population_type_", name_of_skill, 1:7)] <- Equilibrium_objects[["Total_Population"]][[skill]]
  
  } #Additional useful variables
  
  save(Master, file = paste0("DataV2/Counterfactuals/Init_eq_", EquilibriumType$bySkill, "_pref_", pref, ".RData"))
  
#___________________________________________________________________________________________________________________________________
#_______________________________________INITIAL FUNCTION EVALUATIONS________________________________________________________________
#___________________________________________________________________________________________________________________________________
  
  #Side, need original prices/consumption values if Partial_Dereg == TRUE to start algorithm
  if (EquilibriumType$Partial_Dereg == TRUE) {

    Equilibrium_objects[["housingPrice"]][[1]] <- Master$price_regulated
    Equilibrium_objects[["housingPrice"]][[2]] <- Master$price_unregulated
    
    
  }else{ #End preliminaries if Partial_Dereg == TRUE
    
    #Take midpoint between reg and unreg prices at initial values. Does not matter what you do here.
    Equilibrium_objects[["housingPrice"]][["Merged"]] <- (1/2)*Master$price_regulated + (1/2)*Master$price_unregulated
    
  } 
  
  #Initial by-zone consumption values and spending shares
  for (zone in zoneList) {
    if (zone == 1) { #if zone == 1, we know we are in partial regulated eq. apply
      reg_id <- TRUE
    }else{
      reg_id <- FALSE
    }

    InitialConsumptionValues <- getConsumptionValues(Eq_objects = Equilibrium_objects,
                                           demandParameters = demandParameters_to_pass,
                                           reg_identifier = reg_id,
                                           zoneType = zone)
    for (skill in skillVector) {
      for (var in c("uncl_cons", "hSpendShare")) {
       Equilibrium_objects[[var]][[skill]][[zone]] <-   InitialConsumptionValues[[var]][[skill]][[zone]]  #Save these initial spending shares for later
      }
    }
  }
  
  
  #Aggregate consumption values across zones to zone level
  Equilibrium_objects[c("fr_zone", "consumption_Val")] <- ZoneAggregation(Eq_objects = Equilibrium_objects)
  
  #Zonal populations at initial iteration
  for (zone in zoneList) {
    for (skill in skillVector) {
      Equilibrium_objects[["Population_zone"]][[skill]][[zone]] <- Equilibrium_objects[["Population"]][[skill]]*Equilibrium_objects[["fr_zone"]][[skill]][[zone]]
    }
  }
    
  
  #Exogenous amenities, invert amenity function
  for (skill in skillVector) {
    Equilibrium_objects[["exogenous_Amenity"]][[skill]] <- Equilibrium_objects[["Amenity"]][[skill]]/( Equilibrium_objects$Avg_income^( matrix(Omega, nrow = nrow(Master), ncol = 7, byrow = TRUE)) )
  }
  
  
  Equilibrium_objects[["avg_exogenous_amenity"]] <- rowMeans(as.matrix(bind_cols(Equilibrium_objects[["exogenous_Amenity"]]))) #Take average across all types
  
  #If no fundamentals, change exogenous amenity to equal average across all types--muting all income sorting. 
  if (EquilibriumType["NoFundamentals"] == TRUE) { 
    
    for (skill in skillVector) {
      
      Equilibrium_objects[["exogenous_Amenity"]][[skill]] <- Equilibrium_objects$avg_exogenous_amenity
      
    } 
  }
  
  #Solving for fundamental productivity if proportion of productivity is endogenous...
  if (EquilibriumType$bySkill == FALSE) {
    
    Equilibrium_objects[["fundamental_prod"]][["Pooled"]] <- Equilibrium_objects[["Wage"]][["Pooled"]]/(Equilibrium_objects[["Total_City_Population"]]$Aggregate^(Agglomeration_elast))
    
    
  }else{ #If bySkill == TRUE, different sln to fundamental productivity using Diamond (2016) elasticities
    
    skillIndex <- 0
    for (skill in skillVector) {
      skillIndex <- skillIndex + 1
      Equilibrium_objects[["fundamental_prod"]][[skill]] <-  Equilibrium_objects[["skillProd"]][[skill]]/( (Equilibrium_objects$Total_City_Population[["College"]]^(bySkill_agg_matrix[2, skillIndex]))*
                                                                                                           (Equilibrium_objects$Total_City_Population[["NoCollege"]]^(bySkill_agg_matrix[1, skillIndex])) )
    }
      
      
  }
  
#______________________________________________________________________________________________________________________________________________________
#_______________________________________ START SOLVING FOR EQUILIBRIA___________________________________________________________________
#______________________________________________________________________________________________________________________________________________________
  
  #Iteration count
  iter <- 1
  
  #____ Adjustment speeds that work (trial and error) __________________________
  
    #baseline...
    adjustment_speed_Mobility <- 0.25
    adjustment_speed_SpendShares <- 0.05
    
    #Else, slower models...
    if ( (EquilibriumType$bySkill == TRUE & EquilibriumType$EndogenousAmenities == TRUE) |
          EquilibriumType$Partial_Dereg == TRUE)  {
      adjustment_speed_Mobility <- 0.15
      adjustment_speed_SpendShares <- 0.05
   }
 
  
  
  #initializing set of old equilibrium objects from last iteration
  old_Equilibrium_objects <- Equilibrium_objects
  
  #Error tolerances
  if (pref == "SG" | EquilibriumType$Partial_Dereg == TRUE) {
    
    eq_error_tol_Spendshares <- 0.01
    error_Spendshare <- eq_error_tol_Spendshares + 1 #initializing running error
    
  }else{
    eq_error_tol_Spendshares <- 0.01
    error_Spendshare <- 0  #CD preferences don't adjust spending shares
  }
  
    eq_error_tol_Mobility <- 5  #Equilibrium error tolerances of 5 households
    error_Mobility <- eq_error_tol_Mobility + 1 #initial error to start loop
    
    #START WHILE LOOP HERE....
    
    while (error_Mobility > eq_error_tol_Mobility | error_Spendshare > eq_error_tol_Spendshares) {
          
      #PART 0.25: Get City populations at this iteration.
        Equilibrium_objects[["City_Population"]] <- getCityPopulations_byType(Equilibrium_objects)
        Equilibrium_objects[["Total_City_Population"]] <- getTotalCityPopulations(Equilibrium_objects)
        
      #PART 0.5: Updating productivity if needed
      if (EquilibriumType$EndogenousProductivity == TRUE) {
        
        if (EquilibriumType$bySkill == FALSE) {
          
          #Update productivity if no education types
          Equilibrium_objects[["Wage"]][["Pooled"]] <- Equilibrium_objects[["fundamental_prod"]][["Pooled"]]*(Equilibrium_objects[["Total_City_Population"]]$Aggregate^(Agglomeration_elast))
          
        }else{
          
          skillIndex <- 0
          for (skill in skillVector) {
            skillIndex <- skillIndex + 1  
            Equilibrium_objects[["skillProd"]][[skill]] <- Equilibrium_objects[["fundamental_prod"]][[skill]]*( (Equilibrium_objects$Total_City_Population[["College"]]^(bySkill_agg_matrix[2, skillIndex]))*
                                                                                                                       (Equilibrium_objects$Total_City_Population[["NoCollege"]]^(bySkill_agg_matrix[1, skillIndex])) )
             
          }
          
        } #end updating bySkill == TRUE
        
      } #End check for endogeous productivity
      
      
      #PART 0.75: Updating Wages if equilibrium is done by skill
      if (EquilibriumType$bySkill == TRUE) {
        
        UpdateWagesBySkill <- getSkillWages(Eq_objects = Equilibrium_objects) #Storing new wages 
        
        for (skill in skillVector) {
          Equilibrium_objects[["Wage"]][[skill]] <- matrix(UpdateWagesBySkill[[paste0(skill, "Wage")]], nrow = nrow(Master), ncol = 7)
        }
        rm(UpdateWagesBySkill)
        
      }
      
      
      #PART 1: SOLVING FOR PRICES BY ZONE GIVEN CURRENT SPENDING SHARES
      Equilibrium_objects$housingPrice <- getHousingPrices(Eq_objects = Equilibrium_objects) #Can ignore NaN's in housing prices if zone does not exist
      
      #PART 2: SOLVING FOR AVERAGE INCOME
      Equilibrium_objects[["Avg_income"]] <- getAvgIncome(Eq_objects = Equilibrium_objects)
          
      #PART 3: SOLVING FOR AMENITIES AT CURRENT POPULATION DISTRIBUTION IF UPDATING
      if (EquilibriumType$EndogenousAmenities == TRUE) {
        Equilibrium_objects[["Amenity"]] <- getLocationAmenity(Eq_objects = Equilibrium_objects)
      }
      
      #PART 4: SOLVING FOR CONSUMPTION VALUES IN EACH ZONE
      for (zone in zoneList) {
        if (zone == 1) { #if zone == 1, we know we are in partial regulated zone so we must use slower function to calc consumption valuess
          reg_id <- TRUE
        }else{
          reg_id <- FALSE
        }
        
        tmp_cons_value <- getConsumptionValues(Eq_objects = Equilibrium_objects,
                                               demandParameters = demandParameters_to_pass,
                                               reg_identifier = reg_id,
                                               zoneType = zone)
        for (skill in skillVector) {
          for (var in c("uncl_cons", "hSpendShare")) {
            Equilibrium_objects[[var]][[skill]][[zone]] <- tmp_cons_value[[var]][[skill]][[zone]] 
          }
        }
      }
      rm(tmp_cons_value)
      
      
      #Aggregate consumption values across zones to zone level + fractions
      Equilibrium_objects[c("fr_zone", "consumption_Val")] <- ZoneAggregation(Eq_objects = Equilibrium_objects)
      
      
      #DEBUGGING ONLY, TEST IF POPULATION SHARES ARE GOOD
      if (check_init_eq == 1 & EquilibriumType$Partial_Dereg == FALSE) { 
        
        for (skill in skillVector) {
          
          if (EquilibriumType$bySkill == TRUE) {
            Equilibrium_objects[["consumption_Val"]][[skill]] <-  as.matrix(select(ungroup(Master),
                                                                                   starts_with(paste0("consumption_Val_", skill)))) #Set consumption values to initial calibrated levels to see if population calculation is correct.
          }else{
            Equilibrium_objects[["consumption_Val"]][[skill]] <-  as.matrix(select(ungroup(Master),
                                                                                   starts_with("consumption_Val_"),
                                                                                   -contains("College")))
          }
          
        }
      }
      
      
      #PART 5: SOLVING FOR TOTAL VALUE OF NEIGHBORHOOD RAISED TO MIGRATION ELASTICITY TO CALCULATE NEW DESIRED POPULATIONS. 
      for (skill in skillVector) {
        Equilibrium_objects[["Val"]][[skill]] <- (exp(Equilibrium_objects[["consumption_Val"]][[skill]])*Equilibrium_objects[["Amenity"]][[skill]])^(rho)
      }
      
      
      
      if (EquilibriumType$WithinCityMobility == TRUE | EquilibriumType$Full == TRUE) { #only required for within city and full mobility models
        
        #PART 6: SOLVING FOR DESIRED SHARE OF CITY C IN NEIGHBORHOOD N.
        Equilibrium_objects[c("Neighborhood_shares", "Val_city")] <-  getNeighborhoodShares(Eq_objects = Equilibrium_objects)
        
        #Version of model where population identified if only considering within-city mobility
          if (EquilibriumType$WithinCityMobility == TRUE) {  
          
            for (skill in skillVector) {
              Equilibrium_objects[["Population"]][[skill]] <- Equilibrium_objects[["Neighborhood_shares"]][[skill]]*Equilibrium_objects[["City_Population"]][[skill]] #Note: city population not updated whatsoever in this if-statement tree. 
            }
          }      
        
        #PART 7: SOLVING FOR DESIRED NATIONAL SHARE IN CITY C AND TOTAL POPULATIONS
        if (EquilibriumType$Full == TRUE) { #Only update if we assume perfect mobility
          for (skill in skillVector) {
            
            #Taking sum over all city values, deweight by inverse city weight (number of block groups per city)
            Equilibrium_objects[["City_shares"]][[skill]] <- (Equilibrium_objects[["Val_city"]][[skill]]^(theta)) / (matrix(colSums( (Equilibrium_objects[["Val_city"]][[skill]]^(theta))*matrix(as.matrix(Master$inverse_city_weights), nrow = nrow(Master), ncol = 7)), 
                                                                                                                     nrow = nrow(Master), ncol = 7, byrow = TRUE)) #Note: expanded by row
            
            
            #Test to see if all city shares sum to one after deweighting (they do... )
            #Relevant test is colSums(Equilibrium_objects[["City_shares"]][[skill]]*matrix(as.matrix(Master$inverse_city_weights), nrow = nrow(Master), ncol = 7))
            
            #Update desired population using these shares at current tattonement values
            Equilibrium_objects[["Population"]][[skill]] <- Equilibrium_objects[["Neighborhood_shares"]][[skill]]*Equilibrium_objects[["City_shares"]][[skill]]*  
                                                            Equilibrium_objects[["Total_Population"]][[skill]] #forward population levels based on what we observe here
            
            #Note: colSums(Equilibrium_objects[["Population"]][[skill]]) == colSums(old_Equilibrium_objects[["Population"]][[skill]) exactly.
            
          }
        } #End check of full mobility
        
      } #End Check for within/full mobility
      
      
        #Finally, Updating zonal populations if necessary
        if (EquilibriumType$Partial_Dereg == TRUE) {
          
          for (zone in zoneList) {
            for (skill in skillVector) {
              Equilibrium_objects[["Population_zone"]][[skill]][[zone]] <- Equilibrium_objects[["Population"]][[skill]]*Equilibrium_objects[["fr_zone"]][[skill]][[zone]]
            }
          }
          
        }else{
          for (skill in skillVector) {
            Equilibrium_objects[["Population_zone"]][[skill]][[zone]] <- Equilibrium_objects[["Population"]][[skill]] #update if necessary
          }
        }
      
      
      #PART 8: CALCULATING ERROR FROM SPATIAL EQUILIBRIUM
      
        #8 a): Mobility errors from spatial equilibrium
        #Initializing errors
        error_calculate <- list()
        
        for (skill in skillVector) {
          for (zone in zoneList) {
            
            #Take maximum error across all income types; by zone and education
              error_calculate[[skill]][[zone]] <- max(abs(Equilibrium_objects[["Population_zone"]][[skill]][[zone]] - 
                                                          old_Equilibrium_objects[["Population_zone"]][[skill]][[zone]]) )

          
          }
        }
        
        #Update halting condition, taking maximum across all errors by education and zone
        error_Mobility <- max(unlist(error_calculate))
        
        #Update populations
        for (skill in skillVector) {
          for (zone in zoneList) {
            
            #Updating according to tatonnement process..
              Equilibrium_objects[["Population_zone"]][[skill]][[zone]] <- old_Equilibrium_objects[["Population_zone"]][[skill]][[zone]] + 
                                                                           adjustment_speed_Mobility*(Equilibrium_objects[["Population_zone"]][[skill]][[zone]] - old_Equilibrium_objects[["Population_zone"]][[skill]][[zone]])
  
          }
        }
        
        #8 b): Spending share errors
        if (pref == "SG" | EquilibriumType["Partial_Dereg"] == TRUE) { #only relevant here...
          
          error_calculate <- list()
          
          for (skill in skillVector) {
            for (zone in zoneList) {
              
              #Take maximum error across all income types, by zone and education
              error_calculate[[skill]][[zone]] <- max(abs(Equilibrium_objects[["hSpendShare"]][[skill]][[zone]] -  
                                                          old_Equilibrium_objects[["hSpendShare"]][[skill]][[zone]]), na.rm = TRUE) #remove NaNs in locations where zone does not exist
              
            }
          }
          #Update second halting condition
          error_Spendshare <- max(unlist(error_calculate)) 
          
          #Adjusting spending shares from old prices given adjustment speed...
          for (skill in skillVector) {
            for (zone in zoneList) {
              
              #Take maximum error across all income types, by zone and education
              Equilibrium_objects[["hSpendShare"]][[skill]][[zone]] <- old_Equilibrium_objects[["hSpendShare"]][[skill]][[zone]] + 
                                                                       adjustment_speed_SpendShares*(Equilibrium_objects[["hSpendShare"]][[skill]][[zone]] - old_Equilibrium_objects[["hSpendShare"]][[skill]][[zone]])
                                                                           
                
            }
          }
          
        } #end check if updating spending shares
        
        
        #Re-updating populations after adjusting them on equilibrium path
        for (skill in skillVector) {
          
            Equilibrium_objects[["Population"]][[skill]] <- Reduce("+", Equilibrium_objects[["Population_zone"]][[skill]])
                                                                    #Pop = sum across zones after adjustment
        }
        
        
        
        #printing total error
        print(paste0("At iteration ", iter, ", the error for labour mobility is ", error_Mobility,
                     ", the error for spending shares is ", error_Spendshare,
                     ". The time is ", Sys.time(), "."))
        
        #Updating old equilibrium objects with new
        old_Equilibrium_objects <- Equilibrium_objects
        
        #Iteration count
        iter <- iter + 1
        
        
      
      
    } #END WHILE LOOP
    
  
#________________________________________________________________________________________________________________
  

#Updating simple dataframe for use with analysis programs
  
  #Creating data frame with equilibrium objects that will be updated
  Equilibrium_objects_output <- Master %>% select(State, County, Tract, BlockGroup,
                                                  CBSA, CBSA_NAME, lambda, final_land_for_res, starts_with("Total_Population_"),
                                                  starts_with("ability_grp"), 
                                                  starts_with("HS_Elasticity_imputed"),
                                                  starts_with("land"),
                                                  inverse_city_weights) #select time invariant parameters from master
  
  #Update columns with changed equilibrium values that we want...
  #Populations, wages, consumption values, amenities, income, exogenous amenities & housing prices by zone 
  
  for (skill in skillVector) {
    
    name_of_skill <- skillName[which(skill == skillVector)]
    
    #Pops
    Equilibrium_objects_output[paste0("Population_type_", name_of_skill, 1:7)] <- Equilibrium_objects[["Population"]][[skill]]
    
    #Wages
    Equilibrium_objects_output[paste0(skill, "Wage")] <- Equilibrium_objects[["Wage"]][[skill]][, 1]
    
    #Consumption values
    Equilibrium_objects_output[paste0("consumption_Val_", name_of_skill, 1:7)] <- Equilibrium_objects[["consumption_Val"]][[skill]]
    
    #Spending shares, by zone
    
    
    #Amenities
    Equilibrium_objects_output[paste0("Amenity_", name_of_skill, 1:7)] <- Equilibrium_objects[["Amenity"]][[skill]]
    
    #Exogenous amenities
    Equilibrium_objects_output[paste0("exogenous_Amenity_", name_of_skill, 1:7)] <- Equilibrium_objects[["exogenous_Amenity"]][[skill]]
    
    
    for (zone in zoneList) {
      
      if (zone == "Merged"){
        zoneName <- ""
      }else{
        zoneName <- paste0("_z", zone)
      }
      
      #Pops; by zone...
      Equilibrium_objects_output[paste0("Population_type_", name_of_skill, 1:7, zoneName)] <- Equilibrium_objects[["Population_zone"]][[skill]][[zone]]
      
      #Spending shares on housing, by zone
      Equilibrium_objects_output[paste0("hSpendShare_", name_of_skill, 1:7, zoneName)] <-  Equilibrium_objects[["hSpendShare"]][[skill]][[zone]] 
      
    }
    
    
  
  }
  
   #Avg income
   Equilibrium_objects_output["Avg_income"] <- Equilibrium_objects[["Avg_income"]]
   
   #Housing prices by zone
   for (zone in zoneList) {
     
     if (zone == "Merged"){
       zoneName <- ""
     }else{
       zoneName <- paste0("_z", zone)
     }
     
     Equilibrium_objects_output[[paste0("housingPrice", zoneName)]] <- Equilibrium_objects[["housingPrice"]][[zone]]
     
   }
   
  
  
#_______________________________________________________________________________________________________________
#_____________________________________________Saving files!_____________________________________________________
#_______________________________________________________________________________________________________________

  
  
if (EquilibriumType$SocialOpt_gridSearch == FALSE) {  
  
  #Save if not testing...
  if (check_init_eq == 0) {

  
    if (EquilibriumType$Partial_Dereg == TRUE) {
      OutFolder <- "PartialDeregulation"
    
    
      if (EquilibriumType["NoFundamentals"] == FALSE) {
        save(Equilibrium_objects_output, file =paste0("DataV2/Counterfactuals/Counterfactual_Output/", OutFolder, "/", 
                                                      FileOutputName, "_", CurrentVersion, 
                                                      "_EndoAmen_", EquilibriumType$EndogenousAmenities, 
                                                      "_EndoProd_", EquilibriumType$EndogenousProductivity,
                                                      "_bySkill_", EquilibriumType$bySkill,
                                                      "_pref_", pref, ".RData"))
      }
    
      if (EquilibriumType["NoFundamentals"] == TRUE) {
        save(Equilibrium_objects_output, file =paste0("DataV2/Counterfactuals/Counterfactual_Output/DiffFundamentals/", 
                                                      "InitialEq_NoFundamentals", CurrentVersion, 
                                                      "_EndoAmen_", EquilibriumType$EndogenousAmenities, 
                                                      "_EndoProd_", EquilibriumType$EndogenousProductivity,
                                                      "_bySkill_", EquilibriumType$bySkill,
                                                      "_pref_", pref, ".RData"))
      }
      
      
      
    
    }else{
      OutFolder <- "FullDeregulation"
      FileOutputName <- "Eq_objects"
  }

    if (EquilibriumType["NoFundamentals"] == FALSE) {
      save(Equilibrium_objects_output, file =paste0("DataV2/Counterfactuals/Counterfactual_Output/", OutFolder, "/", 
                                             FileOutputName, "_", CurrentVersion, 
                                             "_EndoAmen_", EquilibriumType$EndogenousAmenities, 
                                             "_EndoProd_", EquilibriumType$EndogenousProductivity,
                                             "_bySkill_", EquilibriumType$bySkill,
                                             "_pref_", pref, ".RData"))
    }

    if (EquilibriumType["NoFundamentals"] == TRUE) {
      save(Equilibrium_objects_output, file =paste0("DataV2/Counterfactuals/Counterfactual_Output/DiffFundamentals/", 
                                             "FullDereg_NoFundamentals", CurrentVersion, 
                                             "_EndoAmen_", EquilibriumType$EndogenousAmenities, 
                                             "_EndoProd_", EquilibriumType$EndogenousProductivity,
                                             "_bySkill_", EquilibriumType$bySkill,
                                             "_pref_", pref, ".RData"))
    }
    
  } #end checking testing original equilibrium

} #End check for socially optimal regulation == FALSE
   
if (EquilibriumType$SocialOpt_gridSearch == TRUE) {
  
  #Save definition of parameter space, do not allow definitions to vary
  save(Equilibrium_objects_output, 
       file = paste0("DataV2/Counterfactuals/Counterfactual_Output/OptimalPolicy/Equilibrium_F1_", F1, "_F2_", F2, ".RData"))

}
   
   

#Removing all unneeded objects for restart..
rm(list = ls()[!ls() %in% c("EquilibriumType", "solveEquilibrium", 
                            "solver",
                            "check_init_eq",
                            "IncomeStringency_ctfl",
                            "cities", "cityNames")])

} #end wrapper function

solveEquilibrium <- cmpfun(solveEquilibrium) #compile this ultra-large function

