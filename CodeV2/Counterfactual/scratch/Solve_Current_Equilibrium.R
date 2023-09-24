#Date created: Feb 5th, 2023

#This file solves for current set of equilibria under full deregulation.

library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)
library(stringr)


#CHECKING IF CURRENT PARAMETERIZATION IS KOSHER!

#Equilibrium types in vector
EquilibriumType_vec <- rep(NA, 3)
for (type in 1:3) {
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

CurrentVersion <- which(EquilibriumType_vec == TRUE)
CurrentVersion <- names(EquilibriumType[CurrentVersion]) #For saving 
rm(EquilibriumType_vec)

#PRELIMINARIES________________________________________________
if (EquilibriumType$bySkill == FALSE) {
  
  #Import Master Dataset
  Master <- read_dta("DataV2/Counterfactuals/Master_post_calibration_amenities.dta") 
  consumptionAdjustment <- read_dta("DataV2/Counterfactuals/Calibration_Output/consumption_AdjustmentFactor_gumbel.dta")
  
  #Creating total measures of aggregate and city population
  for (incomeType in 1:7) {
    Master[paste0("Total_Population_type_", incomeType)] <- sum(Master[[paste0("Population_type_", incomeType)]])
    
  }
  
  for (incomeType in 1:7) {
    pop_sym <- paste0("Population_type_", incomeType)
    city_pop_sym <- paste0("City_Population_type_", incomeType)
    Master <- Master %>% group_by(CBSA) %>% mutate(!!sym(city_pop_sym) := sum(!!sym(pop_sym)))
    
  }
  
}

#Note: BySkill version of the model uses amenity values calibrated on the non-BySkill version of the model (to keep things simple).
if (EquilibriumType$bySkill == TRUE) {
  
  Master <- read_dta("DataV2/Counterfactuals/Master_post_calibration_amenities.dta")
  consumptionAdjustment <- read_dta("DataV2/Counterfactuals/Calibration_Output/consumption_AdjustmentFactor_gumbel_bySkill.dta")
  
  #Creating total measures of aggregate and city population
  for (skill in c("College", "NoCollege")) {
    for (incomeType in 1:7) {
      Master[paste0("Total_Population_type_", skill, "_", incomeType)] <- sum(Master[[paste0("Population_type_", skill, "_", incomeType)]])
      
    }
  }
  
  for (skill in c("College", "NoCollege")) {
    for (incomeType in 1:7) {
      pop_sym <- paste0("Population_type_", skill, "_", incomeType)
      city_pop_sym <- paste0("City_Population_type_", skill, "_",  incomeType)
      Master <- Master %>% group_by(CBSA) %>% mutate(!!sym(city_pop_sym) := sum(!!sym(pop_sym)))
    }
  }
  
}


#Inverse city weights for calculation
Master <- Master %>% group_by(CBSA) %>% mutate(inverse_city_weights = 1/n())

#Avg income to compare with counterfactuals
Master["Avg_income"] <- getAvgIncome(Master_data = Master, 
                                     bySkill = EquilibriumType$bySkill)

#Storing Master for counterfactuals surrounding the initial equilibrium
if (EquilibriumType$bySkill == FALSE) {
  
  save(Master, file = "DataV2/Counterfactuals/Init_eq.RData")
  
  #Creating data frame with equilibrium objects
  
  Equilibrium_objects <- Master %>% select(State, County, Tract, BlockGroup,
                                           CBSA, CBSA_NAME, lambda, 
                                           ends_with(paste0("Total_Population_type_", 1:7)), 
                                           ends_with(paste0("City_Population_type_", 1:7)), 
                                           ends_with(paste0("Population_type_", 1:7)),
                                           starts_with("ability_grp"), starts_with("Pooled"), starts_with("HS_Elasticity_imputed"),
                                           starts_with("land"),
                                           starts_with("Amenity_"), starts_with("hSpendShare_"), starts_with("consumption_Val"),
                                           inverse_city_weights)
}

if (EquilibriumType$bySkill == TRUE) {
  
  save(Master, file = "DataV2/Counterfactuals/Init_eq_bySkill.RData")
  
  #Creating data frame with equilibrium objects
  
  Equilibrium_objects <- Master %>% select(State, County, Tract, BlockGroup,
                                           CBSA, CBSA_NAME, lambda_bySkill, 
                                           starts_with(paste0("Total_Population_type_", c("College", "NoCollege"))), 
                                           starts_with(paste0("City_Population_type_", c("College", "NoCollege"))), 
                                           starts_with(paste0("Population_type_", c("College", "NoCollege"))),
                                           starts_with("ability_grp"), ends_with("Wage"), starts_with("HS_Elasticity_imputed"),
                                           starts_with("final_land_"),
                                           starts_with("Amenity_"), starts_with("hSpendShare_"),
                                           inverse_city_weights)
}


#INITIAL FUNCTION EVALUATIONS________________________________________________________________

#Calculating total model implied land in block group
Equilibrium_objects["final_land_for_res"] <- Equilibrium_objects$land_regulated + Equilibrium_objects$land_unregulated

#Initial evaluations at those functions
Equilibrium_objects["housingPrice"] <- getHousingPrices(Master_data = Equilibrium_objects, 
                                                        bySkill = EquilibriumType$bySkill)


#First, income levels at current population
Equilibrium_objects["Avg_income"] <- getAvgIncome(Master_data = Equilibrium_objects, 
                                                  bySkill = EquilibriumType$bySkill)



#Creating measures of exogenous amenities (Required for some functions) + fundamental productivity
if (EquilibriumType$bySkill == FALSE) {
  
  #Running total for city population
  Equilibrium_objects["City_Population"] <- rep(0, nrow(Equilibrium_objects))
    
  for (incomeType in 1:7) {
    Equilibrium_objects[paste0("exogenous_Amenity_", incomeType)] <- Equilibrium_objects[[paste0("Amenity_", incomeType)]]/(Equilibrium_objects$Avg_income^(Omega[incomeType]))
    
    Equilibrium_objects[["City_Population"]] <- Equilibrium_objects[["City_Population"]] + Equilibrium_objects[[paste0("City_Population_type_", incomeType)]]
  } 
  
  
  #Exogenous component of wages-- "fundamental wages"
  Equilibrium_objects["fund_Wage"] <- Equilibrium_objects$PooledWage/(Equilibrium_objects[["City_Population"]]^(Agglomeration_elast))
    
}


#Repeating for bySkill version of the model 
if (EquilibriumType$bySkill == TRUE) {
  
  #Running total for city population
  Equilibrium_objects["City_Population"] <- rep(0, nrow(Equilibrium_objects))
  
  for (skill in c("College", "NoCollege")) {
    for (incomeType in 1:7) {
      Equilibrium_objects[paste0("exogenous_Amenity_", skill, "_", incomeType)] <- Equilibrium_objects[[paste0("Amenity_", skill, "_", incomeType)]]/(Equilibrium_objects$Avg_income^(Omega[incomeType]))
      
      Equilibrium_objects[["City_Population"]] <- Equilibrium_objects[["City_Population"]] + Equilibrium_objects[[paste0("City_Population_type_", skill, "_", incomeType)]]
    }
  }
  
  for (skill in c("College", "NoCollege")) {
    
    Equilibrium_objects[paste0("fund_Wage_", skill)] <- Equilibrium_objects[[paste0(skill, "Wage")]]/(Equilibrium_objects[["City_Population"]]^(Agglomeration_elast))
    
  }
  
  
}

#_____________________________________________________________________________________________
#START SOLVING FOR EQUILIBRIA

iter <- 1
eq_error_tol <- 0.01 #Tolerance for equilibrium error (i.e. one hundreth of a household!)
error <- eq_error_tol + 1 #initial error to start loop

adjustment_speed <- 0.1 #Speed of adjustment for populations

#initializing set of old equilibrium objects from last iteration
old_Equilibrium_objects <- Equilibrium_objects

#START WHILE LOOP HERE
while (error > eq_error_tol ) {
  
  #PART 0.5: SOLVING FOR WAGES IF ENDOGENOUS PRODUCTIVITY 
  if (EquilibriumType$EndogenousProductivity == TRUE) {
    
      if (EquilibriumType$bySkill == FALSE) {
    
      #Running total for city population
      Equilibrium_objects["City_Population"] <- rep(0, nrow(Equilibrium_objects))
    
      #Calculate new city-by-city total population 
      for (incomeType in 1:7) {
        pop_sym <- paste0("Population_type_", incomeType)
        city_pop_sym <- paste0("City_Population_type_", incomeType)
        Master <- Master %>% group_by(CBSA) %>% mutate(!!sym(city_pop_sym) := sum(!!sym(pop_sym)))
        
        #Running total for total city population
        Equilibrium_objects[["City_Population"]] <- Equilibrium_objects[["City_Population"]] + Equilibrium_objects[[paste0("City_Population_type_", incomeType)]]
      
      }
    
      Equilibrium_objects$PooledWage <- Equilibrium_objects$fund_Wage*(Equilibrium_objects[["City_Population"]]^(Agglomeration_elast))
    
      }
    
    if (EquilibriumType$bySkill == TRUE) {
      
      
    }
    
  }
  
  
  #PART 1: SOLVING FOR PRICES
  Equilibrium_objects["housingPrice"] <- getHousingPrices(Master_data = Equilibrium_objects, 
                                                          bySkill = EquilibriumType$bySkill)
  
  #PART 2: SOLVING FOR AVERAGE INCOME
  Equilibrium_objects["Avg_income"] <- getAvgIncome(Master_data = Equilibrium_objects, 
                                                    bySkill = EquilibriumType$bySkill)
  
  if (EquilibriumType$bySkill == FALSE) { #SOLVING BYSKILL == FALSE version of the model
    
    #PART 3: SOLVING FOR AMENITIES AT CURRENT POPULATION DISTRIBUTION
    if (EquilibriumType$EndogenousAmenities == TRUE) { #Updating amenities if they are endogenous
      for (i in 1:7) {
        Equilibrium_objects[paste0("Amenity_", i)] <- getLocationAmenity(Master_data = Equilibrium_objects,
                                                                         bySkill = EquilibriumType$bySkill,
                                                                         incomeType = i)
        
      }
    }
    
    #PART 4: SOLVING FOR CONSUMPTION VALUES
    for (i in 1:7) {
      if (check_init_eq == 0) { #Do not update consumption values if we are checking if we calculated the population distribution correctly.
        Equilibrium_objects[paste0("consumption_Val_", i)] <- getConsumptionValues(Master_data = Equilibrium_objects, 
                                                                                   bySkill = EquilibriumType$bySkill,
                                                                                   incomeType = i)
        
      }                                                                           
    }
    
    
    #PART 5: SOLVING FOR TOTAL VALUE OF NEIGHBORHOOD RAISED TO MIGRATION ELASTICITY TO CALCULATE NEW DESIRED POPULATIONS 
    for (i in 1:7) {
      Equilibrium_objects[paste0("Val_", i)] <- ((exp(Equilibrium_objects[[paste0("consumption_Val_", i)]])*(Equilibrium_objects[[paste0("Amenity_", i)]]))^(rho))
      
    }
    
    if (EquilibriumType$WithinCityMobility == TRUE | EquilibriumType$Full == TRUE) {#only required for within city and full model
      
      #PART 6: SOLVING FOR DESIRED SHARE OF CITY C IN NEIGHBORHOOD N.
      for (i in 1:7) {
        value_n_sym <- paste0("Val_", i)
        value_n_city_sym <- paste0("Val_city_", i) 
        
        Equilibrium_objects <- Equilibrium_objects %>% group_by(CBSA) %>% mutate(!!sym(value_n_city_sym) := (sum(!!sym(value_n_sym)))^(1/rho))
        
        Equilibrium_objects[paste0("Neighborhood_frac_type_", i)] <- (Equilibrium_objects[[paste0("Val_", i)]]/(Equilibrium_objects[[paste0("Val_city_", i)]]^(rho))) #Note: this is city c fraction in n of type i
        
        
        if (EquilibriumType$WithinCityMobility == TRUE) { #population identified if only considering within-city mobility
          #Population updating for within-city version of model
          Equilibrium_objects[paste0("Population_type_", i)] <- Equilibrium_objects[[paste0("Neighborhood_frac_type_", i)]]*
                                                                Equilibrium_objects[[paste0("City_Population_type_", i)]]  
        }                                                       
        
        
        #PART 7: SOLVING FOR DESIRED NATIONAL SHARE IN CITY C AND TOTAL POPULATIONS
        if (EquilibriumType$Full == TRUE) {
          
          #NATIONAL SHARE OF HOUSEHOLDS IN CITY C
          Equilibrium_objects[paste0("City_frac_type_", i)] <- (Equilibrium_objects[paste0("Val_city_", i)]^(theta))/(sum((Equilibrium_objects[paste0("Val_city_", i)]^(theta))*(Equilibrium_objects$inverse_city_weights))) #note: val is withincitywelfare^rho, so correcting for that
          
          #Total Population updating
          Equilibrium_objects[paste0("Population_type_", i)] <- Equilibrium_objects[[paste0("City_frac_type_", i)]]*
                                                                Equilibrium_objects[[paste0("Neighborhood_frac_type_", i)]]*Equilibrium_objects[[paste0("Total_Population_type_", i)]]
          
          
          
        }#End check for full equilibrium
        
      } #End loop over income types
      
    }#END CHECK FOR ANY MOBILITY EQUILIBRIUM
    
    
    #CALCULATING ERROR FROM SPATIAL EQUILIBRIUM_________________________________________________
  
    error_calculate <- rep(0, 7) #checking error in deltaPopulation by skills
    for (i in 1:7) {
      error_calculate[i] <- max(abs(Equilibrium_objects[[paste0("Population_type_", i)]] - old_Equilibrium_objects[[paste0("Population_type_", i)]]))
    }
    
    error <- max(error_calculate) #calculating total error for all
    
    #Calculating population adjustments using desired location choices at current restricted equilibria, slowing down movement by adjustmentspeed
    for (i in 1:7) {
      Equilibrium_objects[[paste0("Population_type_", i)]] <- old_Equilibrium_objects[[paste0("Population_type_", i)]] + 
        adjustment_speed*(Equilibrium_objects[[paste0("Population_type_", i)]] - old_Equilibrium_objects[[paste0("Population_type_", i)]])
    }
    
    
    #printing error
    print(paste0("At iteration ", iter, ", the error is ", error, ". The time is ", Sys.time(), "."))
    
    #Updating old equilibrium objects with new
    old_Equilibrium_objects <- Equilibrium_objects
    
    #Iteration count
    iter <- iter + 1
    #______________________________________________________________________________________________
    
  } #END BYSKILL VERSION CHECK
  
}#END WHILE LOOP HERE

#Saving file
if (check_init_eq == 0) {
  save(Equilibrium_objects, file =paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", 
                                         "Eq_Objects_", CurrentVersion, 
                                         "_EndoAmen_", EquilibriumType$EndogenousAmenities, 
                                         "_bySkill_", EquilibriumType$bySkill,
                                         "_EndProd_", EquilibriumType$EndogenousProductivity, ".RData"))
}

rm(list = str_replace(ls(), "solver", "???"))
