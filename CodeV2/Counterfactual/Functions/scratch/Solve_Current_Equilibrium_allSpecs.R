#Date created: Feb 5th, 2023

#This file solves for current set of equilibria under full deregulation.

library(doParallel)
library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)
library(collapse)

ncores <- min(detectCores() - 1, 8) #8 cores for now

#Importing functions
source("CodeV2/Counterfactual/Functions/scratch/Solve_Equilibrium_Functions_FullDereg_current.R")
source("CodeV2/Counterfactual/Parameters/GlobalParameters.R")

#_______________________________________________________________________________
#Checking legality of current parameterization of the model.____________________
#_______________________________________________________________________________
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

if (EquilibriumType["NoFundamentals"] == TRUE) {
  stop("ERROR: Cant have both extreme and no fundamentals. Must be set to a particular value.")
}

CurrentVersion <- which(EquilibriumType_vec == TRUE)
CurrentVersion <- names(EquilibriumType[CurrentVersion]) #For saving 
rm(EquilibriumType_vec)

#PRELIMINARIES________________________________________________

#Initializing skill names 
if (EquilibriumType$bySkill == TRUE) {
  
  skillVector <-  c("College", "NoCollege")
  skillName <- c("College_", "NoCollege_") 
  
}else{
  
  skillVector <- c("Pooled")
  skillName <- c("")
  
}

if (EquilibriumType$StoneGeary == TRUE) {
  pref <- "SG"
  demandParameters_to_pass <- c(beta_StGeary, min_hReq) #for getConsumptionValue
}else{
  pref <- "CD"
  demandParameters_to_pass <- c(beta, 0) #for getConsumptionValue
}

#Import Master Dataset + consumption adjustment factors
consumptionAdjustment <- load(paste0("DataV2/Counterfactuals/Calibration_Output/consumption_AdjustmentFactor_bySkill", EquilibriumType$bySkill, "_pref_", pref, ".Rdata"))
Master <- read_dta(paste0("DataV2/Counterfactuals/Master_post_calibration_bySkill", EquilibriumType$bySkill, "_pref_", pref, "_amenities.dta"))

#Importing fundamental productivities at city level
City_prod <- read_dta("DataV2/Counterfactuals/Calibration_Output/City_Productivity.dta") %>% select(CBSA, starts_with("Productivity"))

#Matching productivity to master dataset
Master <- left_join(Master, City_prod, by = c("CBSA"))
rm(City_prod)

#Creating total measures of aggregate and city population
for (skill in skillVector) {
  name_of_skill <- skillName[which(skill == skillVector)]
  
  for (incomeType in 1:7) {
    Master[paste0("Total_Population_type_", name_of_skill, incomeType)] <- sum(Master[[paste0("Population_type_",  name_of_skill, incomeType)]])
    
  }  
  
  for (incomeType in 1:7) {
    pop_sym <- paste0("Population_type_", name_of_skill, incomeType)
    city_pop_sym <- paste0("City_Population_type_", name_of_skill, incomeType)
    Master <- Master %>% group_by(CBSA) %>% mutate(!!sym(city_pop_sym) := sum(!!sym(pop_sym)))
    
  }
}


#Inverse city weights for calculation
Master <- Master %>% group_by(CBSA) %>% mutate(inverse_city_weights = 1/n())

#Avg income to compare with counterfactuals
Master["Avg_income"] <- getAvgIncome(Master_data = Master)

#Storing Master for counterfactuals surrounding the initial equilibrium

save(Master, file = paste0("DataV2/Counterfactuals/Init_eq_", EquilibriumType$bySkill, "_pref_", pref, ".RData"))

#Creating data frame with equilibrium objects
Equilibrium_objects <- Master %>% select(State, County, Tract, BlockGroup,
                                         CBSA, CBSA_NAME, lambda, 
                                         starts_with(paste0("Total_Population_type_", skillName)), 
                                         starts_with(paste0("City_Population_type_", skillName)), 
                                         starts_with(paste0("Population_type_", skillName)), 
                                         starts_with("ability_grp"), starts_with(skillVector),
                                         starts_with("HS_Elasticity_imputed"),
                                         starts_with("land"),
                                         starts_with("Amenity_"), starts_with("hSpendShare_"), starts_with("consumption_Val"),
                                         starts_with("Productivity"),
                                         inverse_city_weights)


#INITIAL FUNCTION EVALUATIONS________________________________________________________________

#Calculating total model implied land in block group
Equilibrium_objects["final_land_for_res"] <- Equilibrium_objects$land_regulated + Equilibrium_objects$land_unregulated


#Solve for initial prices at current population if needed (easy if Cobb-Douglas)
if (pref == "SG") {
  
  #Getting housing prices at initial function evaluation
  #Setting up number of cores
  registerDoParallel(ncores)
  
  Sys.time()
  #Takes approximately 1min to run per iteration. Severely slows down equilibrium calculations (can use Cobb-Douglas for testing, which runs much faster)
  Equilibrium_objects["housingPrice"] <- foreach(row = 1:nrow(Equilibrium_objects), 
                                                 .combine = "c") %dopar% { 
                                                   
                                                   #FOREACH output
                                                   return(getHousingPrices_Full(Equilibrium_objects[row, ]))
                                                   
                                                 }
  Sys.time()
  #closing cluster
  stopImplicitCluster()
  
  #Extracting spending shares from this initial price solution to calculate along equilibrium path
  for (skill in skillVector) {
    name_of_skill <- skillName[which(skill == skillVector)]
    for (incomeType in 1:7) {
      Equilibrium_objects[[paste0("hSpendShare_", name_of_skill, incomeType)]] <- ((1-beta_StGeary)*pmin((Equilibrium_objects$housingPrice*min_hReq)/(Equilibrium_objects[[paste0(skill, "Wage")]]*Equilibrium_objects[[paste0("ability_grp", incomeType)]]), 1) + beta_StGeary)
    }
  }
  
  
} #end pref == SG check

if (pref != "SG") { #If Cobb-Douglas, set spending share == cobb-douglas parameter (after Full Deregulation!)
  
  for (skill in skillVector) {
    name_of_skill <- skillName[which(skill == skillVector)]
    for (incomeType in 1:7) {
      Equilibrium_objects[[paste0("hSpendShare_", name_of_skill, incomeType)]] <- beta*rep(1, nrow(Equilibrium_objects))
    }
  }
  
}


#First, income levels at current population
Equilibrium_objects["Avg_income"] <- getAvgIncome(Master_data = Equilibrium_objects)


#Creating measures of exogenous amenities (Required for some functions) + City populations in case we allow productivity to respond to them
Equilibrium_objects["City_Population"] <- rep(0, nrow(Equilibrium_objects))

for (skill in skillVector) {
  name_of_skill <- skillName[which(skill == skillVector)]
  Equilibrium_objects[paste0(name_of_skill, "Population")] <- rep(0, nrow(Equilibrium_objects))
  for (incomeType in 1:7) {
    
    #Calculating fundamental amenities
    Equilibrium_objects[paste0("exogenous_Amenity_", name_of_skill, incomeType)] <- Equilibrium_objects[[paste0("Amenity_", name_of_skill, incomeType)]]/(Equilibrium_objects$Avg_income^(Omega[incomeType]))
    
    
    Equilibrium_objects[paste0(name_of_skill, "Population")] <- Equilibrium_objects[paste0(name_of_skill, "Population")] + Equilibrium_objects[[paste0("City_Population_type_", name_of_skill, incomeType)]]
    Equilibrium_objects["City_Population"] <- Equilibrium_objects[["City_Population"]] + Equilibrium_objects[[paste0("City_Population_type_", name_of_skill, incomeType)]]
    
  }
}

#Calculating mean amenities for adjusting equilibria with other fundamentals.
if (EquilibriumType$bySkill == FALSE) {
  Equilibrium_objects["avg_exogenous_amenity"] <- pmean(Equilibrium_objects[[paste0("exogenous_Amenity_", "", "1")]],
                                                        Equilibrium_objects[[paste0("exogenous_Amenity_", "", "2")]],
                                                        Equilibrium_objects[[paste0("exogenous_Amenity_", "", "3")]], 
                                                        Equilibrium_objects[[paste0("exogenous_Amenity_", "", "4")]],
                                                        Equilibrium_objects[[paste0("exogenous_Amenity_", "", "5")]],
                                                        Equilibrium_objects[[paste0("exogenous_Amenity_", "", "6")]],
                                                        Equilibrium_objects[[paste0("exogenous_Amenity_", "", "7")]], na.rm = TRUE)
  
}

if (EquilibriumType$bySkill == TRUE) {
  Equilibrium_objects["avg_exogenous_amenity"] <- pmean(Equilibrium_objects[[paste0("exogenous_Amenity_", "College_", "1")]],
                                                        Equilibrium_objects[[paste0("exogenous_Amenity_", "College_", "2")]],
                                                        Equilibrium_objects[[paste0("exogenous_Amenity_", "College_", "3")]], 
                                                        Equilibrium_objects[[paste0("exogenous_Amenity_", "College_", "4")]],
                                                        Equilibrium_objects[[paste0("exogenous_Amenity_", "College_", "5")]],
                                                        Equilibrium_objects[[paste0("exogenous_Amenity_", "College_", "6")]],
                                                        Equilibrium_objects[[paste0("exogenous_Amenity_", "College_", "7")]], 
                                                        Equilibrium_objects[[paste0("exogenous_Amenity_", "NoCollege_", "1")]],
                                                        Equilibrium_objects[[paste0("exogenous_Amenity_", "NoCollege_", "2")]],
                                                        Equilibrium_objects[[paste0("exogenous_Amenity_", "NoCollege_", "3")]], 
                                                        Equilibrium_objects[[paste0("exogenous_Amenity_", "NoCollege_", "4")]],
                                                        Equilibrium_objects[[paste0("exogenous_Amenity_", "NoCollege_", "5")]],
                                                        Equilibrium_objects[[paste0("exogenous_Amenity_", "NoCollege_", "6")]],
                                                        Equilibrium_objects[[paste0("exogenous_Amenity_", "NoCollege_", "7")]], na.rm = TRUE)
}

#If no fundamentals, change exogenous amenity to equal average across all types--muting all income sorting. 
if (EquilibriumType["NoFundamentals"] == TRUE) { 
  #Setting exogenous amenities to mean (muting all fundamental differences across space)
  #Inputting this average
  for (skill in skillVector) {
    name_of_skill <- skillName[which(skill == skillVector)]
    
    for (incomeType in 1:7) {
      
      Equilibrium_objects[paste0("exogenous_Amenity_", name_of_skill, incomeType)] <- Equilibrium_objects[["avg_exogenous_amenity"]]
      
      
    }
  }
  
}#End adjustment for no fundamentals


#Finally,
#Solving for fundamental productivity if proportion of productivity is endogenous...
if (EquilibriumType$bySkill == FALSE) {
  
  Equilibrium_objects[paste0("fundamental_prod")] <- Equilibrium_objects[["PooledWage"]]/(Equilibrium_objects[["City_Population"]]^(Agglomeration_elast)) #Simple agglomeration without skill differences
  
}else{ #If bySkill == TRUE, different sln to fundamental productivity using 
  
  skillIndex <- 0
  for (skill in skillVector) {
    name_of_skill <- skillName[which(skill == skillVector)]
    skillIndex <- skillIndex + 1
    #creating fundamental productivity after accounting for agglomeration economies in bySkill version of the model
    Equilibrium_objects[paste0("fundamental_prod_", skill)] <- Equilibrium_objects[paste0("Productivity_", skill)]/(Equilibrium_objects[["College_Population"]]^(bySkill_agg_matrix[2, skillIndex])* 
                                                                                                                      Equilibrium_objects[["NoCollege_Population"]]^(bySkill_agg_matrix[1, skillIndex])) #divide out pairwise wage elasticities from Diamond (2016)
    
  }
  
}

#_____________________________________________________________________________________________
#START SOLVING FOR EQUILIBRIA

iter <- 1

#Error tolerances
if (pref == "SG") {
  eq_error_tol_Spendshares <- 0.01  #tolerance for error
  error_Spendshare <- eq_error_tol_Spendshares + 1 #initializing running error
  
  eq_error_tol_Mobility <- 5 #SG equilibria are a bit harder to solve, so increase equilibrium error to 2 households...
  error_Mobility <- eq_error_tol_Mobility + 1 #initial error to start loop
  
}else{
  error_Spendshare <- 0 #No adjustment of spendshares on CD preferences.
  eq_error_tol_Spendshares <- 0.01
  
  eq_error_tol_Mobility <- 1
  error_Mobility <- eq_error_tol_Mobility + 1 #initial error to start loop
  
  #bySkill == TRUE and Endogenous amenities a bit hard to solve for CD preferences
  if (EquilibriumType$bySkill == TRUE & EquilibriumType$EndogenousAmenities == TRUE) {
    eq_error_tol_Mobility <- 5
    error_Mobility <- eq_error_tol_Mobility + 1
  }
  
}

#Adjustment Speeds
if (pref == "SG") {
  adjustment_speed_Mobility <- 0.1 #Speed of adjustment for populations (keep this smaller if "SG")
  adjustment_speed_SpendShares <- 0.05 #Speed of adjustment for spending shares (if applicable). 
}else{
  adjustment_speed_Mobility <- 0.1 #Faster adjustment speed for pref == "CD"
  
  #bySkill == TRUE and Endogenous amenities a bit hard to solve for CD preferences
  if (EquilibriumType$bySkill == TRUE & EquilibriumType$EndogenousAmenities == TRUE) {
    adjustment_speed_Mobility <- 0.05
  }
}


#initializing set of old equilibrium objects from last iteration
old_Equilibrium_objects <- Equilibrium_objects

#START WHILE LOOP HERE
while (error_Mobility > eq_error_tol_Mobility | error_Spendshare > eq_error_tol_Spendshares) {
  
  
  #PART 0.5: Updating productivity if needed
  if (EquilibriumType$EndogenousProductivity == TRUE) {
    
    #if standard agglomeration elasticity   
    Equilibrium_objects["pop_running_total"] <- rep(0, nrow(Equilibrium_objects))
    
    for (skill in skillVector) {
      
      #Total city populations by skill
      Equilibrium_objects[paste0("pop_running_total_", skill)] <- rep(0, nrow(Equilibrium_objects)) #running total of population in neighborhood
      
      name_of_skill <- skillName[which(skill == skillVector)]
      for (incomeType in 1:7) {
        Equilibrium_objects[[paste0("pop_running_total_", skill)]] <- Equilibrium_objects[[paste0("pop_running_total_", skill)]] + Equilibrium_objects[[paste0("Population_type_", name_of_skill, incomeType)]]
      }
      
      Equilibrium_objects["pop_running_total"] <- Equilibrium_objects["pop_running_total"] + Equilibrium_objects[[paste0("pop_running_total_", skill)]]
      
    }
    
    #Creating city population measures
    for (skill in skillVector) {
      name_of_skill <- skillName[which(skill == skillVector)]
      tpop_sym <- paste0(name_of_skill, "Population")
      pop_sym <- paste0("pop_running_total_", skill)
      
      Equilibrium_objects <- Equilibrium_objects %>% group_by(CBSA) %>% mutate(!!sym(tpop_sym) := sum(!!sym(pop_sym))) #total populations by skill
      Equilibrium_objects[[paste0("pop_running_total_", skill)]] <- NULL
    }
    
    #Creating aggregate city population measures
    Equilibrium_objects <- Equilibrium_objects %>% group_by(CBSA) %>% mutate(City_Population = sum(pop_running_total))
    Equilibrium_objects["pop_running_total"] <- NULL
    
    #Updating productivity == Wage in pooled version of model
    if (EquilibriumType$bySkill == FALSE) {
      
      Equilibrium_objects$PooledWage <- Equilibrium_objects$fundamental_prod*(Equilibrium_objects$City_Population^(Agglomeration_elast))
      
    }else{ #Else, if bySkill == TRUE, use pairwise agglomeration elasticities from Diamond (2016)
      
      skillIndex <- 0
      for (skill in skillVector) {
        skillIndex <- skillIndex + 1
        Equilibrium_objects[paste0("Productivity_", skill)] <- Equilibrium_objects[[paste0("fundamental_prod_", skill)]]*
          ( Equilibrium_objects[["College_Population"]]^(bySkill_agg_matrix[2, skillIndex])* 
              Equilibrium_objects[["NoCollege_Population"]]^(bySkill_agg_matrix[1, skillIndex]) )
      }
      
    } #End check over BySkill
    
  } #end check over endogenous productivity
  
  
  
  #PART 0.75: Updating wages if equilibrium is done by skill == CES
  if (EquilibriumType$bySkill == TRUE) {
    
    UpdateWagesBySkill <- getSkillWages(Master_data = Equilibrium_objects) #Storing new wages 
    
    for (skill in skillVector) {
      Equilibrium_objects[paste0(skill, "Wage")] <- UpdateWagesBySkill[[skill]]
    }
    rm(UpdateWagesBySkill)
    
  }
  
  #PART 1: SOLVING FOR PRICES
  Equilibrium_objects["housingPrice"] <- getHousingPrices(Master_data = Equilibrium_objects) #getting housing prices holding expenditure shares fixed at previous iteration
  
  #PART 2: SOLVING FOR AVERAGE INCOME
  Equilibrium_objects["Avg_income"] <- getAvgIncome(Master_data = Equilibrium_objects)
  
  
  #PART 3: SOLVING FOR AMENITIES AT CURRENT POPULATION DISTRIBUTION
  if (EquilibriumType$EndogenousAmenities == TRUE) { #Updating amenities if they are endogenous
    
    for (skill in skillVector) {
      name_of_skill <- skillName[which(skill == skillVector)]
      for (i in 1:7) {    
        Equilibrium_objects[paste0("Amenity_", name_of_skill, i)] <- getLocationAmenity(Master_data = Equilibrium_objects,
                                                                                        skill = skill,
                                                                                        incomeType = i)
      }
    }
    
  }#End update of endogenous amenities
  
  #PART 4: SOLVING FOR CONSUMPTION VALUES
  if (check_init_eq == 0) { # if not checking initial equilibrium rationalizes population distributions
    for (skill in skillVector) {
      name_of_skill <- skillName[which(skill == skillVector)]
      for (i in 1:7) { #Do not update consumption values if we are checking if we calculated the population distribution correctly.
        Equilibrium_objects[paste0("consumption_Val_", name_of_skill, i)] <- getConsumptionValues(Master_data = Equilibrium_objects, 
                                                                                                  skill = skill,
                                                                                                  incomeType = i,
                                                                                                  demandParameters = demandParameters_to_pass)
        
      }                                                                           
    }
  }
  
  #PART 5: SOLVING FOR TOTAL VALUE OF NEIGHBORHOOD RAISED TO MIGRATION ELASTICITY TO CALCULATE NEW DESIRED POPULATIONS 
  for (skill in skillVector) {
    name_of_skill <- skillName[which(skill == skillVector)]
    for (i in 1:7) {
      Equilibrium_objects[paste0("Val_", name_of_skill, i)] <- ((exp(Equilibrium_objects[[paste0("consumption_Val_", name_of_skill, i)]])*(Equilibrium_objects[[paste0("Amenity_", name_of_skill, i)]]))^(rho))
      
    }
  }
  
  if (EquilibriumType$WithinCityMobility == TRUE | EquilibriumType$Full == TRUE) { #only required for within city and full mobility models
    
    #PART 6: SOLVING FOR DESIRED SHARE OF CITY C IN NEIGHBORHOOD N.
    for (skill in skillVector) {
      name_of_skill <- skillName[which(skill == skillVector)]
      
      for (i in 1:7) {
        
        value_n_sym <- paste0("Val_", name_of_skill, i)
        value_n_city_sym <- paste0("Val_city_", name_of_skill, i) 
        
        Equilibrium_objects <- Equilibrium_objects %>% group_by(CBSA) %>% mutate(!!sym(value_n_city_sym) := (sum(!!sym(value_n_sym)))^(1/rho))
        
        Equilibrium_objects[paste0("Neighborhood_frac_type_", name_of_skill, i)] <- (Equilibrium_objects[[paste0("Val_", name_of_skill, i)]]/(Equilibrium_objects[[paste0("Val_city_", name_of_skill, i)]]^(rho))) #Note: this is city c fraction in n of type i
        
        
        if (EquilibriumType$WithinCityMobility == TRUE) { #population identified if only considering within-city mobility
          #Population updating for within-city version of model
          Equilibrium_objects[paste0("Population_type_", name_of_skill, i)] <- Equilibrium_objects[[paste0("Neighborhood_frac_type_", name_of_skill, i)]]*
            Equilibrium_objects[[paste0("City_Population_type_", name_of_skill, i)]]  
        }                                                       
        
        
        #PART 7: SOLVING FOR DESIRED NATIONAL SHARE IN CITY C AND TOTAL POPULATIONS
        if (EquilibriumType$Full == TRUE) {
          
          #NATIONAL SHARE OF HOUSEHOLDS IN CITY C
          Equilibrium_objects[paste0("City_frac_type_", name_of_skill, i)] <- (Equilibrium_objects[paste0("Val_city_", name_of_skill, i)]^(theta))/(sum((Equilibrium_objects[paste0("Val_city_", name_of_skill, i)]^(theta))*(Equilibrium_objects$inverse_city_weights))) #note: val is withincitywelfare^rho, so correcting for that
          
          #Total Population updating
          Equilibrium_objects[paste0("Population_type_", name_of_skill, i)] <- Equilibrium_objects[[paste0("City_frac_type_", name_of_skill, i)]]*
            Equilibrium_objects[[paste0("Neighborhood_frac_type_", name_of_skill, i)]]*Equilibrium_objects[[paste0("Total_Population_type_", name_of_skill, i)]]
          
        }#End check for FULL MOBILITY EQUILIBRIUM
        
      } #End loop over income types
    }#End loop over skill types
    
  } #END CHECK FOR ANY MOBILITY EQUILIBRIUM
  
  
  #CALCULATING ERROR FROM SPATIAL EQUILIBRIUM_________________________________________________
  
  error_calculate <- matrix(NA, length(skillVector), 7) #checking error in deltaPopulation by skills
  skill_index <- 0
  for (skill in skillVector) {
    skill_index <- skill_index + 1
    name_of_skill <- skillName[which(skill == skillVector)]
    for (i in 1:7) {
      error_calculate[skill_index, i] <- max(abs(Equilibrium_objects[[paste0("Population_type_", name_of_skill, i)]] - old_Equilibrium_objects[[paste0("Population_type_", name_of_skill, i)]]))
    }
  }
  
  error_Mobility <- max(error_calculate) #calculating total error for all
  
  #Calculating population adjustments using desired location choices at current restricted equilibria, slowing down movement by adjustment_speed object
  for (skill in skillVector) {
    name_of_skill <- skillName[which(skill == skillVector)]
    for (i in 1:7) {
      Equilibrium_objects[[paste0("Population_type_", name_of_skill, i)]] <- old_Equilibrium_objects[[paste0("Population_type_", name_of_skill, i)]] + 
        adjustment_speed_Mobility*(Equilibrium_objects[[paste0("Population_type_", name_of_skill,  i)]] - old_Equilibrium_objects[[paste0("Population_type_", name_of_skill, i)]])
    }
  }
  
  #Updating spending shares at new prices if spending shares are endogenous 
  if (pref == "SG") { 
    
    for (skill in skillVector) {
      name_of_skill <- skillName[which(skill == skillVector)]
      for (i in 1:7) {
        Equilibrium_objects[[paste0("hSpendShare_",  name_of_skill, i)]] <- ((1-beta_StGeary)*pmin((Equilibrium_objects$housingPrice*min_hReq)/(Equilibrium_objects[[paste0(skill, "Wage")]]*Equilibrium_objects[[paste0("ability_grp", i)]]), rep(1, nrow(Equilibrium_objects))) + beta_StGeary)
      }    
    }
    
    #Calculating errors...
    error_calculate <- matrix(NA, length(skillVector), 7)  #checking error in deltaPopulation by skills 
    skill_index <- 0
    for (skill in skillVector) {
      skill_index <- skill_index + 1
      name_of_skill <- skillName[which(skill == skillVector)]
      for (i in 1:7) {
        error_calculate[skill_index, i] <- max(abs(Equilibrium_objects[[paste0("hSpendShare_", name_of_skill, i)]] -  old_Equilibrium_objects[[paste0("hSpendShare_", name_of_skill, i)]]))
      }
    }
    
    error_Spendshare <- max(error_calculate) #calculating total error for all...  
    
    #Adjusting spending shares from old prices given adjustment speed
    for (skill in skillVector) {
      name_of_skill <- skillName[which(skill == skillVector)]
      for (i in 1:7) {
        Equilibrium_objects[[paste0("hSpendShare_", name_of_skill, i)]] <- old_Equilibrium_objects[[paste0("hSpendShare_", name_of_skill, i)]] + 
          adjustment_speed_SpendShares*(Equilibrium_objects[[paste0("hSpendShare_", name_of_skill, i)]] - old_Equilibrium_objects[[paste0("hSpendShare_", name_of_skill, i)]])
      }
    }
    
  }#End pref == SG check
  
  #printing total error
  print(paste0("At iteration ", iter, ", the error for labour mobility is ", error_Mobility,
               ", the error for spending shares is ", error_Spendshare,
               ". The time is ", Sys.time(), "."))
  
  #Updating old equilibrium objects with new
  old_Equilibrium_objects <- Equilibrium_objects
  
  #Iteration count
  iter <- iter + 1
  #______________________________________________________________________________________________
  
}#END WHILE LOOP HERE

#Saving file
if (check_init_eq == 0 & EquilibriumType["NoFundamentals"] == FALSE) {
  save(Equilibrium_objects, file =paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", 
                                         "Eq_Objects_", CurrentVersion, 
                                         "_EndoAmen_", EquilibriumType$EndogenousAmenities, 
                                         "_EndoProd_", EquilibriumType$EndogenousProductivity,
                                         "_bySkill_", EquilibriumType$bySkill,
                                         "_pref_", pref, ".RData"))
}

if (EquilibriumType["NoFundamentals"] == TRUE) {
  save(Equilibrium_objects, file =paste0("DataV2/Counterfactuals/Counterfactual_Output/DiffFundamentals/", 
                                         "FullDereg_NoFundamentals", CurrentVersion, 
                                         "_EndoAmen_", EquilibriumType$EndogenousAmenities, 
                                         "_EndoProd_", EquilibriumType$EndogenousProductivity,
                                         "_bySkill_", EquilibriumType$bySkill,
                                         "_pref_", pref, ".RData"))
}



#Removing all unneeded objects for restart..
rm(list = ls()[!ls() %in% c("EquilibriumType", 
                            "solver",
                            "check_init_eq",
                            "IncomeStringency_ctfl",
                            "cities", "cityNames")])