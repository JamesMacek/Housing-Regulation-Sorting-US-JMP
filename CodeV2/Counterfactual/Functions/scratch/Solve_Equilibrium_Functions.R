
#This function solves for housing market prices to clear markets given master data frame

getHousingPrices <- function(Master_data, bySkill) { #Master_data is the main dataframe to pass with proper variable names,
                                                     #bySkill asks if you are using a skill-differentiated version of the model.  
                                                     #This function solves housing markets under no regulation
  
  #This function takes current equilibrium populations (by skill, if specified) and returns vector of housing prices that clear housing markets.
  
  if (bySkill == FALSE) {
    
    #Calculating total spending on housing
    h_spending <- rep(0, nrow(Master_data))
    for (incomeType in 1:7) {
      h_spending <- h_spending + Master_data[[paste0("hSpendShare_", incomeType)]]*Master_data[[paste0("ability_grp", incomeType)]]*
                                      Master_data[[paste0("Population_type_", incomeType)]]*
                                      Master_data$PooledWage
      
    }
    
    #Calculating prices from this
    price <- (h_spending/(Master_data$final_land_for_res*Master_data$lambda))^(1/(Master_data$HS_Elasticity_imputed + 1))
    
    return(price) #save as housingPrice in Equilibrium_objects dataframe.
  }
  
  
  if (bySkill == TRUE) {
    #Calculating total spending on housing
    h_spending <- rep(0, nrow(Master_data))
    
    for (skill in c("College", "NoCollege")) {
      for (incomeType in 1:7) {
        h_spending <- h_spending + Master_data[[paste0("hSpendShare_", incomeType)]]*Master_data[[paste0("ability_grp", incomeType)]]*
                                   Master_data[[paste0("Population_type_", skill, "_", incomeType)]]*
                                   Master_data[[paste0(skill, "Wage")]]
        
      
      }
    }
    
    #Calculating prices from this
    price <- (h_spending/(Master_data$final_land_for_res*Master_data$lambda_bySkill))^(1/(Master_data$HS_Elasticity_imputed + 1))
    
    return(price) #save as housingPrice in Equilibrium_objects dataframe.
  }
  
  
  
}


#This function solves for consumption values 
getConsumptionValues <- function(Master_data, bySkill, incomeType) {
  
  if (bySkill == FALSE) {
    
      betaFactor <- ((1-Master_data[[paste0("hSpendShare_", incomeType)]])^(1-Master_data[[paste0("hSpendShare_", incomeType)]]))*(Master_data[[paste0("hSpendShare_", incomeType)]]^Master_data[[paste0("hSpendShare_", incomeType)]]) #dont forget to multiply standard Cobb-Douglas index by this...
      consumptionValue <- (((betaFactor*Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]])/(Master_data$housingPrice^(Master_data[[paste0("hSpendShare_", incomeType)]]))))*
                                                                                                      (consumptionAdjustment[[paste0("consumption_Adjustment", incomeType)]][1]) #adjusting by consumption adjustment factor calculated in calibrate_amenities.R
      return(consumptionValue)
  }
  
  if (bySkill == TRUE) {
    
    betaFactor <- ((1-Master_data[[paste0("hSpendShare_", incomeType)]])^(1-Master_data[[paste0("hSpendShare_", incomeType)]]))*(Master_data[[paste0("hSpendShare_", incomeType)]]^Master_data[[paste0("hSpendShare_", incomeType)]]) #dont forget to multiply standard Cobb-Douglas index by this...
    
    consumptionValue <- list()
    
    for (skill in c("College", "NoCollege")) {
      
      if (skill == "College") {
        index <- 1
      }else{
        index <- 2
      }
      
      consumptionValue[[skill]] <- (((betaFactor*Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]])/(Master_data$housingPrice^(Master_data[[paste0("hSpendShare_", incomeType)]]))))*
                                                                                        (consumptionAdjustment[[paste0("consumption_Adjustment", incomeType)]][index]) #adjusting by consumption adjustment
      
    }
    
    return(consumptionValue)
    
  }
  
  
}


#This function solves for average income in all block groups. Put in variable called "Avg_income" for use with other functions
getAvgIncome <- function(Master_data, bySkill) {
  
  if (bySkill == FALSE){
    total_income <- 0
    total_population <- 0
    
    for (incomeType in 1:7) {
      total_income <- total_income + Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]*Master_data[[paste0("Population_type_", incomeType)]]
      total_population <- total_population + Master_data[[paste0("Population_type_", incomeType)]]
    }
    
    return(total_income/total_population)
    
  }
  
  if (bySkill == TRUE) {
    total_income <- 0
    total_population <- 0
    
    for (skill in c("College", "NoCollege")) {
      for (incomeType in 1:7) {
        total_income <- total_income + Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]]*Master_data[[paste0("Population_type_", skill, "_", incomeType)]]
        total_population <- total_population + Master_data[[paste0("Population_type_", skill, "_", incomeType)]]
      }
    }
    
    return(total_income/total_population)
    
  }
  
  
}
 
#This function solves for location amenities 

getLocationAmenity <- function(Master_data, bySkill, incomeType) {
  
  if (bySkill == FALSE) {
    
    Amenity <- Master_data[[paste0("exogenous_Amenity_", incomeType)]]*(Master_data$Avg_income^(Omega[incomeType]))
    return(Amenity)
  }
  
  if (bySkill == TRUE) {
    
    Amenity <- list()
    
    for (skill in c("College", "NoCollege")) {
      
      Amenity[[skill]] <- Master_data[[paste0("exogenous_Amenity_", skill, "_", incomeType)]]*(Master_data$Avg_income^(Omega[incomeType]))
       
    }
    
    
    return(Amenity)
  }
  
}



