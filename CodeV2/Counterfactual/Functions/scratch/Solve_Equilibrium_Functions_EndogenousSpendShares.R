

getHousingPrices_Full <- function(Master_data, bySkill) { #Master_data is the main dataframe to pass with proper variable names,
                                                     #bySkill asks if you are using a skill-differentiated version of the model.  
                                                     #This function solves housing markets ALLOWING THE SPENDING SHARES TO BE ENDOGENOUS. 
  
  #This function takes current equilibrium populations (by skill, if specified) and returns vector of housing prices that clear housing markets.
  
  if (bySkill == FALSE) { 
    
    #Start ExcessDemand function
    
    ExcessDemand <- function(price) {
      
      Demand <- 0
      
      for (incomeType in 1:7) {
      
        Demand <- Demand + ((1-beta_StGeary)*min((price*min_hReq)/(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]), 1) + beta_StGeary)* #Spending Share
                           Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]* #Income
                           Master_data[[paste0("Population_type_", incomeType)]] #Population
      }
      
      Supply <- Master_data$final_land_for_res*Master_data$lambda*(price^(Master_data$HS_Elasticity_imputed + 1))
      
      #Return ExcessDemand
      return(Demand - Supply)
      
      
    }
    
    
    #Using uniroot to solve for prices row-by-row. We know a few things about bounds on prices. 
    
    #Lower bound is the prices that would prevail if min_hReq == 0, calculating this price
    
    low_b_Spending <- 0
    
    for (incomeType in 1:7) {
      
      low_b_Spending <- low_b_Spending + beta_StGeary*(Master_data[[paste0("ability_grp", incomeType)]]*Master_data$PooledWage)*Master_data[[paste0("Population_type_", incomeType)]]
      
    }
    
    low_b_price <- (low_b_Spending/((Master_data$final_land_for_res*Master_data$lambda)))^(1/(Master_data$HS_Elasticity_imputed + 1))
    
    
    #Upper bound is what prices would have to be to get everyone to spend all their income
    up_b_Spending <- 0
    
    for (incomeType in 1:7) {
      
      up_b_Spending <- up_b_Spending +  Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]* #Income
                                        Master_data[[paste0("Population_type_", incomeType)]] #Population
      
    }
    
    up_b_price <- (up_b_Spending/((Master_data$final_land_for_res*Master_data$lambda)))^(1/(Master_data$HS_Elasticity_imputed + 1)) #add 1/2...
    
    sln <- uniroot(ExcessDemand, interval = c(0.99*low_b_price, 1.01*up_b_price),
                   extendInt = c("no"))
    
    return(sln$root)
  
    
  } #End bySkill check  
  
  
  if (bySkill == TRUE) {
    
      return("Not ready yet!")
      
  }
  
} 

getHousingPrices <- function(Master_data, bySkill) { #This function solves housing markets KEEPING SPENDING SHARES FIXED (EXOGENOUS) 
  
  if (bySkill == FALSE) {
    
    
    h_spending <- 0
    
    for (incomeType in 1:7) {
      
      h_spending <- h_spending    +   Master_data[[paste0("hSpendShare_", incomeType)]]* #Spending Share (not updated every iteration)
                                      Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]* #Income
                                      Master_data[[paste0("Population_type_", incomeType)]] #Population
    }
    
    
    price <- (h_spending/(Master_data$final_land_for_res*Master_data$lambda))^(1/(Master_data$HS_Elasticity_imputed + 1))
    
    return(price)
  }
  
  if (bySkill == TRUE) {
    
    return("Not ready yet!")
    
  }
  
}


#This function solves for consumption values 
getConsumptionValues <- function(Master_data, bySkill, incomeType) {
  
  if (bySkill == FALSE) {
    
      #parallel maxima of 0, etc 
      consumptionValue <- pmax((Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] - Master_data$housingPrice*min_hReq)/(Master_data$housingPrice^(beta_StGeary)), rep(0, nrow(Master_data)))* #Take vanilla consumption index
                                                                                                         (consumptionAdjustment[[paste0("consumption_Adjustment", incomeType)]][1]) #adjusting by consumption adjustment factor calculated in calibrate_unobserved_amenities.R
      return(consumptionValue)
  }
  
  if (bySkill == TRUE) {
    return("Not ready yet!")
    
  }
  
  
} #End retrieval of housing prices


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



