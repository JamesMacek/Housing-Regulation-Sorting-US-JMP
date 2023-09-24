#ANALYSIS FUNCTIONS


#This function calculates welfare (in utils) at current consumption values and amenities
getWelfare <- function(Master_data, bySkill, incomeType) {
  
  if (bySkill == FALSE) {
    
      Master_data[paste0("Val_temp", incomeType)] <- (Master_data[[paste0("consumption_Val_", incomeType)]]^(rho))*(Master_data[[paste0("Amenity_", incomeType)]]) #Value of a location raised to migration elasticity 
      
      value_n_sym <- paste0("Val_temp", incomeType)
      value_n_city_sym <- paste0("City_Welfare_", incomeType) 
      
      #Creating city welfare index 
      Master_data <- Master_data %>% group_by(CBSA) %>% mutate(!!sym(value_n_city_sym) := (sum(!!sym(value_n_sym)))^(1/rho))
      
      #Creating global welfare index
      Welfare_index <- (sum(((Master_data[[paste0("City_Welfare_", incomeType)]])^(theta))*(Master_data$inverse_city_weights)))^(1/theta)
      
      return(Welfare_index)
    }
    
  }


#alternative welfare measure that is comparable across restricted mobility equilibria
getMobilityWelfare <- function(Master_data, bySkill, incomeType) {
  
  if (bySkill == FALSE) {
    
    #Adjusting amenity for land mass
    Welfare_index <- sum((Master_data[[paste0("consumption_Val_", incomeType)]]^(1/consumptionAdjustment[[paste0("consumption_Adjustment", i)]][1]))*((Master_data[[paste0("Amenity_", incomeType)]]/Master_data$final_land_for_res)^(1/rho))*Master_data[[paste0("Population_type_", incomeType)]])/(sum(Master_data[[paste0("Population_type_", incomeType)]]))
    #weighted average of consumption + amenity values in a location (note: we do things by consumption adjustment factor to get income equivalents)
  }
  
}


#Average amenity
getAmenityExposure <- function(Master_data, bySkill, incomeType) {
 
  
  #Note: we are adjusting amenities downward by land mass (i.e. amenity per unit of land to net of love of variety effects)
  #Note: we are also taking 1/rho because amenities are raised to the power of the within-city migration elasticity. 
  if (bySkill == FALSE) {
    exposure <- (sum(((Master_data[paste0("Amenity_", incomeType)]/Master_data$final_land_for_res)^(1/rho))*Master_data[[paste0("Population_type_", incomeType)]]))/sum(Master_data[[paste0("Population_type_", incomeType)]])
    return(exposure)
  }
}


#Aggregate labour productivity
getAggregateProductivity <- function(Master_data, bySkill) {
  
  if (bySkill == FALSE) {
    
    total_output <- 0
    total_population <- 0
    
    for (incomeType in 1:7) {
      
      total_output <- total_output + sum(Master_data[[paste0("Population_type_", incomeType)]]*
                                      Master_data[[paste0("ability_grp", incomeType)]]*
                                      Master_data$PooledWage)
      
      total_population <- total_population +  sum(Master_data[[paste0("Population_type_", incomeType)]]) 
      
      
    }
    
    return(total_output/total_population)
    
  }
  
  if (bySkill == TRUE) {
    total_output <- 0
    total_population <- 0
    
    for (skill in c("College", "NoCollege")) {
      for (incomeType in 1:7) {
      
        total_output <- total_output + sum(Master_data[[paste0("Population_type_", skill, "_", incomeType)]]*
                                           Master_data[[paste0("ability_grp", incomeType)]]*
                                           Master_data[[paste0(skill, "Wage")]])
      
        total_population <- total_population +  sum(Master_data[[paste0("Population_type_", skill, "_", incomeType)]]) 
      
      
      }
    }
    
    return(total_output/total_population)
  }
  
}


#Average income type
getCityAverageType <- function(Master_data, bySkill) {
  
  if (bySkill == FALSE){
    

    
    #Creating average type in each city
    Master_data <- Master_data %>% group_by(CBSA) %>% mutate(Avg_type_city = (sum(Population_type_1*ability_grp1 + Population_type_2*ability_grp2 + 
                                                                              Population_type_3*ability_grp3 + Population_type_4*ability_grp4 + 
                                                                              Population_type_5*ability_grp5 + Population_type_6*ability_grp6 + 
                                                                              Population_type_7*ability_grp7))/(sum(Population_type_1 + Population_type_2 + 
                                                                                                                Population_type_3 + Population_type_4 + 
                                                                                                                Population_type_5 + Population_type_6 + 
                                                                                                                Population_type_7)) )
      
    return(Master_data$Avg_type_city)
    
  }
  
  if (bySkill == TRUE) {
    
    for (i in 1:7) {
      Master_data[paste0("Population_type_", i)] <- Master_data[[paste0("Population_type_College_", i)]] + Master_data[[paste0("Population_type_NoCollege_", i)]]
    }
    
    #Creating average type in each city
    Master_data <- Master_data %>% group_by(CBSA) %>% mutate(Avg_type_city = (sum(Population_type_1*ability_grp1 + Population_type_2*ability_grp2 + 
                                                                                    Population_type_3*ability_grp3 + Population_type_4*ability_grp4 + 
                                                                                    Population_type_5*ability_grp5 + Population_type_6*ability_grp6 + 
                                                                                    Population_type_7*ability_grp7))/(sum(Population_type_1 + Population_type_2 + 
                                                                                                                            Population_type_3 + Population_type_4 + 
                                                                                                                            Population_type_5 + Population_type_6 + 
                                                                                                                            Population_type_7)) )
    return(Master_data$Avg_type_city)
  }

}


getCityTotalPop <- function(Master_data, bySkill) {
  
  if (bySkill == FALSE){
    
    
    
    #Creating average type in each city
    Master_data <- Master_data %>% group_by(CBSA) %>% mutate(tot_pop_city = sum(Population_type_1 + Population_type_2 + 
                                                                                 Population_type_3 + Population_type_4 + 
                                                                                 Population_type_5 + Population_type_6 + 
                                                                                 Population_type_7))
    
    return(Master_data$tot_pop_city)
    
  }
  
  if (bySkill == TRUE){
    
    for (i in 1:7) {
      Master_data[paste0("Population_type_", i)] <- Master_data[[paste0("Population_type_College_", i)]] + Master_data[[paste0("Population_type_NoCollege_", i)]]
    }
    
    #Creating average type in each city
    Master_data <- Master_data %>% group_by(CBSA) %>% mutate(tot_pop_city = sum(Population_type_1 + Population_type_2 + 
                                                                                  Population_type_3 + Population_type_4 + 
                                                                                  Population_type_5 + Population_type_6 + 
                                                                                  Population_type_7))
    
    return(Master_data$tot_pop_city)
    
  }
  
  
}

