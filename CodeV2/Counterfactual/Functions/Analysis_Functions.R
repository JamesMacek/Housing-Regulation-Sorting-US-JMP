
#Aggregate labour productivity
getAggregateProductivity <- function(Master_data) {
  
    total_output <- 0
    total_population <- 0
    
    for (skill in skillVector) {
    name_of_skill <- skillName[which(skill == skillVector)]
      for (incomeType in 1:7) {
      
        total_output <- total_output + sum(Master_data[[paste0("Population_type_", name_of_skill, incomeType)]]*
                                           Master_data[[paste0("ability_grp", incomeType)]]*
                                           Master_data[[paste0(skill, "Wage")]])
      
        total_population <- total_population + sum(Master_data[[paste0("Population_type_", name_of_skill, incomeType)]]) 
        
      
      }
    }
    return(total_output/total_population)
    
}


#Average income type
getCityAverageType <- function(Master_data) {
    
  Master_data["running_tot_pop"] <- rep(0, nrow(Master_data))
  Master_data["sum_type"] <- rep(0, nrow(Master_data))
  
  for (skill in skillVector) {
    name_of_skill <- skillName[which(skill == skillVector)]
    for (incomeType in 1:7) {
      
      Master_data["sum_type"] <- Master_data[[paste0("Population_type_", name_of_skill, incomeType)]]*Master_data[[paste0("ability_grp", incomeType)]] + Master_data[["sum_type"]]
      Master_data["running_tot_pop"] <- Master_data[[paste0("Population_type_", name_of_skill, incomeType)]] + Master_data[["running_tot_pop"]] 
      
    }
  }
  
  #Calculating average type
  Master_data <- Master_data %>% group_by(CBSA) %>% mutate(Avg_type_city = (sum(sum_type))/(sum(running_tot_pop)))
  return(Master_data$Avg_type_city)
    


}


getCityTotalPop <- function(Master_data) {
    
    #Neighborhood totals
    
    Master_data["running_tot_pop"] <- rep(0, nrow(Master_data))
  
    for (skill in skillVector) {
      name_of_skill <- skillName[which(skill == skillVector)]
      for (incomeType in 1:7) {
        
        Master_data["running_tot_pop"] <- Master_data[[paste0("Population_type_", name_of_skill, incomeType)]] + Master_data[["running_tot_pop"]]
        
      }
    }
    
  
    #Creating average type in each city
    Master_data <- Master_data %>% group_by(CBSA) %>% mutate(tot_pop_city = sum(running_tot_pop))
    
    return(Master_data$tot_pop_city)
    
}



#Welfare effects for shapely decomposition
getWelfare_ShapelyDecomp <- function(Consumption_data, Amenity_data, skill, incomeType) { #Income growth weighted by initial population
  
  #Skill Name
  name_of_skill <- skillName[which(skill == skillVector)]
  
  #measuring growth in incomes only weighted by fraction of these types of workers
  Consumption_data[paste0("Val_temp", name_of_skill, incomeType)] <- (exp(Consumption_data[[paste0("consumption_Val_", name_of_skill, incomeType)]])*
                                                                      (Amenity_data[[paste0("Amenity_", name_of_skill, incomeType)]]))^(rho)
                                                                    
                                                                #Amenity growth in each location x initial fraction of workers in x 
  
  value_n_sym <- paste0("Val_temp", name_of_skill, incomeType)
  value_n_city_sym <- paste0("City_Welfare_", name_of_skill, incomeType) 
  
  #Creating city welfare index 
  Consumption_data <- Consumption_data %>% group_by(CBSA) %>% mutate(!!sym(value_n_city_sym) := (sum(!!sym(value_n_sym), na.rm = TRUE))^(1/rho))
  
  #Creating global welfare index (aggregating up city welfare indices)
  Welfare_index <- (sum( ((Consumption_data[[paste0("City_Welfare_", name_of_skill, incomeType)]])^(theta))*
                           Consumption_data$inverse_city_weights ))^(1/theta) #Take log for differences in shapely decomposition
  
  return(Welfare_index)
  
  
}

#Get city output shares -- this is to do calculations for to get an idea for what productivity changes 
#would have been if we didn't have income sorting...
getCityOutputShares <- function(Master_data) {
  
  Master_data["output"] <- rep(0, nrow(Master_data))
  
  for (skill in skillVector) {
    name_of_skill <- skillName[which(skill == skillVector)]
    for (incomeType in 1:7) {
      
      Master_data["output"] <- Master_data[[paste0("Population_type_", name_of_skill, incomeType)]]*Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] + Master_data[["output"]]
    
    }
  }
  
  #total output 
  output <- sum(Master_data["output"])
  
  #Creating city output measures to weight household growth rates
  Master_data <- Master_data %>% group_by(CBSA) %>% mutate(output_city = sum(output)) %>% arrange(CBSA_NAME)
  
  return(unique(cbind(Master_data$CBSA_NAME, Master_data$output_city/output))) 
  
}

#Get populations of neighborhoods
getNeighborhoodPop <- function(Master_data) {
  
  neighborhood_pop <- rep(0, nrow(Master_data))
  
  for (skill in skillVector) {
    name_of_skill <- skillName[which(skill == skillVector)]
    for (incomeType in 1:7) {
      
      neighborhood_pop <- Master_data[[paste0("Population_type_", name_of_skill, incomeType)]] + neighborhood_pop
      
    }
  }
  
  return(neighborhood_pop)
  
  
}