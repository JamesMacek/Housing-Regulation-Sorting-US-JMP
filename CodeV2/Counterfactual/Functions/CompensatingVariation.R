#COMPENSATING VARIATION

GetWelfareCompVar <- function(Master_data, skill, incomeType, EqVar, demandParameters,
                              initCalc) { #Calculates welfare at new deregulated equilibrium as function of EqVar (to solve). initCalc tells the function which dataset was passed. See below.
    
  if (initCalc == FALSE) {
    #Skill Name
    name_of_skill <- skillName[which(skill == skillVector)]
    
    #ConsumptionValue for this type as a function of EqVar
    consumptionValue <- pmax((Master_data[[paste0(skill, "Wage")]]*EqVar*Master_data[[paste0("ability_grp", incomeType)]] - Master_data$housingPrice*demandParameters[2])/(Master_data$housingPrice^(demandParameters[1])), rep(0, nrow(Master_data)))* #Take vanilla consumption index
                                                                                                                                                                          (consumption_AdjustmentFactor[[skill, paste0("consumption_Adjustment", incomeType)]]) #adjusting by consumption adjustment factor calculated in calibrate_unobserved_amenities.R
  
    Master_data[paste0("Val_temp", name_of_skill, incomeType)] <- (exp(consumptionValue)*
                                                                  (Master_data[[paste0("Amenity_", name_of_skill, incomeType)]]))^(rho) #Value of a location raised to migration elasticity 
    
    value_n_sym <- paste0("Val_temp", name_of_skill, incomeType)
    value_n_city_sym <- paste0("City_Welfare_", name_of_skill, incomeType) 
    
    #Creating city welfare index 
    Master_data <- Master_data %>% group_by(CBSA) %>% mutate(!!sym(value_n_city_sym) := (sum(!!sym(value_n_sym)))^(1/rho))
    
    #Creating global welfare index
    Welfare_index <- (sum(((Master_data[[paste0("City_Welfare_", name_of_skill, incomeType)]])^(theta))*(Master_data$inverse_city_weights)))^(1/theta)
    
    return(Welfare_index)
  }
  
  if (initCalc == TRUE) { #else, calculate original welfare 
    
    #Skill Name
    name_of_skill <- skillName[which(skill == skillVector)]
    
    Master_data[paste0("Val_temp", name_of_skill, incomeType)] <- (exp(Master_data[[paste0("consumption_Val_", name_of_skill, incomeType)]])*
                                                                     (Master_data[[paste0("Amenity_", name_of_skill, incomeType)]]))^(rho) #Value of a location raised to migration elasticity 
    
    value_n_sym <- paste0("Val_temp", name_of_skill, incomeType)
    value_n_city_sym <- paste0("City_Welfare_", name_of_skill, incomeType) 
    
    #Creating city welfare index 
    Master_data <- Master_data %>% group_by(CBSA) %>% mutate(!!sym(value_n_city_sym) := (sum(!!sym(value_n_sym)))^(1/rho))
    
    #Creating global welfare index
    Welfare_index <- (sum(((Master_data[[paste0("City_Welfare_", name_of_skill, incomeType)]])^(theta))*(Master_data$inverse_city_weights)))^(1/theta)
    
    return(Welfare_index)
    
  }
  
}

#solution function
getVariation <- function(Init, Ct, skill, incomeType, demandParameters) {
  
    CompVarSolve <- function(EqVar) {
      return(  GetWelfareCompVar(Master_data = Init, skill = skill, 
                                 incomeType = incomeType, 
                                 demandParameters = demandParameters, 
                                 initCalc = TRUE,
                                 EqVar = 1) - 
                 
               GetWelfareCompVar(Master_data = Ct, skill = skill, 
                                 incomeType = incomeType, 
                                 demandParameters = demandParameters, 
                                 initCalc = FALSE,
                                 EqVar = EqVar)   )
    }
    
    sln <- uniroot(f = CompVarSolve, interval = c(0.5, 2), extendInt = "yes")
    
    return(sln$root)
  
  
}

