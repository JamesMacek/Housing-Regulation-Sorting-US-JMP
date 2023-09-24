#EQUIVALENT VARIATION (an alternative measure of welfare)

GetWelfareEqVar <- function(Master_data, skill, incomeType, demandParameters, EqVar,
                            initCalc) {
  
  if (initCalc == TRUE) { #Master_data == Init_eq
    
    #Skill Name
    name_of_skill <- skillName[which(skill == skillVector)]
    
    #first, calculate consumption values in each neighborhood 
    #Note: this requires calculating consumption indices from an equilibrium with a mix of regulated and unregulated structures. 
     
    adjustment_factor_temp <- Master_data[[paste0("ability_grp", incomeType)]][1] #computational adjustment factor used in Calibrate_consumptionValues_SupplyShifters_all.R
    
    #Housing prices...
    price <- list() #price vector in each zone
    price[[1]] <- Master_data$price_regulated #prices at initial equilibrium
    price[[2]] <- Master_data$price_unregulated
  
    #Stringency...
    housing_stringency <- list()
    housing_stringency[[1]] <- Master_data$IncomeStringency_model_rents
    housing_stringency[[2]] <- rep(0, nrow(Master_data))
    
    #Initial regulated housing unit shares
    regulated_unit_shares <- matrix(NA, nrow(Master_data), 2)
    regulated_unit_shares[, 1] <- Master_data$regulated_housingUnit_share
    regulated_unit_shares[, 2] <- 1 - Master_data$regulated_housingUnit_share
  
    #Calculating spendshares + Stringnecy code
    #Initializing
    spendShares <- list(rep(NA, nrow(Master_data)), rep(NA, nrow(Master_data)))
    stringencyCode <- list(rep(0, nrow(Master_data)), rep(0, nrow(Master_data))) #Tells us a stringencyCode, whether we are unconstrained 1, partially constrained 2 or fully constrained by minimum lot size 3
    
    #loop over zones
    for (zone in c(1, 2)) {
      #Spending shares
      spendShares[[zone]] <- pmax(
        
                                  (1-demandParameters[1])*pmin((price[[zone]]*demandParameters[2])/(Master_data[[paste0(skill, "Wage")]]*EqVar*Master_data[[paste0("ability_grp", incomeType)]]), 1) + demandParameters[1], #Stone geary shares if unconstrained
        
                                                          pmin(housing_stringency[[zone]]/(Master_data[[paste0(skill, "Wage")]]*EqVar*Master_data[[paste0("ability_grp", incomeType)]]), 1) ) #Constrained, housing spend share is minimal lot exp/income
      #Stringency indicators
      stringencyCode[[zone]] <-   ifelse(spendShares[[zone]] == 1, 3, 0) + #IF FULLY CONSTRAINED, spending shares == 100% of income
                                  ifelse(spendShares[[zone]] == housing_stringency[[zone]]/(Master_data[[paste0(skill, "Wage")]]*EqVar*Master_data[[paste0("ability_grp", incomeType)]]), 2, 0) +  #partially constrained                               
                                  ifelse(spendShares[[zone]] == (1-demandParameters[1])*pmin((price[[zone]]*demandParameters[2])/(Master_data[[paste0(skill, "Wage")]]*EqVar*Master_data[[paste0("ability_grp", incomeType)]]), 1) + demandParameters[1], 1, 0)                               
      
    }
    
    #consumption values by zone
    consValue <- list(rep(NA, nrow(Master_data)), rep(NA, nrow(Master_data)))
    
    betaFactor <- ((1-demandParameters[1])^(1-demandParameters[1]))*((demandParameters[1])^(demandParameters[1])) #Porportional adjustment factor (comes out of the Cobb-Douglas algebra)
    
    for (zone in c(1, 2)) { 
      
     consValue[[zone]] <- pmax(0,(Master_data[[paste0(skill, "Wage")]]*EqVar*Master_data[[paste0("ability_grp", incomeType)]] - demandParameters[2]*price[[zone]])/(price[[zone]]^demandParameters[1]))*
                          (       ifelse(stringencyCode[[zone]] == 3, 0, 0) + #If completely constrained, earn consumption index == 0)
                                  ifelse(stringencyCode[[zone]] == 2, (((Master_data[[paste0(skill, "Wage")]]*EqVar*Master_data[[paste0("ability_grp", incomeType)]] - housing_stringency[[zone]])/(Master_data[[paste0(skill, "Wage")]]*EqVar*Master_data[[paste0("ability_grp", incomeType)]] - demandParameters[2]*price[[zone]]))^(1-demandParameters[1]))*
                                                                      (((housing_stringency[[zone]] - price[[zone]]*demandParameters[[2]])/(Master_data[[paste0(skill, "Wage")]]*EqVar*Master_data[[paste0("ability_grp", incomeType)]] - demandParameters[2]*price[[zone]]))^(demandParameters[1]))/betaFactor, 0) + 
                                  ifelse(stringencyCode[[zone]] == 1, 1, 0)  #If unconstrained, do not change main consumption index
                                    )
    }
    
    #Adjusting for computational reasons to calculate index, putting in matrix
    consValue_adjusted <- matrix(NA, nrow(Master_data), 2)
    
    for (zone in c(1, 2)) {
      consValue_adjusted[, zone] <- exp(consValue[[zone]]/adjustment_factor_temp) #adjustment_factor_temp purely computational to calculate exp(large number)
    }
    
    #Calculating vector of consumption indices in each neighborhood
    consIndex <- log((rowSums((consValue_adjusted^(wN_elast))*regulated_unit_shares))^(1/wN_elast))
    consIndex <- consIndex*consumption_AdjustmentFactor[[skill, paste0("consumption_Adjustment", incomeType)]]*adjustment_factor_temp
    
    #Finally, using neighborhood consumption indices to calculate welfare
    Master_data[paste0("Val_temp", name_of_skill, incomeType)] <- (exp(consIndex)*
                                                                   (Master_data[[paste0("Amenity_", name_of_skill, incomeType)]]))^(rho) #Value of a location raised to migration elasticity 
    
    value_n_sym <- paste0("Val_temp", name_of_skill, incomeType)
    value_n_city_sym <- paste0("City_Welfare_", name_of_skill, incomeType) 
    
    #Creating city welfare index 
    Master_data <- Master_data %>% group_by(CBSA) %>% mutate(!!sym(value_n_city_sym) := (sum(!!sym(value_n_sym)))^(1/rho))
    
    #Creating global welfare index
    Welfare_index <- (sum(((Master_data[[paste0("City_Welfare_", name_of_skill, incomeType)]])^(theta))*(Master_data$inverse_city_weights)))^(1/theta)
    
    return(Welfare_index)
    
  }#end initCalc check
  
  if (initCalc == FALSE) { #else, just calculate welfare normally using counterfactual equilibrium values
    
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


getVariation <- function(Init, Ct, skill, incomeType, demandParameters) {
  
  EqVarSolve <- function(EqVar) {
    return(  GetWelfareEqVar(Master_data = Init, skill = skill, 
                               incomeType = incomeType, 
                               demandParameters = demandParameters, 
                               initCalc = TRUE,
                               EqVar = EqVar) - 
               
               GetWelfareEqVar(Master_data = Ct, skill = skill, 
                                 incomeType = incomeType, 
                                 demandParameters = demandParameters, 
                                 initCalc = FALSE,
                                 EqVar = 1)   )
  }
  
  sln <- uniroot(f = EqVarSolve, interval = c(0.95, 1.05), extendInt = "yes")
  
  return(sln$root)
  
  
}