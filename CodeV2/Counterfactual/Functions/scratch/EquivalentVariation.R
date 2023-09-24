InitialWelfare <- function(Master, bySkill, i, EqVar) {
  
  if (bySkill == FALSE) {
        
        Welfare <- function(Master_data, EqVar, i)  {
      
        #Calculating prices in each zone
        price <- list()
        price[[1]] <- log(Master_data$price_reg)
        price[[2]] <- (4)*log(Master_data$hedonicPrice) - (3)*log(Master_data$price_reg)
    
        #geometric weights == land shares
    
        price[[1]] <- exp(price[[1]])
        price[[2]] <- exp(price[[2]])
    
        #storing total housing supply expenditure in each location
        housing_sp_exp <- list()
        housing_sp_exp[[1]] <- Master_data$land_regulated*Master_data$lambda*(price[[1]]^(Master_data$HS_Elasticity_imputed + 1))
        housing_sp_exp[[2]] <- Master_data$land_unregulated*Master_data$lambda*(price[[2]]^(Master_data$HS_Elasticity_imputed + 1))
    
        housing_stringency <- list()
        housing_stringency[[1]] <- Master_data$UnitDensityRestriction_cl*Master_data$lambda*(price[[1]]^(Master_data$HS_Elasticity_imputed + 1))
        housing_stringency[[2]] <- 0
        
        #Consumption value
        consValue <- list(list(), list())
    
        for (zone in c(1, 2)) { #1 for regulated, 2 for unregulated
          for (incomeType in 1:7) {
        
            consValue[[zone]][[incomeType]] <- 0*ifelse(EqVar*Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] <= housing_stringency[[zone]], 1, 0) + #IFELSE current income type priced out of market
          
              ifelse((EqVar*Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] > housing_stringency[[zone]] &
                      EqVar*Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] <= (housing_stringency[[zone]]/Master_data[[paste0("hSpendShare_", incomeType)]])), 
                   ((housing_stringency[[zone]]/price[[zone]])^(Master_data[[paste0("hSpendShare_", incomeType)]]))*((EqVar*Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] - housing_stringency[[zone]])^(1-Master_data[[paste0("hSpendShare_", incomeType)]])), 0) + #Constrained at current income                     
            
              ifelse(EqVar*Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] > (housing_stringency[[zone]]/Master_data[[paste0("hSpendShare_", incomeType)]]),
                   (Master_data[[paste0("hSpendShare_", incomeType)]]^Master_data[[paste0("hSpendShare_", incomeType)]])*((1-Master_data[[paste0("hSpendShare_", incomeType)]])^(1-Master_data[[paste0("hSpendShare_", incomeType)]]))*(EqVar*Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]])/(price[[zone]]^Master_data[[paste0("hSpendShare_", incomeType)]]) , 0) #consumption value bounded above by residential zone for same prices
        
          }#end loop over income types
        }#end loop over zones
    
      
        #Land_shares
        land_shares <- list()
        land_shares[[1]] <- Master_data$land_regulated/Master_data$final_land_for_res
        land_shares[[2]] <- 1 - Master_data$land_regulated/Master_data$final_land_for_res
      
        consIndex <- list() 
        for (incomeType in 1:7) {
          consIndex[[incomeType]] <- (((consValue[[1]][[incomeType]]^(wN_elast))*land_shares[[1]] + (consValue[[2]][[incomeType]]^(wN_elast))*land_shares[[2]])^(1/wN_elast))
        }
    
    
        return(consIndex[[i]])
      }
        
    
    consIndex <- Welfare(Master, EqVar, i)
    
    #Calculating welfare index
    Master[paste0("Val_temp", i)] <- (consIndex^(rho*consumptionAdjustment[[paste0("consumption_Adjustment", i)]][1]))*(Master[[paste0("Amenity_", i)]]) #Value of a location raised to migration elasticity 
    
    value_n_sym <- paste0("Val_temp", i)
    value_n_city_sym <- paste0("City_Welfare_", i) 
    
    #Creating city welfare index 
    Master<- Master %>% group_by(CBSA) %>% mutate(!!sym(value_n_city_sym) := (sum(!!sym(value_n_sym)))^(1/rho))
    
    #Creating global welfare index
    Welfare_index <- (sum(((Master[[paste0("City_Welfare_", i)]])^(theta))*(Master$inverse_city_weights)))^(1/theta)
    
        
    return(Welfare_index) #Function works well via testing!
        
  }#End BySkill
  
}#End function



#Getting Equivalent Variation
getEquivalentVariation <- function(Initial, Ct, bySkill, i) {
  
  if (bySkill == FALSE) {
    
    objective <- function(EqVar) { #Objective function to solve for
      
      return(InitialWelfare(Master = Initial, bySkill = FALSE, i, EqVar) - getWelfare(Master_data = Ct, bySkill = FALSE, i))
      
      
    }
    
    
    sln <- uniroot(f = objective, interval = c(0.5, 2), extendInt = "yes")
      
    return(sln$root)
      
  }
  
}

