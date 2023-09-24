
#Getting prices for regulated and unregulated structures + consumption values to clear markets 
Calibrate_prices <- function(Master_data) {
  
 if (Master_data$Regulation_code == 1) { #neighborhood with partial regulation
   
  ExcessDemand <- function(price_reg) { #Function that calculates excess demand in regulated neighborhood 
                                        #relative to observed share of housing units in regulated structures 
                                        #Yields measures of consumption by income type in each neighborhood, as well
    
    
    #Regulated housing unit shares from ACS
    regulated_unit_shares <- rep(NA, 2)
    regulated_unit_shares[1] <- Master_data$regulated_housingUnit_share
    regulated_unit_shares[2] <- 1 - Master_data$regulated_housingUnit_share
    
    
    price <- rep(NA, 2) #A set of two prices (one in the regulated, one in the unregulated)
    
    price[1] <- Master_data$hedonicPrice
    price[2] <- price_reg
    
    #Measuring housing stringency (value of a minimal lot in regulated neighborhood)
    housing_stringency <- rep(NA, 2)
    housing_stringency[1] <- Master_data$IncomeStringency_model_rents
    housing_stringency[2] <- 0
    
    #Consumption value matrix (note: consumption value is exp(CobbDouglasIndex) from paper)
    consValue <- matrix(NA, 7, 2)
    
    for (zone in c(1, 2)) { #NOTATION: 1 for regulated, 2 for unregulated
      for (incomeType in 1:7) {
        
        consValue[incomeType, zone] <- 0*ifelse(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] <= housing_stringency[zone], 1, 0) + #IFELSE current income type priced out of market
          
          ifelse((Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] > housing_stringency[zone] &
                    Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] <= (housing_stringency[zone]/Master_data[[paste0("hSpendShare_", incomeType)]])), 
                 ((housing_stringency[zone]/price[zone])^(Master_data[[paste0("hSpendShare_", incomeType)]]))*((Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] - housing_stringency[zone])^(1-Master_data[[paste0("hSpendShare_", incomeType)]])), 0) + #Constrained at current income                     
          
          ifelse(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] > (housing_stringency[zone]/Master_data[[paste0("hSpendShare_", incomeType)]]),
                 (Master_data[[paste0("hSpendShare_", incomeType)]]^Master_data[[paste0("hSpendShare_", incomeType)]])*((1-Master_data[[paste0("hSpendShare_", incomeType)]])^(1-Master_data[[paste0("hSpendShare_", incomeType)]]))*(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]])/(price[zone]^Master_data[[paste0("hSpendShare_", incomeType)]]) , 0) #consumption value bounded above by residential zone for same prices
      
        
      }#end loop over income types
    }#end loop over zones
    
    
    #Standardizing consValue because of numerical error for population allocations (cant take exp of very large number) (this does not matter for allocations)
    consValue_adjusted <- matrix(NA, 7, 2)
    
    for (zone in c(1, 2)) { #NOTATION: 1 for regulated, 2 for unregulated
      for (incomeType in 1:7) {
        
        consValue_adjusted[incomeType, zone] <- exp(consValue[incomeType, zone]/adjustment_factor_temp) 
        
      }
    }
    
    #Calculating fractions of populations in each type
    Population_allocations <- matrix(NA, 7, 2)
    
    
    #Note: we set amenities == regulated housing unit shares so that the population allocations in an equilibrium where regulation is never binding 
    for (zone in c(1, 2)) {
      for (incomeType in 1:7) {
        
        Population_allocations[incomeType, zone] <- ((consValue_adjusted[incomeType, zone])^(wN_elast)*regulated_unit_shares[zone])/(sum((consValue_adjusted[incomeType, ]^(wN_elast))*regulated_unit_shares))*
                                                      Master_data[[paste0("Population_type_", incomeType)]]
        
      }
    }
    
    
    consIndex <- rep(1, 7) # AGAIN--this is to "APPROXIMATE" PERFECT SUBSTITUTES OVER STRUCTURE TYPES WITHIN NEIGHBORHOODS.
    for (incomeType in 1:7) {
      consIndex[incomeType] <- log((sum((consValue_adjusted[incomeType, ]^(wN_elast))*regulated_unit_shares))^(1/wN_elast))
    }
    
    
    #Housing demand expenditure in each zone at current prices (this can be used to solve for land in each neighborhood)
    #NOTE: if minimal lot more expensive than income, 
    housing_demand_exp <- rep(0, 2) 
    
    for (zone in c(1, 2)) {
      for (incomeType in 1:7) {
        
        housing_demand_exp[zone] <- housing_demand_exp[zone] + 
                                    as.numeric(housing_stringency[zone]*ifelse((Master_data[[paste0("hSpendShare_", incomeType)]]*Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] <= housing_stringency[zone]) &
                                                                                (Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] > housing_stringency[zone]), 1, 0) + #if constrained but not priced out, consume at minimum lot size
                                                 
                                              Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]*ifelse(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] <= housing_stringency[zone], 1, 0) +
                                                 
                                              Master_data[[paste0("hSpendShare_", incomeType)]]*Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]*ifelse(Master_data[[paste0("hSpendShare_", incomeType)]]*Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] > housing_stringency[zone], 1, 0))* #if unconstrained, spend fraction beta of expenditure on housing 
                                              Population_allocations[incomeType, zone]
        
      }
    }
    
    
    #Calculating difference between population share 
    model_regulated_share <- sum(Population_allocations[, 1])/sum(Population_allocations)
    
    ExcessDemand_regulated <- model_regulated_share - regulated_unit_shares[1]
    
    #Returning excess demand for regulated zone
    return(list(ExcessDemand_regulated, Population_allocations, consValue, consIndex, housing_demand_exp))
    
    
  } #End function defining excess demand
  
  
  #New function that takes first component of previous fn
  
  ToSolve <- function(price_reg) {
    
    return(as.numeric(ExcessDemand(price_reg)[1]))
    
  }
  
  
  #Solving via UniRoot
  sln <- uniroot(f = ToSolve, interval = c(0.9*Master_data$hedonicPrice, 1.1*Master_data$hedonicPrice), extendInt = "yes", maxiter = 10000)
  
  price_sln <- sln$root
  
  ExcessDemand_object_at_sln <- ExcessDemand(price_sln)
  
  
  return(list(price = price_sln,
              consumption_Index = ExcessDemand_object_at_sln[[4]],
              housing_demand_expenditure = ExcessDemand_object_at_sln[[5]],
              Populations = ExcessDemand_object_at_sln[[2]],
              ExcessDemand = ExcessDemand_object_at_sln[[1]],
              RegulationCode = Master_data$Regulation_code[1], 
              Geo = c(Master_data$State[1], Master_data$County[1], Master_data$Tract[1], Master_data$BlockGroup[1])))
  
 }
  
  
  if (Master_data$Regulation_code == 2 | Master_data$Regulation_code == 0) { #If 100% of the neighborhood is regulated, we know price = hedonicIndex, set housing expenditure == to solve for land (or lambda when regulation == 0)
    
    
    housing_stringency <- Master_data$IncomeStringency_model_rents
    price <- Master_data$hedonicPrice
    
    #Consumption index
    consIndex <- rep(NA, 7) 
      for (incomeType in 1:7) {
        
        consIndex[incomeType] <- 0*ifelse(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] <= housing_stringency, 1, 0) + #IFELSE current income type priced out of market
          
                                    ifelse((Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] > housing_stringency &
                                            Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] <= (housing_stringency/Master_data[[paste0("hSpendShare_", incomeType)]])), 
                                          ((housing_stringency/price)^(Master_data[[paste0("hSpendShare_", incomeType)]]))*((Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] - housing_stringency)^(1-Master_data[[paste0("hSpendShare_", incomeType)]])), 0) + #Constrained at current income                     
          
                                    ifelse(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] > (housing_stringency/Master_data[[paste0("hSpendShare_", incomeType)]]),
                                    (Master_data[[paste0("hSpendShare_", incomeType)]]^Master_data[[paste0("hSpendShare_", incomeType)]])*((1-Master_data[[paste0("hSpendShare_", incomeType)]])^(1-Master_data[[paste0("hSpendShare_", incomeType)]]))*(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]])/(price^Master_data[[paste0("hSpendShare_", incomeType)]]) , 0) #consumption value bounded above by residential zone for same prices
        
        
      }#end loop over income types
    
    #Adjusting consIndex by middle income type (nothing but a normalization for computational purposes)
    consIndex <- consIndex/adjustment_factor_temp
    
    #Housing demand expenditure
    housing_demand_exp <- 0
    for (incomeType in 1:7) {
      
      housing_demand_exp <-       housing_demand_exp + 
                                                            as.numeric(housing_stringency*ifelse((Master_data[[paste0("hSpendShare_", incomeType)]]*Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] <= housing_stringency) &
                                                                        (Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] > housing_stringency), 1, 0) + #if constrained but not priced out, consume at minimum lot size
                     
                                                                        Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]*ifelse(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] <= housing_stringency, 1, 0) + #if priced out, spend all current income
                     
                                                                        Master_data[[paste0("hSpendShare_", incomeType)]]*Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]*ifelse(Master_data[[paste0("hSpendShare_", incomeType)]]*Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] > housing_stringency, 1, 0))* #if unconstrained, spend fraction beta of expenditure on housing 
                                                                        Master_data[[paste0("Population_type_", incomeType)]]
      
    }
      
    
    return(list(price = Master$hedonicPrice[1],
                consumption_Index = consIndex,
                housing_demand_expenditure = housing_demand_exp,
                Populations = NA,
                ExcessDemand = 0,
                RegulationCode = Master_data$Regulation_code[1], 
                Geo = c(Master_data$State[1], Master_data$County[1], Master_data$Tract[1], Master_data$BlockGroup[1])))
    
  } 
  
} #End function

#BYSKILL VERSION of the above function -- feeds population by income and skill level 
Calibrate_prices_bySkill <- function(Master_data) {
  
  if (Master_data$Regulation_code == 1) { #neighborhood with partial regulation
    
    ExcessDemand <- function(price_reg) {
      
      #Regulated housing unit shares from ACS
      regulated_unit_shares <- rep(NA, 2)
      regulated_unit_shares[1] <- Master_data$regulated_housingUnit_share
      regulated_unit_shares[2] <- 1 - Master_data$regulated_housingUnit_share
      
      
      price <- rep(NA, 2) #A set of two prices (one in the regulated, one in the unregulated)
      
      price[1] <- Master_data$hedonicPrice
      price[2] <- price_reg
      
      #Measuring housing stringency (value of a minimal lot in regulated neighborhood)
      housing_stringency <- rep(NA, 2)
      housing_stringency[1] <- Master_data$IncomeStringency_model_rents
      housing_stringency[2] <- 0
      
      
    
    
      #Consumption value matrix (note: consumption value is exp(CobbDouglasIndex) from paper)
      consValue <- list()
    
      for (skill in c("College", "NoCollege")) {
        consValue[[skill]] <- matrix(NA, 7, 2)
      
      }
    
    
      for (skill in c("College", "NoCollege")) {
        for (zone in c(1, 2)) { #NOTATION: 1 for regulated, 2 for unregulated
          for (incomeType in 1:7) {
          
          consValue[[skill]][incomeType, zone] <- 0*ifelse(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] <= housing_stringency[zone], 1, 0) + #IFELSE current income type priced out of market
            
                                                    ifelse((Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] > housing_stringency[zone] &
                                                            Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] <= (housing_stringency[zone]/Master_data[[paste0("hSpendShare_", incomeType)]])), 
                                                    ((housing_stringency[zone]/price[zone])^(Master_data[[paste0("hSpendShare_", incomeType)]]))*((Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] - housing_stringency[zone])^(1-Master_data[[paste0("hSpendShare_", incomeType)]])), 0) + #Constrained at current income                     
            
                                                    ifelse(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] > (housing_stringency[zone]/Master_data[[paste0("hSpendShare_", incomeType)]]),
                                                    (Master_data[[paste0("hSpendShare_", incomeType)]]^Master_data[[paste0("hSpendShare_", incomeType)]])*((1-Master_data[[paste0("hSpendShare_", incomeType)]])^(1-Master_data[[paste0("hSpendShare_", incomeType)]]))*(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]])/(price[zone]^Master_data[[paste0("hSpendShare_", incomeType)]]) , 0) #consumption value bounded above by residential zone for same prices
          
          
          
          
        
          }
        }
 
      }
    
      #Standardizing consValue because of numerical error for population allocations (cant take exp of very large number) (this does not matter for allocations)
      consValue_adjusted <- list()
      for (skill in c("College", "NoCollege")) {
        consValue_adjusted[[skill]] <- matrix(NA, 7, 2)
      
      }
    
      for (skill in c("College", "NoCollege")) {
        for (zone in c(1, 2)) { #NOTATION: 1 for regulated, 2 for unregulated
          for (incomeType in 1:7) {
        
            consValue_adjusted[[skill]][incomeType, zone] <- exp(consValue[[skill]][incomeType, zone]/adjustment_factor_temp) 
        
          }
        }
      }
    
    
    #Calculating fractions of populations in each type
      Population_allocations <- list()
    
      for (skill in c("College", "NoCollege")) {
      
        Population_allocations[[skill]] <- matrix(NA, 7, 2)
      
        #Note: we set amenities == regulated housing unit shares so that the population allocations in an equilibrium where regulation is never binding 
        for (zone in c(1, 2)) {
          for (incomeType in 1:7) {
          
            Population_allocations[[skill]][incomeType, zone] <- ((consValue_adjusted[[skill]][incomeType, zone])^(wN_elast)*regulated_unit_shares[zone])/(sum((consValue_adjusted[[skill]][incomeType, ]^(wN_elast))*regulated_unit_shares))*
                                                                 Master_data[[paste0("Population_type_", skill, "_", incomeType)]]
          
          }
        }
      
      }
    
      consIndex <- list() # AGAIN--this is to "APPROXIMATE" PERFECT SUBSTITUTES OVER STRUCTURE TYPES WITHIN NEIGHBORHOODS.    
    
      for (skill in c("College", "NoCollege")) {
        consIndex[[skill]] <- rep(NA, 7)  
      
        for (incomeType in 1:7) {
          consIndex[[skill]][incomeType] <- log((sum((consValue_adjusted[[skill]][incomeType, ]^(wN_elast))*regulated_unit_shares))^(1/wN_elast))
        }
      
      }
  
    
      #Housing demand 
      housing_demand_exp <- rep(0, 2) 
    
      for (skill in c("College", "NoCollege")) {
        for (zone in c(1, 2)) {
          for (incomeType in 1:7) {
        
            housing_demand_exp[zone] <- housing_demand_exp[zone] + 
                                      as.numeric(housing_stringency[zone]*ifelse((Master_data[[paste0("hSpendShare_", incomeType)]]*Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] <= housing_stringency[zone]) &
                                                       (Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] > housing_stringency[zone]), 1, 0) + #if constrained but not priced out, consume at minimum lot size
                       
                                      Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]]*ifelse(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] <= housing_stringency[zone], 1, 0) +
                       
                                      Master_data[[paste0("hSpendShare_", incomeType)]]*Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]]*ifelse(Master_data[[paste0("hSpendShare_", incomeType)]]*Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] > housing_stringency[zone], 1, 0))* #if unconstrained, spend fraction beta of expenditure on housing 
                                      Population_allocations[[skill]][incomeType, zone]
        
          }
        }
      }
    
    
      #Calculating difference between population share 
      model_regulated_share <- sum(Population_allocations$College[, 1] + Population_allocations$NoCollege[, 1])/sum(Population_allocations$College + Population_allocations$NoCollege)
      
      ExcessDemand_regulated <- model_regulated_share - regulated_unit_shares[1]
      
      #Returning excess demand for regulated zone
      return(list(ExcessDemand_regulated, Population_allocations, consValue, consIndex, housing_demand_exp))
    }
    
    #New function that takes first component of previous fn
    
    ToSolve <- function(price_reg) {
      
      return(as.numeric(ExcessDemand(price_reg)[1]))
      
    }
    
    
    #Solving via UniRoot
    sln <- uniroot(f = ToSolve, interval = c(0.9*Master_data$hedonicPrice, 1.1*Master_data$hedonicPrice), extendInt = "yes", maxiter = 10000)
    
    price_sln <- sln$root
    
    ExcessDemand_object_at_sln <- ExcessDemand(price_sln)
    
    
    return(list(price = price_sln,
                consumption_Index = ExcessDemand_object_at_sln[[4]],
                housing_demand_expenditure = ExcessDemand_object_at_sln[[5]],
                Populations = ExcessDemand_object_at_sln[[2]],
                ExcessDemand = ExcessDemand_object_at_sln[[1]],
                RegulationCode = Master_data$Regulation_code[1], 
                Geo = c(Master_data$State[1], Master_data$County[1], Master_data$Tract[1], Master_data$BlockGroup[1])))
    
  }#end check if regulation_code == 1
  

  if (Master_data$Regulation_code == 2 | Master_data$Regulation_code == 0) { #If 100% of the neighborhood is regulated, we know price = hedonicIndex, set housing expenditure == to solve for land (or lambda when regulation == 0)
    
    
    housing_stringency <- Master_data$IncomeStringency_model_rents
    price <- Master_data$hedonicPrice
    
    #Consumption index
    consIndex <- list()
    
    for (skill in c("College", "NoCollege")) {
      consIndex[[skill]] <- rep(NA, 7)
      
      for (incomeType in 1:7) {
      
        consIndex[[skill]][incomeType] <- 0*ifelse(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] <= housing_stringency, 1, 0) + #IFELSE current income type priced out of market
        
          ifelse((Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] > housing_stringency &
                    Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] <= (housing_stringency/Master_data[[paste0("hSpendShare_", incomeType)]])), 
                 ((housing_stringency/price)^(Master_data[[paste0("hSpendShare_", incomeType)]]))*((Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] - housing_stringency)^(1-Master_data[[paste0("hSpendShare_", incomeType)]])), 0) + #Constrained at current income                     
        
          ifelse(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] > (housing_stringency/Master_data[[paste0("hSpendShare_", incomeType)]]),
                 (Master_data[[paste0("hSpendShare_", incomeType)]]^Master_data[[paste0("hSpendShare_", incomeType)]])*((1-Master_data[[paste0("hSpendShare_", incomeType)]])^(1-Master_data[[paste0("hSpendShare_", incomeType)]]))*(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]])/(price^Master_data[[paste0("hSpendShare_", incomeType)]]) , 0) #consumption value bounded above by residential zone for same prices
      
      
      }#end loop over income types
    }
    
    #Adjusting consIndex by middle income type (nothing but a normalization for computational purposes)
    for (skill in c("College", "NoCollege")) {
      consIndex[[skill]] <- consIndex[[skill]]/adjustment_factor_temp
    }
    
    
    #Housing demand expenditure
    housing_demand_exp <- 0
    
    for (skill in c("College", "NoCollege")) {
      for (incomeType in 1:7) {
      
        housing_demand_exp <-       housing_demand_exp + 
                                    as.numeric(housing_stringency*ifelse((Master_data[[paste0("hSpendShare_", incomeType)]]*Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] <= housing_stringency) &
                                               (Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] > housing_stringency), 1, 0) + #if constrained but not priced out, consume at minimum lot size
                       
                                    Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]]*ifelse(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] <= housing_stringency, 1, 0) + #if priced out, spend all current income
                     
                                    Master_data[[paste0("hSpendShare_", incomeType)]]*Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]]*ifelse(Master_data[[paste0("hSpendShare_", incomeType)]]*Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] > housing_stringency, 1, 0))* #if unconstrained, spend fraction beta of expenditure on housing 
                                    Master_data[[paste0("Population_type_", skill, "_", incomeType)]]
      
      }
    }
    
    
    return(list(price = Master$hedonicPrice[1],
                consumption_Index = consIndex,
                housing_demand_expenditure = housing_demand_exp,
                Populations = NA,
                ExcessDemand = 0,
                RegulationCode = Master_data$Regulation_code[1], 
                Geo = c(Master_data$State[1], Master_data$County[1], Master_data$Tract[1], Master_data$BlockGroup[1])))
    
  } 
  
  
  
} #end function