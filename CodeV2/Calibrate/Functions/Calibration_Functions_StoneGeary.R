
#Getting prices for regulated and unregulated structures + consumption values to clear markets 
Calibrate_prices <- function(Master_data, demandParameters) {
  
  beta_StGeary <- demandParameters[1] #beta -- stone geary
  min_hReq <- demandParameters[2] #Minimum housing consumption requirement

 if (Master_data$Regulation_code == 1) { #neighborhood with partial regulation
   
  ExcessDemand <- function(price_reg) { #Function that calculates excess demand in regulated neighborhood 
                                        #relative to observed share of housing units in regulated structures 
                                        #Yields measures of consumption by income type in each neighborhood, as well
    
    
    #Regulated housing unit shares from ACS
    regulated_unit_shares <- rep(NA, 2)
    regulated_unit_shares[1] <- Master_data$regulated_housingUnit_share
    regulated_unit_shares[2] <- 1 - Master_data$regulated_housingUnit_share
    
    
    price <- rep(NA, 2) #A set of two prices (one in the regulated, one in the unregulated)
    
    price[1] <- Master_data$hedonicPrice #Unregulated neighborhood price index identified from hedonic index
    price[2] <- price_reg
    
    #Measuring housing stringency (value of a minimal lot in regulated neighborhood)
    housing_stringency <- rep(NA, 2)
    housing_stringency[1] <- Master_data$IncomeStringency_model_rents
    housing_stringency[2] <- 0
    
    #First, calculate spending shares on housing by incomeType and zone at current prices
    spendShares <- matrix(NA, 7, 2)
    stringencyCode <- matrix(0, 7, 2) #Tells us a stringencyCode, whether we are unconstrained 1, partially constrained 2 or fully constrained by minimum lot size 3
    
    for (zone in c(1, 2)) { #NOTATION: 1 for regulated, 2 for unregulated. Take maximum spend share
      for (incomeType in 1:7) {
        
        spendShares[incomeType, zone] <- max(
          
                                         (1-beta_StGeary)*min((price[zone]*min_hReq)/(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]), 1) + beta_StGeary, #Stone geary shares if unconstrained
                                          
                                         min(housing_stringency[zone]/(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]), 1) ) #Constrained, housing spend share is minimal lot exp/income
                                         
        stringencyCode[incomeType, zone] <- ifelse(spendShares[incomeType, zone] == 1, 3, 0) + #IF FULLY CONSTRAINED, spending shares == 100% of income
                                            ifelse(spendShares[incomeType, zone] == housing_stringency[zone]/(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]), 2, 0) +  #partially constrained                               
                                            ifelse(spendShares[incomeType, zone] == (1-beta_StGeary)*min((price[zone]*min_hReq)/(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]), 1) + beta_StGeary, 1, 0)                               
        
      }
    }
    
    #Consumption value matrix (note: consumption value is exp(StoneGearyIndex) from paper)
    consValue <- matrix(NA, 7, 2)
    betaFactor <- ((1-beta_StGeary)^(1-beta_StGeary))*((beta_StGeary)^(beta_StGeary)) #Porportional adjustment factor (comes out of the algebra)
    
    for (zone in c(1, 2)) { #NOTATION: 1 for regulated, 2 for unregulated
      for (incomeType in 1:7) {
                consValue[incomeType, zone] <- max(0,(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] - min_hReq*price[zone])/(price[zone]^beta_StGeary))* #Main index * constraint factor!
                                               (ifelse(stringencyCode[incomeType, zone] == 3, 0, 0) + #If completely constrained, earn consumption index == 0
                                                ifelse(stringencyCode[incomeType, zone] == 2, (((Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] - housing_stringency[zone])/(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] - min_hReq*price[zone]))^(1-beta_StGeary))*
                                                                                              (((housing_stringency[zone] - price[zone]*min_hReq)/(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] - min_hReq*price[zone]))^(beta_StGeary))/betaFactor, 0) + 
                                                ifelse(stringencyCode[incomeType, zone] == 1, 1, 0)) #If unconstrained, do not change main consumption index
                                                                                
      } #end loop over income types
    } #end loop over zones
    
    
    #Standardizing consValue because of numerical error for population allocations (cant take exp of very large number) (this does not matter for allocations)
    consValue_adjusted <- matrix(NA, 7, 2)
    
    for (zone in c(1, 2)) { #NOTATION: 1 for regulated, 2 for unregulated
      for (incomeType in 1:7) {
        
        consValue_adjusted[incomeType, zone] <- exp(consValue[incomeType, zone]/adjustment_factor_temp[incomeType]) 
        
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
    
    
    #Housing demand expenditure in each zone at current prices (this can be used to solve for land in each neighborhood
    housing_demand_exp <- rep(0, 2) #2-vector of housing demand (in $)
    
    for (zone in c(1, 2)) {
      for (incomeType in 1:7) {
        
        housing_demand_exp[zone] <- housing_demand_exp[zone] + spendShares[incomeType, zone]*Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]*
                                                               Population_allocations[incomeType, zone] #total spending on housing in each zone
        
      }
    }
    
    
    #Calculating difference between population share 
    model_regulated_share <- sum(Population_allocations[, 1])/sum(Population_allocations)
    
    ExcessDemand_regulated <- model_regulated_share - regulated_unit_shares[1]
    
    #Returning excess demand for regulated zone
    return(list(ExcessDemand_regulated, Population_allocations, consValue, consIndex, housing_demand_exp, spendShares))
    
    
  } #End function defining excess demand
  
  
  #New function that takes first component of previous fn
  
  ToSolve <- function(price_reg) {
    
    return(as.numeric(ExcessDemand(price_reg)[1]))
    
  }
  
  
  #Solving via UniRoot
  sln <- uniroot(f = ToSolve, interval = c(Master_data$hedonicPrice, 1.2*Master_data$hedonicPrice), extendInt = "yes", maxiter = 10000)
  
  price_sln <- sln$root
  
  ExcessDemand_object_at_sln <- ExcessDemand(price_sln)
  
  
  return(list(price = price_sln,
              consumption_Index = list(Pooled = ExcessDemand_object_at_sln[[4]]),
              housing_demand_expenditure = ExcessDemand_object_at_sln[[5]],
              Populations = list(Pooled = ExcessDemand_object_at_sln[[2]]),
              ExcessDemand = ExcessDemand_object_at_sln[[1]],
              RegulationCode = Master_data$Regulation_code, 
              housingExpenditureShare = list(Pooled = ExcessDemand_object_at_sln[[6]]),
              Geo = c(Master_data$State[1], Master_data$County[1], Master_data$Tract[1], Master_data$BlockGroup[1])))
  
 }
  
  
  if (Master_data$Regulation_code == 2 | Master_data$Regulation_code == 0) { #If 100% of the neighborhood is regulated, we know price = hedonicIndex, set housing expenditure == to solve for land (or lambda when regulation == 0)
    
    
    housing_stringency <- Master_data$IncomeStringency_model_rents
    price <- Master_data$hedonicPrice
    
    #Calculating spending shares
    spendShares <- rep(NA, 7)
    stringencyCode <- rep(NA, 7)
    
    
    for (incomeType in 1:7) {
      
     #Spending shares on housing +  checking if minimum lot size is binding
     spendShares[incomeType] <-  max(
        
                                    (1-beta_StGeary)*min((price*min_hReq)/(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]), 1) + beta_StGeary, #Stone geary shares if unconstrained
        
                                    min(housing_stringency/(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]), 1) ) #Constrained, housing spend share is minimal lot exp/income
     
     stringencyCode[incomeType] <- ifelse(spendShares[incomeType] == 1, 3, 0) + #IF FULLY CONSTRAINED, spending shares == 100% of income
                                   ifelse(spendShares[incomeType] == housing_stringency/(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]), 2, 0) +  #partially constrained                               
                                   ifelse(spendShares[incomeType] == (1-beta_StGeary)*min((price*min_hReq)/(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]), 1) + beta_StGeary, 1, 0)  #Unconstrained                           
     
      
    }
    
    
    #Consumption index
    
    betaFactor <- ((1-beta_StGeary)^(1-beta_StGeary))*((beta_StGeary)^(beta_StGeary)) #factor of proportionality for consumption index
    
    consIndex <- rep(NA, 7) 
      for (incomeType in 1:7) {
        
        consIndex[incomeType] <- max(0,(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] - min_hReq*price)/(price^beta_StGeary))* #Main index * constraint multiplier
                                 (ifelse(stringencyCode[incomeType] == 3, 0, 0) + #If completely constrained, earn consumption index == 0
                                  ifelse(stringencyCode[incomeType] == 2, (((Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] - housing_stringency)/(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] - min_hReq*price))^(1-beta_StGeary))*
                                                                          (((housing_stringency - price*min_hReq)/(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]] - min_hReq*price))^(beta_StGeary))/betaFactor, 0) + 
                                  ifelse(stringencyCode[incomeType] == 1, 1, 0)) #If unconstrained, do not change main consumption index
        
      }#end loop over income types
    
    #Adjusting consIndex by middle income type (nothing but a normalization for computational purposes)
    consIndex <- consIndex/adjustment_factor_temp
    
    #Housing demand expenditure and expenditure shares
    housing_demand_exp <- 0
    
    for (incomeType in 1:7) {
      
      housing_demand_exp <- housing_demand_exp + spendShares[incomeType]*Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]*Master_data[[paste0("Population_type_", incomeType)]]
        
    }
      
    
    return(list(price = Master$hedonicPrice[1],
                consumption_Index = list(Pooled = consIndex),
                housing_demand_expenditure = housing_demand_exp,
                Populations = NA, #Populations not required
                ExcessDemand = 0,
                RegulationCode = Master_data$Regulation_code,
                housingExpenditureShare = list(Pooled = spendShares),
                Geo = c(Master_data$State[1], Master_data$County[1], Master_data$Tract[1], Master_data$BlockGroup[1])))
    
  } 
  
}


