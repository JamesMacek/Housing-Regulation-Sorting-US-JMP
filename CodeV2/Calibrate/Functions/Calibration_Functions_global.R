
#Getting prices for regulated and unregulated structures + consumption values to clear markets.
#The following function has three arguments:
#______________________________________________________________________________________________
#1 ) Row of master data to calibrate
#2 ) Demand parameters for StoneGeary specification, which nests identical Cobb-Douglas preferences
#3 ) List of skill objects and names, which determine whether we are calibrating bySkill or not. 
#______________________________________________________________________________________________

#First, function that calibrates excess demand given prices in unregulated zone.
#Takes objects that will be passed to the solver function that follows this one.
  ExcessDemand <- function(price_unreg, Master_data, 
                           demandParameters, bySkillVector, 
                           bySkillNames) { 
    
    #_______________________________SET PARAMETERS______________________________
    #Set up vector of targeted shares of regulated housing units - 1-4 units per lot.
    regulated_unit_shares <- matrix(c(Master_data$regulated_housingUnit_share, 
                                      1 - Master_data$regulated_housingUnit_share), nrow = 7, ncol = 2,
                                    byrow = TRUE)
    
    #Vector of prices by zone (note, replicated over income types)
    price <- matrix(c(Master_data$hedonicPrice, price_unreg), nrow = 7, ncol = 2, byrow = TRUE) #A set of two prices (one in the regulated, one in the unregulated)
    
    #Measuring housing stringency (value of a minimal lot in regulated neighborhood), replicated over zones
    housing_stringency <- matrix(c(Master_data$IncomeStringency_model_rents, 0), nrow = 7, ncol = 2, byrow = TRUE)
    #NOTATION: 1 for regulated, 2 for unregulated. Take maximum spend share
    
    #Ability vector, replicated over zones
    abilityVector <- matrix(as.numeric(select(ungroup(Master_data), starts_with("ability_grp"))[1, ]), nrow = 7, ncol = 2) #Vector containing abilities (income in average city...)
    
    #Population matrix (by income type, for each zone)
    Populations <- list()
    for (skill in bySkillVector) {
      
      #Imputing populations if college educated...
      if (bySkill_to_pass == TRUE) {
        Populations[[skill]] <- matrix(as.numeric(select(ungroup(Master_data),
                                                         contains(paste0("Population_type_", bySkillNames[which(skill == bySkillVector)])))[1, ]), 
                                       nrow = 7, ncol = 2)
      }
      
      #Imputing populations if aggregated across skill
      if (bySkill_to_pass == FALSE) {
        Populations[[skill]] <- matrix(as.numeric(select(ungroup(Master_data), 
                                                              starts_with("Population_type_"),
                                                              -contains("College"))[1, ]), nrow = 7, ncol = 2) 
        
      }
      
    }
    
    
    #_______________________________START SOLUTION HERE__________________________________________
    #First, calculate spending shares on housing by skill, incomeType and zone at current prices.
    spendShares <- list()
    stringencyCode <- list() #Tells us a stringencyCode, whether we are unconstrained 1. partially constrained 2 or fully constrained by minimum lot size 3
    
    for (skill in bySkillVector) {
      stringencyCode[[skill]] <- matrix(NA, 7, 2) #instantiating this vector...
      
    }
    
    #Creating regulation codes and spending shares at current prices:
    for (skill in bySkillVector) {
      
      #Spending shares...
      spendShares[[skill]] <- pmax( 
                                   (1 - demandParameters[1])*pmin( (price*demandParameters[2])/(Master_data[[paste0(skill, "Wage")]]*abilityVector) , 1) + demandParameters[1], #Stone geary shares if unconstrained...
                                                          #Create 7x2 matrix of spending shares absent regulation
                                   pmin( housing_stringency/(Master_data[[paste0(skill, "Wage")]]*abilityVector) , 1)  #Constrained, housing spend share is minimal lot expenditure/income
                                                          #matrix multiply column vector with row vector, creates 7x2 matrix by zone. 
                                   ) #Taking pmax of unregulated/regulated spending shares
        
      #Regulation codes...
      stringencyCode[[skill]][spendShares[[skill]] == 1] <- 3 #if spending all income at this location, set spending code == 3
      #if constrained by regulation, set code to 2
      stringencyCode[[skill]][ (spendShares[[skill]] < 1) & 
                               ( (housing_stringency/(Master_data[[paste0(skill, "Wage")]]*abilityVector)) > 
                               (1 - demandParameters[1])*(price*demandParameters[2])/(Master_data[[paste0(skill, "Wage")]]*abilityVector) + demandParameters[1]) ] <- 2   
      #if unconstrained, set code to 1: 
      stringencyCode[[skill]][ (spendShares[[skill]] < 1) & 
                                 ( (housing_stringency/(Master_data[[paste0(skill, "Wage")]]*abilityVector)) <= 
                                     (1 - demandParameters[1])*(price*demandParameters[2])/(Master_data[[paste0(skill, "Wage")]]*abilityVector) + demandParameters[1]) ] <- 1
      
    }
    
    #Consumption index
    betaFactor <- ((1-demandParameters[1])^(1-demandParameters[1]))*((demandParameters[1])^(demandParameters[1])) #factor of proportionality for consumption index. (See formula!)
    
    #Instantiate consumption index (utility by zone and income)
    consValue <- list()
    
    #Solving for consumption index at current prices...
    for (skill in bySkillVector) {
      
      consValue[[skill]] <-   ((Master_data[[paste0(skill, "Wage")]]*abilityVector - demandParameters[2]*price)/(price^demandParameters[1]))* #Overall consumption index (undistorted)
                                  #distortion factor     
                                  ( ifelse(stringencyCode[[skill]] == 3, 0, 0) + #if completely constrained, earn zero utility
                                    ifelse(stringencyCode[[skill]] == 2, (((Master_data[[paste0(skill, "Wage")]]*abilityVector - housing_stringency)/(Master_data[[paste0(skill, "Wage")]]*abilityVector - demandParameters[2]*price))^(1-demandParameters[1]))*
                                                                         (((housing_stringency - price*demandParameters[2])/(Master_data[[paste0(skill, "Wage")]]*abilityVector - demandParameters[2]*price))^(demandParameters[1]))/betaFactor, 0) + #
                                    ifelse(stringencyCode[[skill]] == 1, 1, 0) ) #if unconstrained, earn distortion index == 1. 
                            
    }
    
    #Adjusting consValue by exp(X/ (ability = z)) to make computation of this large number possible. !Has no impact on counterfactual outcomes if wN_elast is large enough!.
    consValue_adjusted <- list()
    for (skill in bySkillVector) {
      consValue_adjusted[[skill]] <- exp(consValue[[skill]]/abilityVector) 
    }
    
    
    #Population allocations given consumption values calculated....
    Population_allocations <- list()
    
    for (skill in bySkillVector) {
      Population_allocations[[skill]] <- (((consValue_adjusted[[skill]])^(wN_elast)*regulated_unit_shares)/
                                            (matrix(rowSums( (consValue_adjusted[[skill]]^(wN_elast))*regulated_unit_shares), nrow = 7, ncol = 2)))* #exp(value_{reg})/(sum_{o}exp(value_{o})) (gumbel shocks)
                                             Populations[[skill]]
    }
    
    #Creating aggregate consumption index
    consIndex <- list() # AGAIN--this is to "APPROXIMATE" PERFECT SUBSTITUTES OVER STRUCTURE TYPES WITHIN NEIGHBORHOODS.
    
    for (skill in bySkillVector) {
      consIndex[[skill]] <- log(rowSums( (consValue_adjusted[[skill]]^(wN_elast))*regulated_unit_shares)^(1/wN_elast))
    }
    
    #Housing demand expenditure in each zone at current prices (this can be used to solve for land in each neighborhood)
    housing_demand_exp <- c(0, 0)
    
    for (skill in bySkillVector) {
      housing_demand_exp <- housing_demand_exp + 
                            colSums(spendShares[[skill]]*Master_data[[paste0(skill, "Wage")]]*abilityVector*Population_allocations[[skill]])
    }
    
      
    #Calculating difference between observed regulated share and model implied 
    #Populations 
    pop_Reg <- 0
    pop_Total <- 0
    
    for (skill in bySkillVector) {
      pop_Reg <- pop_Reg + sum(Population_allocations[[skill]][, 1])
      pop_Total <- pop_Total + sum(Population_allocations[[skill]])
    }
    
    #Excess demand
    model_regulated_share <- pop_Reg/pop_Total
    ExcessDemand_regulated <- model_regulated_share - regulated_unit_shares[1]
    
    
    #Returning excess demand for regulated zone and other model calculations
    return(list(ExcessDemand_regulated, Population_allocations, consValue, consIndex, housing_demand_exp, spendShares))
    
  
  }
  
  
  
  
  
  #____________________________________________________________________________________________________
  
  #FUNCTION: CALIBRATE PRICES:
  Calibrate_prices <- function(Master_data, demandParameters, #Pass data frame, housing demand parameters.
                               bySkillVector, bySkillNames) { #Pass vectors to tell function if we are calibrating by skill or not.
    
    
    #CASE 1: PARTIALLY REGULATED:
    
    if (Master_data$Regulation_code  == 1) { #neighborhood with partial or complete regulation
      #New function defined as solver
      ToSolve <- function(price_unreg) {
        
        return(as.numeric(ExcessDemand(price_unreg = price_unreg, Master_data = Master_data,
                                       demandParameters = demandParameters,
                                       bySkillVector = bySkillVector,
                                       bySkillNames = bySkillNames)[1]))
        
      }
      
      
      #Solving partial regulation function given unitRoot:
      sln <- uniroot(f = ToSolve, interval = c(Master_data$hedonicPrice, 1.2*Master_data$hedonicPrice), 
                     extendInt = "yes", maxiter = 100)
      
      price_sln <- sln$root
      
      ExcessDemand_object_at_sln <- ExcessDemand(price_unreg = price_sln,
                                                 Master_data = Master_data,
                                                 demandParameters = demandParameters,
                                                 bySkillVector = bySkillVector,
                                                 bySkillNames = bySkillNames)
      
      
      return(list(price = price_sln,
                  consumption_Index = ExcessDemand_object_at_sln[[4]],
                  housing_demand_expenditure = ExcessDemand_object_at_sln[[5]],
                  Populations = ExcessDemand_object_at_sln[[2]],
                  ExcessDemand = ExcessDemand_object_at_sln[[1]],
                  RegulationCode = Master_data$Regulation_code, 
                  housingExpenditureShare = ExcessDemand_object_at_sln[[6]],
                  Geo = c(Master_data$State[1], Master_data$County[1], Master_data$Tract[1], Master_data$BlockGroup[1])))
    }
    
    
    #CASE 2: Fully or Not Fully Regulated
    #If 100% of the neighborhood is regulated, we know price = hedonicIndex, set housing expenditure == to solve for land (or lambda when regulation == 0)
    if (Master_data$Regulation_code == 0 | Master_data$Regulation_code == 2) {
      
      #Initial 7-vector of parameters (note: there is only one zone here, so no need for matrix)
      housing_stringency <- rep(Master_data$IncomeStringency_model_rents, 7) #Note, if RegulationCode == 0, this is ZERO. Else, it is positive. 
      price <- rep(Master_data$hedonicPrice, 7)
      abilityVector <- as.numeric(select(ungroup(Master_data), starts_with("ability_grp"))[1, ])
      
      Populations <- list()
      for (skill in bySkillVector) {
        
        if (bySkill_to_pass == TRUE) {
          Populations[[skill]] <- as.numeric(select(ungroup(Master_data),
                                                    contains(paste0("Population_type_", bySkillNames[which(skill == bySkillVector)])))[1, ])
        }
        
        if (bySkill_to_pass == FALSE) {
          Populations[[skill]] <- as.numeric(select(ungroup(Master_data), 
                                                              starts_with("Population_type_"),
                                                              -contains("College"))[1, ])
          
        }
        
      }
      
      #Calculating spending shares and stringency codes:
      spendShares <- list()
      stringencyCode <- list()
      
      for (skill in bySkillVector) {
        
        #Spending shares...
        spendShares[[skill]] <- pmax( 
                                      (1 - demandParameters[1])*pmin( (price*demandParameters[2])/(Master_data[[paste0(skill, "Wage")]]*abilityVector) , 1) + demandParameters[1], #Stone geary shares if unconstrained...
                                                            #Create 7x2 matrix of spending shares absent regulation
                                                            pmin( housing_stringency/(Master_data[[paste0(skill, "Wage")]]*abilityVector) , 1)  #Constrained, housing spend share is minimal lot expenditure/income
                                                            #matrix multiply column vector with row vector, creates 7x2 matrix by zone. 
                                      ) #Taking pmax of unregulated/regulated spending shares
        #Regulation codes...
        stringencyCode[[skill]][spendShares[[skill]] == 1] <- 3 #if spending all income at this location, set spending code == 3
        #if constrained by regulation, set code to 2
        stringencyCode[[skill]][ (spendShares[[skill]] < 1) & 
                                   ( (housing_stringency/(Master_data[[paste0(skill, "Wage")]]*abilityVector)) > 
                                       (1 - demandParameters[1])*(price*demandParameters[2])/(Master_data[[paste0(skill, "Wage")]]*abilityVector) + demandParameters[1]) ] <- 2   
        #if unconstrained, set code to 1: 
        stringencyCode[[skill]][ (spendShares[[skill]] < 1) & 
                                   ( (housing_stringency/(Master_data[[paste0(skill, "Wage")]]*abilityVector)) <= 
                                       (1 - demandParameters[1])*(price*demandParameters[2])/(Master_data[[paste0(skill, "Wage")]]*abilityVector) + demandParameters[1]) ] <- 1
        
      }
      
      #Consumption values
      betaFactor <- ((1-demandParameters[1])^(1-demandParameters[1]))*((demandParameters[1])^(demandParameters[1])) #factor of proportionality for consumption index. (See formula!)
      
      #Instantiate consumption index (utility by zone and income)
      consValue <- list()
      
      #Solving for consumption index at current prices...
      for (skill in bySkillVector) {
        
        consValue[[skill]] <-   ((Master_data[[paste0(skill, "Wage")]]*abilityVector - demandParameters[2]*price)/(price^demandParameters[1]))* #Overall consumption index (undistorted)
                                                        #distortion factor     
                                 ( ifelse(stringencyCode[[skill]] == 3, 0, 0) + #if completely constrained, earn zero utility
                                   ifelse(stringencyCode[[skill]] == 2, (((Master_data[[paste0(skill, "Wage")]]*abilityVector - housing_stringency)/(Master_data[[paste0(skill, "Wage")]]*abilityVector - demandParameters[2]*price))^(1-demandParameters[1]))*
                                                                        (((housing_stringency - price*demandParameters[2])/(Master_data[[paste0(skill, "Wage")]]*abilityVector - demandParameters[2]*price))^(demandParameters[1]))/betaFactor, 0) + #
                                   ifelse(stringencyCode[[skill]] == 1, 1, 0) ) #if unconstrained, earn distortion index == 1. 
        
      }
      
      #Getting index from this consumption value: by X/ (ability = z) to make computation of this large number possible. !Has no impact on counterfactual outcomes if wN_elast is large enough!.
      consIndex <- list()
      for (skill in bySkillVector) {
        consIndex[[skill]] <- consValue[[skill]]/abilityVector 
      }
      
      #Finally, calculating housing demand..
      housing_demand_exp <- 0 
      
      for (skill in bySkillVector) {
        housing_demand_exp <- housing_demand_exp + sum(spendShares[[skill]]*Master_data[[paste0(skill, "Wage")]]*abilityVector*Populations[[skill]])
      }
      
      
      #Finally, returning what we want:
      return(list(price = Master_data$hedonicPrice[1],
                  consumption_Index = consIndex,
                  housing_demand_expenditure = housing_demand_exp,
                  Populations = NA, #Populations not required, no zone differences
                  ExcessDemand = 0,
                  RegulationCode = Master_data$Regulation_code,
                  housingExpenditureShare = spendShares,
                  Geo = c(Master_data$State[1], Master_data$County[1], Master_data$Tract[1], Master_data$BlockGroup[1])))
      
    }#End case 2
    
  }
  
