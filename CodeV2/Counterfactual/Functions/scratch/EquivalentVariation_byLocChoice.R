#This file calculates the equivalent variation (expressed in growth in every state) using a simple model-implied procedure.
library(compiler)



#Function that calculates city consumption values by location and zone for arbitrary model assumptions. Used to calculate equivalent variation
getConsValuesVariation <- function(Master_data, skill, incomeType, demandParameters, 
                          capitalGains = FALSE, FullDereg) {
  
   #Capital gains changes income paid
    if (capitalGains == TRUE) {
      housingWealth_change <- Master_data[[paste0("Housing_wealth_change_Owner_", name_of_skill, incomeType)]]
    
    }else{
      housingWealth_change <- rep(0, nrow(Master_data))
    }
  
    #Skill Name
    name_of_skill <- skillName[which(skill == skillVector)]
    
    #first, calculate consumption values in each neighborhood 
    #Note: this requires calculating consumption indices from an equilibrium with a mix of regulated and unregulated structures. 
    
    #Stringency...
    housing_stringency <- list()  
    price <- list()
    
    if (FullDereg == FALSE) {
      housing_stringency[[1]] <- Master_data$IncomeStringency_model_rents #use values of regStringency currently in data frame
      price[[1]] <- Master_data$price_regulated    #prices at initial equilibrium
      price[[2]] <- Master_data$price_unregulated  #
      
    } else if (FullDereg == TRUE) {
      housing_stringency[[1]] <- rep(0, nrow(Master_data))
      price[[1]] <- Master_data$housingPrice   #Prices in both zones are equal, identical locations
      price[[2]] <- Master_data$housingPrice   #
    }
    
    housing_stringency[[2]] <- rep(0, nrow(Master_data))
    
  
    #Initial regulated housing unit shares
    #regulated_unit_shares <- matrix(NA, nrow(Master_data), 2)
    #regulated_unit_shares[, 1] <- Master_data$regulated_housingUnit_share
    #regulated_unit_shares[, 2] <- 1 - Master_data$regulated_housingUnit_share
    
    #Calculating spendshares + Stringnecy code
    #Initializing
    spendShares <- list(rep(NA, nrow(Master_data)), rep(NA, nrow(Master_data)))
    stringencyCode <- list(rep(0, nrow(Master_data)), rep(0, nrow(Master_data))) #Tells us a stringencyCode, whether we are unconstrained 1, partially constrained 2 or fully constrained by minimum lot size 3
    
    #loop over zones
    for (zone in c(1, 2)) {
      #Spending shares
      spendShares[[zone]] <- pmax(
        
        (1-demandParameters[1])*pmin((price[[zone]]*demandParameters[2])/(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] + housingWealth_change ), 1) + demandParameters[1], #Stone geary shares if unconstrained
        
        pmin(housing_stringency[[zone]]/(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] + housingWealth_change ), 1) ) #Constrained, housing spend share is minimal lot exp/income
      #Stringency indicators
      stringencyCode[[zone]] <-   ifelse(spendShares[[zone]] == 1, 3, 0) + #IF FULLY CONSTRAINED, spending shares == 100% of income
        ifelse(spendShares[[zone]] == housing_stringency[[zone]]/(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] + housingWealth_change ), 2, 0) +  #partially constrained                               
        ifelse(spendShares[[zone]] == (1-demandParameters[1])*pmin((price[[zone]]*demandParameters[2])/(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] + housingWealth_change ), 1) + demandParameters[1], 1, 0)                               
      
    }
    
    #consumption values by zone
    consValue <- list(rep(NA, nrow(Master_data)), rep(NA, nrow(Master_data)))
    
    betaFactor <- ((1-demandParameters[1])^(1-demandParameters[1]))*((demandParameters[1])^(demandParameters[1])) #Porportional adjustment factor (comes out of the Cobb-Douglas algebra)
    
    for (zone in c(1, 2)) { 
      
      consValue[[zone]] <- pmax(0,(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] + housingWealth_change  - demandParameters[2]*price[[zone]])/(price[[zone]]^demandParameters[1]))*
        (       ifelse(stringencyCode[[zone]] == 3, 0, 0) + #If completely constrained, earn consumption index == 0)
                  ifelse(stringencyCode[[zone]] == 2, (((Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] + housingWealth_change  - housing_stringency[[zone]])/(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] + housingWealth_change  - demandParameters[2]*price[[zone]]))^(1-demandParameters[1]))*
                           (((housing_stringency[[zone]] - price[[zone]]*demandParameters[[2]])/(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] + housingWealth_change  - demandParameters[2]*price[[zone]]))^(demandParameters[1]))/betaFactor, 0) + 
                  ifelse(stringencyCode[[zone]] == 1, 1, 0)  #If unconstrained, do not change main consumption index
        )
    }
    
    #Now, creating neighborhood value using consumption adjustment factor and...
    V_n <- list()
    V_n[[1]] <- consValue[[1]]*consumption_AdjustmentFactor[[skill, paste0("consumption_Adjustment", incomeType)]] + log(Master_data[[paste0("Amenity_", name_of_skill, incomeType)]])
    V_n[[2]] <- consValue[[2]]*consumption_AdjustmentFactor[[skill, paste0("consumption_Adjustment", incomeType)]] + log(Master_data[[paste0("Amenity_", name_of_skill, incomeType)]])
    
    
   return(V_n)
  
}


aggregateVariation <- function(Initial_data, Counterfactual_data, #Pass initial and counterfactual data frames
                               skill, incomeType, demandParameters,
                               capitalGains = FALSE, FullDereg) {  
  
  #Skill name in data frame
  name_of_skill <- skillName[which(skill == skillVector)]
  
  #set capitalGains == TRUE if you want to incorporate capital gains at the counterfactual equilibrium
  
  if (capitalGains == TRUE) {
    housingWealth_change <- Initial_data[[paste0("Housing_wealth_change_Owner_", name_of_skill, incomeType)]]
  }else{
    housingWealth_change <- rep(0, nrow(Initial_data))
  }
  
  #Get V_n, the value of a neighborhood/zone in counterfactual equilibrium
  V_n <- getConsValuesVariation(Master_data = Counterfactual_data, skill = skill, incomeType = incomeType,
                                demandParameters = demandParameters, capitalGains = capitalGains, FullDereg = FullDereg)
  
  #Defining "adjustment factor", see formula in the text
  adjustment_factor <- consumption_AdjustmentFactor[[skill, paste0("consumption_Adjustment", incomeType)]]*(demandParameters[1]^(-demandParameters[1]))*((1-demandParameters[1])^(-(1-demandParameters[1])))
  
  #Data frame to organize function
  EqVar_df <- select(Init_eq, CBSA, CBSA_NAME)
  
  #Calculating preliminaries
  EqVar_df["P_BarA"] <- Initial_data$price_regulated*demandParameters[2]
  EqVar_df["IncomeStringency_model_rents"] <- Initial_data$IncomeStringency_model_rents
  
  #Take amenities for reference
  EqVar_df["InitAmenity"] <- Initial_data[[paste0("Amenity_", name_of_skill, incomeType)]]
  
  #1. Calculate equivalent variation assuming housing consumption constrained at value of minimal lot R (see formula in paper)
  EqVar_df["EqVar_constrained_reg"] <- ( ( ((V_n[[1]] - log(Initial_data[[paste0("Amenity_", name_of_skill, incomeType)]]))/adjustment_factor)^(1/(1-demandParameters[1])) )*
                                         ( ((Initial_data$price_regulated)/(Initial_data$IncomeStringency_model_rents - Initial_data$price_regulated*demandParameters[2]))^(demandParameters[1]/(1 - demandParameters[1])) ) + 
                                        Initial_data$IncomeStringency_model_rents + housingWealth_change)/(Initial_data[[paste0(skill, "Wage")]]*Initial_data[[paste0("ability_grp", incomeType)]])
                                          
                                          #NaNs/-Inf imply either 1) nobody is sorting in this neighborhood (zero amenities) or EqVar = 1 because households still priced out of market OR R > P_Bar{A}
  
  #Calculating which households are constrained after compensation
  EqVar_df["constrained_status"] <- ifelse(demandParameters[1]*EqVar_df$EqVar_constrained_reg*Initial_data[[paste0(skill, "Wage")]]*Initial_data[[paste0("ability_grp", incomeType)]] + 
                                           (1-demandParameters[1])*Initial_data$price_regulated*demandParameters[1] < Initial_data$IncomeStringency_model_rents, 1, 0) #unconstrained expenditures > constrained expenditures
  
  #Set constrained status = 0 if R < PbarA (must be unconstrained)
  EqVar_df$constrained_status[EqVar_df$P_BarA > EqVar_df$IncomeStringency_model_rents] <- 0
  
  #
  
  #2. Calculate equivalent variation assuming housing consumption not constrained (see formula in paper)
  EqVar_df["EqVar_unconstrained_reg"] <- (( V_n[[1]] - log(Initial_data[[paste0("Amenity_", name_of_skill, incomeType)]]) )*(Initial_data$price_regulated^demandParameters[1])/(consumption_AdjustmentFactor[[skill, paste0("consumption_Adjustment", incomeType)]]) + 
                                          Initial_data$price_regulated*demandParameters[2] - housingWealth_change)/(Initial_data[[paste0(skill, "Wage")]]*Initial_data[[paste0("ability_grp", incomeType)]])
    
  #3. Selecting from constrained and unconstrained
  EqVar_df["EqVar_reg"] <- ifelse(EqVar_df$constrained_status == 1, 
                                  yes =  EqVar_df$EqVar_constrained_reg,
                                  no = EqVar_df$EqVar_unconstrained_reg)
  
  #Calculate equivalent variation in the unregulated zone (demands' unconstrained no matter what)
  EqVar_df["EqVar_unreg"] <- (( V_n[[2]] - log(Initial_data[[paste0("Amenity_", name_of_skill, incomeType)]]) )*(Initial_data$price_unregulated^demandParameters[1])/(consumption_AdjustmentFactor[[skill, paste0("consumption_Adjustment", incomeType)]]) + 
                                 Initial_data$price_regulated*demandParameters[2] - housingWealth_change)/(Initial_data[[paste0(skill, "Wage")]]*Initial_data[[paste0("ability_grp", incomeType)]])
  
  
  #4.Taking welfare formula replaced with this equivalent variation
  #Taking populations from initial data, calculating city conditional shares and city shares for given type
  for (zone in c(1, 2)) {
    EqVar_df[paste0("zone_pop_frac_z", zone)] <- Init_eq[[paste0("Population_type_", name_of_skill, incomeType, "_z", zone)]]/
                                                (Init_eq[[paste0("Population_type_", name_of_skill, incomeType, "_z1")]] + Init_eq[[paste0("Population_type_", name_of_skill, incomeType, "_z2")]])
                                               
  }
  
  #Across zone welfare
  EqVar_df["Welfare_ac_zone"] <- (EqVar_df$zone_pop_frac_z1*(EqVar_df$EqVar_reg)^(wN_elast) + EqVar_df$zone_pop_frac_z2*(EqVar_df$EqVar_unreg)^(wN_elast))^(1/wN_elast)
  #Set welfare effect to zero if NaN -- neighborhood becomes poor
  EqVar_df$Welfare_ac_zone[is.na(EqVar_df$Welfare_ac_zone)] <- 0
    
  #Now, creating city pop fractions
  EqVar_df["cityPop_frac"] <- Init_eq[[paste0("Population_type_", name_of_skill, incomeType)]]/Init_eq[[paste0("cityPopulation_type_", name_of_skill, incomeType)]]
  
  
  
  #Creating city welfare index 
  EqVar_df <-  EqVar_df %>% group_by(CBSA) %>% mutate(city_welfare = sum( (Welfare_ac_zone^(rho))*(cityPop_frac), na.rm = TRUE  )^(1/rho))
  
}