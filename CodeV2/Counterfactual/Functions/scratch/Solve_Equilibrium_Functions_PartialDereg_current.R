#This file reads in and compiles all functions used in the companion file Solve_Current_Equilibrium_allSpecs_PartialDereg.R
library(compiler) #Compile function for added speed. Short functions don't need to be compiled, very little speed advantages



#_____________________________________________________________________________________________________________________________________________
#This function solves housing market equilibrium KEEPING SPENDING SHARES FIXED (EXOGENOUS). 
#Pass whole data frame, not row-by-row.

getHousingPrices <- function(Master_data, zone) { #Do this twice for each zone
  
  h_spending <- 0
  
  if (zone == 1) {
    land_name <- "land_regulated"
  }
  if (zone == 2) {
    land_name <- "land_unregulated"
  }
  
  
  for (skill in skillVector) {
    name_of_skill <- skillName[which(skill == skillVector)]
    for (incomeType in 1:7) {
      
      h_spending <- h_spending    +   Master_data[[paste0("hSpendShare_", name_of_skill, incomeType, "_z", zone)]]* #Spending Share (not updated every iteration)
                                      Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]]* #Income
                                      Master_data[[paste0("Population_type_", name_of_skill, incomeType, "_z", zone)]] #Population
    }
  }
  
  price <- (h_spending/(Master_data[[land_name]]*Master_data$lambda))^(1/(Master_data$HS_Elasticity_imputed + 1)) #Only be finite if land in regulated zone exists
  
  return(price)
  
}
getHousingPrices <- cmpfun(getHousingPrices) #compiling function
#_____________________________________________________________________________________________________________________________________________

#This function solves for consumption values GIVEN SET OF MINIMUM LOT SIZES
getConsumptionValues <- function(Master_data, skill, incomeType, zone, demandParameters) {
  #Skill Name
  name_of_skill <- skillName[which(skill == skillVector)]
  
  #if zone regulated, use observed min lot sizes
  if (zone == 1) {
    price <- Master_data$housingPrice_z1
    housing_stringency <- IncomeStringency_ctfl #if zone == 1, pass income stringency counterfactual
  }
  if (zone == 2) { #if unregulated, minimumlotsizes == 0
    price <- Master_data$housingPrice_z2
    housing_stringency <- rep(0, nrow(Master_data))
  }
  
  #Calculating stringency vector 
   #lambda * price (pow) * minimum lot size
  
  
  #demandParameters[1] is beta
  #demandParameters[2] is the minimum housing unit requirement
  
  #beta factor (algebra)
  betaFactor <- ((1-demandParameters[1])^(1-demandParameters[1]))*((demandParameters[1])^(demandParameters[1])) #Porportional adjustment factor (comes out of the Cobb-Douglas algebra)
  
  
  #1. FIGURING OUT WHAT SPENDING SHARES WOULD BE AT CURRENT HOUSING MARKET PRICES
  #Vector of spending shares
  spendShares <-          pmax(
    
                              (1-demandParameters[1])*pmin((price*demandParameters[2])/(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]]), 1) + demandParameters[1], #Stone geary shares if unconstrained
    
                               pmin(housing_stringency/(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]]), 1) ) #
  
  
  
  #2. CREATING DUMMIES THAT TELL US IF CONSUMER IS CONSTRAINED AT CURRENT ITERATION
  #Stringency indicators
  stringencyCode <-   ifelse(spendShares == 1, 3, 0) + #IF FULLY CONSTRAINED, spending shares == 100% of income
                      ifelse(spendShares == housing_stringency/(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]]), 2, 0) +  #partially constrained                               
                      ifelse(spendShares == (1-demandParameters[1])*pmin((price*demandParameters[2])/(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]]), 1) + demandParameters[1], 1, 0)  #unconstrained                             
  
  
  
  #3. START CONSUMPTION VALUE
  consumptionValue <- pmax(0,(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] - demandParameters[2]*price)/(price^demandParameters[1]))*
                          (       ifelse(stringencyCode == 3, 0, 0) + #If completely constrained, earn consumption index == 0)
                                  ifelse(stringencyCode == 2, (((Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] - housing_stringency)/(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] - demandParameters[2]*price))^(1-demandParameters[1]))*
                                                              (((housing_stringency - price*demandParameters[[2]])/(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] - demandParameters[2]*price))^(demandParameters[1]))/betaFactor, 0) + 
                                  ifelse(stringencyCode == 1, 1, 0)  #If unconstrained, do not change main consumption index
                                                                              )
  #NOTE: consumptionValue NOT ADJUSTED FOR adjustmentFactor_temp for computation, so that needs to be done afterward when doing wN_elasticity 
  
    
  return(list(consumptionValue = consumptionValue,
              spendShares = spendShares))
  
  
} #End function
getConsumptionValues <- cmpfun(getConsumptionValues) #compiling function
#_____________________________________________________________________________________________________________________________________________


#Zone level consumption aggregators + adjusting scale of consumption
ZoneAggregation <- function(Master_data, skill, incomeType) {
  
  #Preliminaries
  name_of_skill <- skillName[which(skill == skillVector)]
  adjustment_factor_temp <- Master_data[[paste0("ability_grp", incomeType)]][1] #computational adjustment factor used in Calibrate_consumptionValues_SupplyShifters_all.R

  #Master_data has correctly named consumption value from previous column
  consValue_adjusted <- matrix(NA, nrow(Master_data), 2)
  
  #Loop over zones
  for (z in c(1, 2)) {
    consValue_adjusted[, z] <- exp(Master_data[[paste0("uncl_cons_zone_", name_of_skill, incomeType, "_z", z)]]/adjustment_factor_temp) #adjustment_factor_temp purely computational to calculate exp(large number)
  }
  
  #Replacing NA's with zeros in consValue_adjusted (i.e. with zero land mass)
  #consValue_adjusted[is.na(consValue_adjusted)] <- 0
  
  #Aggregating
  consIndex <- (rowSums((consValue_adjusted^(wN_elast_ctfl))*regulated_unit_shares, na.rm = TRUE))^(1/wN_elast_ctfl)

  frac_pop_zone <- matrix(NA, nrow(Master_data), 2) #population fractions
  
  for (z in c(1, 2)) {
    frac_pop_zone[, z] <- ((consValue_adjusted[, z]^(wN_elast_ctfl))*regulated_unit_shares[, z])/(consIndex^(wN_elast_ctfl))
  }
  frac_pop_zone[is.nan(frac_pop_zone)] <- 1 #NaN's map to one where population failed to calculate
  frac_pop_zone[is.na(frac_pop_zone)] <- 0 #NA's map to zero population (see above, location empty for this type)
  #Note: some zones have zero land mass. Correcting for this.
  
  #returning adjusted consumption index as well that aggregates utility to neighborhood
  consIndex <- log(consIndex)*adjustment_factor_temp*consumption_AdjustmentFactor[[skill, paste0("consumption_Adjustment", incomeType)]] #This is our consValue!
  
  return(list(Zone_fraction = frac_pop_zone,
              consVal = consIndex
                ))
} #Returns consumption values and zone fractions!

ZoneAggregation <- cmpfun(ZoneAggregation) #compiling function
#_____________________________________________________________________________________________________________________________________________





#
#______________________________________________________________________________________________________________________________________________
# Basic functions that remain the same for both complete and partial deregulation

#Function to retrieve average income
getAvgIncome <- function(Master_data) {
  
  total_income <- rep(0, nrow(Master_data))
  total_population <- rep(0, nrow(Master_data))
  
  for (skill in skillVector) {
    name_of_skill <- skillName[which(skill == skillVector)]
    for (incomeType in 1:7) {
      total_income <- total_income + Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]]*Master_data[[paste0("Population_type_", name_of_skill, incomeType)]]
      total_population <- total_population + Master_data[[paste0("Population_type_", name_of_skill, incomeType)]]
    }
  }
  
  return(total_income/total_population)
  
}

#Function to update location amenity
getLocationAmenity <- function(Master_data, skill, incomeType) {
  
  name_of_skill <- skillName[which(skill == skillVector)]
  
  Amenity <- Master_data[[paste0("exogenous_Amenity_", name_of_skill, incomeType)]]*(Master_data$Avg_income^(Omega[incomeType])) #Requires Avg_income in dataframe and exogenous amenity value
  
}

#_______________________________________________________________________________________________________________
#Small function for parralel means -- source https://rdrr.io/github/tanaylab/tgutil/src/R/utils.R
pmean <- function(..., na.rm = FALSE) {
  d <- do.call(cbind, list(...))
  res <- rowMeans(d, na.rm = na.rm)
  idx_na <- !rowMeans(!is.na(d))
  res[idx_na] <- NA
  return(res)
}
