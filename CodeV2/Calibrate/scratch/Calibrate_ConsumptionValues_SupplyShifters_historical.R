#To do: Merge this file to Calibrate_ConsumptionValues_SupplyShifters_all.R

#Date created: November 23, 2023
library(dplyr)
library(haven)
library(labelled)
library(readr)
library(doParallel) #for parallel computing on calibration.
library(collapse)
library(estimatr)

#for plotting same relationship with calibration
library(mgcv)
library(ggplot2)
library(gratia)

#This file calibrates 2008-2012 consumption values to calculate for placebo check.
#This is virtually identical 

###_________PARAMETERS_________########
source("CodeV2/Calibrate/Parameters/GlobalParameters.R")

#Preferred specification for estimation (see Calibrate_amenities module)
BASELINE_SPECIFICATION <- list(pref = "SG", bySkill_to_pass = FALSE)


#Importing sample geography
SmpleGeo <- read_dta("DataV2/US_Data/Output/SampleGeography.dta") %>% select(-CBSA,)

#Importing raw regulation file
Regulation <- read_dta("DataV2/US_Data/Output/Constructed_Block_V2.dta") %>% 
  select(State, County, Tract, BlockGroup, CBSA, UnitDensityRestriction_cl)
Regulation$UnitDensityRestriction_cl[is.na(Regulation$UnitDensityRestriction_cl)] <- 0

#Land value density from raw regulation file
Regulation_V2 <- read_dta("DataV2/US_Data/Output/raw_Regulation_ByBlockGroup.dta") %>% select(State, County, Tract, BlockGroup, LandValueDensity_hist)

#Importing housing prices (note: difference in sample size is what we could construct from CoreLogic transactions + assessment coverage)
HedonicIndex <- read_dta("DataV2/US_Data/Output/FullHedonicIndex_complete_historical.dta") %>% select(-CBSA, hedonicPrice_regulated, hedonicPrice_total)

#Housing supply elasticities (assume time constant)
HS_Elasticities <- read_dta("DataV2/US_Data/Output/HS_Elasticities.dta") %>% select(-CBSA, -CBSA_NAME)

#Spending shares for each income type (to choose preference parameters to match demand)
SpendShare <- read_dta("DataV2/US_Data/Output/HousingSpendshare_ByCity.dta") %>% select(starts_with("Income_spendshare_owner"), -ends_with("1")) #Note: median income spendshare is 0.25-- roughly in line with estimates from other data

#LOCAL ABILITY DISTRIBUTIONS.  
LocalAbilityDist <- read_dta("DataV2/US_Data/Output/LocalAbilityDistributions_historical.dta") %>% select(State, County, Tract, BlockGroup, starts_with("Population"),
                                                                                                          total_housing_units_cen, regulated_housingUnit_share)

#Wages
Wages <- read_dta("DataV2/US_Data/Output/CityProd_individual_historical.dta")
Wages_V2 <- read_dta("DataV2/US_Data/Output/CityProd_individual.dta") %>% select(CBSA, starts_with("ability_grp")) #ability measures of center

#Joining this all to master
Master <- left_join(HedonicIndex, SmpleGeo, by = c("State", "County", "Tract", "BlockGroup"))
Master <- left_join(Master, Regulation, by = c("State", "County", "Tract", "BlockGroup"))
Master <- left_join(Master, Regulation_V2, by = c("State", "County", "Tract", "BlockGroup"))
Master <- left_join(Master, Wages, by = c("CBSA"))
Master <- left_join(Master, Wages_V2, by = c("CBSA"))
Master <- left_join(Master, LocalAbilityDist, by = c("State", "County", "Tract", "BlockGroup"))
Master <- left_join(Master, HS_Elasticities, by = c("State", "County", "Tract", "BlockGroup"))

rm(HedonicIndex, HS_Elasticities, 
   LocalAbilityDist, 
   Regulation, Wages, Wages_V2, SpendShare,
   SmpleGeo, Regulation_V2)

#Normalizing average income type in midpoint income group to 1 to measure welfare
adjustment_factor_temp <- as.numeric(select(Master, starts_with("ability_grp"))[1,]) #divide by ability_grp for computational purposes 

#Dropping block groups with zero land mass and zero population (this is the same sample of block groups used in the facts.)
Master <- Master[Master$total_housing_units_cen > 0 & Master$ALAND > 0 & !is.na(Master$ALAND) & !is.na(Master$total_housing_units_cen),]

#With measured supply elasticity (either via MSA average or not)
Master <- Master[!is.na(Master$HS_Elasticity_imputed),]

#If no coverage measuring share of housing units in regulated structures, remove
Master <- Master[!is.na(Master$regulated_housingUnit_share),] 

#Set UnitDensityRestriction == 0 for block groups with no regulated housing units
Master$UnitDensityRestriction_cl[Master$regulated_housingUnit_share == 0] <- 0

#Dropping block groups that have no measured pooled wage (see construction-- we drop 3 small CBSAs that are very close by to others.)
Master <- Master[!is.na(Master$PooledWage),] #only a few hundred block groups dropped

print(paste0("There are ", nrow(Master), " block groups remaining in the final sample."))

#Arranging data
Master <- Master %>% arrange(State, County, Tract, BlockGroup)
Master <- Master %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, everything())

#Genering dummies for completely regulated, completely unregulated and mixed block groups
Master["Regulation_code"] <- rep(NA, nrow(Master))

Master$Regulation_code[Master$UnitDensityRestriction_cl == 0 | Master$regulated_housingUnit_share == 0] <- 0 #Locations with no person(s) in regulated structures
Master$Regulation_code[(Master$regulated_housingUnit_share > 0 & Master$regulated_housingUnit_share < 1) & Master$UnitDensityRestriction_cl > 0] <- 1 #Some people in regulated structures
Master$Regulation_code[(Master$regulated_housingUnit_share == 1) & Master$UnitDensityRestriction_cl > 0] <- 2 #"Totally" regulated

#Setting regulation == 0 for block groups assigned a regulation code of 0
Master$UnitDensityRestriction_cl[Master$Regulation_code == 0] <- 0

#Note: lambda is not defined for zero regulated neighborhoods (it is not separately identifiable from land mass). So set land mass in these block groups == ALAND. (measured land mass of the tract)

#Creating hedonicPrice variable from hedonic regressions.
Master["hedonicPrice"] <- rep(NA, nrow(Master))
Master$hedonicPrice[Master$Regulation_code == 1 | Master$Regulation_code == 2] <- Master$hedonicPrice_regulated[Master$Regulation_code == 1 | Master$Regulation_code == 2]
Master$hedonicPrice[Master$Regulation_code == 0] <- Master$hedonicPrice_total[Master$Regulation_code == 0]


#_____________________________________________________________________________________________________________________________________________________________________
#PART 1 OF CALIBRATION________________________________________________________________________________________________________________________________________________
#PART 1: Calculating the lambdas that corresponds to the hedonic indices we observe in the data.
#_____________________________________________________________________________________________________________________________________________________________________


#1.1: Measure of stringency of regulation = LandValueDensity/acre x Minimum lot size (acres)

#Normalizing land value density == 0 if UnitDensityRestriction == 0 --this does NOT matter for unregulated neighborhoods.
Master$LandValueDensity_hist[Master$UnitDensityRestriction_cl == 0 | is.na(Master$LandValueDensity_hist)] <- 0


#Assume implicit rent == 0.06*house value (maintenance cost of housing + interest payments)
#Convert_value_to_yr_flow_cost is in GlobalParameters, telling us how to make the adjustment. 
Master["IncomeStringency_model_rents"] <- (Master$UnitDensityRestriction_cl*Master$LandValueDensity_hist)*(Convert_value_to_yr_flow_cost)


#________________CENSORTING ULTRA STRINGENT NEIGHBORHOODS__________________________
#Note, for historical calibration, we only use block groups with overlap.

#Removing ultra rich neighborhoods to make sure they don't drive results.
print(paste0("There are ", nrow(Master[Master$IncomeStringency_model_rents > regulation_censoring,]), " block groups that will have censored stringency"))
Master <- Master[Master$IncomeStringency_model_rents <= regulation_censoring, ] #Deleting...
#We don't want a handful of ultra-stringent neighborhoods influencing our results, 
#because we also censor the income distribution by using the ACS data. 


#Normalizing prices so that mean == 1
Master$hedonicPrice <- Master$hedonicPrice/mean(Master$hedonicPrice, na.rm = TRUE)
#____________________________________________________________________________________

#Median rent at the minimum lot size is $6500/year (after adjustments from merge_stringency.R -- remove locations with regulated lot size > 2x average lot size)
print(paste0("The median income stringency (model based) is ", median(Master$IncomeStringency_model_rents)))
#7,326 after controlling
print(paste0("The median income stringency (conditional on positive regulation) is ", median(Master[Master$UnitDensityRestriction_cl > 0,]$IncomeStringency_model_rents)))

#highly skewed
print(paste0("The mean income stringency is ", mean(Master$IncomeStringency_model_rents)))
print(paste0("The mean income stringency (conditional on positive regulation) is ", mean(Master[Master$UnitDensityRestriction_cl > 0,]$IncomeStringency_model_rents))) #Conditional on a unit density restriction


#____________START CALIBRATION HERE_______________________
bySkill_to_pass <- BASELINE_SPECIFICATION$bySkill_to_pass
pref <- BASELINE_SPECIFICATION$pref #loading preferred specification parameters

#Setup variables to pass to functions
if (bySkill_to_pass == TRUE) {
  
  skillVector <-  c("College", "NoCollege")
  skillName <- c("College_", "NoCollege_") 
  
  source("CodeV2/Calibrate/Functions/Calibration_Functions_StoneGeary_bySkill.R")
  
  
}else{
  
  skillVector <- c("Pooled")
  skillName <- c("")
  
  source("CodeV2/Calibrate/Functions/Calibration_Functions_StoneGeary.R")
  
  
}

if (pref == "CD"){ #if cobb-douglas preferences
  
  demandParameters_to_pass <- c(beta, 0) #min_hReq = 0  => Cobb-Douglas
  rm(min_hReq, beta_StGeary)
  
}else{ #if stone geary preferences, pass new preference parameters
  
  demandParameters_to_pass <- c(beta_StGeary, min_hReq)
}


#START CALIBRATION _____________________________________________

#Registering doParallel
ncores  <- detectCores() - 1

#Setting up number of cores (we want to conserve ram as this program is reasonably ram intensive)
registerDoParallel(ncores)

range <- 1:nrow(Master)

#Running dopar loop to solve for all variables
print(paste0("Calibrating model bySkill=", bySkill_to_pass, " ", pref, " at ", Sys.time()))  
tmp <- foreach(row = range,
               .errorhandling = "pass") %dopar% { 
                 
                 #FOREACH output
                 return(Calibrate_prices(Master[row,], demandParameters = demandParameters_to_pass))
                 
               }
print(paste0("Calibrated model bySkill=", bySkill_to_pass, " ", pref, " at ", Sys.time()))  

#closing cluster
stopImplicitCluster()

#Saving temp object as checkpoint (previous object takes about two hours to run on 10 cores)
save(tmp, file = "DataV2/Counterfactuals/Calibration_Output/SupplyShifter_sln.RData")


#Putting all these objects into the master data frame for use with other objects

Excess_demand_index <- rep(NA, nrow(Master))
price_reg <- rep(NA, nrow(Master))
lambda <- rep(NA, nrow(Master))
consumptionValIndex <- list()

houseExp_regulated <- rep(NA, nrow(Master))
houseExp_unregulated <- rep(NA, nrow(Master))


for (skill in skillVector) {
  for (incomeType in 1:7) {
    consumptionValIndex[[skill]][[incomeType]] <- rep(NA, length(tmp))
    
  }
}

#Storing list objects in vector for before storing in data frame (this is a lot faster)
for (row in 1:length(tmp)) {
  if (is.numeric(tmp[[row]][[1]][1])) {
    
    Excess_demand_index[row] <- tmp[[row]]$ExcessDemand
    price_reg[row] <- tmp[[row]]$price
    
    houseExp_regulated[row] <- tmp[[row]]$housing_demand_expenditure[1]
    houseExp_unregulated[row] <- tmp[[row]]$housing_demand_expenditure[2]
    
    for (skill in skillVector) {  
      for (incomeType in 1:7) {
        consumptionValIndex[[skill]][[incomeType]][row] <- tmp[[row]]$consumption_Index[[skill]][incomeType]
        
      }
    }
    
  }
}


#Creating output of master dataset to save
Master_out <- Master


#Regulated housing price
Master_out["price_regulated"] <- Master_out$hedonicPrice #measured hedonic price for regulated structures only
Master_out["price_unregulated"] <- price_reg

#If completely regulated/unregulated
Master_out$price_unregulated[Master_out$Regulation_code == 2 | Master_out$Regulation_code == 0] <- Master_out$price_regulated[Master_out$Regulation_code == 2 | Master_out$Regulation_code == 0] #setting reg=unreg price if no within-block-group variation in regulation

#Excess demand index at current solutions
Master_out["ExcessDemand"] <- Excess_demand_index

#checking maximum deviation from equilibrium
print(paste0("The max excess demand (reg share) is ", max(abs(Master_out$ExcessDemand), na.rm = TRUE))) #pretty close.

#Putting consumption values in data frame
for (skill in skillVector) {
  for (incomeType in 1:7) {
    Master_out[paste0("consumption_Val_", skillName[which(skill == skillVector)], incomeType)] <- consumptionValIndex[[skill]][[incomeType]]
    
  }
}


#Calculating lambda
Master_out["lambda"] <- rep(NA, nrow(Master_out))

#If regulation_code == 1 or 2, calculate lambda the traditional way (to target the measured land value per acre)
Master_out$lambda[Master_out$Regulation_code == 1 | Master_out$Regulation_code == 2] <- (Master_out$IncomeStringency_model_rents[Master_out$Regulation_code == 1 | Master_out$Regulation_code == 2]/Master_out$UnitDensityRestriction_cl[Master_out$Regulation_code == 1 | Master_out$Regulation_code == 2])/
  (Master_out$price_regulated[Master_out$Regulation_code == 1 | Master_out$Regulation_code == 2]^(Master_out$HS_Elasticity_imputed[Master_out$Regulation_code == 1 | Master_out$Regulation_code == 2] + 1))

#Otherwise, assume land mass == ALAND and calculate lambda (for these regions this is a normalization and thus has NO impact on counterfactuals. This is because land mass and productivity per unit of land serve identical functions)
Master_out$lambda[Master_out$Regulation_code == 0] <- houseExp_regulated[Master_out$Regulation_code == 0]/(Master_out$ALAND[Master_out$Regulation_code == 0]*(Master_out$price_regulated[Master_out$Regulation_code == 0]^(Master_out$HS_Elasticity_imputed[Master_out$Regulation_code == 0] + 1)))

#Now, solving for land to clear housing markets
Master_out["land_regulated"] <- rep(NA, nrow(Master_out))
Master_out["land_unregulated"] <- rep(NA, nrow(Master_out))

#if Regulation_code == 0, set ALAND = land (as a normalization!!)
Master_out$land_unregulated[Master_out$Regulation_code == 0] <- Master_out$ALAND[Master_out$Regulation_code == 0] 
Master_out$land_regulated[Master_out$Regulation_code == 0] <- 0

#If Regulation_code == 1 or 2, calculate regulated land the normal way (to clear housing markets) (this is the ratio between total housing spending and value per acre)
Master_out$land_regulated[Master_out$Regulation_code == 1 | Master_out$Regulation_code == 2] <- houseExp_regulated[Master_out$Regulation_code == 1 | Master_out$Regulation_code == 2]/
  (Master_out$IncomeStringency_model_rents[Master_out$Regulation_code == 1 | Master_out$Regulation_code == 2]/Master_out$UnitDensityRestriction_cl[Master_out$Regulation_code == 1 | Master_out$Regulation_code == 2])

#Note: by the old calibration strategy, these measures aren't correlated with direct measurement of land. This is because the static model cannot match all key moments. We need to choose the best moments to target.

#If Regulation code == 1, calculate unregulated land to clear housing markets (assuming no variation in lambda within block groups)
Master_out$land_unregulated[Master_out$Regulation_code == 1] <-  houseExp_unregulated[Master_out$Regulation_code == 1]/
  (Master_out$lambda[Master_out$Regulation_code == 1]*(Master_out$price_unregulated[Master_out$Regulation_code == 1])^(Master_out$HS_Elasticity_imputed[Master_out$Regulation_code == 1] + 1))


#If Regulation code == 2, unregulated land is zero.
Master_out$land_unregulated[Master_out$Regulation_code == 2] <- 0

#Removing block groups with infinite pricing (most block groups have extremely reasonable relative prices). This is due to numerical error (it is a number beyond maximum processed by R)
#Also removing block groups where solutions could not be found
print(paste0("removed ", nrow(Master_out) - nrow(Master_out[is.finite(Master_out$price_unregulated) & is.finite(Master_out$lambda) & !is.na(Master_out$land_regulated) & !is.na(Master_out$land_unregulated), ])  ," due to no supply side sln"))
Master_out <- Master_out[is.finite(Master_out$price_unregulated) & is.finite(Master_out$lambda) & !is.na(Master_out$land_regulated) & !is.na(Master_out$land_unregulated), ]

#and instances where we do not find a with small excess demand
Master_out <- Master_out[Master_out$ExcessDemand < 0.01,]

#Print number of block groups in final sample
print(paste0("There are ", nrow(Master_out), " block groups remaining in the final (final) sample on this iteration."))

#Saving for use with other programs
write_dta(Master_out, paste0("DataV2/Counterfactuals/Master_post_calibration_historical.dta"))

rm(list = ls())
