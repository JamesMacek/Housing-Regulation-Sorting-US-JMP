#This file calibrates block group variables for the third calibration strategy, which chooses land to directly match share of households in regulated structures

#Date created: Jan 26th, 2023

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

#This calibration is done to get supply shifters by block group and zoning (i.e. a neighborhood). 

###_________PARAMETERS_________########
source("CodeV2/Calibrate/Parameters/GlobalParameters.R")


#Importing city density distribution
load(file = "DataV2/US_Data/Output/CBSA_quantiles_dens.Rdata")
load(file = "DataV2/US_Data/Output/CBSA_quantiles.Rdata")

BlockGroup <- read_dta("DataV2/US_Data/Output/Constructed_Block_V2.dta") %>% #Block group level variables
  select(State, County, Tract, BlockGroup,
         rank_density_CBSA, City_housing_density, CBSA_med_house_value)

#Importing sample geography
SmpleGeo <- read_dta("DataV2/US_Data/Output/SampleGeography.dta") %>% select(-CBSA)

#Regulation file (NOTE: THESE ARE NOT ADJUSTED REGULATIONS AFTER CHECKING HOW MANY PARCELS ARE BELOW MEASURED MINIMAL LOT.)
Regulation <- read_dta("DataV2/US_Data/Output/Constructed_Block_V2.dta") %>% 
  select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, UnitDensityRestriction_cl, IncomeStringency_cl)
Regulation$UnitDensityRestriction_cl[is.na(Regulation$UnitDensityRestriction_cl) | is.na(Regulation$IncomeStringency_cl) | Regulation$IncomeStringency_cl == 0] <- 0 #setting NA's to zero that were imputed in Merge_stringency.R

#Land value density from raw regulation file
Regulation_V2 <- read_dta("DataV2/US_Data/Output/raw_Regulation_ByBlockGroup.dta") %>% select(State, County, Tract, BlockGroup, LandValueDensity)

#Importing housing prices (note: difference in sample size is what we could construct from CoreLogic transactions + assessment coverage)
HedonicIndex <- read_dta("DataV2/US_Data/Output/FullHedonicIndex_complete.dta") %>% select(-CBSA)

#Housing supply elasticities
HS_Elasticities <- read_dta("DataV2/US_Data/Output/HS_Elasticities.dta") %>% select(-CBSA, -CBSA_NAME)

#LOCAL ABILITY DISTRIBUTIONS
LocalAbilityDist <- read_dta("DataV2/US_Data/Output/LocalAbilityDistributions.dta") %>% select(-CBSA)
#Note: differences in block groups are block groups that have households = 0 in 2020 census. 

#Wages
Wages <- read_dta("DataV2/US_Data/Output/CityProd_individual.dta")

#Joining this all to master
Master <- left_join(HedonicIndex, SmpleGeo, by = c("State", "County", "Tract", "BlockGroup"))
Master <- left_join(Master, BlockGroup, by = c("State", "County", "Tract", "BlockGroup"))
Master <- left_join(Master, Regulation, by = c("State", "County", "Tract", "BlockGroup"))
Master <- left_join(Master, Regulation_V2, by = c("State", "County", "Tract", "BlockGroup"))
Master <- left_join(Master, Wages, by = c("CBSA"))
Master <- left_join(Master, LocalAbilityDist, by = c("State", "County", "Tract", "BlockGroup"))
Master <- left_join(Master, HS_Elasticities, by = c("State", "County", "Tract", "BlockGroup"))

rm(HedonicIndex, HS_Elasticities, 
   LocalAbilityDist, 
   Regulation, Wages,
   SmpleGeo, BlockGroup, Regulation_V2)

#Housing spendshare == beta for everyone (this is actually closer to reality than you think--see additional model with StoneGeary preferences)
Master["hSpendShare_1"] <- beta
Master["hSpendShare_2"] <- beta
Master["hSpendShare_3"] <- beta
Master["hSpendShare_4"] <- beta
Master["hSpendShare_5"] <- beta
Master["hSpendShare_6"] <- beta
Master["hSpendShare_7"] <- beta


#Normalizing average income type in midpoint income group to 1 to measure welfare
adjustment_factor_temp <- Master$ability_grp4[1]

#Dropping block groups with zero land mass and zero population (this is the same sample of block groups used in the facts.)
Master <- Master[Master$total_housing_units_cen > 0 & Master$ALAND > 0 & !is.na(Master$ALAND) & !is.na(Master$total_housing_units_cen),]

#With measured supply elasticity (either via MSA average or not)
Master <- Master[!is.na(Master$HS_Elasticity_imputed),]

#If no coverage measuring share of housing units in regulated structures, remove
Master <- Master[!is.na(Master$regulated_housingUnit_share),] 

#Set UnitDensityRestriction == 0 for block groups with no regulated housing units
Master$UnitDensityRestriction_cl[Master$regulated_housingUnit_share == 0] <- 0

#Getting block group populations of each type
Master["T_pop_temp"] <- rep(0, nrow(Master))
for (incomeType in 1:7) {
  
  Master[paste0("Population_type_", incomeType)] <- Master[[paste0("Pooled_Ability_bin_", incomeType)]]*Master$total_housing_units_cen
  
  for (edu in c("College", "NoCollege")) {
    Master[paste0("Population_type_", edu, "_", incomeType)] <- Master[[paste0(edu, "_Ability_bin_", incomeType)]]*Master$total_housing_units_cen
  }
  
  Master$T_pop_temp <- Master$T_pop_temp + Master[[paste0("Population_type_", incomeType)]] 
  # total population across all income types should add to total housing units, otherwise they are all 0 and ACS has no data on local income distributions. 
  
}

#Dropping block groups from sample that don't have local income distribution information (There are a few thousand block groups without this information)
max(abs(Master$T_pop_temp - Master$total_housing_units_cen), na.rm = TRUE) #Up to machine precision
Master <- Master[!is.na(Master$T_pop_temp),] %>% select(-T_pop_temp, -contains("_Ability_bin_"))

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

#Creating hedonicPrice variable
Master["hedonicPrice"] <- rep(NA, nrow(Master))
Master$hedonicPrice[Master$Regulation_code == 1 | Master$Regulation_code == 2] <- Master$hedonicPrice_regulated[Master$Regulation_code == 1 | Master$Regulation_code == 2]
Master$hedonicPrice[Master$Regulation_code == 0] <- Master$hedonicPrice_total[Master$Regulation_code == 0]

#Normalizing prices so that mean == 1
Master$hedonicPrice <- Master$hedonicPrice/mean(Master$hedonicPrice, na.rm = TRUE)

#_____________________________________________________________________________________________________________________________________________________________________
#PART 1 OF CALIBRATION________________________________________________________________________________________________________________________________________________
#PART 1: Calculating the lambdas that corresponds to the hedonic indices we observe in the data.
#_____________________________________________________________________________________________________________________________________________________________________


#1.1: Measure of stringency of regulation = LandValueDensity/acre x Minimum lot size (acres) / Price to Rent Ratio By City

#Normalizing land value density == 0 if UnitDensityRestriction == 0
Master$LandValueDensity[Master$UnitDensityRestriction_cl == 0] <- 0

#Deflating by aggregate price to rent ratio (note: we do not want to deflate by city specific price-to-rent. We care about affordability)
#20.6 is the median house value in US/ median rent from 2016-2019 ACS. == 20.6
Master["IncomeStringency_model_rents"] <- (Master$UnitDensityRestriction_cl*Master$LandValueDensity)/20.6

#Average rent at the minimum lot size is $10,292/year (after adjustments from merge_stringency.R -- remove locations with regulated lot size > 2x average lot size)
mean(Master$IncomeStringency_model_rents)
mean(Master[Master$UnitDensityRestriction_cl > 0,]$IncomeStringency_model_rents) #Conditional on a unit density restriction

nrow(Master[Master$IncomeStringency_model_rents > 250000,]) #only 281 block groups with stringency > 250,000

#Importing function to solve for block group level prices, housing expenditure shares, etc.
source("CodeV2/Calibrate/Functions/scratch/Calibration_Functions.R")

#Registering doParallel
ncores  <- detectCores() - 1

#Setting up number of cores (we want to conserve ram as this program is reasonably ram intensive)
registerDoParallel(ncores)

range <- 1:nrow(Master)

#Running dopar loop to solve for all variables
print(Sys.time()) 
tmp <- foreach(row = range, .packages = c("stats"),
               .errorhandling = "pass") %dopar% { 
                 
                 #FOREACH output
                 return(Calibrate_prices(Master[row,]))
                 
               }
print(Sys.time())  

#closing cluster
stopImplicitCluster()

#Saving temp object as checkpoint
save(tmp, file = "DataV2/Counterfactuals/Calibration_Output/SupplyShifter_sln.RData")


#Putting all these objects into the master data frame for use with other objects

Excess_demand_index <- rep(NA, nrow(Master))
price_reg <- rep(NA, nrow(Master))
lambda <- rep(NA, nrow(Master))
consumptionValIndex <- list()

houseExp_regulated <- rep(NA, nrow(Master))
houseExp_unregulated <- rep(NA, nrow(Master))

for (incomeType in 1:7) {
  consumptionValIndex[[incomeType]] <- rep(NA, length(tmp))
  
}

#Storing list objects in vector for now
for (row in 1:length(tmp)) {
  if (is.numeric(tmp[[row]][[1]][1])) {
    
    Excess_demand_index[row] <- tmp[[row]]$ExcessDemand
    price_reg[row] <- tmp[[row]]$price
    
    houseExp_regulated[row] <- tmp[[row]]$housing_demand_expenditure[1]
    houseExp_unregulated[row] <- tmp[[row]]$housing_demand_expenditure[2]
    
    for (incomeType in 1:7) {
      consumptionValIndex[[incomeType]][row] <- tmp[[row]]$consumption_Index[incomeType]
      
    }
    
  }
}

#Regulated housing price
Master["price_regulated"] <- Master$hedonicPrice
Master["price_unregulated"] <- price_reg

Master$price_unregulated[Master$Regulation_code == 2 | Master$Regulation_code == 0] <- Master$price_regulated[Master$Regulation_code == 2 | Master$Regulation_code == 0] #setting reg=unreg price if no within-block-group variation in regulation

#Excess demand index at current solutions
Master["ExcessDemand"] <- Excess_demand_index

#checking maximum deviation from equilibrium
max(abs(Master$ExcessDemand), na.rm = TRUE) #pretty close.

#
for (incomeType in 1:7) {
  Master[paste0("consumption_Val_", incomeType)] <- consumptionValIndex[[incomeType]]
  
}


#Calculating lambda
Master["lambda"] <- rep(NA, nrow(Master))

#If regulation_code == 1 or 2, calculate lambda the traditional way (to target the measured land value per acre)
Master$lambda[Master$Regulation_code == 1 | Master$Regulation_code == 2] <- Master$IncomeStringency_model_rents[Master$Regulation_code == 1 | Master$Regulation_code == 2]/
                                                                            (Master$price_regulated[Master$Regulation_code == 1 | Master$Regulation_code == 2]^(Master$HS_Elasticity_imputed[Master$Regulation_code == 1 | Master$Regulation_code == 2] + 1))

#Otherwise, assume land mass == ALAND and calculate lambda
Master$lambda[Master$Regulation_code == 0] <- houseExp_regulated[Master$Regulation_code == 0]/(Master$ALAND[Master$Regulation_code == 0]*(Master$price_regulated[Master$Regulation_code == 0]^(Master$HS_Elasticity_imputed[Master$Regulation_code == 0] + 1)))

#Now, solving for land to clear housing markets
Master["land_regulated"] <- rep(NA, nrow(Master))
Master["land_unregulated"] <- rep(NA, nrow(Master))

#if Regulation_code == 0, set ALAND = land (as a normalization!!)
Master$land_unregulated[Master$Regulation_code == 0] <- Master$ALAND[Master$Regulation_code == 0] 
Master$land_regulated[Master$Regulation_code == 0] <- 0

#If Regulation_code == 1 or 2, calculate regulated land the normal way (to clear housing markets) (this is the ratio between total housing spending and value per acre)
Master$land_regulated[Master$Regulation_code == 1 | Master$Regulation_code == 2] <- houseExp_regulated[Master$Regulation_code == 1 | Master$Regulation_code == 2]/
                                                                                    (Master$IncomeStringency_model_rents[Master$Regulation_code == 1 | Master$Regulation_code == 2])
#Note: by the old calibration strategy, these measures aren't correlated with direct measurement of land. This is because the static model cannot match all key moments. We need to choose the best moments to target.


#If Regulation code == 1, calculate unregulated land to clear housing markets (assuming no variation in lambda within block groups)
Master$land_unregulated[Master$Regulation_code == 1] <-  houseExp_unregulated[Master$Regulation_code == 1]/
                                                        (Master$lambda[Master$Regulation_code == 1]*(Master$price_unregulated[Master$Regulation_code == 1])^(Master$HS_Elasticity_imputed[Master$Regulation_code == 1] + 1))


#If Regulation code == 2, unregulated land is zero.
Master$land_unregulated[Master$Regulation_code == 2] <- 0

#Removing 20 block groups with infinite pricing (most block groups have extremely reasonable relative prices). This is due to numerical error (it is a number beyond maximum processed by R)
#Also removing block groups where solutions could not be found
Master <- Master[is.finite(Master$price_unregulated) & is.finite(Master$lambda), ]

#Excess demand doesn't seem to be too large, but we do not have solutions for some block groups (approx 700 of them. Go back and see what's up later.)


#Finally, removing block groups with insanely expensive housing (more than what could possibly be rationalized by the model)
Master <- Master[Master$IncomeStringency_model_rents <= 250000,] #only 281 block groups with stringency > 250,000


#Print number of block groups in final sample
print(paste0("There are ", nrow(Master), " block groups remaining in the final (final) sample."))

#Saving for use with other programs
write_dta(Master, "DataV2/Counterfactuals/Master_post_calibration.dta")


#______________DOING THE SAME FOR THE BYSKILL VERSION_____________________________________________________

#Registering doParallel
ncores  <- detectCores() - 1

#Setting up number of cores (we want to conserve ram as this program is reasonably ram intensive)
registerDoParallel(ncores)

range <- 1:nrow(Master)

#Running dopar loop to solve for all variables
print(Sys.time()) 
tmp <- foreach(row = range, .packages = c("stats"),
               .errorhandling = "pass") %dopar% { 
                 
                 #FOREACH output
                 return(Calibrate_prices_bySkill(Master[row,]))
                 
               }
print(Sys.time())  

#closing cluster
stopImplicitCluster()

#Saving temp object as checkpoint
save(tmp, file = "DataV2/Counterfactuals/Calibration_Output/SupplyShifter_sln_bySkill.RData")
#Takes about 35 minutes to run (more calculations!)

#Putting all these objects into the master data frame for use with other objects
Excess_demand_index <- rep(NA, nrow(Master))
price_reg <- rep(NA, nrow(Master))
lambda <- rep(NA, nrow(Master))
consumptionValIndex <- list()

houseExp_regulated <- rep(NA, nrow(Master))
houseExp_unregulated <- rep(NA, nrow(Master))

for (skill in c("College", "NoCollege")) {
  for (incomeType in 1:7) {
    
    consumptionValIndex[[skill]][[incomeType]] <- rep(NA, length(tmp))
  
  }
}

#Storing list objects in vector for now
for (row in 1:length(tmp)) {
  if (is.numeric(tmp[[row]][[1]][1])) {
    
    Excess_demand_index[row] <- tmp[[row]]$ExcessDemand
    price_reg[row] <- tmp[[row]]$price
    
    houseExp_regulated[row] <- tmp[[row]]$housing_demand_expenditure[1]
    houseExp_unregulated[row] <- tmp[[row]]$housing_demand_expenditure[2]
    
    for (skill in c("College", "NoCollege"))
      for (incomeType in 1:7) {
        consumptionValIndex[[skill]][[incomeType]][row] <- tmp[[row]]$consumption_Index[[skill]][incomeType]
      
      }
    }
    
}

#Regulated housing price
Master["price_regulated"] <- Master$hedonicPrice
Master["price_unregulated"] <- price_reg

Master$price_unregulated[Master$Regulation_code == 2 | Master$Regulation_code == 0] <- Master$price_regulated[Master$Regulation_code == 2 | Master$Regulation_code == 0] #setting reg=unreg price if no within-block-group variation in regulation

#Excess demand index at current solutions
Master["ExcessDemand"] <- Excess_demand_index

#checking maximum deviation from equilibrium
max(abs(Master$ExcessDemand), na.rm = TRUE) #pretty close.

#Consumption values

  for (incomeType in 1:7) {
    
    Master[paste0("consumption_Val_", incomeType)] <- NULL #Removing pooled consumption values from previous iteration
    
    for (skill in c("College", "NoCollege")) {
      Master[paste0("consumption_Val_", skill, "_", incomeType)] <- consumptionValIndex[[skill]][[incomeType]]
    }
  
}

#Calculating lambda
Master["lambda"] <- rep(NA, nrow(Master))

#If regulation_code == 1 or 2, calculate lambda the traditional way (to target the measured land value per acre)
Master$lambda[Master$Regulation_code == 1 | Master$Regulation_code == 2] <- Master$IncomeStringency_model_rents[Master$Regulation_code == 1 | Master$Regulation_code == 2]/
  (Master$price_regulated[Master$Regulation_code == 1 | Master$Regulation_code == 2]^(Master$HS_Elasticity_imputed[Master$Regulation_code == 1 | Master$Regulation_code == 2] + 1))

#Otherwise, assume land mass == ALAND and calculate lambda
Master$lambda[Master$Regulation_code == 0] <- houseExp_regulated[Master$Regulation_code == 0]/(Master$ALAND[Master$Regulation_code == 0]*(Master$price_regulated[Master$Regulation_code == 0]^(Master$HS_Elasticity_imputed[Master$Regulation_code == 0] + 1)))

#Now, solving for land to clear housing markets
Master["land_regulated"] <- rep(NA, nrow(Master))
Master["land_unregulated"] <- rep(NA, nrow(Master))

#if Regulation_code == 0, set ALAND = land (as a normalization!!)
Master$land_unregulated[Master$Regulation_code == 0] <- Master$ALAND[Master$Regulation_code == 0] 
Master$land_regulated[Master$Regulation_code == 0] <- 0

#If Regulation_code == 1 or 2, calculate regulated land the normal way (to clear housing markets) (this is the ratio between total housing spending and value per acre)
Master$land_regulated[Master$Regulation_code == 1 | Master$Regulation_code == 2] <- houseExp_regulated[Master$Regulation_code == 1 | Master$Regulation_code == 2]/
  (Master$IncomeStringency_model_rents[Master$Regulation_code == 1 | Master$Regulation_code == 2])
#Note: by the old calibration strategy, these measures aren't correlated with direct measurement of land. This is because the static model cannot match all key moments. We need to choose the best moments to target.


#If Regulation code == 1, calculate unregulated land to clear housing markets (assuming no variation in lambda within block groups)
Master$land_unregulated[Master$Regulation_code == 1] <-  houseExp_unregulated[Master$Regulation_code == 1]/
  (Master$lambda[Master$Regulation_code == 1]*(Master$price_unregulated[Master$Regulation_code == 1])^(Master$HS_Elasticity_imputed[Master$Regulation_code == 1] + 1))


#If Regulation code == 2, unregulated land is zero.
Master$land_unregulated[Master$Regulation_code == 2] <- 0

#Removing 20 block groups with infinite pricing (most block groups have extremely reasonable relative prices). This is due to numerical error (it is a number beyond maximum processed by R)
#Also removing block groups where solutions could not be found
Master <- Master[is.finite(Master$price_unregulated) & is.finite(Master$lambda), ]

#Excess demand doesn't seem to be too large, but we do not have solutions for some block groups (approx 700 of them. Go back and see what's up later.)


#Finally, removing block groups with insanely expensive housing (more than what could possibly be rationalized by the model)
Master <- Master[Master$IncomeStringency_model_rents <= 250000,] #only 281 block groups with stringency > 250,000


#Print number of block groups in final sample
print(paste0("There are ", nrow(Master), " block groups remaining in the final (final) sample for the bySkill model."))

#Saving for use with other programs
write_dta(Master, "DataV2/Counterfactuals/Master_post_calibration_bySkill.dta")






