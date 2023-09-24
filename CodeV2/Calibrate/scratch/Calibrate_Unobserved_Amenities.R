#This file calibrates unobserved amenities assuming a particular value for the migration elasticity.
#Do we want to split things up by income type? 


#Date created: Feb 2nd, 2023

library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)

###_________PARAMETERS_________########
source("CodeV2/Calibrate/Parameters/GlobalParameters.R") #Harmonized list of parameters


Master <- read_dta("DataV2/Counterfactuals/Master_post_calibration.dta")  #Read post calibration consumption values

#Calculating what the welfare index would be if we assumed Cobb-Douglas preferences at parameter beta--which is what previous elasticities were estimated on!
#(ignoring skill, as this does not change log variance at all!)
#And, we are assuming that prices == hedonic index (which would be the case if housing unit density restrictions == 0)
CobbDouglas_consValue <- Master$PooledWage/((Master$hedonicPrice)^(beta)) 
#Taking log variance of this index
logVariance_CobbDouglas <- var(log(CobbDouglas_consValue))
rm(CobbDouglas_consValue)

#Rescaling each consumption index by a power of rho[ability_grp] to match this Cobb-Douglas (log) variance. 
#Note: Baum-Snow and Han (2022) + Hornbeck and Moretti (2018) use a Frechet model (which is just a log-gumbel model)
#This is precisely why these adjustments take this form.

#Making sure each consumption measure has the log-variance of the Cobb-Douglas preferences used to estimate rho and theta in other papers.
consumption_AdjustmentFactor <- matrix(nrow = 1, ncol = 7)
for (incomeType in 1:7) {
  #calculating reported variance for each income type
  consumption_AdjustmentFactor[1, incomeType] <- sqrt(logVariance_CobbDouglas/(var(Master[[paste0("consumption_Val_", incomeType)]], na.rm = TRUE)))
  
}

consumption_AdjustmentFactor <- data.frame(consumption_AdjustmentFactor)
colnames(consumption_AdjustmentFactor) <- paste0("consumption_Adjustment", 1:7)

#Adjusting consumption values in ConsValue 
for (incomeType in 1:7) {
  Master[paste0("consumption_Val_", incomeType)] <- (Master[[paste0("consumption_Val_", incomeType)]])*
                                                       (consumption_AdjustmentFactor[[paste0("consumption_Adjustment", incomeType)]][1])
}


#Creating matrix/dataframe of these adjustments to pass to other program to solve for equilibrium values

#NOTE: In Calibrate_ConsumptionValues_SupplyShifters.R, we used an adjustment factor because of computational reasons to construct the consumption index.
adjustment_factor_temp <- Master$ability_grp4[1]

#Incorporating this adjustment factor into this index for use with other programs
consumption_AdjustmentFactor <- consumption_AdjustmentFactor/adjustment_factor_temp
write_dta(consumption_AdjustmentFactor, "DataV2/Counterfactuals/Calibration_Output/consumption_AdjustmentFactor_gumbel.dta")



#Constructing amentities, rescaling by average within city amenity so that avg == 1 in all cities
for (incomeType in 1:7) { 
  
  #varnames to pass to symbol in dplyr::mutate
  cityAvg_symb <- paste0("cityAverage_amenity_", incomeType)
  withinCity_symb <- paste0("WithinCity_Amenity_", incomeType)
  
  #note: solving for exp(amenity) B = e^b
  Master[paste0("WithinCity_Amenity_", incomeType)] <- ((Master[[paste0("Population_type_", incomeType)]])^(1/rho))* #population density + 
                                                        (1/(exp(Master[paste0("consumption_Val_", incomeType)])))#x consumption reweighted by migration elasticity within cities
  
  Master <- Master %>% group_by(CBSA) %>% mutate(!!sym(cityAvg_symb) := mean(!!sym(withinCity_symb), na.rm = TRUE))
  
  Master[paste0("WithinCity_Amenity_", incomeType)] <- (Master[[paste0("WithinCity_Amenity_", incomeType)]])/(Master[[paste0("cityAverage_amenity_", incomeType)]])
  Master[paste0("cityAverage_amenity_", incomeType)] <- NULL #deleting column 
  
}

#Calculating amenities under BSH (2022) 2 standard-error bands to check robustness of Omega estimate
forInstrument <- Master %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME)

count <- 0
for (rho_band in c(rho + 2*rho_se, rho - 2*rho_se)) {
  count <- count + 1
  
  if (count == 1) {
    name <- "upper"
  }else{
    name <- "lower"
  }
  
  for (incomeType in 1:7) { 
    
    #varnames to pass to symbol in dplyr::mutate
    cityAvg_symb <- paste0(name, "_cityAverage_amenity_", incomeType)
    withinCity_symb <- paste0(name, "_WithinCity_Amenity_", incomeType)
    
    forInstrument[paste0(name, "_WithinCity_Amenity_", incomeType)] <- ((Master[[paste0("Population_type_", incomeType)]])^(1/rho_band))* #population density + 
                                                                          (1/(exp(Master[paste0("consumption_Val_", incomeType)]))) #x consumption reweighted by migration elasticity within cities
    
    forInstrument <- forInstrument %>% group_by(CBSA) %>% mutate(!!sym(cityAvg_symb) := mean(!!sym(withinCity_symb), na.rm = TRUE))
    
    forInstrument[paste0(name, "_WithinCity_Amenity_", incomeType)] <- (forInstrument[[paste0(name, "_WithinCity_Amenity_", incomeType)]])/(forInstrument[[paste0(name, "_cityAverage_amenity_", incomeType)]])
    forInstrument[paste0(name, "_cityAverage_amenity_", incomeType)] <- NULL #deleting column 
  }
}

#Saving these within-city amenities for use with instrumental variables estimation
#Note: we don't need cross-city amenities because we use MSA fixed effects in these regressions. 
forInstrument_merge <- Master %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, starts_with("WithinCity_Amenity_"), hedonicPrice)

forInstrument <- left_join(forInstrument, forInstrument_merge)

write_dta(forInstrument, "DataV2/US_Data/Instrument/Amenity_values_forEstimation.dta")
rm(forInstrument, forInstrument_merge)


#Next, calculate the city amenity value.
for (incomeType in 1:7) {
  #Symbolic variables
  Value_symb <- paste0("ValIndex_tmp", incomeType)
  city_Value_symb <- paste0("city_Val_", incomeType)
  
  city_pop_symb <- paste0("cityPopulation_type_", incomeType)
  pop_symb <- paste0("Population_type_", incomeType)
  
  #Constructing city welfare aggregators net of city-wide amenity value.
  #Creating Welfare index when average withincity amenity == 1
  Master[paste0("ValIndex_tmp", incomeType)] <- (Master[[paste0("WithinCity_Amenity_", incomeType)]]*(exp(Master[[paste0("consumption_Val_", incomeType)]])))^(rho)
  Master <- Master %>% group_by(CBSA) %>% mutate(!!sym(city_Value_symb) := 
                                                         sum(!!sym(Value_symb))^(1/rho))
  
  #Deleting temporary value index
  Master[paste0("ValIndex_tmp", incomeType)] <- NULL
  
  #Calculating city populations at current geography
  Master <- Master %>% group_by(CBSA) %>% mutate(!!sym(city_pop_symb) := sum(!!sym(pop_symb)))
  
  
  #Constructing across city amenity values
  
  #Symbolic variables
  AcrossCity_symb <- paste0("AcrossCity_Amenity_", incomeType)
  
  Master[paste0("AcrossCity_Amenity_", incomeType)] <- ((Master[[paste0("cityPopulation_type_", incomeType)]])^(1/theta))*
                                                             (1/Master[[paste0("city_Val_", incomeType)]]) #correcting by (rho/theta so that it multiplies with wihtincity amenity to get total amenity)
  
  #demeaning by US average (weighted by block groups) (note: location amenities are identified up to scale)
  Master[paste0("AcrossCity_Amenity_", incomeType)] <- Master[[paste0("AcrossCity_Amenity_", incomeType)]]/mean(Master[[paste0("AcrossCity_Amenity_", incomeType)]])
  
  #Finally, creating total amenity value, which is just the product of the across and within city values
  
  Master[paste0("Amenity_", incomeType)] <- Master[paste0("AcrossCity_Amenity_", incomeType)]*Master[[paste0("WithinCity_Amenity_", incomeType)]]
}

#Saving master data
write_dta(Master, "DataV2/Counterfactuals/Master_post_calibration_amenities.dta")

#Robustness:
#VERIFYING THAT THESE AMENITIES RATIONALIZE THE OBSERVED POPULATION DISTRIBUTIONS.

Test_Amenities <- Master %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME,
                                       starts_with("consumption_Val_"),
                                       starts_with("Amenity_"),
                                       starts_with("Population_type_"))

Test_Amenities <- Test_Amenities %>% group_by(CBSA) %>% mutate(inv_city_weight = 1/n()) 
for (incomeType in 1:7) {
  
  Test_Amenities[paste0("Total_Population_type_", incomeType)] <- sum(Test_Amenities[[paste0("Population_type_", incomeType)]])
  
  #Symbolic vars
  value_n_sym <- paste0("value_n_", incomeType)
  value_n_city_sym <- paste0("value_n_city", incomeType)
  
  #creating value of neighborhood (raised to migration elasticity)
  Test_Amenities[paste0("value_n_", incomeType)] <- (Test_Amenities[[paste0("Amenity_", incomeType)]]*exp(Test_Amenities[[paste0("consumption_Val_", incomeType)]]))^(rho) #flow utility from neighborhood (product of amneity and consumption value)
  
  #Constructing within-city welfare index
  Test_Amenities <- Test_Amenities %>% group_by(CBSA) %>% mutate(!!sym(value_n_city_sym) := (sum(!!sym(value_n_sym)))^(1/rho))
  
  #constructing fraction of city i population in neighborhood i
  Test_Amenities[paste0("frac_neighborhood_", incomeType)] <- Test_Amenities[[paste0("value_n_", incomeType)]]/(Test_Amenities[[paste0("value_n_city", incomeType)]]^(rho))
  
  #Constructing total welfare index
  Test_Amenities[paste0("value_n_national", incomeType)] <- (sum((Test_Amenities[[paste0("value_n_city", incomeType)]]^(theta))*Test_Amenities$inv_city_weight))^(1/theta)
  #Inverse city weight means one index per city
  
  #Constructing fraction in city
  Test_Amenities[paste0("frac_city_", incomeType)] <- (Test_Amenities[[paste0("value_n_city", incomeType)]]^(theta))/(Test_Amenities[[paste0("value_n_national", incomeType)]]^(theta))
  
  #Constructing model population
  Test_Amenities[paste0("Model_Population_type_", incomeType)] <- Test_Amenities[[paste0("frac_city_", incomeType)]]*Test_Amenities[[paste0("frac_neighborhood_", incomeType)]]*
                                                                  Test_Amenities[[paste0("Total_Population_type_", incomeType)]]
  
}

#Checking error
Error <- 0
for (incomeType in 1:7) {
  Error <- Error + sum(abs(Test_Amenities[[paste0("Model_Population_type_", incomeType)]] - Test_Amenities[[paste0("Population_type_", incomeType)]]))
  
}

print(paste0("The error is ", Error))


rm(list = ls())


#_____REPEATING SAME THING FOR bySkill version of the model_________________________________________

###_________PARAMETERS_________########
source("CodeV2/Calibrate/Parameters/GlobalParameters.R") #Harmonized list of parameters

Master <- read_dta("DataV2/Counterfactuals/Master_post_calibration_bySkill.dta")  #Read post calibration consumption values

#Calculating what the welfare index would be if we assumed Cobb-Douglas preferences at parameter beta--which is what previous elasticities were estimated on!
#(ignoring skill, as this does not change log variance at all!)
#And, we are assuming that prices == hedonic index (which would be the case if housing unit density restrictions == 0)
CobbDouglas_consValue <- Master$PooledWage/((Master$hedonicPrice)^(beta)) 
#Taking log variance of this index
logVariance_CobbDouglas <- var(log(CobbDouglas_consValue))
rm(CobbDouglas_consValue)

#Rescaling each consumption index by a power of rho[ability_grp] to match this Cobb-Douglas (log) variance. 
#Note: Baum-Snow and Han (2022) + Hornbeck and Moretti (2018) use a Frechet model (which is just a log-gumbel model)
#This is precisely why these adjustments take this form.

#Making sure each consumption measure has the log-variance of the Cobb-Douglas preferences used to estimate rho and theta in other papers.
consumption_AdjustmentFactor <- matrix(nrow = 2, ncol = 7)

skill_index <- 0
for (skill in c("College", "NoCollege")) {
  skill_index <- skill_index + 1
  for (incomeType in 1:7) {
    #calculating reported variance for each income type
    consumption_AdjustmentFactor[skill_index, incomeType] <- sqrt(logVariance_CobbDouglas/(var(Master[[paste0("consumption_Val_", skill, "_", incomeType)]], na.rm = TRUE)))
  
  }
}

consumption_AdjustmentFactor <- data.frame(consumption_AdjustmentFactor)
colnames(consumption_AdjustmentFactor) <- paste0("consumption_Adjustment", 1:7)
row.names(consumption_AdjustmentFactor) <- c("College", "NoCollege")

#Adjusting consumption values in ConsValue 
for (skill in c("College", "NoCollege")) {
  for (incomeType in 1:7) {
    Master[paste0("consumption_Val_", skill, "_", incomeType)] <- (Master[[paste0("consumption_Val_", skill, "_", incomeType)]])*
                                                                  (consumption_AdjustmentFactor[[paste0("consumption_Adjustment", incomeType)]][skill])
  }
}

#NOTE: In Calibrate_ConsumptionValues_SupplyShifters.R, we used an adjustment factor because of computational reasons to construct the consumption index.
adjustment_factor_temp <- Master$ability_grp4[1]

#Incorporating this adjustment factor into this index for use with other programs
consumption_AdjustmentFactor <- consumption_AdjustmentFactor/adjustment_factor_temp
write_dta(consumption_AdjustmentFactor, "DataV2/Counterfactuals/Calibration_Output/consumption_AdjustmentFactor_gumbel_bySkill.dta")


#Constructing amentities, rescaling by average within city amenity so that avg == 1 in all cities
for (skill in c("College", "NoCollege")) {
  for (incomeType in 1:7) { 
  
    #varnames to pass to symbol in dplyr::mutate
    cityAvg_symb <- paste0("cityAverage_amenity_", skill, "_", incomeType)
    withinCity_symb <- paste0("WithinCity_Amenity_", skill, "_", incomeType)
  
    #note: solving for exp(amenity) B = e^b
    Master[paste0("WithinCity_Amenity_", skill, "_", incomeType)] <- ((Master[[paste0("Population_type_", skill, "_", incomeType)]])^(1/rho))* #population density + 
                                                                     (1/(exp(Master[paste0("consumption_Val_", skill, "_", incomeType)])))#x consumption reweighted by migration elasticity within cities
  
    Master <- Master %>% group_by(CBSA) %>% mutate(!!sym(cityAvg_symb) := mean(!!sym(withinCity_symb), na.rm = TRUE))
  
    Master[paste0("WithinCity_Amenity_", skill, "_" ,  incomeType)] <- (Master[[paste0("WithinCity_Amenity_", skill, "_", incomeType)]])/(Master[[paste0("cityAverage_amenity_", skill, "_", incomeType)]])
    Master[paste0("cityAverage_amenity_", skill, "_", incomeType)] <- NULL #deleting column 
  
  }
}

#Next, calculate the city amenity value.
for (skill in c("College", "NoCollege")) {
  for (incomeType in 1:7) {
    #Symbolic variables
    Value_symb <- paste0("ValIndex_tmp", skill, "_", incomeType)
    city_Value_symb <- paste0("city_Val_", skill, "_", incomeType)
  
    city_pop_symb <- paste0("cityPopulation_type_", skill, "_", incomeType)
    pop_symb <- paste0("Population_type_", skill, "_", incomeType)
  
    #Constructing city welfare aggregators net of city-wide amenity value.
    #Creating Welfare index when average withincity amenity == 1
    Master[paste0("ValIndex_tmp", skill, "_", incomeType)] <- (Master[[paste0("WithinCity_Amenity_", skill, "_", incomeType)]]*(exp(Master[[paste0("consumption_Val_", skill, "_", incomeType)]])))^(rho)
    Master <- Master %>% group_by(CBSA) %>% mutate(!!sym(city_Value_symb) := 
                                                   sum(!!sym(Value_symb))^(1/rho))
  
    #Deleting temporary value index
    Master[paste0("ValIndex_tmp", skill, "_", incomeType)] <- NULL
  
    #Calculating city populations at current geography
    Master <- Master %>% group_by(CBSA) %>% mutate(!!sym(city_pop_symb) := sum(!!sym(pop_symb)))
  
  
    #Constructing across city amenity values
  
    #Symbolic variables
    AcrossCity_symb <- paste0("AcrossCity_Amenity_", skill, "_", incomeType)
  
    Master[paste0("AcrossCity_Amenity_", skill, "_", incomeType)] <- ((Master[[paste0("cityPopulation_type_", skill, "_", incomeType)]])^(1/theta))*
                                                                      (1/Master[[paste0("city_Val_", skill, "_", incomeType)]]) #correcting by (rho/theta so that it multiplies with wihtincity amenity to get total amenity)
  
    #demeaning by US average (weighted by block groups) (note: location amenities are identified up to scale)
    Master[paste0("AcrossCity_Amenity_", skill, "_", incomeType)] <- Master[[paste0("AcrossCity_Amenity_", skill, "_", incomeType)]]/mean(Master[[paste0("AcrossCity_Amenity_", skill, "_", incomeType)]])
  
    #Finally, creating total amenity value, which is just the product of the across and within city values
  
    Master[paste0("Amenity_", skill, "_", incomeType)] <- Master[paste0("AcrossCity_Amenity_", skill, "_", incomeType)]*Master[[paste0("WithinCity_Amenity_", skill, "_", incomeType)]]
  }
}

#Saving master data
write_dta(Master, "DataV2/Counterfactuals/Master_post_calibration_amenities_bySkill.dta")

rm(list = ls())




