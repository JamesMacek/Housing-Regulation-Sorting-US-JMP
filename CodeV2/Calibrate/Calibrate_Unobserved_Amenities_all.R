#This file calibrates unobserved amenities assuming a particular value for the migration elasticity (by income type)
#It also calculates "smoothed" versions of these amenity values for estimation of Omega(z). 
#Otherwise, issues with zeros come into play.

#Date created: Feb 2nd, 2023

library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)

###_________PARAMETERS_________########
source("CodeV2/Calibrate/Parameters/GlobalParameters.R") #Harmonized list of parameters

#CHOOSE BASELINE SPECIFICATION FOR ESTIMATING OMEGA
BASELINE_SPECIFICATION <- list(pref = "SG", bySkill_to_pass = FALSE) #Estimating omega will not work for bySkill == True at current codebase; though little reason to believe this matters.
#                                                                    #So make sure this is set to false
                                                                     #Will also output historical data for placebo test
#Logs...
sink("DataV2/Counterfactuals/logs/Calibration_amenities.txt")
print(paste0("Calibrating amenities at ", Sys.time()))

for (bySkill_to_pass in c(FALSE, TRUE)) { #BySkill or Pooled
  for (pref in c("CD", "SG")) { #Cobb-Douglas, StoneGeary
    for (sample in c("current", "historical")) { #Check Sample-- 2020 cross section or historical panel?
    
        
      #Initializing skill names 
      if (bySkill_to_pass == TRUE) {
    
        skillVector <-  c("College", "NoCollege")
        skillName <- c("College_", "NoCollege_") 
      
      }else{
      
        skillVector <- c("Pooled")
        skillName <- c("")
      
      }
      
      #Check if historical sample and baseline specification, else continue
      if (sample == "historical" & (BASELINE_SPECIFICATION$pref != pref | BASELINE_SPECIFICATION$bySkill_to_pass != bySkill_to_pass)) {
       next #continue on loop if historical AND not baseline specification
      }
    
      
      #Importing data
      if (sample == "current") { #if current sample
        Master <- read_dta(paste0("DataV2/Counterfactuals/Master_post_calibration_bySkill", bySkill_to_pass, "_pref_", pref, ".dta"))  #Read post calibration consumption values
      }
      
      if (sample == "historical") {
        Master <- read_dta(paste0("DataV2/Counterfactuals/Master_post_calibration_historical.dta"))
      }
      
      #Calculating what the welfare index would be if we assumed Cobb-Douglas preferences at parameter beta--which is what previous elasticities were estimated on!
      #(ignoring skill, as this does not change log variance at all!)
      #And, we are assuming that prices == hedonic index (which would be the case if housing unit density restrictions == 0)
    
      #Use PooledWage to mimic consumption value measures from Baum-Snow and Han (2023) and Hornbeck and Moretti (2022)
      CobbDouglas_consValue <- Master$PooledWage/((Master$hedonicPrice)^(beta)) 
      #Taking log variance of this index
      logVariance_CobbDouglas <- var(log(CobbDouglas_consValue))
      rm(CobbDouglas_consValue)

      #Rescaling each consumption index by a power of rho[ability_grp] to match this Cobb-Douglas (log) variance. 
      #Note: Baum-Snow and Han (2022) + Hornbeck and Moretti (2018) use a Frechet model (which is just a log-gumbel model)
      #This is precisely why these adjustments take this form.

      #Making sure each consumption measure has the log-variance of the Cobb-Douglas preferences used to estimate rho and theta in other papers.
      consumption_AdjustmentFactor <- matrix(nrow = length(skillVector), ncol = 7)
      row.names(consumption_AdjustmentFactor) <- skillVector
    
      for (skill in skillVector) {
        for (incomeType in 1:7) {
          #calculating reported variance for each income type
          consumption_AdjustmentFactor[skill, incomeType] <- sqrt(logVariance_CobbDouglas/(var(Master[[paste0("consumption_Val_", skillName[which(skill == skillVector)],
                                                                                                       incomeType)]], na.rm = TRUE)))
        }
      }
  
      consumption_AdjustmentFactor <- data.frame(consumption_AdjustmentFactor)
      colnames(consumption_AdjustmentFactor) <- paste0("consumption_Adjustment", 1:7)

      #Adjusting consumption values in ConsValue  
      for (skill in skillVector) {
        for (incomeType in 1:7) {
          Master[paste0("consumption_Val_", skillName[which(skill == skillVector)],  incomeType)] <- (Master[[paste0("consumption_Val_", skillName[which(skill == skillVector)],  incomeType)]])*
                                                                                                      (consumption_AdjustmentFactor[[skill, paste0("consumption_Adjustment", incomeType)]])
        }
      }

      #Creating matrix/dataframe of these adjustments to pass to other program to solve for equilibrium values

      #NOTE: In Calibrate_ConsumptionValues_SupplyShifters.R, we used an adjustment factor
      #because of computational reasons to construct the consumption index. Correcting for this.
      adjustment_factor_temp <- as.numeric(select(Master, starts_with("ability_grp"))[1,]) 

      #Incorporating this adjustment factor into this index for use with other programs.
      #NOTE: ONLY SAVE CALIBRATION FACTOR FOR CURRENT SAMPLE
      if (sample == "current") {
        for (row in 1:nrow(consumption_AdjustmentFactor)) {
          consumption_AdjustmentFactor[row,] <- consumption_AdjustmentFactor[row, ]/adjustment_factor_temp #adjusting!
        }
        save(consumption_AdjustmentFactor, file = paste0("DataV2/Counterfactuals/Calibration_Output/consumption_AdjustmentFactor_bySkill", bySkill_to_pass, "_pref_", pref, ".Rdata"))
      }
    
      #____________________________________________________________________________________________________________________________________________________
      #Constructing amenities, rescaling by average within city amenity so that avg == 1 in all cities
    
      #For test dataset subsets, make sure city has positive population of all types, also calculate city population.
      #Note: ensuring positivity does not matter in main dataset. 
      for (skill in skillVector) {
        name_of_skill <- skillName[which(skill == skillVector)]
      
        for (incomeType in 1:7) { 
          #Calculating city populations at current geography
          pop_symb <- paste0("Population_type_", name_of_skill, incomeType)
          city_pop_symb <- paste0("cityPopulation_type_", name_of_skill, incomeType)
          Master <- Master %>% group_by(CBSA) %>% mutate(!!sym(city_pop_symb) := sum(!!sym(pop_symb)))
        
          Master <- Master[Master[[paste0("cityPopulation_type_", name_of_skill, incomeType)]] > 0, ]
        }
      }
    
    
      for (skill in skillVector) {
        name_of_skill <- skillName[which(skill == skillVector)]
      
        for (incomeType in 1:7) { 
      
          #varnames to pass to symbol in dplyr::mutate
          cityAvg_symb <- paste0("cityAverage_amenity_", name_of_skill, incomeType)
          withinCity_symb <- paste0("WithinCity_Amenity_", name_of_skill, incomeType)
  
          #note: solving for exp(amenity) B = e^b
          Master[paste0("WithinCity_Amenity_", name_of_skill, incomeType)] <- ((Master[[paste0("Population_type_", name_of_skill, incomeType)]])^(1/rho))* #population density + 
                                                                              (1/(exp(Master[paste0("consumption_Val_", name_of_skill, incomeType)])))#x consumption reweighted by migration elasticity within cities
  
          Master <- Master %>% group_by(CBSA) %>% mutate(!!sym(cityAvg_symb) := mean(!!sym(withinCity_symb), na.rm = TRUE))
  
          Master[paste0("WithinCity_Amenity_", name_of_skill, incomeType)] <- (Master[[paste0("WithinCity_Amenity_", name_of_skill, incomeType)]])/(Master[[paste0("cityAverage_amenity_", name_of_skill, incomeType)]])
          Master[paste0("cityAverage_amenity_", name_of_skill, incomeType)] <- NULL #deleting column 
  
        }
      }
    
    
      #OUTPUTTING VARIABLES FOR USE IN INSTRUMENT ONLY FOR BASELINE SPECIFICATION ________________________________________
    
      if (BASELINE_SPECIFICATION$pref == pref & bySkill_to_pass == FALSE) { #Do not calculate for the byskill == TRUE version of the model
        
        #For instrument, we use AGGREGATED amenity values with (potentially overlapping populations) to avoid the zero problem. 
          #Do this for three income types -- LOW = Types 1, 2, 3 (0 - 50,000)
          #                                  MEDIUM = Types 3, 4, 5 (50,000 - 100,000)
          #                                  HIGH = Types 5, 6, 7 (100,000 + )
        
    
        
        forInstrument <- Master %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, hedonicPrice)
        
        #Creating aggregated population bins that keep sample balanced (in linear regression, we must drop zeros in the data)
        forInstrument[["Population_type_l"]] <- Master[[paste0("Population_type_1")]] + Master[[paste0("Population_type_2")]] 
        forInstrument[["Population_type_m"]] <- Master[[paste0("Population_type_3")]] + Master[[paste0("Population_type_4")]]
        forInstrument[["Population_type_h"]] <- Master[[paste0("Population_type_5")]] + Master[[paste0("Population_type_6")]] + Master[[paste0("Population_type_7")]] #summing populations
        
    
        #Defining consumption value of bin to be population weighted average of each consumption value from those bins
        forInstrument[["consumption_Val_l"]] <- (Master[[paste0("consumption_Val_1")]]*Master[[paste0("Population_type_1")]] + Master[[paste0("consumption_Val_2")]]*Master[[paste0("Population_type_2")]])/forInstrument[["Population_type_l"]]
        
        forInstrument[["consumption_Val_m"]] <- ( Master[[paste0("consumption_Val_3")]]*Master[[paste0("Population_type_3")]] + Master[[paste0("consumption_Val_4")]]*Master[[paste0("Population_type_4")]] )/forInstrument[["Population_type_m"]]
        
        forInstrument[["consumption_Val_h"]] <- (  Master[[paste0("consumption_Val_5")]]*Master[[paste0("Population_type_5")]] + Master[[paste0("consumption_Val_6")]]*Master[[paste0("Population_type_6")]] +
                                                   Master[[paste0("consumption_Val_7")]]*Master[[paste0("Population_type_7")]] )/forInstrument[["Population_type_h"]]
        
        
        #Consumption values defined whenever respective population bin is positive. 
        #Very few zeroes, especially for lower income types. only 10k zeroes for high income. 
        #Note: population smoothing procedure will affect weights used to construct consumption values, so estimates will change with different parameters (only slightly)
        
        #Start calculation of amenity values
        for (rho_band in c(rho, rho + 2*rho_se, rho - 2*rho_se)) { #Calculating amenities under BSH (2022) point estimate + 2 standard-error bands to check robustness of Omega estimate
  
          if (rho_band == rho) {
            name <- ""
          }
          if (rho_band == rho + 2*rho_se) {
            name <- "upper_"
          }
          if (rho_band == rho - 2*rho_se) {
            name <- "lower_"
          }
            
            #Start imputing amenity value 
            for (smoothedIncomeType in c("l", "m", "h")) { 
    
              #varnames to pass to symbol in dplyr::mutate
              cityAvg_symb <- paste0(name, "_cityAverage_amenity_", smoothedIncomeType)
              withinCity_symb <- paste0(name, "WithinCity_Amenity_", smoothedIncomeType)
    
              forInstrument[paste0(name, "WithinCity_Amenity_", smoothedIncomeType)] <- ((forInstrument[[paste0("Population_type_", smoothedIncomeType)]])^(1/rho_band))* #population + 
                                                                                                (1/(exp(forInstrument[paste0("consumption_Val_", smoothedIncomeType)]))) #x consumption reweighted by migration elasticity within cities
    
              forInstrument <- forInstrument %>% group_by(CBSA) %>% mutate(!!sym(cityAvg_symb) := mean(!!sym(withinCity_symb), na.rm = TRUE))
    
              forInstrument[paste0(name, "WithinCity_Amenity_", smoothedIncomeType)] <- (forInstrument[[paste0(name, "WithinCity_Amenity_", smoothedIncomeType)]])/(forInstrument[[paste0(name, "_cityAverage_amenity_", smoothedIncomeType)]])
              forInstrument[paste0(name, "_cityAverage_amenity_", smoothedIncomeType)] <- NULL #deleting column 
          }
          
        } #end loop over rho bands
      
        #Saving these within-city amenities for use with instrumental variables estimation
        #Note: we don't need across-city variation in amenities because we use MSA fixed effects in these regressions. 
        
        if (sample == "current") {
          write_dta(forInstrument, "DataV2/US_Data/Instrument/Amenity_values_forEstimation.dta")
        }
        
        if (sample == "historical") {
          write_dta(forInstrument, "DataV2/US_Data/Instrument/Amenity_values_forEstimation_historical.dta")
        }
        rm(forInstrument)
      
      }#END BASELINE SPECIFICATION CHECK FOR OMEGA[z]

      #Next, calculate the city amenity value_______________________________________________________________
    
      for (skill in skillVector) {
        name_of_skill <- skillName[which(skill == skillVector)]
        for (incomeType in 1:7) {
      
          #Symbolic variables
          Value_symb <- paste0("ValIndex_tmp_", name_of_skill, incomeType)
          city_Value_symb <- paste0("city_Val_", name_of_skill, incomeType)
  
          city_pop_symb <- paste0("cityPopulation_type_", name_of_skill, incomeType)
          pop_symb <- paste0("Population_type_", name_of_skill, incomeType)
  
          #Constructing city welfare aggregators net of city-wide amenity value.
          #Creating Welfare index when average withincity amenity == 1
          Master[paste0("ValIndex_tmp_", name_of_skill,  incomeType)] <- (Master[[paste0("WithinCity_Amenity_", name_of_skill, incomeType)]]*(exp(Master[[paste0("consumption_Val_", name_of_skill, incomeType)]])))^(rho)
          Master <- Master %>% group_by(CBSA) %>% mutate(!!sym(city_Value_symb) := 
                                                             sum(!!sym(Value_symb))^(1/rho))
  
          #Deleting temporary value index
          Master[paste0("ValIndex_tmp_", name_of_skill, incomeType)] <- NULL
  

          #Constructing across city amenity values
  
          #Symbolic variables
          AcrossCity_symb <- paste0("AcrossCity_Amenity_", name_of_skill, incomeType)
  
          Master[paste0("AcrossCity_Amenity_", name_of_skill, incomeType)] <- ((Master[[paste0("cityPopulation_type_", name_of_skill, incomeType)]])^(1/theta))*
                                                                              (1/Master[[paste0("city_Val_", name_of_skill, incomeType)]]) #correcting by (rho/theta so that it multiplies with wihtincity amenity to get total amenity)
    
          #demeaning by US average (weighted by block groups) (note: location amenities are identified up to scale)
          Master[paste0("AcrossCity_Amenity_", name_of_skill, incomeType)] <- Master[[paste0("AcrossCity_Amenity_", name_of_skill, incomeType)]]/mean(Master[[paste0("AcrossCity_Amenity_", name_of_skill, incomeType)]])
  
          #Finally, creating total amenity value, which is just the product of the across and within city values
  
          Master[paste0("Amenity_", name_of_skill, incomeType)] <- Master[paste0("AcrossCity_Amenity_", name_of_skill, incomeType)]*Master[[paste0("WithinCity_Amenity_", name_of_skill, incomeType)]]
        }
      }

      #Saving master data only if current sample (do not use historical amenities for model counterfactuals)
      if (sample == "current") {
        write_dta(Master, paste0("DataV2/Counterfactuals/Master_post_calibration_bySkill", bySkill_to_pass, "_pref_", pref, "_amenities.dta")) #amenity calibrations
      }
      
      #Robustness:
      #VERIFYING THAT THESE AMENITIES RATIONALIZE THE OBSERVED POPULATION DISTRIBUTIONS.

      Test_Amenities <- Master %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME,
                                          starts_with("consumption_Val_"),
                                          starts_with("Amenity_"),
                                          starts_with("Population_type_"))
  
      Test_Amenities <- Test_Amenities %>% group_by(CBSA) %>% mutate(inv_city_weight = 1/n()) #inverse city weights for calculations 
      for (skill in skillVector) {
        name_of_skill <- skillName[which(skill == skillVector)]
      
        for (incomeType in 1:7) {
  
          #total populations for testing
          Test_Amenities[paste0("Total_Population_type_", name_of_skill,  incomeType)] <- sum(Test_Amenities[[paste0("Population_type_", name_of_skill, incomeType)]])
  
          #Symbolic vars
          value_n_sym <- paste0("value_n_", name_of_skill, incomeType)
          value_n_city_sym <- paste0("value_n_city", name_of_skill, incomeType)
  
          #creating value of neighborhood (raised to migration elasticity)
          Test_Amenities[paste0("value_n_", name_of_skill, incomeType)] <- (Test_Amenities[[paste0("Amenity_", name_of_skill, incomeType)]]*exp(Test_Amenities[[paste0("consumption_Val_", name_of_skill, incomeType)]]))^(rho) #flow utility from neighborhood (product of amneity and consumption value)
  
          #Constructing within-city welfare index
          Test_Amenities <- Test_Amenities %>% group_by(CBSA) %>% mutate(!!sym(value_n_city_sym) := (sum(!!sym(value_n_sym)))^(1/rho))
  
          #constructing fraction of city i population in neighborhood i
          Test_Amenities[paste0("frac_neighborhood_", name_of_skill, incomeType)] <- Test_Amenities[[paste0("value_n_", name_of_skill, incomeType)]]/(Test_Amenities[[paste0("value_n_city", name_of_skill, incomeType)]]^(rho))
  
          #Constructing total welfare index
          Test_Amenities[paste0("value_n_national", name_of_skill, incomeType)] <- (sum((Test_Amenities[[paste0("value_n_city", name_of_skill, incomeType)]]^(theta))*Test_Amenities$inv_city_weight))^(1/theta)
          #Inverse city weight means one index per city
  
          #Constructing fraction in city
          Test_Amenities[paste0("frac_city_", name_of_skill, incomeType)] <- (Test_Amenities[[paste0("value_n_city", name_of_skill, incomeType)]]^(theta))/(Test_Amenities[[paste0("value_n_national", name_of_skill, incomeType)]]^(theta))
  
          #Constructing model population
          Test_Amenities[paste0("Model_Population_type_", name_of_skill, incomeType)] <- Test_Amenities[[paste0("frac_city_", name_of_skill, incomeType)]]*Test_Amenities[[paste0("frac_neighborhood_", name_of_skill, incomeType)]]*
                                                                                       Test_Amenities[[paste0("Total_Population_type_", name_of_skill, incomeType)]]
  
        }
      }

      #Checking error
      Error <- 0
      for (skill in skillVector) {
      
        name_of_skill <- skillName[which(skill == skillVector)]
      
        for (incomeType in 1:7) {
        
          Error <- Error + sum(abs(Test_Amenities[[paste0("Model_Population_type_", name_of_skill, incomeType)]] - Test_Amenities[[paste0("Population_type_", name_of_skill, incomeType)]]))
  
        }
      }
      print(paste0("The error for model bySkill=", bySkill_to_pass, " preferences ", pref, " is ", Error))
    

    
    }#END LOOP OVER DIFFERENT SPECIFICATIONS
  }
}
  
rm(list = ls())

#Closing logs
sink(NULL)


