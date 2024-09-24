#This file calibrates block group variables for the third calibration strategy, which chooses land to directly match share of households in regulated structures
#Uses 

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

#Compiling functions for greater speed.
library(compiler) 


#This file calibrates prices in each zone, recovering consumption values and supply shifters.
#This file also calibrates 2008-2012 consumption values to calculate for placebo check.

#Starting logs...
sink("DataV2/Counterfactuals/logs/Calibration_preprocess.txt")

for (sample in c("current", "historical")) { #loop over cross sections
  
  ###_________PARAMETERS_________########
  source("CodeV2/Calibrate/Parameters/GlobalParameters.R")

  #Preferred specification for estimation (see Calibrate_amenities module). 
  #Will only be used to calibrate on historical data for placebo.
  BASELINE_SPECIFICATION <- list(pref = "SG", bySkill_to_pass = FALSE)



#Start code here
#_______________________________________________________________________________

  BlockGroup <- read_dta("DataV2/US_Data/Output/Constructed_Block_V2.dta") %>% #Block group level variables
                select(State, County, Tract, BlockGroup,
                       rank_density_CBSA, rank_inv_D2CBD, City_housing_density, CBSA_med_house_value) #Some variables we may want to retain for analysis later on. 

  #Importing sample geography
  SmpleGeo <- read_dta("DataV2/US_Data/Output/SampleGeography.dta") %>% select(-CBSA)

  #Regulation file 
  Regulation <- read_dta("DataV2/US_Data/Output/Constructed_Block_V2.dta") %>% 
    select(State, County, Tract, BlockGroup, CBSA, UnitDensityRestriction_cl, IncomeStringency_cl)
  Regulation$UnitDensityRestriction_cl[is.na(Regulation$UnitDensityRestriction_cl) | is.na(Regulation$IncomeStringency_cl) | Regulation$IncomeStringency_cl == 0] <- 0 #setting NA's to zero that were imputed in Merge_stringency.R

  #Land value density from raw regulation file
  if (sample == "current") {
    Regulation_V2 <- read_dta("DataV2/US_Data/Output/raw_Regulation_ByBlockGroup.dta") %>% select(State, County, Tract, BlockGroup, LandValueDensity_matched)
  }
  if (sample == "historical") {
    Regulation_V2 <- read_dta("DataV2/US_Data/Output/raw_Regulation_ByBlockGroup.dta") %>% select(State, County, Tract, BlockGroup, LandValueDensity_hist)
  }
  
  #Importing housing prices (note: difference in sample size is what we could construct from CoreLogic transactions + assessment coverage)
  if (sample == "current") {
    HedonicIndex <- read_dta("DataV2/US_Data/Output/FullHedonicIndex_complete.dta") %>% select(-CBSA)
  }
  if (sample == "historical") {
    HedonicIndex <- read_dta("DataV2/US_Data/Output/FullHedonicIndex_complete_historical.dta") %>% select(-CBSA, hedonicPrice_regulated, hedonicPrice_total)
  }
  
  #Housing supply elasticities
  HS_Elasticities <- read_dta("DataV2/US_Data/Output/HS_Elasticities.dta") %>% select(-CBSA, -CBSA_NAME)
  
  #Spending shares for each income type (to choose preference parameters to match demand)
  SpendShare <- read_dta("DataV2/US_Data/Output/HousingSpendshare_ByCity.dta") %>% select(starts_with("Income_spendshare_owner"), -ends_with("1")) #Note: median income spendshare is 0.25-- roughly in line with estimates from other data

  #Storing estimates of target spendshares in a vector
  SpendShares_to_Target <- as.numeric(SpendShare[1, ])

  #LOCAL ABILITY DISTRIBUTIONS.
  if (sample == "current") {
    LocalAbilityDist <- read_dta("DataV2/US_Data/Output/LocalAbilityDistributions.dta") %>% select(State, County, Tract, BlockGroup, starts_with("Population"),
                                                                                                   total_housing_units_cen, regulated_housingUnit_share,
                                                                                                   OwnerOccupier_share)
    
  }
  if (sample == "historical") {
    LocalAbilityDist <- read_dta("DataV2/US_Data/Output/LocalAbilityDistributions_historical.dta") %>% select(State, County, Tract, BlockGroup, starts_with("Population"),
                                                                                                              total_housing_units_cen, regulated_housingUnit_share)
  }
  
    
  #Note: differences in block groups are block groups that have households = 0 in 2020 census. 

  #Wages
  if (sample == "current") {
    Wages <- read_dta("DataV2/US_Data/Output/CityProd_individual.dta")
  }
  
  if (sample == "historical") {
    Wages <- read_dta("DataV2/US_Data/Output/CityProd_individual_historical.dta")
    Wages_V2 <- read_dta("DataV2/US_Data/Output/CityProd_individual.dta") %>% select(CBSA, starts_with("ability_grp")) #ability measures of center
  }
  
  #Joining this all to master
  Master <- left_join(HedonicIndex, SmpleGeo, by = c("State", "County", "Tract", "BlockGroup"))
  Master <- left_join(Master, BlockGroup, by = c("State", "County", "Tract", "BlockGroup"))
  Master <- left_join(Master, Regulation, by = c("State", "County", "Tract", "BlockGroup"))
  Master <- left_join(Master, Regulation_V2, by = c("State", "County", "Tract", "BlockGroup"))
  Master <- left_join(Master, Wages, by = c("CBSA"))
  if (sample == "historical") {
    Master <- left_join(Master, Wages_V2, by = c("CBSA"))
  }
  Master <- left_join(Master, LocalAbilityDist, by = c("State", "County", "Tract", "BlockGroup"))
  Master <- left_join(Master, HS_Elasticities, by = c("State", "County", "Tract", "BlockGroup"))

  rm(HedonicIndex, HS_Elasticities, 
   LocalAbilityDist, 
   Regulation, Wages, Wages_V2, SpendShare,
   SmpleGeo, BlockGroup, Regulation_V2)


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

  print(paste0("There are ", nrow(Master), " block groups remaining before calibration of sample ", sample))

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

  #Creating hedonicPrice variable from hedonic regressions. This will be a parameter in calibration.
  Master["hedonicPrice"] <- rep(NA, nrow(Master))
  Master$hedonicPrice[Master$Regulation_code == 1 | Master$Regulation_code == 2] <- Master$hedonicPrice_regulated[Master$Regulation_code == 1 | Master$Regulation_code == 2]
  Master$hedonicPrice[Master$Regulation_code == 0] <- Master$hedonicPrice_total[Master$Regulation_code == 0]

  #_____________________________________________________________________________________________________________________________________________________________________
  #PART 1 OF CALIBRATION________________________________________________________________________________________________________________________________________________
  #PART 1: Calculating the lambdas that corresponds to the hedonic indices we observe in the data.
  #_____________________________________________________________________________________________________________________________________________________________________


  #1.1: Measure of stringency of regulation = LandValueDensity/acre x Minimum lot size (acres)
  #Normalizing land value density == 0 if UnitDensityRestriction == 0 --this does NOT matter for unregulated neighborhoods.
  

  #Converting land values to flow costs. 
  #Convert_value_to_yr_flow_cost is in GlobalParameters, telling us how to make the adjustment. 
  if (sample == "current") {
    Master$LandValueDensity_matched[Master$UnitDensityRestriction_cl == 0] <- 0
    Master["IncomeStringency_model_rents"] <- (Master$UnitDensityRestriction_cl*Master$LandValueDensity_matched)*(Convert_value_to_yr_flow_cost)
  }
  if (sample == "historical") {
    Master$LandValueDensity_hist[Master$UnitDensityRestriction_cl == 0 | is.na(Master$LandValueDensity_hist)] <- 0 #replacing missing with no effective regulation for historical sample (subset of current due to more data sparsity)
    Master["IncomeStringency_model_rents"] <- (Master$UnitDensityRestriction_cl*Master$LandValueDensity_hist)*(Convert_value_to_yr_flow_cost)
  }

  #________________CENSORTING ULTRA STRINGENT NEIGHBORHOODS__________________________

  #Removing ultra rich neighborhoods to make sure they don't drive results.
  print(paste0("There are ", nrow(Master[Master$IncomeStringency_model_rents > regulation_censoring,]), " block groups that will have censored stringency"))
  Master$IncomeStringency_model_rents[Master$IncomeStringency_model_rents > regulation_censoring] <- regulation_censoring #Deleting...
  #We don't want a handful of ultra-stringent neighborhoods influencing our results, 
  #because we also censor the income distribution by using the ACS data. 


  #Normalizing prices so that mean == 1 
  #(note: some block groups dropped after calibration if no solution can be found, so final indices need not be exactly one)
  Master$hedonicPrice <- Master$hedonicPrice/mean(Master$hedonicPrice, na.rm = TRUE)
  #____________________________________________________________________________________

  #Median rent at the minimum lot size is $6500/year (after adjustments from merge_stringency.R -- remove locations with regulated lot size > 2x average lot size)
  print(paste0("The median income stringency (model based) for sample ", sample, " is ", median(Master$IncomeStringency_model_rents)))
  #7,326 after controlling
  print(paste0("The median income stringency (conditional on positive regulation) for sample ", sample, " is ", 
               median(Master[Master$UnitDensityRestriction_cl > 0,]$IncomeStringency_model_rents)))

  #highly skewed
  print(paste0("The mean income stringency for sample ", sample, " is ", mean(Master$IncomeStringency_model_rents)))
  print(paste0("The mean income stringency (conditional on positive regulation) for sample ", sample, " is ",
               mean(Master[Master$UnitDensityRestriction_cl > 0,]$IncomeStringency_model_rents))) #Conditional on a unit density restriction
  
 ######################################### FOR TESTING ##################################################################################
 #_______ SETTING POPULATIONS COMPLETELY PRICED OUT OF NEIGHBORHOODS TO ZERO. 
 # For Robustness: eliminates concerning assumptions about wealth and the dollar value of deregulation for these highly constrained types.
  
  #Make lowest income types not exist (impute to second highest will fix this in the model by changing price definitions)
  Master$ability_grp1 <- Master$ability_grp2
  
  test_pre <- rep(0, nrow(Master))
  for (incomeType in 1:7) {
    
    test_pre <- test_pre + Master[[paste0("Population_type_", incomeType)]]
    
    
  }

  if (sample == "current") { #Do this only for current sample, for now...
    for (incomeType in 1:7) {
      
      Master[[paste0("MLS_exceeds_income_", incomeType)]] <- ifelse(Master$IncomeStringency_model_rents >= Master[[paste0("ability_grp", incomeType)]]*Master$PooledWage,
                                                                    1, 0) 
      # Approx 1/6 of neighborhoods have the lowest income types priced out compl. of regulated zone. Only 4500 for second highest income levels
      # Suggests these types are considerably wealthier than what current observed income predicts, (transient income shocks, voluntary labour non-participation)
      # or they have access to low cost housing.
      # Either way...
      #Setting these to zero and re-scaling populations so total housing units in neighborhood is preserved; will only matter significantly for lowest income types
      #Will inflate average income of a household away from data; but that's fine.
      
      #Pushing populations over to next-highest income level if priced out locally.
      if (incomeType < 7) { #This procedure is never done for highest income types due to censoring
        Master[[paste0("Population_type_", incomeType + 1)]][Master[[paste0("MLS_exceeds_income_", incomeType)]] == 1] <- Master[[paste0("Population_type_", incomeType)]][Master[[paste0("MLS_exceeds_income_", incomeType)]] == 1] +
                                                                                                                          Master[[paste0("Population_type_", incomeType + 1)]][Master[[paste0("MLS_exceeds_income_", incomeType)]] == 1]
        Master[[paste0("Population_type_", incomeType)]][Master[[paste0("MLS_exceeds_income_", incomeType)]] == 1] <- 0 #setting this to zero
      }
      
      
      #For bySkill == TRUE, doing the same thing
      for (skill in c("College", "NoCollege")) {
        Master[[paste0("MLS_exceeds_income_", incomeType, "_", skill)]] <- ifelse(Master$IncomeStringency_model_rents >= Master[[paste0("ability_grp", incomeType)]]*Master[[paste0(skill, "Wage")]],
                                                                                  1, 0) 
        if (incomeType < 7) {
          Master[[paste0("Population_type_", skill, "_", incomeType + 1)]][Master[[paste0("MLS_exceeds_income_", incomeType, "_", skill)]] == 1] <- Master[[paste0("Population_type_", skill, "_", incomeType)]][Master[[paste0("MLS_exceeds_income_", incomeType, "_", skill)]] == 1] +
                                                                                                                                                    Master[[paste0("Population_type_", skill, "_", incomeType + 1)]][Master[[paste0("MLS_exceeds_income_", incomeType, "_", skill)]] == 1]
          Master[[paste0("Population_type_", skill, "_", incomeType)]][Master[[paste0("MLS_exceeds_income_", incomeType, "_", skill)]] == 1] <- 0 #setting this to zero
        }
        
      }
    
    }
  }
  
  #Testing if populations are preserved...
  test <- rep(0, nrow(Master))
  for (incomeType in 1:7) {
    
    test <- test + Master[[paste0("Population_type_", incomeType)]]
    
    
  }
  print(max(abs(test - test_pre)))
  
  
  
  #############################################################################################################
  ############################ START CALIBRATION HERE #########################################################
  #############################################################################################################
  
  #Importing function to solve for block group level prices, housing expenditure shares, etc.
  #Loop over all possible model calibrations we want to see...

  for (bySkill_to_pass in c(FALSE, TRUE)) { #BySkill or Pooled
    for (pref in c("CD", "SG")) { #Cobb-Douglas, StoneGeary
    
      #reloading parameters and function, compiling functions for additional speed
      source("CodeV2/Calibrate/Parameters/GlobalParameters.R") #Harmonized list of parameters
      source("CodeV2/Calibrate/Functions/Calibration_Functions_global.R")
      Calibrate_prices <- cmpfun(Calibrate_prices)
      ExcessDemand <- cmpfun(ExcessDemand)
      
      
      #Check if historical sample and baseline specification, else continue
      if (sample == "historical" & (BASELINE_SPECIFICATION$pref != pref | BASELINE_SPECIFICATION$bySkill_to_pass != bySkill_to_pass)) {
        next #continue on loop if historical AND not baseline specification
      }
      
      #Setup skill names to pass to functions
      if (bySkill_to_pass == TRUE) {
      
        skillVector <-  c("College", "NoCollege")
        skillName <- c("College_", "NoCollege_") 

      }else{
      
        skillVector <- c("Pooled")
        skillName <- c("")
  
      }
      
      #Setting up preference parameters
      if (pref == "CD"){ #if cobb-douglas preferences
      
        demandParameters_to_pass <- c(beta, 0) #min_hReq = 0  => Cobb-Douglas
        rm(min_hReq, beta_StGeary)
      
      }else{ #if stone geary preferences, pass new preference parameter
        demandParameters_to_pass <- c(beta_StGeary, min_hReq)
      }
      
    
      #START CALIBRATION _____________________________________________
    
      #Registering doParallel
      ncores  <- detectCores() - 1

      #Setting up number of cores (we want to conserve ram as this program is reasonably ram intensive)
      registerDoParallel(ncores)

      #Running dopar loop to solve for all variables
      print(paste0("Calibrating model bySkill=", bySkill_to_pass, " ", pref, " at ", Sys.time()))  
      tmp <- foreach(row = 1:nrow(Master),
                     .errorhandling = "pass", .packages = c("dplyr")) %dopar% { 
                 
                       #FOREACH output
                       return(   Calibrate_prices(Master[row,], 
                                                  demandParameters = demandParameters_to_pass,
                                                  bySkillVector = skillVector,
                                                  bySkillNames = skillName)    )
                 
                     }
      print(paste0("Calibrated model bySkill=", bySkill_to_pass, " ", pref, " at ", Sys.time()))  

      #closing cluster
      stopImplicitCluster()
  
      #Saving temp object as checkpoint (previous object takes about two hours to run on 10 cores)
      save(tmp, file = "DataV2/Counterfactuals/Calibration_Output/SupplyShifter_sln.RData")


      #Putting all these objects into the master data frame for use with other objects...
      Excess_demand_index <- rep(NA, nrow(Master))
      price_reg <- rep(NA, nrow(Master))
      lambda <- rep(NA, nrow(Master))
      consumptionValIndex <- list()
      PopulationAlloc <- list()
      hSpendShares <- list()
      
      houseExp_regulated <- rep(NA, nrow(Master))
      houseExp_unregulated <- rep(NA, nrow(Master))
    
      
      #Initializing vectors for storage
      for (skill in skillVector) {
        for (incomeType in 1:7) {
          consumptionValIndex[[skill]][[incomeType]] <- rep(NA, length(tmp))
          
          PopulationAlloc[[skill]][[incomeType]] <- list(rep(NA, length(tmp)),
                                                         rep(NA, length(tmp)))
          hSpendShares[[skill]][[incomeType]] <- list(rep(NA, length(tmp)),
                                                      rep(NA, length(tmp)))
    
        }
      }
      
      #Reg code vector (faster)
      Reg_code <- Master$Regulation_code
      
      #Storing list objects in vector for before storing in data frame (this is a lot faster than putting it directly in)
      for (row in 1:length(tmp)) { #Loop over all block groups
        if (is.numeric(tmp[[row]][[1]][1])) {
    
          Excess_demand_index[row] <- tmp[[row]]$ExcessDemand
          price_reg[row] <- tmp[[row]]$price
    
          houseExp_regulated[row] <- tmp[[row]]$housing_demand_expenditure[1]
          houseExp_unregulated[row] <- tmp[[row]]$housing_demand_expenditure[2]
        
          for (skill in skillVector) {  
            for (incomeType in 1:7) {
            
              consumptionValIndex[[skill]][[incomeType]][row] <- tmp[[row]]$consumption_Index[[skill]][incomeType]
              
              if (Reg_code[row] == 1) { #If partially regulated, disaggregate by zone
                  for (zone in c(1, 2)) {
                    PopulationAlloc[[skill]][[incomeType]][[zone]][row] <- tmp[[row]][["Populations"]][[skill]][incomeType, zone]
                    hSpendShares[[skill]][[incomeType]][[zone]][row] <- tmp[[row]][["housingExpenditureShare"]][[skill]][incomeType, zone]
                  }
              }else{
                  for (zone in c(1, 2)) {
                    hSpendShares[[skill]][[incomeType]][[zone]][row] <- tmp[[row]][["housingExpenditureShare"]][[skill]][incomeType] #spend shares same across zones
                  }
              }
              
              
              
              
            }
          }
    
        }
      }
   
    
    #Creating output of master dataset to save
    Master_out <- Master
    
     
    #Regulated housing price
    Master_out["price_regulated"] <- Master_out$hedonicPrice #measured hedonic price for regulated structures only
    Master_out["price_unregulated"] <- price_reg #regulated price solution. 
   
    #If completely regulated/unregulated
    Master_out$price_unregulated[Master_out$Regulation_code == 2 | Master_out$Regulation_code == 0] <- Master_out$price_regulated[Master_out$Regulation_code == 2 | Master_out$Regulation_code == 0] #setting reg=unreg price if no within-block-group variation in regulation

    #Excess demand index at current solutions
    Master_out["ExcessDemand"] <- Excess_demand_index

    #checking maximum deviation from equilibrium
    print(paste0("The max excess demand (reg share) is ", max(abs(Master_out$ExcessDemand), na.rm = TRUE))) #pretty close.
 
    #Putting consumption values in data frame, + zonal population and spending share allocations
    for (skill in skillVector) {
      for (incomeType in 1:7) {
         Master_out[paste0("consumption_Val_", skillName[which(skill == skillVector)], incomeType)] <- consumptionValIndex[[skill]][[incomeType]]
         
         for (zone in c(1, 2)) {
           Master_out[paste0("hSpendShare_", skillName[which(skill == skillVector)], incomeType, "_z", zone)] <- hSpendShares[[skill]][[incomeType]][[zone]]
           Master_out[paste0("Population_type_", skillName[which(skill == skillVector)], incomeType, "_z", zone)] <- PopulationAlloc[[skill]][[incomeType]][[zone]]
           
         }
  
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
     Master_out$land_regulated[Master_out$Regulation_code == 0] <- 0 #zero land for this regulation code

     #If Regulation_code == 1 or 2, calculate regulated land the normal way (to clear housing markets) (this is the ratio between total housing spending and value per acre)
     Master_out$land_regulated[Master_out$Regulation_code == 1 | Master_out$Regulation_code == 2] <- houseExp_regulated[Master_out$Regulation_code == 1 | Master_out$Regulation_code == 2]/
                                                                                                     (Master_out$IncomeStringency_model_rents[Master_out$Regulation_code == 1 | Master_out$Regulation_code == 2]/Master_out$UnitDensityRestriction_cl[Master_out$Regulation_code == 1 | Master_out$Regulation_code == 2])

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
     if (sample == "current") {
      write_dta(Master_out, paste0("DataV2/Counterfactuals/Master_post_calibration_bySkill", bySkill_to_pass, "_pref_", pref, ".dta"))
     }
     
     if (sample == "historical") {
      write_dta(Master_out, paste0("DataV2/Counterfactuals/Master_post_calibration_historical.dta"))
     }
  
    }
  }

  rm(list = ls())

} #end loop over samples


sink(NULL)


