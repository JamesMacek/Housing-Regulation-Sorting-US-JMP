#This file takes all unilateral deregulation output and creates a table for welfare analysis for different cities. 
#Renters and homeowners

#Packages
library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)
library(ggplot2)
library(forcats)
library(sf)
library(haven) #Reading stata.dta files 
sf_use_s2(FALSE) #Switching off spherical geometry
library(vtable) #for easy conditional tables

#PARAMETERS
source("CodeV2/Counterfactual/Parameters/GlobalParameters.R")

#FUNCTIONS
source("CodeV2/Counterfactual/Functions/Analysis_Functions.R")

#Vector of cities to put into table (must be saved on disk from Solve_All_Equilibria_Loop_allSpecs_PartialDereg.R)
cities <- c(#Additional Superstars
            "San Francisco-Oakland-Hayward, CA",
            "Washington-Arlington-Alexandria, DC-VA-MD-WV", 
            "Denver-Aurora-Lakewood, CO",
            "Los Angeles-Long Beach-Anaheim, CA",
            "New York-Newark-Jersey City, NY-NJ-PA",
  
  
            #Non-Superstars
            "Tampa-St. Petersburg-Clearwater, FL", 
            "San Antonio-New Braunfels, TX",
            "Rochester, NY",
            "Tucson, AZ",
            "St. Louis, MO-IL")

cityNames <- c("SanFrancisco", "Washington", "Denver", "LosAngeles", "NewYorkCity", "TampaBay", "SanAntonio", "Rochester", "Tucson", "St.Louis")

Table_cityNames <- c("San Francisco", "", 
                     "Washington", "", 
                     "Denver", "", 
                     "Los Angeles", "",
                     "New York City", "",
                     "Tampa Bay", "", 
                     "San Antonio", "", 
                     "Rochester" , "",
                     "Tucson", "",
                     "St.Louis", "") #City names for table

SuperStar <- c(rep(c("Yes", ""), length(cityNames)/2), rep(c("No", ""), length(cityNames)/2))

Amenities <- rep(c("Yes", "No"), length(cityNames))

#Create initial running table
Table_toOutput <- data.frame(matrix(nrow = 2*length(cities), ncol =  7 + 1 + 3)) #7 income types + 1 
                                                                               #landowner type + city name + SuperStar
Table_toOutput[, 1] <- Table_cityNames
Table_toOutput[, 2] <- SuperStar
Table_toOutput[, 3] <- Amenities
colnames(Table_toOutput) <- c("City", "Superstar?", "Endogenous Amenities", "0-25k", "25-50k", "50-75k",
                              "75-100k", "100-150k", "150-200k", "200k+", "Land Values")


#Baseline spec to run this code
BASELINE_SPECIFICATION <- list(pref = "SG", bySkill_to_pass = FALSE)#Only works for bySkill == FALSE currently

if (BASELINE_SPECIFICATION$bySkill == TRUE) {
  
  skillVector <-  c("College", "NoCollege")
  skillName <- c("College_", "NoCollege_") 
  
}else{
  
  skillVector <- c("Pooled")
  skillName <- c("")
  
}

if (BASELINE_SPECIFICATION$pref == "CD") {
  demandParameters_to_pass <- c(beta, 0)
}
if (BASELINE_SPECIFICATION$pref == "SG") {
  demandParameters_to_pass <- c(beta_StGeary, min_hReq)
}

#Fix 
Ct <- list("EndoAmen" = list(),
           "ExoAmen" = list()) #create list of counterfactual outcomes for each city deregulation, by assumptions on amenities


for (amen in c("EndoAmen", "ExoAmen")) {
  for (cityName in cityNames) {

  #Importing all files from Counterfactual_Output 
    if (amen == "EndoAmen") {
      EndoAmen_load <- TRUE
    }else{
      EndoAmen_load <- FALSE
    }
    
    load(paste0("DataV2/Counterfactuals/Counterfactual_Output/PartialDeregulation/", cityName, "_ReZoning_Full", 
                "_EndoAmen_", EndoAmen_load, 
                "_EndoProd_", FALSE,
                "_bySkill_", BASELINE_SPECIFICATION$bySkill_to_pass,
                "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
    Ct[[amen]][[cityName]] <- Equilibrium_objects_output
    rm(Equilibrium_objects_output)

  }
}

#Importing master data for comparison to equilibrium
load(paste0("DataV2/Counterfactuals/Init_eq_", 
            BASELINE_SPECIFICATION$bySkill_to_pass, 
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
Init_eq <- Master
rm(Master)

#Consumption adjustment factors for baseline spec
load(paste0("DataV2/Counterfactuals/Calibration_output/consumption_AdjustmentFactor_bySkill",
            BASELINE_SPECIFICATION$bySkill_to_pass,
            "_pref_", BASELINE_SPECIFICATION$pref, ".Rdata"))

#Final land for residential use for all data frames
Init_eq["final_land_for_res"] <- Init_eq$land_regulated + Init_eq$land_unregulated #calibrated land

for (amen in c("EndoAmen", "ExoAmen")) {
  for (cityName in cityNames) {
    Ct[[amen]][[cityName]]["ALAND"] <- Init_eq$ALAND #official landmass from census shapefiles
  }
}

#________________________________________________________________________________________________________
#__ START FILE HERE
#________________________________________________________________________________________________________

  #1. Calculate change in land values from baseline 

  #Calculating land values in initial regulated vs. unregulated equilibrium
  Init_eq["LandValInitEq_regulated"] <- (1/(1 + Init_eq$HS_Elasticity_imputed))*(Init_eq$price_regulated^(Init_eq$HS_Elasticity_imputed + 1))*Init_eq$lambda 
  Init_eq["LandValInitEq_unregulated"] <- (1/(1 + Init_eq$HS_Elasticity_imputed))*(Init_eq$price_unregulated^(Init_eq$HS_Elasticity_imputed + 1))*Init_eq$lambda #land type weighted land values per acre.
  
  #total land value in initial neighborhood
  Init_eq["Total_land_values_initial"] <- Init_eq$land_regulated*Init_eq$LandValInitEq_regulated + Init_eq$land_unregulated*Init_eq$LandValInitEq_unregulated
  
  for (amen in c("EndoAmen", "ExoAmen")) {
    for (cityName in cityNames) {
    
      #Replace housing price z1 and z2 equal to the other IF only regulated or unregulated land, this does not matter for calculations other to remove NaNs
      Ct[[amen]][[cityName]]$housingPrice_z1[is.nan(Ct[[amen]][[cityName]]$housingPrice_z1)] <- Ct[[amen]][[cityName]]$housingPrice_z2[is.nan(Ct[[amen]][[cityName]]$housingPrice_z1)] 
      Ct[[amen]][[cityName]]$housingPrice_z2[is.nan(Ct[[amen]][[cityName]]$housingPrice_z2)] <- Ct[[amen]][[cityName]]$housingPrice_z1[is.nan(Ct[[amen]][[cityName]]$housingPrice_z2)] 
      
      #Additional variables in data frame that we need
      Ct[[amen]][[cityName]]["price_regulated"] <-  Ct[[amen]][[cityName]]$housingPrice_z1
      Ct[[amen]][[cityName]]["price_unregulated"] <-  Ct[[amen]][[cityName]]$housingPrice_z2
      Ct[[amen]][[cityName]]["regulated_housingUnit_share"] <- Init_eq$regulated_housingUnit_share #shares at initial equilibrium to pass to equivalent variation function
      Ct[[amen]][[cityName]]["IncomeStringency_model_rents"] <- ifelse(test = ( Ct[[amen]][[cityName]]$CBSA_NAME == cities[which(cityName == cityNames)]),       #Cutting value of minimal lot in 2 -- a bit more than eliminating SF zoning
                                                                       yes = 1/2,                                                    # 
                                                                       no = 1)*Init_eq$IncomeStringency_model_rents #NOTE: IF YOU CHANGE COUNTERFACTUAL YOU !MUST MUST MUST! CHANGE THIS
      
      
      #Counterfactual land values
      Ct[[amen]][[cityName]]["LandValCtEq_regulated"] <- (1/(1 + Init_eq$HS_Elasticity_imputed))*(Ct[[amen]][[cityName]]$housingPrice_z1^(Ct[[amen]][[cityName]]$HS_Elasticity_imputed + 1))*Ct[[amen]][[cityName]]$lambda
      Ct[[amen]][[cityName]]["LandValCtEq_unregulated"] <- (1/(1 + Init_eq$HS_Elasticity_imputed))*(Ct[[amen]][[cityName]]$housingPrice_z2^(Ct[[amen]][[cityName]]$HS_Elasticity_imputed + 1))*Ct[[amen]][[cityName]]$lambda
    
      #Growth rates (log difference land value)
      Ct[[amen]][[cityName]]["LandValGrowth"] <- log( ((Init_eq$land_regulated*Init_eq$LandValInitEq_regulated)/(Init_eq$Total_land_values_initial))* #weights
                                                       (Ct[[amen]][[cityName]]$LandValCtEq_regulated/Init_eq$LandValInitEq_regulated) + 
                                       
                                                        ((Init_eq$land_unregulated*Init_eq$LandValInitEq_unregulated)/(Init_eq$Total_land_values_initial))* #weights by land in zone
                                                        (Ct[[amen]][[cityName]]$LandValCtEq_unregulated/Init_eq$LandValInitEq_unregulated)  )
    
      #Individual growth rate of total city land values, only in desired city
      growthRate_landval <- weighted.mean(exp(Ct[[amen]][[cityName]][Ct[[amen]][[cityName]]$CBSA_NAME == cities[which(cityName == cityNames)],]$LandValGrowth), 
                                          w = Init_eq[Init_eq$CBSA_NAME == cities[which(cityName == cityNames)],]$Total_land_values_initial) - 1

      
      print(paste0("The land value growth rate in ", cityName, " for model ", amen, " is ", 100*growthRate_landval))
      
      if (amen == "EndoAmen") {
        position = 0
      }else{
        position = 1
      }
      Table_toOutput[["Land Values"]][2*(which(cityName == cityNames)) - 1 + position] <- round(100*growthRate_landval, 2)
    
    
    }
  }
  
  #2. Calculating equivalent variation welfare effects...
  
  #Equivalent variation function
  source("CodeV2/Counterfactual/Functions/EquivalentVariation.R")
  
  #Start loop over specifications
  for (amen in c("EndoAmen", "ExoAmen")) {
  
  #Start loop over cities
    for (cityName in cityNames) {
    
      var_Amen <- matrix(NA, length(skillVector) , 7)
    
      #Extracting equivalent variation
      skillIndex <- 0
      for (skill_to_pass in skillVector) {
        skillIndex <- skillIndex + 1
        for (i in 1:7) { 
          
          new_welfare <- GetWelfareEqVar(Master_data = Ct[[amen]][[cityName]], EqVar = 1, FullDereg = FALSE, capitalGains = FALSE, #This call just calculates welfare in utils in ctfl
                                         skill = skill_to_pass, incomeType = i, demandParameters = demandParameters_to_pass)
          
          var_Amen[skillIndex, i] <- getVariation(Init = Init_eq, Welfare = new_welfare,
                                                  incomeType = i, skill = skill_to_pass, #partial deregulation calculation...
                                                  demandParameters = demandParameters_to_pass)
        
        
        }
      }
    
      #Storing welfare estimates in matrix
      
      if (amen == "EndoAmen") {
        position = 0
      }else{
        position = 1
      }
    
      Table_toOutput[2*(which(cityName == cityNames)) - 1 + position, 4:10] <- round(100*(var_Amen - 1), 2)
    
    }
  }
  
  #Writing table
  vtable::dftoLaTeX(Table_toOutput, 
                    file = "DataV2/Counterfactuals/Counterfactual_Output/PartialDeregulation/Welfare_UnilateralDereg.tex",
                    note = "Changes in renter welfare are conditional on living in the respective city post deregulation (as % of income).")
  
  #_____________________________________________________
  # Write long table using San Francisco only.
  #_____________________________________________________
  
  LongTable_toOutput <- as.data.frame(t(Table_toOutput[1:2, c(3:11)]))
  LongTable_toOutput  <-   LongTable_toOutput  %>% mutate(Firstrow = rownames(LongTable_toOutput)) %>% select(Firstrow, 1, 2)
  colnames(LongTable_toOutput) <- LongTable_toOutput[1, ] 
  LongTable_toOutput <- LongTable_toOutput[-1, ]
  vtable::dftoLaTeX(LongTable_toOutput, file = "DataV2/Counterfactuals/Counterfactual_Output/PartialDeregulation/SF_dereg_long.tex")
  
  rm(list = ls())
  
  