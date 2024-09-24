#This file solves for productivity by skill to pass to the bySkill version of the model.
#Date created: Febuary 10th.
#Pulls any bySkill == TRUE version of the model

library(dplyr)
library(haven)
library(labelled)
library(readr)
library(collapse)


###_________PARAMETERS_________########
source("CodeV2/Calibrate/Parameters/GlobalParameters.R")

Master <- read_dta("DataV2/Counterfactuals/Master_post_calibration_bySkillTRUE_pref_SG.dta") 
         #city populations targeted by data, so calibration procedure does not depend on assumptions

#Calculating a few things we need: total labour supply by skill, total output, and wages by skill

#Summing up output for residents within neighborhood by skill
for (skill in c("College", "NoCollege")) {
  
  Master[paste0("Labour_supply_city_", skill)] <- 0 
  
  for (incomeType in 1:7) {
    Master[paste0("Labour_supply_city_", skill)] <- Master[[paste0("Labour_supply_city_", skill)]] + 
                                                    Master[[paste0("ability_grp", incomeType)]]*Master[[paste0("Population_type_", skill, "_", incomeType)]]
    
    
  }
  
  Master[paste0("Output_city_", skill)] <- Master[[paste0(skill, "Wage")]]*Master[paste0("Labour_supply_city_", skill)]
}

Master[paste0("Output_city")] <-   Master[paste0("Output_city_", "College")] +  Master[paste0("Output_city_", "NoCollege")]

#Collapsing at city level
CityProd <- collap(Master, Labour_supply_city_College + Labour_supply_city_NoCollege +
                           Output_city ~ CBSA + CBSA_NAME, FUN = c("fsum"))

CityProd_toJoin <- collap(Master, CollegeWage + NoCollegeWage + PooledWage ~ CBSA + CBSA_NAME, FUN = c("fmean"))

CityProd <- left_join(CityProd, CityProd_toJoin, by = c("CBSA", "CBSA_NAME"))
rm(CityProd_toJoin)


for (skill in c("College", "NoCollege")) {
  
  CityProd[paste0("Productivity_", skill)] <- ((CityProd[["Output_city"]]/CityProd[[paste0("Labour_supply_city_", skill)]])^(1/(1-sigma)))*(CityProd[[paste0(skill, "Wage")]]^((sigma)/(sigma-1))) 
}

#Writing data
write_dta(CityProd, "DataV2/Counterfactuals/Calibration_Output/City_Productivity.dta")

#Showing that all of this rationalizes total output

for (skill in c("College", "NoCollege")) {
  
  CityProd[paste0("output_ag_", skill)] <- CityProd[[paste0("Productivity_", skill)]]*CityProd[[paste0("Labour_supply_city_", skill)]]
  
}

CityProd["output_ag"] <- (CityProd[paste0("output_ag_", "College")]^((sigma - 1)/sigma) + CityProd[paste0("output_ag_", "NoCollege")]^((sigma - 1)/sigma))^(sigma/(sigma - 1)) 

print(paste0("The error is ", max(abs(CityProd$output_ag - CityProd$Output_city)))) #very close. 

rm(list = ls())
