#This file analyzes complete deregulation in an alternative world 
#where there were no fundamental amenities.
library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)
library(ggplot2)
library(forcats)
library(sf)
library(haven) #Reading stata.dta files 

#PARAMETERS
source("CodeV2/Counterfactual/Parameters/GlobalParameters.R")

#FUNCTIONS
source("CodeV2/Counterfactual/Functions/Analysis_Functions.R")

#_________________ PRELIMINARIES _________________________#
#SPECIFY BASELINE COUNTERFACTUAL OUTPUT
BASELINE_SPECIFICATION <- list(pref = "SG", bySkill_to_pass = FALSE) 
#Change specification here to get welfare statistics for different specifications.

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


#Load deregulation counterfactual 
load(paste0("DataV2/Counterfactuals/Counterfactual_Output/DiffFundamentals/", "FullDereg_NoFundamentalsFull", 
            "_EndoAmen_", TRUE, 
            "_EndoProd_", FALSE,
            "_bySkill_", BASELINE_SPECIFICATION$bySkill_to_pass,
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
Ct_Amenities <- Equilibrium_objects
rm(Equilibrium_objects)


#Load initial equilibrium with no fundamentals, everything else equal
load(paste0("DataV2/Counterfactuals/Counterfactual_Output/DiffFundamentals/", "InitialEq_NoFundamentalsFull", 
            "_EndoAmen_", TRUE, 
            "_EndoProd_", FALSE,
            "_bySkill_", BASELINE_SPECIFICATION$bySkill_to_pass, 
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
Init_eq <- Equilibrium_objects
rm(Equilibrium_objects)

#Load master initial file for extra columns
#Importing master data for comparison to equilibrium
load(paste0("DataV2/Counterfactuals/Init_eq_", 
            BASELINE_SPECIFICATION$bySkill_to_pass, 
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))

#Additional columns that are required for computation but didn't get passed
Init_eq["price_regulated"] <- Init_eq$housingPrice_z1
Init_eq["price_unregulated"] <- Init_eq$housingPrice_z2 #initial equilibrium prices

for (additional_data in c("IncomeStringency_model_rents", "regulated_housingUnit_share", "ALAND")) {
  Init_eq[[additional_data]] <- Master[[additional_data]]
}
rm(Master)



#Consumption adjustment factors for baseline spec
load(paste0("DataV2/Counterfactuals/Calibration_Output/consumption_AdjustmentFactor_bySkill",
            BASELINE_SPECIFICATION$bySkill_to_pass,
            "_pref_", BASELINE_SPECIFICATION$pref, ".Rdata"))


#Final land for residential use 
Init_eq["final_land_for_res"] <- Init_eq$land_regulated + Init_eq$land_unregulated #calibrated land
Ct_Amenities["ALAND"] <- Init_eq$ALAND #official landmass from census shapefiles

#Replace housing price z1 and z2 equal to the other IF only regulated or unregulated land, this does not matter for calculations other to remove NaNs from calculation. 
#NAs appear when land in a particular zone is zero.
Init_eq$price_regulated[is.nan(Init_eq$price_regulated)] <- Init_eq$price_unregulated[is.nan(Init_eq$price_regulated)] 
Init_eq$price_unregulated[is.nan(Init_eq$price_unregulated)] <- Init_eq$price_regulated[is.nan(Init_eq$price_unregulated)]

#_____________START ANALYSIS______________________________________
#PART 1: WELFARE 

source("CodeV2/Counterfactual/Functions/EquivalentVariation.R")
var_Amen <- matrix(NA, length(skillVector) , 7)

skillIndex <- 0
for (skill_to_pass in skillVector) {
  skillIndex <- skillIndex + 1
  for (i in 1:7) { 
    
    var_Amen[skillIndex, i] <- getVariation(Init = Init_eq, Ct = Ct_Amenities, incomeType = i, skill = skill_to_pass, demandParameters = demandParameters_to_pass)
    
    
  }
}

print(paste0("Welfare by income type from this deregulation exercise is... "))
print(var_Amen) #CONSIDERABLY LOWER. Can't tell if this is due to muting diff. in amenity value across space, tbh

#Shapely decomposition
#Amenities
Amenity_shapely <- matrix(NA, length(skillVector), 7)
skillIndex <- 0
for (skill_to_pass in skillVector) {
  skillIndex <- skillIndex + 1
  
  for (i in 1:7) {                        #Holding consumption at counterfactual levels
    Amenity_shapely[skillIndex, i] <-   (1/2)*( getWelfare_ShapelyDecomp(Consumption_data = Ct_Amenities, Amenity_data = Ct_Amenities, 
                                                                         skill = skill_to_pass, incomeType = i) -  
                                                  getWelfare_ShapelyDecomp(Consumption_data = Ct_Amenities, Amenity_data = Init_eq, 
                                                                           skill = skill_to_pass, incomeType = i) ) + 
      #Holding consumption at initial equilibrium levels
      (1/2)* ( getWelfare_ShapelyDecomp(Consumption_data = Init_eq, Amenity_data = Ct_Amenities, 
                                        skill = skill_to_pass, incomeType = i) - 
                 getWelfare_ShapelyDecomp(Consumption_data = Init_eq, Amenity_data = Init_eq, 
                                          skill = skill_to_pass, incomeType = i) ) 
    #Making sure this is relative to initial welfare levels, as they are measured in utils
    Amenity_shapely[skillIndex, i] <- Amenity_shapely[skillIndex, i]/getWelfare_ShapelyDecomp(Consumption_data = Init_eq, Amenity_data = Init_eq, 
                                                                                              skill = skill_to_pass, incomeType = i)
  }
  
}

#Consumption
Consumption_shapely <- matrix(NA, length(skillVector), 7)
skillIndex <- 0
for (skill_to_pass in skillVector) {
  skillIndex <- skillIndex + 1
  
  for (i in 1:7) {         #Holding Amenities at counterfactual levels
    Consumption_shapely[skillIndex, i] <- (1/2)*( getWelfare_ShapelyDecomp(Consumption_data = Ct_Amenities, Amenity_data = Ct_Amenities, 
                                                                           skill = skill_to_pass, incomeType = i) - 
                                                    getWelfare_ShapelyDecomp(Consumption_data = Init_eq, Amenity_data = Ct_Amenities, 
                                                                             skill = skill_to_pass, incomeType = i) ) + 
      #Holding Amenities at initial equilibrium levels
      (1/2)*( getWelfare_ShapelyDecomp(Consumption_data = Ct_Amenities, Amenity_data = Init_eq, 
                                       skill = skill_to_pass, incomeType = i) - 
                getWelfare_ShapelyDecomp(Consumption_data = Init_eq, Amenity_data = Init_eq, 
                                         skill = skill_to_pass, incomeType = i) ) 
    
    #Making sure this is relative to initial welfare level (i.e. expressed as % change in utility)
    Consumption_shapely[skillIndex, i] <- Consumption_shapely[skillIndex, i]/getWelfare_ShapelyDecomp(Consumption_data = Init_eq, Amenity_data = Init_eq, 
                                                                                                      skill = skill_to_pass, incomeType = i)
    
  }
  
}

#Total % change in welfare by type (in $ via equivalent variation)
tWelfare <- Amenity_shapely + Consumption_shapely
Amenity_shapely <- 100*(Amenity_shapely/tWelfare)*(var_Amen - 1) #comp_Var_store was created in welfare calculation above
Consumption_shapely <- 100*(Consumption_shapely/tWelfare)*(var_Amen - 1) 


print(Amenity_shapely)
print(Consumption_shapely) 


#Income exposure metric: How population-weighted neighborhood income changes by type
Income_exposure <- matrix(NA, length(skillVector), 7) #Average income faced by income type 
skillIndex <- 0
for (skill_to_pass in skillVector) {
  name_of_skill <- skillName[which(skill_to_pass == skillVector)]
  skillIndex <- skillIndex + 1 
  for (i in 1:7) { 
    Income_exposure[skillIndex, i] <- sum(Ct_Amenities$Avg_income*Ct_Amenities[[paste0("Population_type_", name_of_skill, i)]]/Ct_Amenities[[paste0("Total_Population_type_", name_of_skill, i)]])/
      sum(Init_eq$Avg_income*Init_eq[[paste0("Population_type_", name_of_skill, i)]]/Init_eq[[paste0("Total_Population_type_", name_of_skill, i)]])
    
  }
}

#Alternative measure: neighborhood income growth weighted by initial population (has origins as first order approximation)

print("Average income faced by type relative to baseline")
print(Income_exposure) #Similar levels of desegregation to exposure

#AMENITY SHAPELY IS POSITIVE FOR NO FUNDAMENTALS LOW INCOME, HIGHLY NEGATIVE FOR HIGH INCOME.
#Suggesting income sorting matters, but pales in comparison to welfare effects of desegregation!

#Aggregate welfare effects by component
total_population <- matrix(NA, length(skillVector), 7)
skillIndex <- 0
for (skill_to_pass in skillVector) {
  name_of_skill <- skillName[which(skill_to_pass == skillVector)]
  skillIndex <- skillIndex + 1
  for (i in 1:7) {
    total_population[skillIndex, i] <- Ct_Amenities[[paste0("Total_Population_type_", name_of_skill, i)]][1]
  }
}

total_Consumption_shapely <- (Consumption_shapely%*%t(total_population))/(sum(total_population))
total_Amenity_shapely <- (Amenity_shapely%*%t(total_population))/(sum(total_population))

#Putting this all in a graph. 

Welfare_barchart <- list()

for (skill_to_pass in skillVector) {
  Welfare_barchart[[skill_to_pass]] <- rep(NA, 2*8)
  
}

#Putting in estimates into the list
skillIndex <- 0
for (skill_to_pass in skillVector) {
  skillIndex <- skillIndex + 1
  for (i in 1:7) {
    
    index <- 2*(i - 1) + 1
    
    Welfare_barchart[[skill_to_pass]][index] <- Consumption_shapely[skillIndex, i]
    Welfare_barchart[[skill_to_pass]][index + 1] <- Amenity_shapely[skillIndex, i] 
    
  }
  
  Welfare_barchart[[skill_to_pass]][index + 2] <- total_Consumption_shapely 
  Welfare_barchart[[skill_to_pass]][index + 3]  <- total_Amenity_shapely 
  
}


#BAR CHART FOR OUTPUT
BarplotDF <- data.frame() #initializing data frame

for (skill_to_pass in skillVector) {
  
  BarplotDF <- rbind(BarplotDF,  data.frame(c(rep("1: 0 - 25,000", 2), rep("2: 25,000 - 50,000", 2), rep("3: 50,000 - 75,000", 2), 
                                              rep("4: 75,000 - 100,000", 2), rep("5: 100,000 - 150,000", 2), rep("6: 150,000 - 200,000", 2), 
                                              rep("7: 200,000+", 2), rep("Social Welfare", 2)), 
                                            rep(c("Consumption", "Amenity"), 2),
                                            Welfare_barchart[[skill_to_pass]], skill_to_pass)   )#end rbind
  
}


colnames(BarplotDF) <- c("Income", "Decomposition", "Value", "Education")


if (BASELINE_SPECIFICATION$bySkill_to_pass == FALSE) {
  
  ggplot(BarplotDF, aes(fill = Decomposition, y = Value, x = factor(Income))) + 
    geom_bar(position = "dodge", stat = "identity") +
    xlab("Household type (income in average city, 2020 USD)") + 
    ylab("Equivalent variation (% of income, by component)") + 
    theme(axis.text.x=element_text(size=rel(1), angle=90))
  ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/DiffFundamentals/WelfareDecompNoFund_Eq_var_bySkill_", 
                BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                BASELINE_SPECIFICATION$pref, ".png"), width = 25, height = 15, units = "cm") 
  
}else{
  
  ggplot(BarplotDF, aes(fill = Decomposition, y = Value, x = factor(Income))) + 
    geom_bar(position = "dodge", stat = "identity") + 
    facet_wrap(~Education) + 
    xlab("Household type (income in average city, 2020 USD)") + 
    ylab("Equivalent variation (% of income, by component)") + 
    theme(axis.text.x=element_text(size=rel(1), angle=90))
  ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/DiffFundamentals/WelfareDecompNoFund_Eq_var_bySkill_", 
                BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                BASELINE_SPECIFICATION$pref, ".png"), width = 25, height = 15, units = "cm") 
  
}

rm(BarplotDF)


#Ending program
#___________________________________________
rm(list = ls())

