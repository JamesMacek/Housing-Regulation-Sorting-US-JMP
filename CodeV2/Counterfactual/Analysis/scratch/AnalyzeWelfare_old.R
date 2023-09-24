#Date created: Feb 6th, 2023

#This file measures welfare for the full deregulation exercise. 
#NOT compatible with BySkill argument. We only compute BySkill to check effect on aggregate labour productivity. 

library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)
library(ggplot2)
library(forcats)


#PARAMETERS
source("CodeV2/Counterfactual/Parameters/GlobalParameters.R")
bySkill = FALSE #Set to true to do equilibrium outcomes by skill

#FUNCTIONS
source("CodeV2/Counterfactual/Functions/Solve_Equilibrium_Functions_FullDereg.R")
source("CodeV2/Counterfactual/Functions/Analysis_Functions.R")
source("CodeV2/Counterfactual/Functions/EquivalentVariation.R")

#Importing consumption adjustment factors
consumptionAdjustment <- read_dta("DataV2/Counterfactuals/Calibration_Output/consumption_AdjustmentFactor.dta")

#Importing all files from Counterfactual_Output (Endogenous amenities and exogenous amenities)
load("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/Eq_Objects_Full_EndoAmen_TRUE_bySkill_FALSE.RData")
Ct_Amenities <- Equilibrium_objects
rm(Equilibrium_objects)
load("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/Eq_Objects_Full_EndoAmen_FALSE_bySkill_FALSE.RData")
Ct_NoAmenities <- Equilibrium_objects
rm(Equilibrium_objects)

#Importing master data for comparison to equilibrium
load("DataV2/Counterfactuals/Init_eq.RData")
Init_eq <- Master
rm(Master)


#PART 2: Finding % wage change to make person as worse off in new equilibrium at they were in old.
comp_var_Amen <- rep(NA, 7)
comp_var_NoAmen <- rep(NA, 7)

for (i in 1:7) { #loop over income types (remember, we raised consumption index to the power of consumptionAdjustment to correct for units in which migration elasticity was estimated on)
                 #This means we need to readjust by the adjustment factor to calculate the inverse compensating variation, which is written in terms of output. 
  comp_var_Amen[i] <- (getWelfare(Master_data = Ct_Amenities, bySkill = FALSE, incomeType = i)/getWelfare(Master_data = Init_eq, bySkill = FALSE, incomeType = i))^(1/consumptionAdjustment[[paste0("consumption_Adjustment", i)]][1]) - 1
  comp_var_NoAmen[i] <- (getWelfare(Master_data = Ct_NoAmenities, bySkill = FALSE, incomeType = i)/getWelfare(Master_data = Init_eq, bySkill = FALSE, incomeType = i))^(1/consumptionAdjustment[[paste0("consumption_Adjustment", i)]][1]) - 1
}

#Can show the (inverse) compensating variation satisfies the above equation. 

#What about equivalent variation? (THIS IS A LOT LOWER!)
eq_var_Amen <- rep(NA, 7)
eq_var_NoAmen <- rep(NA, 7)

for (i in 1:7) { #loop over income types (remember, we raised consumption index to the power of consumptionAdjustment to correct for units in which migration elasticity was estimated on)
 eq_var_Amen[i] <- getEquivalentVariation(Initial = Init_eq, Ct = Ct_Amenities, bySkill = FALSE, i) - 1
  eq_var_NoAmen[i] <- getEquivalentVariation(Initial = Init_eq, Ct = Ct_NoAmenities, bySkill = FALSE, i) - 1

}


#Solving for aggregate welfare changes
#vector of total populations  
total_population <- rep(NA, 7)
for (i in 1:7) {
  total_population[i] <- Ct_Amenities[[paste0("Total_Population_type_", i)]][1]
}

#Calculating social welfare == population weighted average of inverse compensating variation
total_Welfare_Amen <- (total_population%*%(comp_var_Amen + 1))/sum(total_population) - 1 
total_Welfare_NoAmen <- (total_population%*%(comp_var_NoAmen + 1))/sum(total_population) - 1

total_Welfare_EqVarAmen <- (total_population%*%(eq_var_Amen + 1))/sum(total_population) - 1 
total_Welfare_EqVarNoAmen <- (total_population%*%(eq_var_NoAmen + 1))/sum(total_population) - 1


#putting this in its equivalent variation vector for the following barchart
Welfare_barchart <- rep(NA, 2*(7 + 1))

for (i in 1:7) {
  index <- 2*(i - 1) + 1
  
  Welfare_barchart[index] <- eq_var_Amen[i]
  Welfare_barchart[index + 1] <- eq_var_NoAmen[i]
  
  
}

Welfare_barchart[2*(8 - 1) + 1] <- total_Welfare_EqVarAmen
Welfare_barchart[2*(8 - 1) + 2] <- total_Welfare_EqVarNoAmen

#BAR CHART
BarplotDF <- data.frame(c(rep("1: 0 - 25,000", 2), rep("2: 25,000 - 50,000", 2), rep("3: 50,000 - 75,000", 2), 
                          rep("4: 75,000 - 100,000", 2), rep("5: 100,000 - 150,000", 2), rep("6: 150,000 - 200,000", 2), 
                          rep("7: 200,000+", 2), rep("Social Welfare", 2)), 
                          rep(c("Endogenous Amenities", "Exogenous Amenities"), 2),
                          100*Welfare_barchart)
colnames(BarplotDF) <- c("Income", "Amenities", "Value")


ggplot(BarplotDF, aes(fill = Amenities, y = Value, x = factor(Income))) + 
  geom_bar(position = "dodge", stat = "identity") + 
  xlab("Household type (income in average city, 2020 USD)") + 
  ylab("Equivalent Variation (percent of income)") + 
  theme(axis.text.x=element_text(size=rel(1), angle=90))
ggsave("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/Welfare.png", width = 20, height = 12, units = "cm") 
rm(BarplotDF)

#putting this in its own compensation variation vector for the following barchart
Welfare_barchart <- rep(NA, 2*(7 + 1))

for (i in 1:7) {
  index <- 2*(i - 1) + 1
  
  Welfare_barchart[index] <- comp_var_Amen[i]
  Welfare_barchart[index + 1] <- comp_var_NoAmen[i]
  
  
}

Welfare_barchart[2*(8 - 1) + 1] <- total_Welfare_Amen
Welfare_barchart[2*(8 - 1) + 2] <- total_Welfare_NoAmen

#BAR CHART
BarplotDF <- data.frame(c(rep("1: 0 - 25,000", 2), rep("2: 25,000 - 50,000", 2), rep("3: 50,000 - 75,000", 2), 
                          rep("4: 75,000 - 100,000", 2), rep("5: 100,000 - 150,000", 2), rep("6: 150,000 - 200,000", 2), 
                          rep("7: 200,000+", 2), rep("Social Welfare", 2)), 
                        rep(c("Endogenous Amenities", "Exogenous Amenities"), 2),
                        100*Welfare_barchart)
colnames(BarplotDF) <- c("Income", "Amenities", "Value")

ggplot(BarplotDF, aes(fill = Amenities, y = Value, x = factor(Income))) + 
  geom_bar(position = "dodge", stat = "identity") + 
  xlab("Household type (income in average city, 2020 USD)") + 
  ylab("Inverse compensating Variation (percent of income)") + 
  theme(axis.text.x=element_text(size=rel(1), angle=90))
ggsave("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/Welfare_comp_var.png", width = 20, height = 12, units = "cm") 
rm(BarplotDF)



#Checking average income per capita across locations
print(paste0("In the endogenous amenities model, avg income in avg location goes up by ",
              mean(Ct_Amenities$Avg_income - Init_eq$Avg_income)))
print(paste0("In the exogenous amenities model, avg income in avg location goes up by ",
             mean(Ct_NoAmenities$Avg_income - Init_eq$Avg_income))) 

rm(Welfare_barchart, index, comp_var_Amen, comp_var_NoAmen, total_Welfare_Amen,
   total_Welfare_NoAmen)

#PART 2: analyzing the welfare of landlords by asking about the change in the average value of land across all locations.
LandData <- Init_eq %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME)
LandData["LandValAmenities"] <- (Ct_Amenities$housingPrice^(Ct_Amenities$HS_Elasticity_imputed + 1))*Ct_Amenities$lambda
LandData["LandValNoAmenities"] <- (Ct_NoAmenities$housingPrice^(Ct_Amenities$HS_Elasticity_imputed + 1))*Ct_Amenities$lambda

LandData["LandValInitEq"] <- (Init_eq$price_reg^(Init_eq$HS_Elasticity_imputed + 1))*Init_eq$lambda*(Init_eq$land_regulated/Init_eq$final_land_for_res) +
                             ((exp((4)*log(Init_eq$hedonicPrice) - (3)*log(Init_eq$price_reg)))^(Init_eq$HS_Elasticity_imputed + 1))*Init_eq$lambda*(Init_eq$land_unregulated/Init_eq$final_land_for_res) #land type weighted land values per acre.
                              #Remember, hedonic price = 0.75*price_reg + 0.25*price_unreg from Calibrate_SupplyShifters...


LandData["InitialStringency_model"] <- ((Init_eq$price_reg)^(Init_eq$HS_Elasticity_imputed + 1))*Init_eq$lambda*Init_eq$UnitDensityRestriction_cl*Init_eq$Population_reg #use lot size stringency from model
LandData["InitialStringency"] <- Init_eq$IncomeStringency_cl #use lot size stringency from data
LandData["LandValGrowth"] <- log(LandData$LandValAmenities/LandData$LandValInitEq)
LandData["LandValGrowthNoAm"] <- log(LandData$LandValNoAmenities/LandData$LandValInitEq)

#Printing changes in land value for an average block group
print(paste0("The change in land values in an average location with endogenous amenities is ", (mean(exp(LandData$LandValGrowth)) - 1)*100, " percent.")) #large losses to landowners.
print(paste0("The change in land values in an average location with exogenous amenities is ", (mean(exp(LandData$LandValGrowthNoAm)) - 1)*100, " percent."))

#Graphing from Model
ggplot() + 
      geom_point(data = LandData[LandData$InitialStringency_model > 0,], aes(x = InitialStringency_model, y = LandValGrowth), color = "black", size = 0.5, alpha = 0.1) +  #Set manual scales to remove outliers 
      geom_smooth(data = LandData[LandData$InitialStringency_model > 0,], aes(x = InitialStringency_model, y = LandValGrowth, color = "Endogenous Amenities"), method = "lm") +
      geom_smooth(data = LandData[LandData$InitialStringency_model > 0,], aes(x = InitialStringency_model, y = LandValGrowthNoAm, color = "Exogenous Amenities"), method = "lm") +
      scale_colour_manual(name="Model", values = c("red", "blue")) + 
      xlab("Lot Size Stringency in Initial Equilibrium (from Model, in Dollars)") + 
      ylab("Log Differences in Land Value (Dollar per acre)")
      ggsave("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/StringencyChangeLandVal.png", width = 20, height = 12, units = "cm") 

#Graphing from Data (conditional on assigning positive regulation in the data)   
ggplot() + 
  geom_point(data = LandData[LandData$InitialStringency_model > 0,], aes(x = InitialStringency, y = LandValGrowth), color = "black", size = 0.5, alpha = 0.1) +  #Set manual scales to remove outliers 
  geom_smooth(data = LandData[LandData$InitialStringency_model > 0,], aes(x = InitialStringency, y = LandValGrowth, color = "Endogenous Amenities"), method = "lm") +
  geom_smooth(data = LandData[LandData$InitialStringency_model > 0,], aes(x = InitialStringency, y = LandValGrowthNoAm, color = "Exogenous Amenities"), method = "lm") +
  scale_colour_manual(name="Model", values = c("red", "blue")) + 
  xlab("Lot Size Stringency in Initial Equilibrium (from Data, in Dollars)") + 
  ylab("Log Differences in Land Value (Dollar per acre)") + 
  coord_cartesian(xlim = c(0, 1e+07)) #Note: we exclude neighborhoods that have too high regulation in the model. 
  #ggsave("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/StringencyChangeLandVal_DataBased.png", width = 20, height = 12, units = "cm") 
  
       

#PART 3: WELFARE DECOMPOSITION ACROSS MOBILITY, NOMOBILITY AND WITHIN CITY MOBILITY MODELS.
#Loading...
load("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/Eq_Objects_WithinCityMobility_EndoAmen_TRUE_bySkill_FALSE.RData")
Ct_Amenities_WithinCity <- Equilibrium_objects
rm(Equilibrium_objects)

load("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/Eq_Objects_NoMobility_EndoAmen_TRUE_bySkill_FALSE.RData")
Ct_Amenities_NoMobility <- Equilibrium_objects
rm(Equilibrium_objects)

comp_var <- matrix(nrow = 7, ncol = 3)

for (i in 1:7) {
  comp_var[i, 1] <- ((getMobilityWelfare(Master_data = Ct_Amenities, bySkill = FALSE, incomeType = i)/getMobilityWelfare(Master_data = Init_eq, bySkill = FALSE, incomeType = i)) - 
                    (getMobilityWelfare(Master_data = Ct_Amenities_WithinCity, bySkill = FALSE, incomeType = i)/getMobilityWelfare(Master_data = Init_eq, bySkill = FALSE, incomeType = i)))/
                    ((getMobilityWelfare(Master_data = Ct_Amenities, bySkill = FALSE, incomeType = i)/getMobilityWelfare(Master_data = Init_eq, bySkill = FALSE, incomeType = i)) - 1)

  comp_var[i, 2] <- ((getMobilityWelfare(Master_data = Ct_Amenities_WithinCity, bySkill = FALSE, incomeType = i)/getMobilityWelfare(Master_data = Init_eq, bySkill = FALSE, incomeType = i)) - 
                      (getMobilityWelfare(Master_data = Ct_Amenities_NoMobility, bySkill = FALSE, incomeType = i)/getMobilityWelfare(Master_data = Init_eq, bySkill = FALSE, incomeType = i)))/
                    ((getMobilityWelfare(Master_data = Ct_Amenities, bySkill = FALSE, incomeType = i)/getMobilityWelfare(Master_data = Init_eq, bySkill = FALSE, incomeType = i)) - 1)
  
  comp_var[i, 3] <- ((getMobilityWelfare(Master_data = Ct_Amenities_NoMobility, bySkill = FALSE, incomeType = i)/getMobilityWelfare(Master_data = Init_eq, bySkill = FALSE, incomeType = i)) - 1)/
                    ((getMobilityWelfare(Master_data = Ct_Amenities, bySkill = FALSE, incomeType = i)/getMobilityWelfare(Master_data = Init_eq, bySkill = FALSE, incomeType = i)) - 1)
  
}
#Most welfare gains coming equally from within-city mobility + reduced housing prices.

#Getting aggregate measures 
comp_var <- 100*(t(comp_var)%*%total_population)/(sum(total_population))


 
#Show average exposure to amenities unanimously increases, mostly for within-city migration. We will show this.
  
amen_exposure <- matrix(nrow = 7, ncol = 3)
for (i in 1:7) {
  amen_exposure[i, 1] <- ((getAmenityExposure(Master_data = Ct_Amenities, bySkill = FALSE, incomeType = i) - getAmenityExposure(Master_data = Ct_Amenities_WithinCity, bySkill = FALSE, incomeType = i))/getAmenityExposure(Master_data = Init_eq, bySkill = FALSE, incomeType = i))/
                         (getAmenityExposure(Master_data = Ct_Amenities, bySkill = FALSE, incomeType = i)/getAmenityExposure(Master_data = Init_eq, bySkill = FALSE, incomeType = i))
  amen_exposure[i, 2] <- ((getAmenityExposure(Master_data = Ct_Amenities_WithinCity, bySkill = FALSE, incomeType = i))/getAmenityExposure(Master_data = Init_eq, bySkill = FALSE, incomeType = i))/
                          (getAmenityExposure(Master_data = Ct_Amenities, bySkill = FALSE, incomeType = i)/getAmenityExposure(Master_data = Init_eq, bySkill = FALSE, incomeType = i))
  amen_exposure[i, 3] <- 0 #NO CHANGE IN AMENITIES IF RESTRICTING MOBILITY
  
}

amen_exposure <- 100*(t(amen_exposure)%*%total_population)/(sum(total_population))

#Show the breakdown by income type
amen_exposure_decomposition <- matrix(nrow = 7, ncol = 3)
for (i in 1:7) {
  amen_exposure_decomposition [i, 1] <- ((getAmenityExposure(Master_data = Ct_Amenities, bySkill = FALSE, incomeType = i) - getAmenityExposure(Master_data = Ct_Amenities_WithinCity, bySkill = FALSE, incomeType = i))/getAmenityExposure(Master_data = Init_eq, bySkill = FALSE, incomeType = i))

  amen_exposure_decomposition [i, 2] <- ((getAmenityExposure(Master_data = Ct_Amenities_WithinCity, bySkill = FALSE, incomeType = i))/getAmenityExposure(Master_data = Init_eq, bySkill = FALSE, incomeType = i))

  amen_exposure_decomposition [i, 3] <- 1 #NO CHANGE IN AMENITIES IF RESTRICTING MOBILITY
  
}
#seems to be that high income individuals are moving to high density neighborhoods. They like the amenities conferred by these locations. 
#Most amenity boosts come from within city mobility, which is perhaps not surprising 

#Can we explain why?
#In general, people are exposed on average to better amenities, and this must be driven by lower commute times because of the contribution of the policy to urban sprawl.
#This is precisely why we report larger welfare gains coming from within-city mobility rather than across city mobility, and this is corroborated with increases in amenity exposure.

#Creating barchart. 
BarplotDF <- data.frame(rep(c("Across-city Mobility", "Within-city Mobility", "No Mobility"), 2),
                        c(rep("Welfare Decomposition", 3), rep("Amenity Exposure Decomposition", 3)), 
                        c(comp_var, amen_exposure))
colnames(BarplotDF) <- c("Mobility", "Decomposition_Type", "Value")


ggplot(BarplotDF, aes(y = Value, x = fct_inorder(Mobility), fill = fct_inorder(Decomposition_Type))) + 
  geom_bar(position = "dodge", stat = "identity") + 
  facet_wrap(~fct_inorder(Decomposition_Type)) + 
  xlab("Decomposition") + 
  ylab("Decomposition Value (As % of total change in full mobility equilibrium)") + 
  theme(axis.text.x=element_text(size=rel(1), angle=90), legend.position="none")
ggsave("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/Welfare_mobility_decomp.png")


rm(list = ls())

