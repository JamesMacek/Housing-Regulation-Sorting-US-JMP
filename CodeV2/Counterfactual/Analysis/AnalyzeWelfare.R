#Date created: Feb 6th, 2023

#This file measures welfare for the full deregulation exercise. 

library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)
library(ggplot2)
library(forcats)


#PARAMETERS
source("CodeV2/Counterfactual/Parameters/GlobalParameters.R")

#FUNCTIONS
source("CodeV2/Counterfactual/Functions/Solve_Equilibrium_Functions_FullDereg_current.R")
source("CodeV2/Counterfactual/Functions/Analysis_Functions.R")


#_______________________________________________________________________________ PRELIMINARIES

#SPECIFY BASELINE COUNTERFACTUAL OUTPUT
BASELINE_SPECIFICATION <- list(pref = "SG", bySkill_to_pass = FALSE) #estimating omega will not work for bySkill == True at current codebase
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


#Importing all files from Counterfactual_Output (Endogenous amenities)
load(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", "Eq_Objects_FULL", 
            "_EndoAmen_", TRUE, 
            "_EndoProd_", FALSE,
            "_bySkill_", BASELINE_SPECIFICATION$bySkill_to_pass,
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
Ct_Amenities <- Equilibrium_objects
rm(Equilibrium_objects)

#Importing all files from Counterfactual_Output (Exogenous amenities)
load(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", "Eq_Objects_FULL", 
            "_EndoAmen_", FALSE, 
            "_EndoProd_", FALSE,
            "_bySkill_", BASELINE_SPECIFICATION$bySkill_to_pass,
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
Ct_NoAmenities <- Equilibrium_objects
rm(Equilibrium_objects)



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
#Final land for residential use 
Init_eq["final_land_for_res"] <- Init_eq$land_regulated + Init_eq$land_unregulated
Ct_Amenities["ALAND"] <- Init_eq$ALAND #official landmass from census shapefiles
Ct_NoAmenities["ALAND"] <- Init_eq$ALAND


#__________________________________________________________________________________________________
#PART 1: Finding % wage change to make person as worse off in new equilibrium at they were in old.

for (variation in c("Compensating", "Equivalent")) {
  
  if (variation == "Compensating") {
    source("CodeV2/Counterfactual/Functions/CompensatingVariation.R") #Load compensating variation function, changes getVariation
    saveString <- "Comp"
    plotTitle <- "Inverse Compensating Variation (percent of income)"
  }
  
  if (variation == "Equivalent") {
    source("CodeV2/Counterfactual/Functions/EquivalentVariation.R")
    saveString <- "Eq"
    plotTitle <- "Equivalent Variation (percent of income)"
  }

  var_Amen <- matrix(NA, length(skillVector) , 7)
  var_NoAmen <- matrix(NA, length(skillVector), 7)

  skillIndex <- 0
  for (skill_to_pass in skillVector) {
    skillIndex <- skillIndex + 1
    for (i in 1:7) { 
      
      if (variation == "Compensating") {
        var_Amen[skillIndex, i] <- 1/(getVariation(Init = Init_eq, Ct = Ct_Amenities, incomeType = i, skill = skill_to_pass, demandParameters = demandParameters_to_pass)) 
        var_NoAmen[skillIndex, i] <- 1/(getVariation(Init = Init_eq, Ct = Ct_NoAmenities, incomeType = i, skill = skill_to_pass, demandParameters = demandParameters_to_pass)) 
      }
      
      if (variation == "Equivalent") {
        var_Amen[skillIndex, i] <- getVariation(Init = Init_eq, Ct = Ct_Amenities, incomeType = i, skill = skill_to_pass, demandParameters = demandParameters_to_pass)
        var_NoAmen[skillIndex, i] <- getVariation(Init = Init_eq, Ct = Ct_NoAmenities, incomeType = i, skill = skill_to_pass, demandParameters = demandParameters_to_pass)
      }
      
    }
  }

  #Solving for aggregate welfare changes
  #vector of total populations  
  total_population <- matrix(NA, length(skillVector), 7)
  skillIndex <- 0
  for (skill_to_pass in skillVector) {
    name_of_skill <- skillName[which(skill_to_pass == skillVector)]
    skillIndex <- skillIndex + 1
    for (i in 1:7) {
      total_population[skillIndex, i] <- Ct_Amenities[[paste0("Total_Population_type_", name_of_skill, i)]][1]
    }
  }

  #Calculating social welfare == population weighted average of variation across all types
  total_Welfare_Amen <- sum( sum(diag( (total_population%*%(t(var_Amen))) ))/sum(total_population) )
  total_Welfare_NoAmen <- sum( sum(diag( (total_population%*%(t(var_NoAmen))) ))/sum(total_population) )

  print(paste0("The ", saveString, " social welfare for renters at baseline is ", total_Welfare_Amen, " for endogenous amenities."))
  print(paste0("The ", saveString, " social welfare for renters at baseline is ", total_Welfare_NoAmen, " for exogenous amenities."))


  #_______________Reporting welfare in bar chart__________________________________
  #Putting equivalent and compensating variation vectors into bar chart
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
  
      Welfare_barchart[[skill_to_pass]][index] <- var_Amen[skillIndex, i] - 1
      Welfare_barchart[[skill_to_pass]][index + 1] <- var_NoAmen[skillIndex, i] - 1
    
    }
  
    Welfare_barchart[[skill_to_pass]][index + 2] <- total_Welfare_Amen - 1
    Welfare_barchart[[skill_to_pass]][index + 3]  <- total_Welfare_NoAmen - 1
  
  }

  #BAR CHART
  BarplotDF <- data.frame() #initializing data frame

  for (skill_to_pass in skillVector) {
    BarplotDF <- rbind(BarplotDF,  data.frame(c(rep("1: 0 - 25,000", 2), rep("2: 25,000 - 50,000", 2), rep("3: 50,000 - 75,000", 2), 
                                                rep("4: 75,000 - 100,000", 2), rep("5: 100,000 - 150,000", 2), rep("6: 150,000 - 200,000", 2), 
                                                rep("7: 200,000+", 2), rep("Social Welfare", 2)), 
                                                rep(c("Endogenous Amenities", "Exogenous Amenities"), 2),
                                                100*Welfare_barchart[[skill_to_pass]], skill_to_pass)   )#end rbind

  }

  colnames(BarplotDF) <- c("Income", "Amenities", "Value", "Education")
  
  
  if (BASELINE_SPECIFICATION$bySkill_to_pass == FALSE) {
  
    ggplot(BarplotDF, aes(fill = Amenities, y = Value, x = factor(Income))) + 
           geom_bar(position = "dodge", stat = "identity") +
           xlab("Household type (income in average city, 2020 USD)") + 
           ylab(plotTitle) + 
           theme(axis.text.x=element_text(size=rel(1), angle=90))
    ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/Welfare_", saveString, "_var_bySkill_", 
                 BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                 BASELINE_SPECIFICATION$pref, ".png"), width = 25, height = 15, units = "cm") 
  rm(BarplotDF)
  }else{
    
    ggplot(BarplotDF, aes(fill = Amenities, y = Value, x = factor(Income))) + 
      geom_bar(position = "dodge", stat = "identity") + 
      facet_wrap(~Education) + 
      xlab("Household type (income in average city, 2020 USD)") + 
      ylab(plotTitle) + 
      theme(axis.text.x=element_text(size=rel(1), angle=90))
    ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/Welfare_", saveString, "_var_bySkill_", 
                  BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                  BASELINE_SPECIFICATION$pref, ".png"), width = 25, height = 15, units = "cm") 
    rm(BarplotDF)
  }

  rm(skillIndex, skill_to_pass)
} #End loop over welfare estimates

#PART 2: analyzing the welfare of landlords by asking about the change in the average value of land across all locations.
LandData <- Init_eq %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME)
LandData["LandValAmenities"] <- (Ct_Amenities$housingPrice^(Ct_Amenities$HS_Elasticity_imputed + 1))*Ct_Amenities$lambda
LandData["LandValNoAmenities"] <- (Ct_NoAmenities$housingPrice^(Ct_Amenities$HS_Elasticity_imputed + 1))*Ct_Amenities$lambda

LandData["LandValInitEq"] <- (Init_eq$price_regulated^(Init_eq$HS_Elasticity_imputed + 1))*Init_eq$lambda*(Init_eq$land_regulated/Init_eq$final_land_for_res) +
                             ((Init_eq$price_unregulated)^(Init_eq$HS_Elasticity_imputed + 1))*Init_eq$lambda*(Init_eq$land_unregulated/Init_eq$final_land_for_res) #land type weighted land values per acre.

#use lot size stringency from model
LandData["InitialStringency"] <- Init_eq$IncomeStringency_cl #use empirical measure of lot size stringency. Note: non finite values treated as zero in model, NA in empirical work. Distinction does not matter. 
LandData["LandValGrowth"] <- log(LandData$LandValAmenities/LandData$LandValInitEq)
LandData["LandValGrowthNoAm"] <- log(LandData$LandValNoAmenities/LandData$LandValInitEq)

#Printing changes in land value for an average block group
print(paste0("The change in land values in an average location with endogenous amenities at baseline is ", (mean(exp(LandData$LandValGrowth)) - 1)*100, " percent.")) #large losses to landowners.
print(paste0("The change in land values in an average location with exogenous amenities at baseline is ", (mean(exp(LandData$LandValGrowthNoAm)) - 1)*100, " percent."))

ggplot() + 
  geom_point(data = LandData[LandData$InitialStringency > 0,], aes(x = InitialStringency, y = LandValGrowth), color = "black", size = 0.5, alpha = 0.1) +  #censor graph for locations with minimum lot sizes
  geom_smooth(data = LandData[LandData$InitialStringency > 0,], aes(x = InitialStringency, y = LandValGrowth, color = "Endogenous Amenities"), method = "lm") +
  geom_smooth(data = LandData[LandData$InitialStringency > 0,], aes(x = InitialStringency, y = LandValGrowthNoAm, color = "Exogenous Amenities"), method = "lm") +
  scale_colour_manual(name="Model", values = c("red", "blue")) + 
  xlab("Lot Size Stringency in Initial Equilibrium (from Data, in 2020 USD)") + 
  ylab("Log Differences in Land Value") + 
  
  coord_cartesian(xlim = c(0,
                           max(LandData[LandData$InitialStringency > 0 & 
                                        LandData$InitialStringency < quantile(LandData[LandData$InitialStringency > 0,]$InitialStringency, 
                                                                      probs = c(0.99), na.rm = TRUE),]$InitialStringency, na.rm = TRUE))) #Cut off x axis at 99th percentile for plot

ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/StringencyChangeLandVal", 
               BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
               BASELINE_SPECIFICATION$pref, ".png"), width = 20, height = 12, units = "cm") 

#________________________________________________________________________________________________________________________
#PART 3: Consumption and Amenity value decomposition IN CHANGES

AmenityExposure <- matrix(NA, length(skillVector), 7)

skillIndex <- 0
for (skill_to_pass in skillVector) {
  skillIndex <- skillIndex + 1
  for (i in 1:7) {
    
    AmenityExposure[skillIndex, i] <- getAmenityExposureGrowth(Init_data = Init_eq, Ct_data = Ct_Amenities, skill = skill_to_pass, incomeType = i)
    
    
  } 
}

#Income exposure-- no changes given by Omega
IncomeExposure <- matrix(NA, length(skillVector), 7)

skillIndex <- 0
for (skill_to_pass in skillVector) {
  skillIndex <- skillIndex + 1
  for (i in 1:7) {
    
    IncomeExposure[skillIndex, i] <- getIncomeExposureGrowth(Init_data = Init_eq, Ct_data = Ct_Amenities, skill = skill_to_pass, incomeType = i)
    
    
  } 
}

#Consumption exposure
ConsumptionExposure <- matrix(NA, length(skillVector), 7)

skillIndex <- 0
for (skill_to_pass in skillVector) {
  skillIndex <- skillIndex + 1
  for (i in 1:7) {
    
    ConsumptionExposure[skillIndex, i] <- getConsumptionExposureGrowth(Init_data = Init_eq, Ct_data = Ct_Amenities, skill = skill_to_pass, incomeType = i)
    
    
  } 
}


#Note: these are NOT comparable across income types because of scale of utils. We could adjust downward by average marginal utility of income to express in income terms?
print(paste0("Vector of Amenity exposure growth ------- "))
print(AmenityExposure)

print(paste0("Vector of consumption exposure growth ------- "))
print(ConsumptionExposure)
##Note: SCALE ARE NOT NOT NOT!!!! comparable across income types (or measure total welfare benefits) because of differing scale of utils.
#We could adjust downward by average marginal utility of income to express in income terms?



#_____________________________________________________________________________________________________________________________________________
#____________ROBUSTNESS: _____________________________________________________________________________________________________________________
#How do welfare results compare to other specifications?