#Date created: Feb 7th, 2023

#This file measures changes in aggregate productivity across locations. 

library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)
library(ggplot2)
library(collapse)

#PARAMETERS
source("CodeV2/Counterfactual/Parameters/GlobalParameters.R")

#FUNCTIONS
source("CodeV2/Counterfactual/Functions/Solve_Equilibrium_Functions_FullDereg_current.R")
source("CodeV2/Counterfactual/Functions/Analysis_Functions.R")


#_______________________________________________________________________________ PRELIMINARIES

#SPECIFY BASELINE COUNTERFACTUAL OUTPUT
BASELINE_SPECIFICATION <- list(pref = "SG", bySkill_to_pass = TRUE) 

if (BASELINE_SPECIFICATION$bySkill == TRUE) {
  
  skillVector <-  c("College", "NoCollege")
  skillName <- c("College_", "NoCollege_") 
  
}else{
  
  skillVector <- c("Pooled")
  skillName <- c("")
  
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

#Importing quantiles that define superstar cities
load(file = "DataV2/US_Data/Output/CBSA_quantiles_dens.Rdata")
load(file = "DataV2/US_Data/Output/CBSA_quantiles.Rdata")



#_______________________________________________________________________________
#PART 1: 
#Checking changes in aggregate labour productivity across equilibria
print(paste0("The difference in labour productivity after deregulation is ",
             100*((getAggregateProductivity(Ct_Amenities) - getAggregateProductivity(Init_eq))/ 
                    getAggregateProductivity(Init_eq)), " percent.")) #Not high like H & M!

#When amenities are exogenous 
print(paste0("The difference in labour productivity after deregulation is ",
             100*((getAggregateProductivity(Ct_NoAmenities) - getAggregateProductivity(Init_eq))/ 
                    getAggregateProductivity(Init_eq)), " percent for No Endogenous Amenities.")) #Slightly higher 

rm(Ct_NoAmenities)


AcrossCityAnalysis <- Init_eq %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, City_housing_density, CBSA_med_house_value, IncomeStringency_cl)

#Delta Average types, populations
AcrossCityAnalysis["pDelta_AvgType"] <- 100*(getCityAverageType(Ct_Amenities)/getCityAverageType(Init_eq) - 1)
AcrossCityAnalysis["pDelta_Pop"] <- 100*(getCityTotalPop(Ct_Amenities)/getCityTotalPop(Init_eq) - 1)

#Initial city wages
AcrossCityAnalysis["PooledWage"] <- Init_eq$PooledWage #We take pooled wage as initial city.

#Initial city population
AcrossCityAnalysis["Init_City_Population"] <- getCityTotalPop(Init_eq)

AcrossCityAnalysis["SuperStar"] <- rep(0, nrow(AcrossCityAnalysis))
AcrossCityAnalysis$SuperStar[AcrossCityAnalysis$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) &
                               AcrossCityAnalysis$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"])] <- 1 #superstar dummy


AcrossCityAnalysis <- collap(AcrossCityAnalysis, pDelta_AvgType + pDelta_Pop + PooledWage + IncomeStringency_cl + 
                               City_housing_density + CBSA_med_house_value + Init_City_Population + SuperStar ~ CBSA + CBSA_NAME)

#Checking correlation between pop growth, type growth and wages
cor.test(AcrossCityAnalysis$PooledWage, AcrossCityAnalysis$pDelta_Pop, method = "pearson")
cor.test(AcrossCityAnalysis$PooledWage, AcrossCityAnalysis$pDelta_AvgType, method = "pearson")

#With housing prices
cor.test(AcrossCityAnalysis$CBSA_med_house_value, AcrossCityAnalysis$pDelta_Pop, method = "pearson")
cor.test(AcrossCityAnalysis$CBSA_med_house_value, AcrossCityAnalysis$pDelta_AvgType, method = "pearson")

#And measures of stringency
cor.test(AcrossCityAnalysis$IncomeStringency_cl, AcrossCityAnalysis$pDelta_AvgType, method = "pearson") #80% correlation here 
cor.test(AcrossCityAnalysis$IncomeStringency_cl, AcrossCityAnalysis$pDelta_Pop, method = "pearson")


summary(lm(ifelse(pDelta_AvgType < 0, 1, 0) ~ City_housing_density, data = AcrossCityAnalysis))
summary(lm(ifelse(pDelta_AvgType < 0, 1, 0) ~ CBSA_med_house_value, data = AcrossCityAnalysis))
summary(lm(ifelse(pDelta_AvgType < 0, 1, 0) ~ PooledWage, data = AcrossCityAnalysis))
summary(lm(ifelse(pDelta_AvgType < 0, 1, 0) ~ IncomeStringency_cl, data = AcrossCityAnalysis))
summary(lm(ifelse(pDelta_AvgType < 0, 1, 0) ~ SuperStar, data = AcrossCityAnalysis)) 


summary(lm(ifelse(pDelta_Pop > 0, 1, 0) ~ City_housing_density, data = AcrossCityAnalysis))
summary(lm(ifelse(pDelta_Pop > 0, 1, 0) ~ CBSA_med_house_value, data = AcrossCityAnalysis))
summary(lm(ifelse(pDelta_Pop > 0, 1, 0) ~ PooledWage, data = AcrossCityAnalysis))
summary(lm(ifelse(pDelta_Pop > 0, 1, 0) ~ IncomeStringency_cl, data = AcrossCityAnalysis))
summary(lm(ifelse(pDelta_Pop > 0, 1, 0) ~ SuperStar, data = AcrossCityAnalysis))

#Something weird happening with current dataframe formatting
AcrossCityAnalysis <- data.frame(AcrossCityAnalysis)


ggplot() +      #Censoring outliers in plot-- columbia, MO for stoneGeary preferences
  geom_point(data = AcrossCityAnalysis[AcrossCityAnalysis$pDelta_AvgType < 10,], aes(x = pDelta_Pop, y = pDelta_AvgType, color = PooledWage, size = Init_City_Population/1000000),alpha = 0.5) +
  geom_smooth(method = "lm") +
  geom_text(data = AcrossCityAnalysis[AcrossCityAnalysis$pDelta_AvgType < 10,], check_overlap = T, size = 3, nudge_y = 1,
            aes(x = pDelta_Pop, y = pDelta_AvgType, label = CBSA_NAME)) + 
  scale_color_gradient(low = "blue", high = "red", name = "Productivity") +
  xlab("Growth rate in number of households (percent)") + 
  ylab("Growth rate in average household type (percent)") +
  coord_cartesian(clip = "off") + 
  labs(color = "Productivity", size = "Households (millions)")#Setting ranges
ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/IncomeSortingMovement_bySkill", 
              BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", BASELINE_SPECIFICATION$pref, ".png"), width = 30, height = 18, units = "cm") 

#What would labour productivity be if no income sorting occured?
#Change in agg labour productivity = pop growth weighted by output shares (this is assuming everyone makes the same income)
output_shares <- data.frame(getCityOutputShares(Init_eq))
colnames(output_shares) <- c("CBSA_NAME", "OutputShares")
AcrossCityAnalysis <- left_join(AcrossCityAnalysis, output_shares, by = c("CBSA_NAME"))
LabProdGrowth_noincomeSorting <- sum(((AcrossCityAnalysis$pDelta_Pop + 100)*as.numeric(AcrossCityAnalysis$OutputShares))) - 100 #4x higher 
print(paste0("Aggregate productivity growth would have been ", LabProdGrowth_noincomeSorting, " percent if there was no income sorting."))



#ROBUSTNESS: CHECK OTHER DATASET TYPES
#Endogenous productivity at baseeline specification
load(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", "Eq_Objects_FULL", 
            "_EndoAmen_", TRUE, 
            "_EndoProd_", TRUE,
            "_bySkill_", BASELINE_SPECIFICATION$bySkill_to_pass,
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))

Ct_Amenities <- Equilibrium_objects
print(paste0("The difference in labour productivity after deregulation is ",
             100*((getAggregateProductivity(Ct_Amenities) - getAggregateProductivity(Init_eq))/ 
                    getAggregateProductivity(Init_eq)), " percent FOR ENDOGENOUS PRODUCTIVITY.")) #Slight decrease in aggregate labour productivity

#BySkill version:  
load(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", "Eq_Objects_FULL", 
            "_EndoAmen_", FALSE, #NOTE: WE only do bySkill == False with exogenous amenities 
            "_EndoProd_", FALSE,
            "_bySkill_", TRUE,
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))

Ct_Amenities <- Equilibrium_objects
rm(Equilibrium_objects)

load(paste0("DataV2/Counterfactuals/Init_eq_", 
            TRUE, 
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
Init_eq <- Master
rm(Master)

skillVector <-  c("College", "NoCollege")
skillName <- c("College_", "NoCollege_") 
print(paste0("The difference in labour productivity after deregulation is ",
             100*((getAggregateProductivity(Ct_Amenities) - getAggregateProductivity(Init_eq))/ 
                    getAggregateProductivity(Init_eq)), " percent FOR BYSKILL == TRUE.")) #Virtually no change in labour productivity, NOTE: we turned income sorting OFF to calculate equilibrium here. 



