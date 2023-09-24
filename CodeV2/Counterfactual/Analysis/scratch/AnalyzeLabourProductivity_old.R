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
source("CodeV2/Counterfactual/Functions/Solve_Equilibrium_Functions_FullDereg.R")
source("CodeV2/Counterfactual/Functions/Analysis_Functions.R")

#Importing all files from Counterfactual_Output (Endogenous amenities and exogenous amenities)
load("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/Eq_Objects_Full_EndoAmen_TRUE_bySkill_FALSE.RData")
Ct_Amenities <- Equilibrium_objects
rm(Equilibrium_objects)

#Importing master data for comparison to equilibrium
load("DataV2/Counterfactuals/Init_eq.RData")
Init_eq <- Master
rm(Master)

#Importing city density distribution
load(file = "DataV2/US_Data/Output/CBSA_quantiles_dens.Rdata")
load(file = "DataV2/US_Data/Output/CBSA_quantiles.Rdata")


#Quick check of correlation between model and data housing unit shares
cor(Init_eq$regulated_housingUnit_share, Init_eq$Population_reg) #61% correlation across block groups. Pretty powerful. 


#PART 1: 
#Checking changes in aggregate labour productivity across equilibria
print(paste0("The difference in labour productivity after deregulation is ",
             100*((getAggregateProductivity(Ct_Amenities, bySkill = FALSE) - getAggregateProductivity(Init_eq, bySkill = FALSE))/ 
              getAggregateProductivity(Init_eq, bySkill = FALSE)), " percent."))
#-0.06% aggregate productivity gains from across-city mobility. 


#PART 2: 
#Checking relationship between city population and average income type in transition across equilibria. 
AcrossCityAnalysis <- Init_eq %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, IncomeStringency_cl, City_housing_density)
AcrossCityAnalysis["IncomeStringency_model"] <- ((Init_eq$price_reg)^(Init_eq$HS_Elasticity_imputed + 1))*Init_eq$lambda*Init_eq$UnitDensityRestriction_cl*Init_eq$Population_reg #use lot size stringency from model
AcrossCityAnalysis["pDelta_AvgType"] <- 100*(getCityAverageType(Ct_Amenities, bySkill = FALSE)/getCityAverageType(Init_eq, bySkill = FALSE) - 1)
AcrossCityAnalysis["pDelta_Pop"] <- 100*(getCityTotalPop(Ct_Amenities, bySkill = FALSE)/getCityTotalPop(Init_eq, bySkill = FALSE) - 1)
AcrossCityAnalysis["PooledWage"] <- Init_eq$PooledWage
AcrossCityAnalysis["CBSA_med_house_value"] <- Init_eq$CBSA_med_house_value
AcrossCityAnalysis["HousingUnitShare_Data"] <- Init_eq$regulated_housingUnit_share
AcrossCityAnalysis["HousingUnitShare_Model"] <- Init_eq$Population_reg
AcrossCityAnalysis["City_Population"] <- Init_eq$City_Population_type_1 + Init_eq$City_Population_type_2 + Init_eq$City_Population_type_3 + Init_eq$City_Population_type_4 + Init_eq$City_Population_type_5 +
                                         Init_eq$City_Population_type_6 + Init_eq$City_Population_type_7
AcrossCityAnalysis["housingPriceChange"] <- Ct_Amenities$housingPrice/Init_eq$hedonicPrice

AcrossCityAnalysis["SuperStar"] <- rep(0, nrow(AcrossCityAnalysis))
AcrossCityAnalysis$SuperStar[AcrossCityAnalysis$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) &
                   AcrossCityAnalysis$City_housing_density > as.numeric(quantile_CBSA_dens["75.0%"])] <- 1
  
AcrossCityAnalysis <- collap(AcrossCityAnalysis, pDelta_AvgType + pDelta_Pop + PooledWage + IncomeStringency_cl + IncomeStringency_model + 
                             City_housing_density + CBSA_med_house_value + SuperStar + City_Population + housingPriceChange ~ CBSA + CBSA_NAME)

#Checking correlation between pop growth, type growth and wages
cor.test(AcrossCityAnalysis$PooledWage, AcrossCityAnalysis$pDelta_Pop, method = "pearson") #30% correlation!
cor.test(AcrossCityAnalysis$PooledWage, AcrossCityAnalysis$pDelta_AvgType, method = "pearson")

#which is interesting!
cor.test(AcrossCityAnalysis$City_housing_density, AcrossCityAnalysis$pDelta_AvgType, method = "pearson") #much smaller correlation here with density
#Overall weak correlation between density and stringency. But still useful to classify cities on the structure of their within-city heterogeneity.

#With housing prices
cor.test(AcrossCityAnalysis$CBSA_med_house_value, AcrossCityAnalysis$pDelta_AvgType, method = "pearson") #24% correlation here.

#And measures of stringency
cor.test(AcrossCityAnalysis$IncomeStringency_model, AcrossCityAnalysis$pDelta_AvgType, method = "pearson") #45% correlation between our income stringency measure and pDelta_pop--note: correlation would be higher but for city size

#And measures of stringency
cor.test(AcrossCityAnalysis$IncomeStringency_model, AcrossCityAnalysis$pDelta_Pop, method = "pearson")


#Probability models
summary(lm(ifelse(pDelta_AvgType < 0, 1, 0) ~ City_housing_density, data = AcrossCityAnalysis))
summary(lm(ifelse(pDelta_AvgType < 0, 1, 0) ~ CBSA_med_house_value, data = AcrossCityAnalysis))
summary(lm(ifelse(pDelta_AvgType < 0, 1, 0) ~ PooledWage, data = AcrossCityAnalysis))
summary(lm(ifelse(pDelta_AvgType < 0, 1, 0) ~ IncomeStringency_model, data = AcrossCityAnalysis))
summary(lm(ifelse(pDelta_AvgType < 0, 1, 0) ~ SuperStar, data = AcrossCityAnalysis)) 


summary(lm(ifelse(pDelta_Pop > 0, 1, 0) ~ City_housing_density, data = AcrossCityAnalysis))
summary(lm(ifelse(pDelta_Pop > 0, 1, 0) ~ CBSA_med_house_value, data = AcrossCityAnalysis))
summary(lm(ifelse(pDelta_Pop > 0, 1, 0) ~ PooledWage, data = AcrossCityAnalysis))
summary(lm(ifelse(pDelta_Pop > 0, 1, 0) ~ IncomeStringency_model, data = AcrossCityAnalysis))
summary(lm(ifelse(pDelta_Pop > 0, 1, 0) ~ SuperStar, data = AcrossCityAnalysis))
#Superstars have 23% higher probability of being +Delta_Pop cities. 


cor.test(ifelse(AcrossCityAnalysis$pDelta_AvgType < 0, 1, 0), ifelse(AcrossCityAnalysis$pDelta_Pop > 0, 1, 0)) #45% correlation here


#Something weird happening with current dataframe formatting
AcrossCityAnalysis <- data.frame(AcrossCityAnalysis)

#Displaying output
ggplot() +
  geom_point(data = AcrossCityAnalysis, aes(x = pDelta_Pop, y = pDelta_AvgType, color = PooledWage, size = City_Population),alpha = 0.5) +
  geom_smooth(method = "lm") +
  geom_text(data = AcrossCityAnalysis, check_overlap = T, size = 3, nudge_y = 1,
            aes(x = pDelta_Pop, y = pDelta_AvgType, label = CBSA_NAME)) + 
  scale_color_gradient(low = "blue", high = "red", name = "Productivity") +
  xlab("Growth rate in number of households (percent)") + 
  ylab("Growth rate in average household type (percent)") +
  xlim(c(floor(min(AcrossCityAnalysis$pDelta_Pop)), 60)) + #Omit Napes-Immokalee-Marco Island + salisbury-- these seem to be outliers that need to be examined
  ylim(c(-35, 12)) + 
  labs(color = "Productivity", size = "City Population")#Setting ranges
ggsave("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/IncomeSortingMovement.png", width = 30, height = 18, units = "cm") 




#ROBUSTNESS: 
#Checking changes in aggregate labour productivity using the bySkill portion of the model Ct (to do, doesn't imply much!)