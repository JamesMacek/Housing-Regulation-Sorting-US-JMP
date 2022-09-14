#PACKAGES
library(haven) #Reading stata.dta files 
library(dplyr)
library(readr)
library(collapse)
library(stringr)
library(labelled)
library(rlang)
library(estimatr)

#This file calibrates the amenity values of living in each location for each type, inferred from the population flows and measured value of consumption.
#Note: these amenity values INCLUDE COMMUTING COSTS AND ENDOGENOUS AMENITIES. 


US_BLOCK <- read_dta("Data/Counterfactuals/JoinedDataForCounterfactuals.dta")
lambda <- read_dta("Data/Counterfactuals/CalibratedLambda.dta")

load("Data/US_Data/Output/Constructed_2010_Tract.Rdata") #For ACS household income per capita
US_BLOCK_2010_JOINED <- US_BLOCK_2010_JOINED %>% group_by() %>% select(State, County, Tract, BlockGroup, Average_income, rank_density_CBSA)

#joining
US_BLOCK <- left_join(US_BLOCK, lambda, by = c("State", "County", "Tract", "BlockGroup", "CBSA", "CBSA_NAME"))
US_BLOCK <- left_join(US_BLOCK, US_BLOCK_2010_JOINED, by = c("State", "County", "Tract", "BlockGroup"))
rm(lambda)

AmenityDF <- US_BLOCK %>% select(State, County, Tract, BlockGroup, 
                                 CBSA, CBSA_NAME, avg_commuteMins, lambda) #organized dataframe for everything we need in this program


#Migration elasticities converted to a nested logit semi-elasticity.
#Obtain objects from CalibrateMigrationElasticityToLogit.R

Beta <- 0.174 #From CalibrateBeta.R

#REMEMBER. 
#What are we targeting?
#1) The income shares of each block group. These should be independent of skill group conditional on MSA (approximately, up to the error in calibration)
#2) Relative supply of skilled labour in each MSA


#Converting local income distributions to distributions over space.

for (i in 1:7) {
  college_bin_density <- paste("Ability_bin_College", i, sep = "") #Ability_bin_College`i'` from MergeDataForCounterfactuals.dta
  nocollege_bin_density <- paste("Ability_bin_NoCollege", i, sep = "")
  
  AmenityDF[paste("fr_households_College", i, sep="")] <- (US_BLOCK[[college_bin_density]]*
                                                            US_BLOCK$tot_housing_units_cen_2010*
                                                            US_BLOCK$implied_college_share)/sum(US_BLOCK[[college_bin_density]]*US_BLOCK$tot_housing_units_cen_2010*US_BLOCK$implied_college_share, na.rm = TRUE)
  
  AmenityDF[paste("fr_households_NoCollege", i, sep="")] <- (US_BLOCK[[nocollege_bin_density]]*
                                                             US_BLOCK$tot_housing_units_cen_2010*
                                                             (rep(1, nrow(US_BLOCK)) - US_BLOCK$implied_college_share))/sum(US_BLOCK[[nocollege_bin_density]]*US_BLOCK$tot_housing_units_cen_2010*(rep(1, nrow(US_BLOCK)) - US_BLOCK$implied_college_share), na.rm = TRUE)
  
}

AmenityDF["TotalPop"] <- rep(1, nrow(US_BLOCK))*sum(US_BLOCK$tot_housing_units_cen_2010) #105million households
AmenityDF["TotalCollegeShare"] <- rep(1, nrow(US_BLOCK))*sum(US_BLOCK$tot_housing_units_cen_2010*US_BLOCK$implied_college_share)/AmenityDF$TotalPop #we get a lower skill share of households, whatever. 

#Calculating implied consumption values in each location for each group. 
# first, lot size stringency
AmenityDF["LotSizeStringency"] <- (US_BLOCK$DensityRestriction*
                                     ((US_BLOCK$hedonicPrice)^(US_BLOCK$converted_elasticities + rep(1, nrow(US_BLOCK)))))*US_BLOCK$lambda

for (i in 1:7) {
  college_bin_density <- paste("Ability_bin_College", i, sep = "") #Ability_bin_College`i'` from MergeDataForCounterfactuals.dta
  nocollege_bin_density <- paste("Ability_bin_NoCollege", i, sep = "")
  ability_grp <- paste("ability_grp", i, sep = "") #measure of center for each income group
  
  #COLLEGE WORKERS
  AmenityDF[paste("consumption_val_College", i , sep="")] <- ifelse(Beta*US_BLOCK$CollegeWage*US_BLOCK[[ability_grp]] > AmenityDF$LotSizeStringency, 1, 0)* #If Unconstrained in that market
                                                                      (Beta^(Beta)*(1-Beta)^(1-Beta))*(US_BLOCK$CollegeWage*US_BLOCK[[ability_grp]])/(US_BLOCK$hedonicPrice^(Beta)) +
                                                             ifelse((Beta*US_BLOCK$CollegeWage*US_BLOCK[[ability_grp]] <= AmenityDF$LotSizeStringency) & #If constrained, but can afford
                                                                      (US_BLOCK$CollegeWage*US_BLOCK[[ability_grp]] > AmenityDF$LotSizeStringency), 1, 0)*
                                                                      ((AmenityDF$LotSizeStringency/US_BLOCK$hedonicPrice)^(Beta))*((US_BLOCK$CollegeWage*US_BLOCK[[ability_grp]] - AmenityDF$LotSizeStringency)^(1-Beta))
  
  AmenityDF[[paste("consumption_val_College", i , sep="")]][is.nan(AmenityDF[[paste("consumption_val_College", i , sep="")]])] <- 0 #Replacing priced out households with a utility of zero.
  
  #NON-COLLEGE WORKERS
  AmenityDF[paste("consumption_val_NoCollege", i , sep="")] <- ifelse(Beta*US_BLOCK$NoCollegeWage*US_BLOCK[[ability_grp]] > AmenityDF$LotSizeStringency, 1, 0)* #If Unconstrained in that market
                                                                        (Beta^(Beta)*(1-Beta)^(1-Beta))*(US_BLOCK$NoCollegeWage*US_BLOCK[[ability_grp]])/(US_BLOCK$hedonicPrice^(Beta)) +
                                                               ifelse((Beta*US_BLOCK$NoCollegeWage*US_BLOCK[[ability_grp]] <= AmenityDF$LotSizeStringency) & #If constrained, but can afford
                                                                    (US_BLOCK$NoCollegeWage*US_BLOCK[[ability_grp]] > AmenityDF$LotSizeStringency), 1, 0)*
                                                                    ((AmenityDF$LotSizeStringency/US_BLOCK$hedonicPrice)^(Beta))*((US_BLOCK$NoCollegeWage*US_BLOCK[[ability_grp]] - AmenityDF$LotSizeStringency)^(1-Beta))
  
  AmenityDF[[paste("consumption_val_NoCollege", i , sep="")]][is.nan(AmenityDF[[paste("consumption_val_NoCollege", i , sep="")]])] <- 0 #Replacing priced out households with a utility of zero.
}


#Using these consumption values to solve for relative amenitities within each city by group. (Normalize this amenity component to be on avg 1)
#Note: I do this procedure separately for college and non-college, although they should yield a same constant outcome

#Component 1: Within-city amenities.
#Use the relationship that the Within-City averages to 1 for each type and so it is the share of labourers of a particular type raised to the within-city migration elasticity  
#using this normalization for machine precision reasons.

for (i in 1:7) {
  #Column names
  consumption_val_College <- paste("consumption_val_College", i , sep="")
  consumption_val_NoCollege <- paste("consumption_val_NoCollege", i , sep="")
  fr_households_College <- paste("fr_households_College", i, sep="")
  fr_households_NoCollege <- paste("fr_households_NoCollege", i, sep="")
  wc_amenity_College <- paste("withincity_amenity_College", i, sep = "")
  wc_amenity_NoCollege <- paste("withincity_amenity_NoCollege", i, sep = "")
  
  #temporarily storing numerator of amenities value (not with normalized scale within CBSAs)
  AmenityDF <- AmenityDF %>% group_by(CBSA) %>% mutate(amenity_temp := ((!!sym(fr_households_College))/(exp(me_within[i]*!!sym(consumption_val_College))))) #using logit model
  #calculating amenity value-- NORMALIZED TO BE ON AVERAGE 1
  AmenityDF <- AmenityDF %>% group_by(CBSA) %>% mutate(!!sym(wc_amenity_College) := amenity_temp/mean(amenity_temp)) #rescaling so on average 1 within cities.
  
  
  #Repeating for non-college
  #temporarily storing numerator of amenities value (not with normalized scale within CBSAs)
  AmenityDF <- AmenityDF %>% group_by(CBSA) %>% mutate(amenity_temp := ((!!sym(fr_households_NoCollege))/(exp(me_within[i]*!!sym(consumption_val_NoCollege))))) #using logit model
  #calculating amenity value-- NORMALIZED TO BE ON AVERAGE 1
  AmenityDF <- AmenityDF %>% group_by(CBSA) %>% mutate(!!sym(wc_amenity_NoCollege) := amenity_temp/mean(amenity_temp)) #rescaling so on average 1 within cities.
  
  
}
#Should sum to one within each MSA


#Counting observations per MSA
AmenityDF <- AmenityDF %>% mutate(MSAcount = n())

#Component 2: Across city amenities.
for (i in 1:7) {
  MSA_fr_households_College <- paste("MSA_fr_households_College", i, sep="")
  MSA_fr_households_NoCollege <- paste("MSA_fr_households_NoCollege", i, sep="")
  fr_households_College <- paste("fr_households_College", i, sep="")
  fr_households_NoCollege <- paste("MSA_fr_households_NoCollege", i, sep="")
  MSA_welfare_aggregator_College <- paste("MSAWelf_ag_College", i, sep = "")
  MSA_welfare_aggregator_NoCollege <- paste("MSAWelf_ag_NoCollege", i, sep = "")
  wc_amenity_College <- paste("withincity_amenity_College", i, sep = "")
  wc_amenity_NoCollege <- paste("withincity_amenity_NoCollege", i, sep = "")
  consumption_val_College <- paste("consumption_val_College", i , sep="")
  consumption_val_NoCollege <- paste("consumption_val_NoCollege", i , sep="")
  fr_households_College <- paste("fr_households_College", i, sep="")
  fr_households_NoCollege <- paste("fr_households_NoCollege", i, sep="")
  
  #across city amenities
  ac_amenity_College <- paste("acrosscity_amenity_College", i, sep = "")
  ac_amenity_NoCollege <- paste("acrosscity_amenity_NoCollege", i, sep = "")
  
  #Total (aggregated measure)
  Amenity_College <- paste("Amenity_College", i, sep = "")
  Amenity_NoCollege <- paste("Amenity_NoCollege", i, sep = "")
  
  #Creating aggregated city distributions.
  AmenityDF <- AmenityDF %>% group_by(CBSA) %>% mutate(!!sym(MSA_fr_households_College) := sum(!!sym(fr_households_College)))
  AmenityDF <- AmenityDF %>% group_by(CBSA) %>% mutate(!!sym(MSA_fr_households_NoCollege) := sum(!!sym(fr_households_NoCollege)))
  
  #Creating within-city welfare aggregators. Note these two should be precisely equal within income groups given our sorting assumptions. 
  AmenityDF <- AmenityDF %>% group_by(CBSA) %>% mutate(!!sym(MSA_welfare_aggregator_College) := 
                                                         log(sum((!!sym(wc_amenity_College)*exp(me_within[i]*!!sym(consumption_val_College))))) )
  
  AmenityDF <- AmenityDF %>% group_by(CBSA) %>% mutate(!!sym(MSA_welfare_aggregator_NoCollege) := 
                                                         log(sum((!!sym(wc_amenity_NoCollege)*exp(me_within[i]*!!sym(consumption_val_NoCollege))))) )
  
  #Creating our amenities measure, normalized so it is on average 1 as well
  AmenityDF[ac_amenity_College] <- ((AmenityDF[[MSA_fr_households_College]])/(exp(me_across[i]*AmenityDF[[MSA_welfare_aggregator_College]])))/
                                    (sum(((AmenityDF[[MSA_fr_households_College]])/(exp(me_across[i]*AmenityDF[[MSA_welfare_aggregator_College]])))/AmenityDF$MSAcount))
  
  AmenityDF[ac_amenity_NoCollege] <- ((AmenityDF[[MSA_fr_households_NoCollege]])/(exp(me_across[i]*AmenityDF[[MSA_welfare_aggregator_NoCollege]])))/
                                    (sum(((AmenityDF[[MSA_fr_households_NoCollege]])/(exp(me_across[i]*AmenityDF[[MSA_welfare_aggregator_NoCollege]])))/AmenityDF$MSAcount))
  
  #separating across and within city amenities due to computational issues. 
  #Rescaling so across city amenities are on average one
  AmenityDF[ac_amenity_College] <- AmenityDF[[ac_amenity_College]]/mean(AmenityDF[[ac_amenity_College]])
  AmenityDF[ac_amenity_NoCollege] <- AmenityDF[[ac_amenity_NoCollege]]/mean(AmenityDF[[ac_amenity_NoCollege]])
}


#Testing one column.

#1: exp(log(ac)withincity_welfare) should match fraction of workers

#Outputting data for use with other programs.
write_dta(AmenityDF, "Data/Counterfactuals/CalibratedAmenityValues.dta")



#________________________________________________________________________________________________________________________________________________________
#Finally: checking how amenities etc respond to different variables
#1) Take average amenity at block group level, ignoring zeros (this reflects granularity issues)

test <- select(AmenityDF, starts_with("withincity_amenity")) %>% group_by() %>% select(-CBSA) #Storing columns to take row-mean

AmenityDF["wc_aggregate_amenity"] <- rowMeans(test, na.rm = TRUE)
rm(test)

#Joining with average income data
AmenityDF <- left_join(AmenityDF, US_BLOCK_2010_JOINED, by = c("State", "County", "Tract", "BlockGroup"))
rm(US_BLOCK_2010_JOINED)
AmenityDF["adjusted_ag_amenity"] <- AmenityDF$wc_aggregate_amenity*(exp(0.01*AmenityDF$avg_commuteMins))

#OLS ESTIMATE? Seems quite low.
summary(lm_robust(log(adjusted_ag_amenity) ~ log(Average_income), data = AmenityDF, fixed_effects = CBSA, clusters = CBSA, se_type = "stata"))
#checking correlation with hedonic prices and average income

#Checking correlation between amenities and hedonic housing prices/stringency of regulation
summary(lm_robust(log(adjusted_ag_amenity) ~ LotSizeStringency, data = AmenityDF, cluster = CBSA, se_type = "stata")) #No FE's-- large coefficient (note: within city amenity already set to be on average 1 so Fe's redundant)
summary(lm_robust(log(Average_income) ~ LotSizeStringency, data = AmenityDF, fixed_effects = CBSA, cluster = CBSA, se_type = "stata")) #predicts it VERY WELL... rsquared of 0.2561 within city 
#such is positive...which is interesting and to be expected

#Correlation between hedonic prices and amenities is positive...not suprising..?
AmenityDF["hedonicPrice"] <- US_BLOCK$hedonicPrice
summary(lm_robust(log(adjusted_ag_amenity) ~ log(hedonicPrice), data = AmenityDF, fixed_effects = CBSA, cluster = CBSA, se_type = "stata"))

#Why is this positive if there is negative income sorting into high density neighborhoods?
AmenityDF["hedonicPrice"] <- US_BLOCK$hedonicPrice
summary(lm_robust(log(Average_income) ~ log(hedonicPrice), data = AmenityDF, fixed_effects = CBSA, cluster = CBSA, se_type = "stata"))

#Bingo-- hedonic index NOT RELATED TO DENSITY. This... defies the logic of urban equilibria? How is that possible?
#There must be an issue with the hedonic index.
summary(lm_robust(log(hedonicPrice) ~ rank_density_CBSA, data = AmenityDF, fixed_effects = CBSA, cluster = CBSA, se_type = "stata"))
summary(lm_robust(log(Average_income) ~ rank_density_CBSA, data = AmenityDF, fixed_effects = CBSA, cluster = CBSA, se_type = "stata")) #negative income sorting

#Removing all objects but me_within and me_across
rm(list=(ls()[ls()!="me_within" & ls()!="me_across"])) #removing all objects but these