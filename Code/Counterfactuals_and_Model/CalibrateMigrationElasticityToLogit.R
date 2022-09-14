#PACKAGES
library(haven) #Reading stata.dta files 
library(dplyr)
library(readr)
library(collapse)
library(stringr)
library(labelled)
library(rlang)

#This file calibrates the within city migration elasticity to match that of other estimates using the Frechet specification. 

US_BLOCK <- read_dta("Data/Counterfactuals/JoinedDataForCounterfactuals.dta")
lambda <- read_dta("Data/Counterfactuals/CalibratedLambda.dta")
US_BLOCK <- left_join(US_BLOCK, lambda, by = c("State", "County", "Tract", "BlockGroup", "CBSA", "CBSA_NAME"))
rm(lambda)

AmenityDF <- US_BLOCK %>% select(State, County, Tract, BlockGroup, 
                                 CBSA, CBSA_NAME, avg_commuteMins, lambda) #organized dataframe for everything we need in this program

Beta <- 0.174
Theta <- 10/3 #From Hornbeck and Moretti (2018)-- measure long run cross city migration elasticities
Rho <- 0.5377 #using Theta/(1-Rho) = 7.22 from Herzog (2021) 

#Migration elasticities from empirical work

me_within <- Theta/(1-Rho)
me_across <- Theta


#Lot size stringency construct
AmenityDF["LotSizeStringency"] <- (US_BLOCK$DensityRestriction*
                                     ((US_BLOCK$hedonicPrice)^(US_BLOCK$converted_elasticities + rep(1, nrow(US_BLOCK)))))*US_BLOCK$lambda



#Calculating consumption values. 
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
  
  AmenityDF[[paste("consumption_val_College", i , sep="")]][is.nan(AmenityDF[[paste("consumption_val_College", i , sep="")]])] <- 0 #Replacing priced out households with the reservation utility
  
  #NON-COLLEGE WORKERS
  AmenityDF[paste("consumption_val_NoCollege", i , sep="")] <- ifelse(Beta*US_BLOCK$NoCollegeWage*US_BLOCK[[ability_grp]] > AmenityDF$LotSizeStringency, 1, 0)* #If Unconstrained in that market
    (Beta^(Beta)*(1-Beta)^(1-Beta))*(US_BLOCK$NoCollegeWage*US_BLOCK[[ability_grp]])/(US_BLOCK$hedonicPrice^(Beta)) +
    ifelse((Beta*US_BLOCK$NoCollegeWage*US_BLOCK[[ability_grp]] <= AmenityDF$LotSizeStringency) & #If constrained, but can afford
             (US_BLOCK$NoCollegeWage*US_BLOCK[[ability_grp]] > AmenityDF$LotSizeStringency), 1, 0)*
    ((AmenityDF$LotSizeStringency/US_BLOCK$hedonicPrice)^(Beta))*((US_BLOCK$NoCollegeWage*US_BLOCK[[ability_grp]] - AmenityDF$LotSizeStringency)^(1-Beta))
  
  AmenityDF[[paste("consumption_val_NoCollege", i , sep="")]][is.nan(AmenityDF[[paste("consumption_val_NoCollege", i , sep="")]])] <- 0 #Replacing priced out households with a utility of zero.
}

#Calculating converted migration elasticities to move from Frechet shock model to a MNL discrete choice model.
#Rescaling elasticity by average calculated utility
utilities <- as.matrix(select(AmenityDF, contains("consumption_val_"))) #
mean_conversion_factor <- colMeans(utilities) #If thinking about adjusting migration elasticity by group...


#adjusting within-city  migration elasticities by this value. (NOTE: these have to be done on a group-basis.)
me_within <- me_within/mean_conversion_factor


#Calculating what within city welfare indicies must be and adjusting downward by that for the across city elasticity

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

#Calculating within-city we;far eaggregators.
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
  
}

utilities <- select(AmenityDF, contains("MSAWelf_ag_"))
utilities <- utilities %>% group_by() %>% select(-CBSA)
  
mean_conversion_factor <- colMeans(utilities) #If thinking about adjusting migration elasticity by group... Note: elasticity has to be large because within city welfare aggregators are in small units
me_across <- me_across/colMeans(utilities)



#Storing vectors for use with other programs
me_within <- me_within[c(1, 3, 5, 7, 9, 11, 13)]
me_across <- me_across[c(1, 3, 5, 7, 9, 11, 13)] #Extracting first seven vectors
rm(list=(ls()[ls()!="me_within" & ls()!="me_across"])) #removing all objects but these