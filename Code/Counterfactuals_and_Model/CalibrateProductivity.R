#PACKAGES
library(haven) #Reading stata.dta files 
library(dplyr)
library(readr)
library(collapse)
library(stringr)
library(labelled)

#This file calibrates what city productivity needs to be to rationalize observed wages for high and low skilled. 
#Requires output from MergeDataForCounterfactuals.do. 

US_BLOCK <- read_dta("Data/Counterfactuals/JoinedDataForCounterfactuals.dta")

sub_elast <- 1.3 #from Card's work. Substitution elasticity between low and high skilled labour

#constructing crude measure of CBSA labour supply from the constructed ability distributions.

#Summing up labour supply in each neighborhood by skill
US_BLOCK["College_supply_rtotal"] <- rep(0, nrow(US_BLOCK))
US_BLOCK["NoCollege_supply_rtotal"] <- rep(0, nrow(US_BLOCK))

for (i in 1:7) {
  college_bin_density <- paste("Ability_bin_College", i, sep = "") #Ability_bin_College`i'` from MergeDataForCounterfactuals.dta
  nocollege_bin_density <- paste("Ability_bin_NoCollege", i, sep = "")
  ability_grp <- paste("ability_grp", i, sep = "") #measure of center for each income group
  
  #Summing labour supply for college workers
  US_BLOCK$College_supply_rtotal <- US_BLOCK$College_supply_rtotal + 
                                    US_BLOCK[[ability_grp]]*US_BLOCK[[college_bin_density]]*
                                    US_BLOCK$tot_housing_units_cen_2010*US_BLOCK$implied_college_share
  
  US_BLOCK$NoCollege_supply_rtotal <- US_BLOCK$NoCollege_supply_rtotal + 
                                      US_BLOCK[[ability_grp]]*US_BLOCK[[nocollege_bin_density]]*
                                      US_BLOCK$tot_housing_units_cen_2010*(rep(1, nrow(US_BLOCK)) - US_BLOCK$implied_college_share)
  
  
}

#Summing each column across CBSAs
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(CBSA_College_Supply = sum(College_supply_rtotal)) %>%
                         mutate(CBSA_NoCollege_Supply = sum(NoCollege_supply_rtotal))

#Selecting and collapsing.
US_BLOCK <- collap(US_BLOCK, CBSA_College_Supply + CBSA_NoCollege_Supply + CollegeWage + NoCollegeWage ~ CBSA + CBSA_NAME, FUN = fmean)
US_BLOCK["RelativeCollegeSupply"] <- US_BLOCK$CBSA_College_Supply/US_BLOCK$CBSA_NoCollege_Supply
US_BLOCK["RelativeCollegeWage"] <- US_BLOCK$CollegeWage/US_BLOCK$NoCollegeWage
#CES FORMULA TO DETERMINE RELATIVE PRODUCTIVITY of HighSkill
US_BLOCK["RelativeProductivity"] <- (US_BLOCK$RelativeCollegeWage^(sub_elast/(sub_elast - 1)))*(US_BLOCK$RelativeCollegeSupply^(1/(sub_elast - 1)))

#Constructing productivity in levels given our normalization p = 1
US_BLOCK["NoCollegeProductivity"] <- (US_BLOCK$CollegeWage*(US_BLOCK$NoCollegeWage^(-sub_elast))*(US_BLOCK$RelativeCollegeSupply) + US_BLOCK$NoCollegeWage^(1-sub_elast))^(1/(1-sub_elast))
US_BLOCK["CollegeProductivity"] <- US_BLOCK$RelativeProductivity*US_BLOCK$NoCollegeProductivity

#Checking to see if unit cost equation holds
test1 <- (US_BLOCK$CollegeWage^(1-sub_elast))*(US_BLOCK$CollegeProductivity^(sub_elast - 1)) + 
         (US_BLOCK$NoCollegeWage^(1-sub_elast))*(US_BLOCK$NoCollegeProductivity^(sub_elast - 1))
test1 #holds == 1

#Checking to see if relative wage equation holds
test2 <- US_BLOCK$RelativeCollegeSupply - (US_BLOCK$RelativeCollegeWage^(-sub_elast))*(US_BLOCK$RelativeProductivity^(sub_elast - 1))
#True up to machine precision.
test2

#Saving the data for use with other programs.

write_dta(US_BLOCK, "Data/Counterfactuals/CalibratedProductivity.dta")