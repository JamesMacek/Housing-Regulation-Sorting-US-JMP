#Date created: June 20th, 2022
#Date edited: June 20th, 2022

#Constructs minimum lot sizes using the structural break algorithm akin to Song (2021). 
#Requires clustered definitions of zoning districts. 

#Requires output from M1_CurrentAssess_Match_Tracts.R (CurrentAssess_matched.dta)

library(dplyr)
library(haven)
library(labelled)
library(sf)

#Prototyping dataset. 
proto = 1 #Set to 1 if prototyping code on one CBSA

if (proto == 1) {
  CurrentAssess <- read_dta("Data/ZillowData/Output/CurrentAssess_matched.dta", n_max = 2000000) #import stata, 100,000 obs
  #Take only Birmingham-Hoover-Alabama CBSA for testing
  prototype_CBSA <- 45300
  CurrentAssess <- CurrentAssess[CurrentAssess$CBSA == prototype_CBSA,] #choose particular CBSA for prototyping
}

if (proto != 1) {
  CurrentAssess <- read_dta("Data/ZillowData/Output/CurrentAssess_matched.dta")
}

#Importing zoning district definition
ZoningDistricts <- read_dta("Data/ZillowData/Output/ConstructedZoningDistricts.dta")

CurrentAssess <- left_join(CurrentAssess, ZoningDistricts, 
                           by = c("State", "County", "Tract", "BlockGroup"),
                           suffix = c("", ".y")) #merging zoning district definitions

if (proto == 1) {
  #Reading shapefiles for block groups (note, using sp objects for this exercise)
  US_BLOCK_2010 <- st_read("Data/US_Data/CensusBlockGroup2010/US_blck_grp_2010.shp") 
  names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'STATEFP10'] <- 'State'
  names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'COUNTYFP10'] <- 'County'
  names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'TRACTCE10'] <- 'Tract' #Renaming these variables to join on State/County/Tract/BlockGroup
  names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'BLKGRPCE10'] <- 'BlockGroup' #Renaming these variables to join on State/County/Tract/BlockGroup
  
  ZoningDistricts <- ZoningDistricts %>% mutate(temp = row_number())
  ZoningDistricts <- full_join(US_BLOCK_2010, ZoningDistricts, by = c("State", "County", "Tract", "BlockGroup"))
  ZoningDistricts <- ZoningDistricts[!is.na(ZoningDistricts$temp),]
  ZoningDistricts <- ZoningDistricts %>% select(-temp)
  
}


#FUNCTIONS START HERE________________________________________________

LotObjFn <- function(x, b, d) { #input, vector x of effective lot sizes in a census block/zoning district, and break location b lying in range(x)
  #fit dth degree polynomial
  if (length(x) > 0) {
    
    x <- sort(x)
    
    cdf <- ecdf(x) #Generating empirical cdf for x
    cdf_x <- cdf(x) #Evaluating at all points in data
    
    #Generating data frame with ordered variables
    
    df_x <- data.frame(x, cdf_x) #puts into data frame for regression
    
    
    #split x into two different vectors at break b
    df_x1 <- df_x[df_x$x < b, ]
    df_x2 <- df_x[df_x$x >= b, ] #cut off point at x
    
    
    
    #Running dth degree polynomial regression for each x1 and x2, calculating residuals,
    #then delivering the sum of squared residuals
    
    #Problem, # of observations in df_x1 or df_x2 may be less than degree of polynomial
    #In that case, sum of squared residuals is zero (function must be able to fit perfectly)
    
    Resid_x1 <- 0
    Resid_x2 <- 0
    
    if (length(unique(df_x1$x)) > d) {
      Resid_x1 <- sum((lm(df_x1, formula = cdf_x ~ poly(x, d, raw = TRUE))$residuals)^2)
    }
    
    if (length(unique(df_x2$x)) > d) {
      Resid_x2 <- sum((lm(df_x2, formula = cdf_x ~ poly(x, d, raw = TRUE))$residuals)^2)
    }
    
    
    SSR <- (Resid_x1 + Resid_x2)
    
    return(SSR)
    
  }
  
  if (length(x) == 0) {
    return(-1) #-1 is the error code showing that x is empty. 
  }
}


#Compare with mode 
#Maybe, since I use small block groups the mode might be a simple and more appropriate unit of measurement.
#Very weird. 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#_________________________________________ functions end here


#__________________________________________________________________________________________

#Initializing Minimum lot sizes in master data frame

ZoningDistricts["Min_SingleFamilyBreak"] <- rep(NA, nrow(ZoningDistricts))
ZoningDistricts["Min_SingleFamilyMode"] <- rep(NA, nrow(ZoningDistricts))

#Constructing lot size empirical distributions for each block group
#Loop over each block group

for (i in unique(CurrentAssess$uZoningDistrictID)) {
  
  CurrentAssessSub <- CurrentAssess[CurrentAssess$uZoningDistrictID == i,]
  
  #Store empirical CDF of lot size (for single family homes only!)
  SF_vector <- CurrentAssessSub[CurrentAssessSub$PropertyLandUseStndCode == "RR101" | 
                                  CurrentAssessSub$PropertyLandUseStndCode == "RR999",]$LotSizeAcres 
  
  
  #Run structural break detection on single family homes (only, for now!) 
  SFBreakSln <- optimize(f = LotObjFn, d = 15, x = SF_vector,
                         interval = c(0, 10))$minimum #set d too high and you get numerical overflow
  
  SFBreakSln_Mode = Mode(SF_vector)
  
  #Storing
  ZoningDistricts$Min_SingleFamilyBreak[ZoningDistricts$uZoningDistrictID == i] <- SFBreakSln
  ZoningDistricts$Min_SingleFamilyMode[ZoningDistricts$uZoningDistrictID == i] <- SFBreakSln_Mode 
  
  
  #The mode is considerably smaller in almost all scenarios, and the structural
  #break algorithm yields most of the time nonsensical results. Why-- are block groups too small? 
  #Take mode for now-- it seems to outperform in almost every scenario.  
  
}

#Saving
if (proto != 1) {
  write_dta(ZoningDistricts, path = "Data/ZillowData/Output/ConstructedZoningDistrictsv2.dta")
}

#plotting if prototyping
if (proto == 1){
  plot(ZoningDistricts["Min_SingleFamilyMode"][ZoningDistricts$CBSA == prototype_CBSA,])
  
}
