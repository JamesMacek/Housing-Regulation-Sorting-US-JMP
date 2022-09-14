#Date created: June 16th, 2022
#Date edited: June 17th, 2022

#Constructs minimum lot sizes using the structural break algorithm akin to Song (2021). 
#Does this individually for all block groups BEFORE aggregation. 
#Do this again AFTER aggregation? 
#A clustering algorithm will be used later to aggregate 
#them based on how similar they are based on model observables, in a different R file. 

#Requires output from M1_CurrentAssess_Match_Tracts.R (CurrentAssess_matched.dta) + AlgorithmConstructZoningDistricts.R (latest versions)

library(dplyr)
library(haven)
library(labelled)
library(sf)

#Prototyping dataset. 
proto = 1 #Set to 1 if prototyping code on one CBSA

if (proto == 1) {
  CurrentAssess <- read_dta("Data/ZillowData/Output/CurrentAssess_matched.dta", n_max = 1000000) #import stata, 100,000 obs
  #Take only Birmingham-Hoover-Alabama CBSA for testing
  CurrentAssess <- CurrentAssess[CurrentAssess$CBSA == 13820,]
}

if (proto != 1) {
  CurrentAssess <- read_dta("Data/ZillowData/Output/CurrentAssess_matched.dta")
}

#FUNCTION TO CALCULATE MINIMUM LOT SIZE OBJECTIVE GOES HERE________________________________
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


#EXPERIMENTAL: MODE FUNCTION (limit as d -> infty)
#Maybe, since I use small block groups the mode might be a simple and more appropriate unit of measurement.
#Very weird. 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#__________________________________________________________________________________________

#Initializing Minimum lot sizes in master data frame

CurrentAssess["Min_SingleFamilyBreak"] <- rep(NA, nrow(CurrentAssess))
CurrentAssess["Min_SingleFamilyMode"] <- rep(NA, nrow(CurrentAssess))

#Constructing lot size empirical distributions for each block group
#Loop over each block group

for (i in unique(CurrentAssess$GEOID10)) {

CurrentAssessSub <- CurrentAssess[CurrentAssess$GEOID10 == i,]

#Store empirical CDF of lot size (for single family homes only!)
SF_vector <- CurrentAssessSub[CurrentAssessSub$PropertyLandUseStndCode == "RR101" | 
                               CurrentAssessSub$PropertyLandUseStndCode == "RR999",]$LotSizeAcres 


#Run structural break detection on single family homes (only!)
SFBreakSln <- optimize(f = LotObjFn, d = 7, x = SF_vector,
                       interval = c(0, 10))$minimum #set d too high and you get numerical overflow

SFBreakSln_Mode = Mode(SF_vector)

#Storing
CurrentAssess$Min_SingleFamilyBreak[CurrentAssess$GEOID10 == i] <- SFBreakSln
CurrentAssess$Min_SingleFamilyMode[CurrentAssess$GEOID10 == i] <- SFBreakSln_Mode 


#The mode is considerably smaller in almost all scenarios, and the structural
#break algorithm yields most of the time nonsensical results. Why-- are block groups too small? 
#Take mode for now. 

}


#Saving output

