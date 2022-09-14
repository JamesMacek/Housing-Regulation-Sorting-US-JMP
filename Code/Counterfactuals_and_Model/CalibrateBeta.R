#PACKAGES
library(haven) #Reading stata.dta files 
library(dplyr)
library(readr)
library(collapse)
library(stringr)
library(labelled)


#This file calibrates the value of beta to be used for the analysis. 
#Requires output from MergeDataForCounterfactuals.do


#______Parameters_______________________________________________________________
target_parameter <- 0.2 #target beta to roughly match Diamond (2016) --  this is a bit dubious
#_______________________________________________________________________________


#Importing data to measure lot size stringency (NOTE: this uses actual observed data on lot size stringency, 
                                                # and NOT the calibrated values to rationalize an initial equilibrium. 
US_BLOCK <- read_dta("Data/Counterfactuals/JoinedDataForCounterfactuals.dta")

#We have the proportion of individuals in each income bin + a measure of center for each bin:
#Nominal_income_bin_`i' and hhincome_bin`i', respectfully. 

#For each block group and bin in sample, we can calculate 
#fraction of income spent on housing, which is max(beta*hhincome_bin`i', IncomeStringencyofLotSize)

#Converting income stringency of lot size in terms of total yearly mortgage payments. (Remember, this statistic is in terms of the price of the house)
#Average (Yearly) price to rent ratio of 10
interest <- 0.05
US_BLOCK["IncomeStringency_converted"] <- US_BLOCK$IncomeStringencyofRegulation/12 #Assuming a price to yearly rent ratio of 12 from Garner and Verbuggen (2009) (Journal of Housing Economics)
#Note: this is almost exactly in line with CalibrateMigrationElasticityToLogit.R 

#Function takes beta and calculates what the observed housing spending share would be at observed lot size regulation. 
CalibrateBeta <- function(beta, target_share, DataFrame) {
  
  #pass through all 17 columns for hhincome_bin to calculate housing spending
  #Note: we are taking the average 
  spend_rtotal <- 0
  income_rtotal <- 0
  
  for (i in 1:7) {
    string_i <- toString(i)
    income_clname <- paste("hhincome_bin", string_i, sep = "")
    income_bin_clname <- paste("Nominal_Income_bin_", string_i, sep = "")
    spend <- pmax(DataFrame[[income_clname]]*beta, 
                  DataFrame$IncomeStringency_converted) #storing spending on housing for this column
    
    #Weighting spend vector by households in each income bin.
    spend <- spend*DataFrame[[income_bin_clname]]*DataFrame$tot_housing_units_cen_2010
    #Weighting income vector by households in each bin
    income <- DataFrame[[income_clname]]*DataFrame[[income_bin_clname]]*DataFrame$tot_housing_units_cen_2010
    
    #and summing all of them
    spend <- sum(spend, na.rm = TRUE)
    spend_rtotal <- spend + spend_rtotal
    
    income <- sum(income, na.rm = TRUE)
    income_rtotal <- income + income_rtotal
    
  }
  
  #divide total spending share by total number of housing units
    spend_rtotal <- spend_rtotal/(sum(DataFrame$tot_housing_units_cen_2010, na.rm = TRUE))
    income_rtotal <- income_rtotal/(sum(DataFrame$tot_housing_units_cen_2010, na.rm = TRUE))
    return((spend_rtotal/income_rtotal - target_share)^2) #note: since function is monotone (one solution)
                                                #we can have a function like this and use Optimize to find sln when fn == 0. 
    
}

#Testing
CalibratedBeta <- optimize(f = CalibrateBeta, 
                           target_share = target_parameter, DataFrame = US_BLOCK,
                           interval = c(0, 1))$minimum #approx beta = 0.19!

#Adjustment is somewhat large (17.9% instead of 20%), but the average measure of lot size stringency also lines up perfectly.
#only a difference of about 1,300 dollars per year. 