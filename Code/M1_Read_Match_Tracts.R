#Date Created: March 29 2022
#Date Edited: 

#Module_1: 
#This code reads in the Main data from delimited, retains a few set of variables required, such as lot size, 
#co-ordinates, zoning, and land use description codes. Drops rows with missing data in certain fields (i.e. lot sizes required).
#Matches them to 2010 definition tracts. 

#This code needs to be scaled for all states. Currently set for Montana, with FIPS code = 30.
#As a proof of concept.

FIPS <- c(30) #Add entire vector of state codes here. 


#PART 1: 
#Read in "Main" data for each state from the FIPS labeled zip file. 
