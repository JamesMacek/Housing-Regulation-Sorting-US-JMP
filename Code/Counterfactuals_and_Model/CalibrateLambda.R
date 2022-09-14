#PACKAGES
library(haven) #Reading stata.dta files 
library(dplyr)
library(readr)
library(collapse)
library(stringr)
library(labelled)
library(doParallel) #Parallelize for loops, solving housing market equilibria much faster. 

#This file calibrates lambda-- the parameter that controls the cost shifter on the local production function.
#Requires output from CalibrateBeta.R and MergeDataForCounterfactuals.do
US_BLOCK <- read_dta("Data/Counterfactuals/JoinedDataForCounterfactuals.dta")

#Setting up parallel cluster to solve housing market equilibria faster
n.cores <- detectCores() - 1 #11 threads (cores?) on my main machine.
my.cluster <- makeCluster(n.cores, type = "PSOCK") #needs to allow firewall.

#registering cluster
registerDoParallel(cl = my.cluster)

#checking to see if this is registered
getDoParRegistered() #TRUE.
getDoParWorkers()

#PARAMETERS

Beta_choice <- 0.174 #approximate solution from CalibrateBeta.R (adjustment is minimal)

#Designing function that outputs spending on housing in each sample block group
#given the demographics, housing prices, minimum lot sizes and a vector lambda in each block group

#beta is a scalar, Dataframe is US_BLOCK and lambda is a scalar, i is a row to calculate the
#housing market equilibrium in the data frame (i.e. the block group).  
HousingMarketClear <- function(Beta, DataFrame, lambda, i) {
  
  #calculating income stringency of minimum housing requirement
  Incstr_lot_size <- (DataFrame$DensityRestriction[i]*
                     ((DataFrame$hedonicPrice[i])^(DataFrame$converted_elasticities[i] + 1)))*lambda

  
  Spending <- 0 #running total
  #Calculating spending on housing
  for (j in 1:7) {
      
      college_bin_density <- paste("Ability_bin_College", j, sep = "") #Ability_bin_College`i'` from MergeDataForCounterfactuals.do
      nocollege_bin_density <- paste("Ability_bin_NoCollege", j, sep = "")
      ability_grp <- paste("ability_grp", j, sep = "")
      
       #Spending by particular group
      Spending_max <- max(Incstr_lot_size, Beta*DataFrame[[ability_grp]][i]*DataFrame$CollegeWage[i])
      
      #ignore those who are spent out.
      if (Spending_max > DataFrame[[ability_grp]][i]*DataFrame$CollegeWage[i]) {
        Spending_max <- DataFrame[[ability_grp]][i]*DataFrame$CollegeWage[i]
      }
      
                      
      Spending <- Spending + Spending_max*
                             DataFrame[[college_bin_density]][i]*
                             DataFrame$implied_college_share[i]*
                             DataFrame$tot_housing_units_cen_2010[i]
    
      Spending_max <- max(Incstr_lot_size, Beta*DataFrame[[ability_grp]][i]*DataFrame$NoCollegeWage[i]) 
      
      #If priced out of market, spend all income and earn a reservation utility
      if (Spending_max > DataFrame[[ability_grp]][i]*DataFrame$NoCollegeWage[i]) {
        Spending_max <- DataFrame[[ability_grp]][i]*DataFrame$NoCollegeWage[i]
      }
                           
      Spending <- Spending + Spending_max*
                             DataFrame[[nocollege_bin_density]][i]*
                             (1-DataFrame$implied_college_share[i])*
                             DataFrame$tot_housing_units_cen_2010[i]
  
  }
  
  
  #Now, calculating the value of housing supply (using land selected into development = land mass of tract) (For Now?)
  Supply <- (DataFrame$res_land_acres[i]*(DataFrame$hedonicPrice[i])^(DataFrame$converted_elasticities[i] + 1))*lambda
  
  #Returning abs excess demand by row
  return(Spending - Supply)
}

#Testing output of the function
HousingMarketClear(Beta_choice, US_BLOCK, 1, 1)

#Testing solution for minima
test <- uniroot(f = HousingMarketClear, 
                    Beta = Beta_choice, DataFrame = US_BLOCK, i = 1,
                    interval = c(0, 50000000), extendInt = "yes")
HousingMarketClear(Beta_choice, US_BLOCK, test$root, 1) #equals close to zero!


#Looping over each row for a value of lambda, using all 11 cores. Takes about a minute on home PC. 
start.time <- Sys.time()
US_BLOCK["lambda"] <-  foreach (row = 1:nrow(US_BLOCK), .combine = 'c') %dopar% {
                       uniroot(f = HousingMarketClear, 
                                Beta = Beta_choice, DataFrame = US_BLOCK, i = row,
                                interval = c(0, 1000000), extendInt = "downX",
                                tol = 2*.Machine$double.eps, maxiter = 10000)$root
  
  
  
}
end.time <- Sys.time()

#testing to see if we arrived at a solution in all markets.
test_ExcessDemand <- rep(1, nrow(US_BLOCK))
for (row in 1:nrow(US_BLOCK)) {
  test_ExcessDemand[row] <- HousingMarketClear(Beta_choice, US_BLOCK, US_BLOCK$lambda[row], row)
  
}

max(abs(test_ExcessDemand)) #occurs VERY CLOSE TO MACHINE PRECISION!



#Massive amounts of variation in lambda at the block group level. This kinda makes sense.
#very fat tailed distribution. 

#Saving output as data frame
US_BLOCK <- US_BLOCK %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, lambda)
write_dta(US_BLOCK, "Data/Counterfactuals/CalibratedLambda.dta")
