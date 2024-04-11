#Function to check and see what the model implied spending shares are relative to actual observed spending shares
source("CodeV2/Calibrate/Functions/Calibration_Functions_StoneGeary.R")
#To do: this needs to be automated 

#Defining get_spendShare_error. Does not work with BySkill == TRUE!!!

get_spendshare_error <- function(Master_data, demandParameters) {
  
  #Registering doParallel
  #ncores  <- detectCores() - 1
  
  #Setting up number of cores
  #registerDoParallel(ncores)
  
  range <- 1:nrow(Master_data)
  
  tmp <- foreach(row = range,
                 .errorhandling = "pass") %do% { 
                   
                   #FOREACH output
                   return(Calibrate_prices(Master_data[row,], demandParameters = demandParameters))
                   
                 }
  
  #closing cluster
  
  
  #Extracting spending shares in easy-to-use list
  SpendShares <- list()
  
  for (incomeType in 1:7) {
    
    SpendShares[[incomeType]] <- list()
    for (zone in c(1, 2)) {
      SpendShares[[incomeType]][[zone]] <- rep(NA, nrow(Master_data))
    
      for (row in range) { #loop over each row in sample to get spending shares
        
        if (tmp[[row]]$RegulationCode == 1 & !is.na(tmp[[row]]$RegulationCode)) {
          
          SpendShares[[incomeType]][[zone]][row] <- tmp[[row]]$housingExpenditureShare$Pooled[incomeType, zone]
          
        }
        
        if (tmp[[row]]$RegulationCode != 1 & !is.na(tmp[[row]]$RegulationCode)) {
          SpendShares[[incomeType]][[zone]][row] <- tmp[[row]]$housingExpenditureShare$Pooled[incomeType]
          
        }
        
      }
    }
  }
  
  
  Populations <- list()
  
  for (incomeType in 1:7) {
    Populations[[incomeType]] <- list()
    for (zone in c(1, 2)) {
      Populations[[incomeType]][[zone]] <- rep(NA, nrow(Master_data))
      
      for (row in range) { #loop over each row in sample to get spending shares
        
        if (tmp[[row]]$RegulationCode == 1) {
          
          Populations[[incomeType]][[zone]][row] <- tmp[[row]]$Populations$Pooled[incomeType, zone]
          
        }else{
          Populations[[incomeType]][[zone]][row] <- (1/2)*Master_data[[paste0("Population_type_", incomeType)]][row] #1/2 -- population split evenly across "zones" (there are no zones)
          
        }
        
      }
    }
  }
  
  
  
  #Taking object and calculating spending share on housing by income type
  Housing_spending <- rep(0, 7)
  Total_income <- rep(0, 7)
  
  for (incomeType in 1:7) {
    for (zone in c(1, 2)) {
      
      Housing_spending[incomeType] <- Housing_spending[incomeType] + sum(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]*
                                                                         SpendShares[[incomeType]][[zone]]*Populations[[incomeType]][[zone]]) #Sum of all housing payments 
      
      Total_income[incomeType] <- Total_income[incomeType] + sum(Master_data$PooledWage*Master_data[[paste0("ability_grp", incomeType)]]*Populations[[incomeType]][[zone]])
      
    }
  }
  
  stopImplicitCluster()
  
  Ag_spendshare <- Housing_spending/Total_income
  return(Ag_spendshare - SpendShares_to_Target)
  
  
}


#Checking error of our spendshares to target SpendShares_to_target vector
set.seed(123) 

Master_data_sample <- Master[sample(nrow(Master), 4750), ]

print(get_spendshare_error(Master_data = Master_data_sample, 
                     demandParameters = c(0.075, 4750)) ) #THIS WORKS WELL (max error of approximatley 0.0115.)

