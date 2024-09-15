#This file reads in and compiles all functions used in the companion file Solve_Current_Equilibrium_allSpecs_FullDereg.R
library(compiler) #Compile function for added speed. Short functions don't need to be compiled. 



#_____________________________________________________________________________________________________________________________________________
#Full solution to housing market equilibrium given fixed population. NOTE: THIS FUNCTION IS ONLY USEABLE WITH STONE GEARY PREFERENCES!!
#Otherwise, housing market equilibrium solution is extremely easy when spending shares are fixed
getHousingPrices_Full <- function(Master_data) {
  
  #Start excess demand function
  ExcessDemand <- function(price) {
    
    #Initializing demand
    Demand <- 0
    
    for (skill in skillVector) {
      name_of_skill <- skillName[which(skill == skillVector)]
      for (incomeType in 1:7) {
        
        Demand <- Demand + ((1-beta_StGeary)*min((price*min_hReq)/(Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]]), 1) + beta_StGeary)* #Spending Share
          Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]]* #Income
          Master_data[[paste0("Population_type_", name_of_skill, incomeType)]] #Population
        
        
      }
    }
    
    Supply <- Master_data$final_land_for_res*Master_data$lambda*(price^(Master_data$HS_Elasticity_imputed + 1)) #Supply
    
    return(Demand - Supply)
    
  }
  
  #Using uniroot to solve for prices row-by-row. We know a few things about bounds on prices. 
  
  #Lower bound is the prices that would prevail if min_hReq == 0, calculating this price to pass to uniroot
  
  low_b_Spending <- 0
  
  for (skill in skillVector) {
    name_of_skill <- skillName[which(skill == skillVector)]
    for (incomeType in 1:7) {
      
      low_b_Spending <- low_b_Spending + beta_StGeary*(Master_data[[paste0("ability_grp", incomeType)]]*Master_data[[paste0(skill, "Wage")]])*Master_data[[paste0("Population_type_", name_of_skill, incomeType)]]
      
    }
  }
  
  low_b_price <- (low_b_Spending/((Master_data$final_land_for_res*Master_data$lambda)))^(1/(Master_data$HS_Elasticity_imputed + 1))
  
  
  #Upper bound is what prices would have to be to get everyone to spend all their income
  up_b_Spending <- 0
  
  for (skill in skillVector) {
    name_of_skill <- skillName[which(skill == skillVector)]
    for (incomeType in 1:7) {
      
      up_b_Spending <- up_b_Spending +  Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]]* #Income
        Master_data[[paste0("Population_type_", name_of_skill, incomeType)]] #Population
      
    }
  }
  
  up_b_price <- (up_b_Spending/((Master_data$final_land_for_res*Master_data$lambda)))^(1/(Master_data$HS_Elasticity_imputed + 1)) #add 1/2...
  
  sln <- uniroot(ExcessDemand, interval = c(0.99*low_b_price, 1.01*up_b_price),
                 extendInt = c("no"))
  
  return(sln$root)
  
}

getHousingPrices_Full <- cmpfun(getHousingPrices_Full) #compiling function
#___________________________________________________________________________________________________________________________________________


#___________________________________________________________________________________________________________________________________________
getHousingPrices <- function(Master_data) { #This function solves housing markets KEEPING SPENDING SHARES FIXED (EXOGENOUS). Pass whole data frame, not row-by-row.
  
  h_spending <- 0
  
  for (skill in skillVector) {
    name_of_skill <- skillName[which(skill == skillVector)]
    for (incomeType in 1:7) {
      
      h_spending <- h_spending    +   Master_data[[paste0("hSpendShare_", name_of_skill, incomeType)]]* #Spending Share (not updated every iteration)
        Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]]* #Income
        Master_data[[paste0("Population_type_", name_of_skill, incomeType)]] #Population
    }
  }
  
  price <- (h_spending/(Master_data$final_land_for_res*Master_data$lambda))^(1/(Master_data$HS_Elasticity_imputed + 1))
  
  return(price)
  
}

getHousingPrices <- cmpfun(getHousingPrices) #compiling function
#____________________________________________________________________________________________________________________________________________


#____________________________________________________________________________________________________________________________________________
#This function solves for consumption values 
getConsumptionValues <- function(Master_data, skill, incomeType, demandParameters) {
  
  #demandParameters[1] is beta
  #demandParameters[2] is the minimum housing unit requirement
  
  #parallel maxima of 0
  consumptionValue <- pmax((Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]] - Master_data$housingPrice*demandParameters[2])/(Master_data$housingPrice^(demandParameters[1])), rep(0, nrow(Master_data)))* #Take vanilla consumption index
    (consumption_AdjustmentFactor[[skill, paste0("consumption_Adjustment", incomeType)]]) #adjusting by consumption adjustment factor calculated in calibrate_unobserved_amenities.R
  return(consumptionValue)
  
  
} #End retrieval of housing prices
#____________________________________________________________________________________________________________________________________________

#Function to retrieve average income
getAvgIncome <- function(Master_data) {
  
  total_income <- rep(0, nrow(Master_data))
  total_population <- rep(0, nrow(Master_data))
  
  for (skill in skillVector) {
    name_of_skill <- skillName[which(skill == skillVector)]
    for (incomeType in 1:7) {
      total_income <- total_income + Master_data[[paste0(skill, "Wage")]]*Master_data[[paste0("ability_grp", incomeType)]]*Master_data[[paste0("Population_type_", name_of_skill, incomeType)]]
      total_population <- total_population + Master_data[[paste0("Population_type_", name_of_skill, incomeType)]]
    }
  }
  
  return(total_income/total_population)
  
}
#____________________________________________________________________________________________________________________________________________

#Function to retrieve location amenity
getLocationAmenity <- function(Master_data, skill, incomeType) {
  
  name_of_skill <- skillName[which(skill == skillVector)]
  
  Amenity <- Master_data[[paste0("exogenous_Amenity_", name_of_skill, incomeType)]]*(Master_data$Avg_income^(Omega[incomeType])) #Requires Avg_income in dataframe and exogenous amenity value
  
}
#____________________________________________________________________________________________________________________________________________

#____________________________________________________________________________________________________________________________________________
#Function to update college and nocollege wages given relative employments
getSkillWages <- function(Master_data) { #only works if bySkill == TRUE!
  
  #Calculating labour supply by skill/neighborhood
  for (skill in skillVector) {
    name_of_skill <- skillName[which(skill == skillVector)]
    Master_data[paste0("labs_skill_", skill)] <- rep(0, nrow(Master_data))
    
    for (incomeType in 1:7) {
      Master_data[paste0("labs_skill_", skill)] <- Master_data[[paste0("Population_type_", name_of_skill, incomeType)]]*Master_data[[paste0("ability_grp", incomeType)]] +  
                                                   Master_data[[paste0("labs_skill_", skill)]]
      
    }
  }
  
  #Collapsing labour supply by city
  Master_data_collap <- collap(select(Master_data, CBSA, starts_with("labs_skill_"), starts_with("Productivity_")), 
                               by = labs_skill_College + labs_skill_NoCollege +  
                                 Productivity_College + Productivity_NoCollege ~ CBSA, FUN = list(fsum, fmean)) %>% 
    select(CBSA, starts_with("fsum.labs_skill_"), starts_with("fmean.Productivity_"))
  
  #Calculating output by city using productivity 
  Master_data_collap["output_city"] <- ( ((Master_data_collap$fmean.Productivity_College*Master_data_collap$fsum.labs_skill_College)^((sigma - 1)/(sigma))) + 
                                           ((Master_data_collap$fmean.Productivity_NoCollege*Master_data_collap$fsum.labs_skill_NoCollege)^((sigma - 1)/(sigma))) )^(sigma/(sigma - 1))
  
  
  #Calculating wage given FOC
  for (skill in skillVector) {
    Master_data_collap[paste0(skill, "Wage")] <- ( (Master_data_collap$output_city)^(1/sigma) )*( (Master_data_collap[[paste0("fmean.Productivity_", skill)]])^((sigma - 1)/(sigma)) )*
                                                 ( (Master_data_collap[[paste0("fsum.labs_skill_", skill)]])^(-(1/sigma)) )
  }
  
  Master_data_collap <- Master_data_collap %>% select(CBSA, ends_with("Wage")) 
  
  #Merging back as vector of length nrow(Equilibrium_objects) 
  Master_data <- Master_data %>% select(CBSA)
  Master_data <- left_join(Master_data, Master_data_collap, by = c("CBSA"))
  
  #Returning list with vector of college wages
  return(list(College = Master_data$CollegeWage, 
              NoCollege = Master_data$NoCollegeWage))
  
}
getSkillWages <- cmpfun(getSkillWages) #compiling function
#____________________________________________________________________________________________________________________________________________

#Small function for parralel means -- source https://rdrr.io/github/tanaylab/tgutil/src/R/utils.R
pmean <- function(..., na.rm = FALSE) {
  d <- do.call(cbind, list(...))
  res <- rowMeans(d, na.rm = na.rm)
  idx_na <- !rowMeans(!is.na(d))
  res[idx_na] <- NA
  return(res)
}