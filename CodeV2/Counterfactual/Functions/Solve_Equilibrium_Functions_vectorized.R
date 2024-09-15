#This file reads in and compiles all functions used in the companion file Solve_Current_Equilibrium_allSpecs.R
library(compiler) #Compile function for added speed. Short functions don't need to be compiled. 


#F1: Retreiving housing prices given current spending shares at iteration
  getHousingPrices <- function(Eq_objects) { #Pass list of equilibrium objects
    
    hPrice_return <- list()
    
    #Start loop over zones
    for (zone in zoneList) {
      
      h_spending <- rep(0, nrow(Master))
      #Summing spending shares on housing 
      for (skill in skillVector) {
        
        h_spending <- h_spending + rowSums(Eq_objects[["hSpendShare"]][[skill]][[zone]]*Eq_objects[["Wage"]][[skill]]*
                                           Eq_objects[["ability_grp"]]* #Sum all spending by type
                                           Equilibrium_objects[["Population_zone"]][[skill]][[zone]] ) #note, column is income type, sum across
      
      }
      
        
      hPrice_return[[zone]] <- (h_spending/(Eq_objects[["Land"]][[zone]]*Master$lambda))^(1/(Master$HS_Elasticity_imputed + 1)) #Prices to clear markets giving housing spending
  
    }
    
    
    return(hPrice_return)
  
  } #end getHousingPrices
  

#F2: Retrieving consumption values and desired spending shares at current prices (does this for a given zone)
  getConsumptionValues <- function(Eq_objects, demandParameters, reg_identifier, zoneType) { #set reg_identifier == TRUE to calculate assuming regulation binding, set to FALSE to find no regulation consumption values
    
    #demandParameters[1] is beta
    #demandParameters[2] is the minimum housing unit requirement
     
    #Expanding housing price matrix, initializing objects
    hPrice_expanded <- matrix(Eq_objects[["housingPrice"]][[zoneType]], nrow = nrow(Master), ncol = 7)
    consumptionValue <- list()
    spendShares <- list()
    
    if (reg_identifier == FALSE) { #if Ignore all regulation...

      for (skill in skillVector) {
        
        consumptionValue[[skill]][[zoneType]] <- pmax(  ((Eq_objects[["Wage"]][[skill]]*Eq_objects[["ability_grp"]] - demandParameters[2]*hPrice_expanded)/(hPrice_expanded^(demandParameters[1]))),
                                                     matrix(0, nrow = nrow(Master), ncol = 7)  ) #Take parallel maxima between matrix of zeros, etc
          
          #NOTE: consumptionValue NOT ADJUSTED FOR adjustmentFactor_temp for computation, so that needs to be done afterward when doing wN_elasticity 
                                                     #matrix(as.matrix(consumption_AdjustmentFactor[skill, ]), nrow = nrow(Master), ncol = 7, byrow = TRUE) #Multiply by consumption adjustment factors, etc. Conformal because consumption_Adjustment factor has length 7
        
        spendShares[[skill]][[zoneType]] <- ((1-demandParameters[1])*pmin( (hPrice_expanded*demandParameters[2])/(Eq_objects[["Wage"]][[skill]]*Eq_objects[["ability_grp"]]),  matrix(1, nrow = nrow(Master), ncol = 7) ) + demandParameters[1])
                                                                      
      }
      
    }
    
    if (reg_identifier == TRUE) { #If not ignoring regulation...
      
      #Expanding housing_stringency vector at this iteration...
      stringency_expanded <- matrix(IncomeStringency_ctfl, nrow = nrow(Master), ncol = 7)
      
      betaFactor <- ((1-demandParameters[1])^(1-demandParameters[1]))*((demandParameters[1])^(demandParameters[1])) #Porportional adjustment factor (comes out of the Cobb-Douglas algebra)
      
      #Stringency "codes" to identify what peicewise utility we are on
      stringencyCode <- list()
      
      for (skill in skillVector) {
        
        #1. Figuring out what spending shares would be at current iteration
        spendShares[[skill]][[zoneType]] <- pmax(  ((1-demandParameters[1])*pmin( (hPrice_expanded*demandParameters[2])/(Eq_objects[["Wage"]][[skill]]*Eq_objects[["ability_grp"]]),  matrix(1, nrow = nrow(Master), ncol = 7) ) + demandParameters[1]), #Take maximum of unconstrained spend shares and constrained spend shares
                                                   pmin(stringency_expanded/(Eq_objects[["Wage"]][[skill]]*Eq_objects[["ability_grp"]]), 1)  )
        
        
        #2. Creating dummies to tell us if consumer is constrained at current iteration
        stringencyCode[[skill]][[zoneType]] <- ifelse(spendShares[[skill]][[zoneType]] == 1, 3, 0) + #IF FULLY CONSTRAINED, spending shares == 100% of income
                                               ifelse(spendShares[[skill]][[zoneType]] == stringency_expanded/(Eq_objects[["Wage"]][[skill]]*Eq_objects[["ability_grp"]]), 2, 0) +  #partially constrained if spending at MLS                              
                                               ifelse(spendShares[[skill]][[zoneType]] == ((1-demandParameters[1])*pmin( (hPrice_expanded*demandParameters[2])/(Eq_objects[["Wage"]][[skill]]*Eq_objects[["ability_grp"]]),  matrix(1, nrow = nrow(Master), ncol = 7) ) + demandParameters[1]), 1, 0)  #unconstrained 
        
        
        #3. Consumption values
        
        consumptionValue[[skill]][[zoneType]] <-  pmax(0,(Eq_objects[["Wage"]][[skill]]*Eq_objects[["ability_grp"]] - demandParameters[2]*hPrice_expanded)/(hPrice_expanded^demandParameters[1]))*
                                                  (       ifelse(stringencyCode[[skill]][[zoneType]] == 3, 0, 0) + #If completely constrained, earn consumption index == 0)
                                                          ifelse(stringencyCode[[skill]][[zoneType]] == 2, (((Eq_objects[["Wage"]][[skill]]*Eq_objects[["ability_grp"]] - stringency_expanded)/(Eq_objects[["Wage"]][[skill]]*Eq_objects[["ability_grp"]] - demandParameters[2]*hPrice_expanded))^(1-demandParameters[1]))*
                                                                                      (((stringency_expanded - hPrice_expanded*demandParameters[[2]])/(Eq_objects[["Wage"]][[skill]]*Eq_objects[["ability_grp"]] - demandParameters[2]*hPrice_expanded))^(demandParameters[1]))/betaFactor, 0) + #CONSUMPTION DISTORTION FACTOR....
                                                          ifelse(stringencyCode[[skill]][[zoneType]] == 1, 1, 0)  #If unconstrained, do not change main consumption index
                                                  )
        
        #NOTE: consumptionValue NOT ADJUSTED FOR adjustmentFactor_temp for computation, so that needs to be done afterward when doing wN_elasticity 
        
        
        
      }
      
      
    } #End check for regulation...
    
    return(list(uncl_cons = consumptionValue,
                hSpendShare = spendShares))
     
    
  } #End check for consumption values
  
  #Compiling this large function
  getConsumptionValues <- cmpfun(getConsumptionValues)
  
  
#F3: aggregating across zones and applying rescaling factor k(z)
  ZoneAggregation <- function(Eq_objects) {
    
    #Initializing lists
    consIndex <- list() #zone aggregated consumption value
    consValue_adjusted <- list()
    fr_zone <- list()
    
    
    if (EquilibriumType$Partial_Dereg == TRUE) {
    
      #Start loop over skill types
      for (skill in skillVector) {
      
        #Adjusted consumption values
        for (zoneType in zoneList) {
          consValue_adjusted[[skill]][[zoneType]] <- exp(Eq_objects[["uncl_cons"]][[skill]][[zoneType]]/Eq_objects[["ability_grp"]])
          consValue_adjusted[[skill]][[zoneType]][is.na(consValue_adjusted[[skill]][[zoneType]])] <- 0 #Set NAs to zero so they don't appear in sum
        }
      
        #Initialize aggregated consumption index
        consIndex[[skill]] <-  ( consValue_adjusted[[skill]][[1]]^(wN_elast_ctfl)*Equilibrium_objects[["init_unit_share"]][[1]] + 
                                 consValue_adjusted[[skill]][[2]]^(wN_elast_ctfl)*Equilibrium_objects[["init_unit_share"]][[2]] )^(1/wN_elast_ctfl)
      
        #Creating population fractions by zone
        for (zoneType in zoneList) {
          fr_zone[[skill]][[zoneType]] <- ((consValue_adjusted[[skill]][[zoneType]]^(wN_elast_ctfl))*Equilibrium_objects[["init_unit_share"]][[zoneType]])/(consIndex[[skill]]^(wN_elast_ctfl))
          
          
          fr_zone[[skill]][[zoneType]][is.nan(fr_zone[[skill]][[zoneType]])] <- 1 #NaN's map to one where population failed to calculate
          fr_zone[[skill]][[zoneType]][is.na(fr_zone[[skill]][[zoneType]])] <- 0 #NA's map to zero population (see above, location empty for this type)
        }
        
        
        #Create consumption index, adjust by consumption_AdjustmentFactor...
        consIndex[[skill]] <- log(consIndex[[skill]])*Eq_objects[["ability_grp"]]*matrix(as.matrix(consumption_AdjustmentFactor[skill, ]), nrow = nrow(Master), ncol = 7, byrow = TRUE) #This is our consValue!
      
      }
      
      return(list(fr_zone = fr_zone,
                  consumptionValue = consIndex)) 
    } #End check if Partial_Dereg
    
    
   
    
    if (EquilibriumType$Partial_Dereg == FALSE) {
      
      #Else, just apply consumption adjustment factor, set fraction regulated to 1 (not binding...)
      consIndex <- list()
      fr_zone <- list()
      
      for (skill in skillVector) {
        
        consIndex[[skill]] <- Eq_objects[["uncl_cons"]][[skill]][["Merged"]]*matrix(as.matrix(consumption_AdjustmentFactor[skill, ]), nrow = nrow(Master), ncol = 7, byrow = TRUE)
        
        #No zones anymore...
        fr_zone[[skill]][["Merged"]] <- matrix(1, nrow = nrow(Master), ncol = 7)
      }
      
      return(list(fr_zone = fr_zone,
                  consumptionValue = consIndex)) 
      
    }
      
    
  } #End zone aggregation function. 
      
   
 #Function that gets neighborhood population shares conditional on city c      
 getNeighborhoodShares <- function(Eq_objects) {
   
   #Initializing..
   Neighborhood_fractions <- list()
   Val_city <- list()
   
   #Loop over skills
   for (skill in skillVector) {
     
     grouped_values <- as.data.frame(cbind(select(Master, CBSA), Eq_objects[["Val"]][[skill]])) #matching neighborhood values to types
     colnames(grouped_values) <- c("CBSA", paste0("Val_", 1:7))
     
     #Loop over income types to calculate city-sum values
     for (incomeType in 1:7) {

       #Value symbolics to pass to dplyr
       value_n_sym <- paste0("Val_", incomeType)
       value_n_city_sym <- paste0("Val_city_", incomeType) 
        
       grouped_values <- grouped_values %>% group_by(CBSA) %>% mutate(!!sym(value_n_city_sym) := sum(!!sym(value_n_sym)))
       
     }
     
     #Neighborhood fractions...
     Neighborhood_fractions[[skill]] <-  Eq_objects[["Val"]][[skill]]/as.matrix(select(ungroup(grouped_values), contains("Val_city_")))
     Val_city[[skill]] <- (as.matrix(select(ungroup(grouped_values), contains("Val_city_"))))^(1/rho) #deflate by 1/rho to get measure of average utility
   } 
   
   #Returning these shares
   
   return(list(Neighborhood_shares = Neighborhood_fractions,
               Val_city = Val_city))
   
   #Test to see if sum to 1...it does
   
 }#End retrieval of neighborhood shares     
    

#_________________________________________________________________________________________________________________________
#                         Ancilliary functions
#_________________________________________________________________________________________________________________________
#Function to retrieve average income; not vectorized
getAvgIncome <- function(Eq_objects) {
  
  total_income <- rep(0, nrow(Master)) #by neighborhood
  total_population <- rep(0, nrow(Master))
  
  for (skill in skillVector) { #sum all incomes and populations
    total_income <- total_income + rowSums(Eq_objects[["Wage"]][[skill]]*
                                           Eq_objects[["ability_grp"]]* #Sum all spending by type
                                           Eq_objects[["Population"]][[skill]])
    
    total_population <- total_population + rowSums(Eq_objects[["Population"]][[skill]])
    
  }
  
  return(total_income/total_population)
  
}

#Get location amenities...
getLocationAmenity <- function(Eq_objects) {
  
  Amenity_out <- list()
  
  for (skill in skillVector) {
    Amenity_out[[skill]] <- Equilibrium_objects[["exogenous_Amenity"]][[skill]]*(Equilibrium_objects$Avg_income^( matrix(Omega, nrow = nrow(Master), ncol = 7, byrow = TRUE)))
    
  }
  
  return(Amenity_out)
  
}

getCityPopulations_byType <- function(Eq_objects) { #Returns city populations by income type and education
  
  #City group definition come from master object...
  CityPop_list <- list() #initializing list
  
  for (skill in skillVector) {
    
    name_of_skill <- skillName[which(skill == skillVector)]
    grouped_populations <- as.data.frame(cbind(select(Master, CBSA), Eq_objects[["Population"]][[skill]])) #matching populations to types 
    colnames(grouped_populations) <-  c("CBSA", paste0("Population_type_", 1:7))
    
    for (incomeType in 1:7) {
      
      pop_sym <- paste0("Population_type_", incomeType)
      city_pop_sym <- paste0("City_Population_type_", incomeType)
      
      grouped_populations <- grouped_populations %>% group_by(CBSA) %>% mutate(!!sym(city_pop_sym) := sum(!!sym(pop_sym)))
      
    }
    
    CityPop_list[[skill]] <- as.matrix(select(ungroup(grouped_populations), contains("City_Pop")))
  
    
  }
  
  return(CityPop_list)
  
}

getTotalCityPopulations <- function(Eq_objects) { #This takes total city populations across income types, break down by skill level where available
  
  Total_city_pop <- list()
  Total_city_pop[["Aggregate"]] <- rep(0, nrow(Master)) #aggregate city populations across educations
  
  for (skill in skillVector) {
    Total_city_pop[[skill]] <- rowSums(Eq_objects[["City_Population"]][[skill]])
    Total_city_pop[["Aggregate"]] <- Total_city_pop[["Aggregate"]] + Total_city_pop[[skill]]
  }
  
  return(Total_city_pop)
  
  
}


#Get current endogenous bySkill wages
getSkillWages <- function(Eq_objects) {
  
  
  #City group definition come from master object...
  CityOutput_list <- list() #initializing list
    
  #Labour supply list
  labs_skill <- list()
  
  for (skill in skillVector) {
    labs_skill[[skill]] <- rep(0, nrow(Master)) #initializing  
    
    #Calculate total labour supply in from neighborhood
    for (incomeType in 1:7) {
      labs_skill[[skill]] <- labs_skill[[skill]] + Eq_objects[["Population"]][[skill]][, incomeType]*Eq_objects[["ability_grp"]][, incomeType]
    }
  }
  
  #Put in data frame and collapse to city level
  Eq_objects_collap <- cbind(Master$CBSA, bind_cols(labs_skill), bind_cols(Eq_objects[["skillProd"]])) 
  colnames(Eq_objects_collap) <- c("CBSA", "labs_College", "labs_NoCollege", "Prod_College", "Prod_NoCollege")
  
  Eq_objects_collap <- collap(Eq_objects_collap, ~ CBSA, custom = list(fsum = c("labs_College", "labs_NoCollege"),
                                                                       fmean = c("Prod_College", "Prod_NoCollege"))  )
  
  #Total city output
  Eq_objects_collap["output_city"] <- ( ((Eq_objects_collap$Prod_College*Eq_objects_collap$labs_College)^((sigma - 1)/(sigma))) + 
                                           ((Eq_objects_collap$Prod_NoCollege*Eq_objects_collap$labs_NoCollege)^((sigma - 1)/(sigma))) )^(sigma/(sigma - 1))
  
  #Calculating wage given FOC
  for (skill in skillVector) {
    Eq_objects_collap[paste0(skill, "Wage")] <- ( ( Eq_objects_collap$output_city)^(1/sigma) )*( (Eq_objects_collap[[paste0("Prod_", skill)]])^((sigma - 1)/(sigma)) )*
                                                 ( ( Eq_objects_collap[[paste0("labs_", skill)]])^(-(1/sigma)) )
  }

  #Merging back to dataframe with rows consistent with Master and Equilibrium Objects
  Eq_objects_collap <- Eq_objects_collap %>% select(CBSA, ends_with("Wage")) 
  Eq_objects_collap <- left_join(select(Master, CBSA), Eq_objects_collap, by = c("CBSA"))
    
  
  #Returning this data frame
  return(Eq_objects_collap)
  
}#End function
