# This file probes for socially optimal regulation.

library(dplyr)
library(haven)
library(labelled)
library(readr)
library(stringr)
library(rlang)
library(ggplot2)

#PARAMETERS
source("CodeV2/Counterfactual/Parameters/GlobalParameters.R")

#FUNCTIONS
source("CodeV2/Counterfactual/Functions/Analysis_Functions.R")
#Equivalent variation import
source("CodeV2/Counterfactual/Functions/EquivalentVariation.R")


#Logs...
sink("DataV2/Counterfactuals/logs/AnalyzeSocialOptimalRegulation.txt")


#Load baseline specification
BASELINE_SPECIFICATION <- list(pref = "SG", bySkill_to_pass = FALSE)
#(use cap gains version of model to assess welfare effects)

if (BASELINE_SPECIFICATION$bySkill == TRUE) {
  
  skillVector <-  c("College", "NoCollege")
  skillName <- c("College_", "NoCollege_") 
  
}else{
  
  skillVector <- c("Pooled")
  skillName <- c("")
  
}

if (BASELINE_SPECIFICATION$pref == "CD") {
  demandParameters_to_pass <- c(beta, 0)
}
if (BASELINE_SPECIFICATION$pref == "SG") {
  demandParameters_to_pass <- c(beta_StGeary, min_hReq)
}


#Load initial equilibrium...
load(paste0("DataV2/Counterfactuals/Init_eq_", 
            BASELINE_SPECIFICATION$bySkill_to_pass, 
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
Init_eq <- Master
rm(Master)

#Consumption adjustment factors for baseline spec
load(paste0("DataV2/Counterfactuals/Calibration_output/consumption_AdjustmentFactor_bySkill",
            BASELINE_SPECIFICATION$bySkill_to_pass,
            "_pref_", BASELINE_SPECIFICATION$pref, ".Rdata"))

#Getting full deregulation income vector




#____________________________________________________________________________________________________________
#Search for all files in socially optimal regulation solution for both counterfactuals
  PolicyList <- list.files(path = "DataV2/Counterfactuals/Counterfactual_output/OptimalPolicy")
  
  
  #Load income after complete deregulation at baseline.
  load(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", "Eq_Objects_FULL", 
              "_EndoAmen_", TRUE, 
              "_EndoProd_", FALSE,
              "_bySkill_",  FALSE,
              "_pref_SG", ".RData"))
  Income_vector <- Equilibrium_objects_output$Avg_income #preserve income level
  rm(Equilibrium_objects_output)
  
  #Loop over two separate search spaces to look at regulation differences
  for (OptGrid in c("F2", "F1")) {
  
    if (OptGrid == "F2") {
      reduced_PolicyList <- PolicyList[grepl("F1_1", PolicyList)]
      
    }
    
    if (OptGrid == "F1") {
      reduced_PolicyList <- PolicyList[grepl("_F2_1.RData", PolicyList)]
      
    }
  
    #For Ctfl1, loop over all files and find welfare maximizing along grid, choose policies where F2 == 0
    for (policy in reduced_PolicyList) {
    
      #Importing equilibrium objects
      load(paste0("DataV2/Counterfactuals/Counterfactual_output/OptimalPolicy/", policy))
      Ct_Amenities <- Equilibrium_objects_output
      rm(Equilibrium_objects_output)
    
      #Extract F1 and F2 on this iteration
      
      if (OptGrid == "F1") { #After targeting income, how much smaller should regulation be?
        
        F1 <- as.numeric(word(policy, start = 3 ,  sep = "_")) #Third word of filename is F1 value always
        F2 <- 1 #Set to zero always along this counterfactual 
        
      }
      
      if (OptGrid == "F2") { #Holding average regulation fixed, how strongly should we target neighborhood income distributions?
        F1 <- 1 #F1 == 1
        F2 <- as.numeric( str_replace(word(policy, start = 5 ,  sep = c("_")), ".RData", "" ) ) #Extract from file name
      }
      
       #Creating counterfactual parameter vector
       Ct_Amenities["IncomeStringency_model_rents"] <- F2*Income_vector  + (1 - F2)*Init_eq[["IncomeStringency_model_rents"]]
       #F2 == 1, implies total targeting of neighborhood income distributions
      
       #Rescaling so that mean regulation is the same at baseline (to control for level effects)
       Ct_Amenities[["IncomeStringency_model_rents"]] <- Ct_Amenities[["IncomeStringency_model_rents"]]*(mean(Init_eq[["IncomeStringency_model_rents"]])/mean(Ct_Amenities[["IncomeStringency_model_rents"]]))
      
       #Rescaling by F1
       Ct_Amenities["IncomeStringency_model_rents"] <- F1*Ct_Amenities[["IncomeStringency_model_rents"]]
      
    
        #Calcualte land values 
        #_________________________________________________________
        #Additional variables to feed into EquivalentVariation.R   
    
        #Replace housing price z1 and z2 equal to the other IF only regulated or unregulated land, this does not matter for calculations other to remove NaNs
        Ct_Amenities$housingPrice_z1[is.nan(Ct_Amenities$housingPrice_z1)] <- Ct_Amenities$housingPrice_z2[is.nan(Ct_Amenities$housingPrice_z1)] 
        Ct_Amenities$housingPrice_z2[is.nan(Ct_Amenities$housingPrice_z2)] <- Ct_Amenities$housingPrice_z1[is.nan(Ct_Amenities$housingPrice_z2)] 
      
        Ct_Amenities["price_regulated"] <- Ct_Amenities$housingPrice_z1
        Ct_Amenities["price_unregulated"] <- Ct_Amenities$housingPrice_z2
        Ct_Amenities["regulated_housingUnit_share"] <- Init_eq$regulated_housingUnit_share #shares at initial equilibrium
      
   
        #Calculating new land values by 
        Init_eq["LandValCtEq_regulated"] <- (1/(1 + Ct_Amenities$HS_Elasticity_imputed))*(Ct_Amenities$housingPrice_z1^(Ct_Amenities$HS_Elasticity_imputed + 1))*Ct_Amenities$lambda
        Init_eq["LandValCtEq_unregulated"] <- (1/(1 + Ct_Amenities$HS_Elasticity_imputed))*(Ct_Amenities$housingPrice_z2^(Ct_Amenities$HS_Elasticity_imputed + 1))*Ct_Amenities$lambda
        #Share of land in production-- does not change W.R.T regulation
        #Calculating original land values
        Init_eq["LandValInitEq_regulated"] <- (1/(1 + Init_eq$HS_Elasticity_imputed))*(Init_eq$price_regulated^(Init_eq$HS_Elasticity_imputed + 1))*Init_eq$lambda 
        Init_eq["LandValInitEq_unregulated"] <- (1/(1 + Init_eq$HS_Elasticity_imputed))*(Init_eq$price_unregulated^(Init_eq$HS_Elasticity_imputed + 1))*Init_eq$lambda 
      
        #total land value in initial neighborhood
        Init_eq["Total_land_values_initial"] <-  Init_eq$land_regulated*Init_eq$LandValInitEq_regulated + Init_eq$land_unregulated*Init_eq$LandValInitEq_unregulated #land type weighted land values per acre.
        #Total expenditure on housing services in the neighborhood
      
        #Calculating growth at neighborhood level (log differences in land-value-weighted growth)
        Init_eq["LandValGrowth"] <- log( ((Init_eq$land_regulated*Init_eq$LandValInitEq_regulated)/(Init_eq$Total_land_values_initial))* #weights
                                         (Init_eq$LandValCtEq_regulated/Init_eq$LandValInitEq_regulated) + 
                                         
                                           ((Init_eq$land_unregulated*Init_eq$LandValInitEq_unregulated)/(Init_eq$Total_land_values_initial))* #weights
                                           (Init_eq$LandValCtEq_unregulated/Init_eq$LandValInitEq_unregulated)  )
      
        #Note: lambda drops out from calculation. 
      
        #Storing average growth rate in land values
        growthRate_landval <- weighted.mean(exp(Init_eq$LandValGrowth), w = Init_eq$Total_land_values_initial) - 1
      
      #Calculate capital gains
      #___________________________
        #National housing expenditure in original equilibrium by type -- used to build total portfolio size by income type
        Init_eq["total_spending"] <- rep(0, nrow(Init_eq))
    
        for (skill_to_pass in skillVector) {
          name_of_skill <- skillName[which(skill_to_pass == skillVector)]
          for (i in 1:7) {
            Init_eq[[paste0("total_spending_type_", name_of_skill, i)]] <- sum(Init_eq[[paste0(skill_to_pass, "Wage")]]*
                                                                               Init_eq[[paste0("ability_grp", i)]]*
                                                                               Init_eq[[paste0("Population_type_", name_of_skill, i)]]*
                                                                               spendShares_targeted[i]*  #Spending shares targeted in calibration
                                                                               (1/(1 + Init_eq$HS_Elasticity_imputed))) #Multiplied by fraction of spending on housing services going to landowners
        
            Init_eq["total_spending"] <- Init_eq[["total_spending"]] + Init_eq[[paste0("total_spending_type_", name_of_skill, i)]]
        
          }
        }
    
        #Taking total spending as share of total housing wealth, dividing it by the 
        #population by type to arrive at imputed rents for homeowners
    
        for (skill_to_pass in skillVector) {
          name_of_skill <- skillName[which(skill_to_pass == skillVector)]
          for (i in 1:7) {
            
            #Initial equilibrium
            Init_eq[[paste0("Housing_wealth_change_Owner_", name_of_skill, i)]] <- Init_eq[[paste0("total_spending_type_", name_of_skill, i)]]/ #Note: total housing wealth owned by these types is just total spending by these types
                                                                                  (sum(Init_eq[[paste0("Population_type_", name_of_skill, i)]])*ownerOccupier_rate[i]) 
            # Divide by total number of owner occupiers in that type 
            # (assume owner occupiers own rented housing within same skill/education level); renters own no housing wealth.
            
            #Calculating capital loss--multiplying housing "wealth" by capital loss rate nationally
            Ct_Amenities[[paste0("Housing_wealth_change_Owner_", name_of_skill, i)]] <- (1 + growthRate_landval)*Init_eq[[paste0("Housing_wealth_change_Owner_", name_of_skill, i)]]  #capital losses in Ct_Amenities
            #This will be used to calculate the equivalent variation
            
          }
        }
      #______________________________________
      
      #______________________________________
      # Calculate equivalent variation
      #______________________________________
      
        #Calculating welfare of homeowners/renters using equivalent variation
        Eq_var <- list() 
      
        for (type in c("renter", "homeowner", "pooled")) {
          Eq_var[[type]] <- matrix(NA, length(skillVector), 7)
        }
      
      
        skillIndex <- 0
        for (skill_to_pass in skillVector) {
          skillIndex <- skillIndex + 1
          name_of_skill <- skillName[which(skill_to_pass == skillVector)]
        
          for (i in 1:7) {
          
            new_welfare <- GetWelfareEqVar(Master_data = Ct_Amenities, EqVar = 1, FullDereg = FALSE, capitalGains = TRUE, #This call just calculates welfare in utils in ctfl
                                           skill = skill_to_pass, incomeType = i, demandParameters = demandParameters_to_pass) #calculate new welfare incorporating capital gains
          
            Eq_var[["homeowner"]][skillIndex, i] <- 100*(getVariation(Init = Init_eq,
                                                                      Welfare = new_welfare,
                                                                      capitalGains = TRUE,
                                                                      skill = skill_to_pass,
                                                                      incomeType = i,
                                                                      demandParameters = demandParameters_to_pass) - 1)
          
            new_welfare <- GetWelfareEqVar(Master_data = Ct_Amenities, EqVar = 1, FullDereg = FALSE, capitalGains = FALSE, #This call just calculates welfare in utils in ctfl
                                           skill = skill_to_pass, incomeType = i, demandParameters = demandParameters_to_pass) #calculate new welfare incorporating capital gains
          
            Eq_var[["renter"]][skillIndex, i] <- 100*(getVariation(Init = Init_eq,
                                                                   Welfare = new_welfare,
                                                                   skill = skill_to_pass,
                                                                   incomeType = i,
                                                                   demandParameters = demandParameters_to_pass) - 1) #in % terms
          
          
            # Pooling welfare measure using fraction of owner-occupiers
            Eq_var[["pooled"]][skillIndex, i] <-  ownerOccupier_rate[i]*Eq_var[["homeowner"]][skillIndex, i] + 
                                                 (1-ownerOccupier_rate[i])*Eq_var[["renter"]][skillIndex, i]
          
          
          }
        }
      
        #Store pooled social welfare measure for comparison
      
        #Getting total population
        total_population <- matrix(NA, length(skillVector), 7)
        skillIndex <- 0
        for (skill_to_pass in skillVector) {
          name_of_skill <- skillName[which(skill_to_pass == skillVector)]
          skillIndex <- skillIndex + 1
          for (i in 1:7) {
            total_population[skillIndex, i] <- Ct_Amenities[[paste0("Total_Population_type_", name_of_skill, i)]][1]
          }
        }
      
        #Welfare...
        Welfare <- sum( sum(diag( (total_population%*%(t(Eq_var[["pooled"]]))) ))/sum(total_population) )
        print(paste0("Social welfare for model F1 ", F1, ", F2 ", F2, " is ", round(Welfare, 7), "%."))
        
        #Create breakdown figures for welfare effects, also perform Shapely decomposition for renters
        
        Welfare_barchart <- list()
        
        for (skill_to_pass in skillVector) {
          Welfare_barchart[[skill_to_pass]] <- rep(NA, 2*8)
          
        }
        
        #Putting in estimates into the list
        skillIndex <- 0
        for (skill_to_pass in skillVector) {
          skillIndex <- skillIndex + 1
          
          for (i in 1:7) {
            
            index <- 2*(i - 1) + 1
            Welfare_barchart[[skill_to_pass]][index] <- Eq_var[["pooled"]][skillIndex, i] #
            Welfare_barchart[[skill_to_pass]][index + 1] <- Eq_var[["homeowner"]][skillIndex, i] #Homeowners only in second position
            
          }
          
          Welfare_barchart[[skill_to_pass]][index + 2] <- (Eq_var[["pooled"]]%*%t(total_population))/(sum(total_population))
          Welfare_barchart[[skill_to_pass]][index + 3] <- (Eq_var[["homeowner"]]%*%t(total_population))/(sum(total_population)) #homeowner and pooled aggregate welfare
          
        }
        
        #BAR CHART FOR OUTPUT
        BarplotDF <- data.frame() #initializing data frame
        
        for (skill_to_pass in skillVector) {
          
          BarplotDF <- rbind(BarplotDF,  data.frame(c(rep("1. 0-25k", 2), rep("2. 25-50k", 2), rep("3. 50-75k", 2), 
                                                      rep("4. 75-100k", 2), rep("5. 100-150k", 2), rep("6. 150-200k", 2), 
                                                      rep("7. 200k+", 2), rep("Social Welfare", 2)), 
                                                    rep(c("Owners and renters pooled", "Owners only"), 2),
                                                    Welfare_barchart[[skill_to_pass]], skill_to_pass)   )#end rbind
          
        }
        
        
        colnames(BarplotDF) <- c("Income", "Households", "Value", "Education")
        
        
        if (BASELINE_SPECIFICATION$bySkill_to_pass == FALSE) {
          
          ggplot(BarplotDF, aes(y = Value, x = factor(Income), fill = Households,)) + 
            geom_bar(position = "dodge", stat = "identity") +
            xlab("Household type (income in average city, 2020 USD)") + 
            ylab("Equivalent Variation (% of income)") + 
            scale_fill_manual(values = c("royalblue", "red4")) +
            theme_gray(base_size = 15) +
            theme(axis.text.x=element_text(size=rel(1), angle=90), 
                  legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 
          ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/OptimalPolicy/PooledWelfare_Eq_var_bySkill_", 
                        BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                        BASELINE_SPECIFICATION$pref, "_F1_", F1, "_F2_", F2, ".png"), width = 25, height = 15, units = "cm") 
          
        } #Dont create graph for no bySkill
        
        rm(BarplotDF)
        
        #Next, do Shapely decomposition on renters, sum to pooled welfare values 
        
        if (OptGrid == "F2") {
        
        #Amenities
        Amenity_shapely <- matrix(NA, length(skillVector), 7)
        skillIndex <- 0
        for (skill_to_pass in skillVector) {
          skillIndex <- skillIndex + 1
          
          for (i in 1:7) {                        #Holding consumption at counterfactual levels
            Amenity_shapely[skillIndex, i] <-   (1/2)*( getWelfare_ShapelyDecomp(Consumption_data = Ct_Amenities, Amenity_data = Ct_Amenities, 
                                                                                 skill = skill_to_pass, incomeType = i) -  
                                                          getWelfare_ShapelyDecomp(Consumption_data = Ct_Amenities, Amenity_data = Init_eq, 
                                                                                   skill = skill_to_pass, incomeType = i) ) + 
                                                  #Holding consumption at initial equilibrium levels
                                                (1/2)* ( getWelfare_ShapelyDecomp(Consumption_data = Init_eq, Amenity_data = Ct_Amenities, 
                                                        skill = skill_to_pass, incomeType = i) - 
                                                        getWelfare_ShapelyDecomp(Consumption_data = Init_eq, Amenity_data = Init_eq, 
                                                  skill = skill_to_pass, incomeType = i) ) 
            #Making sure this is relative to initial welfare levels, as they are measured in utils
            Amenity_shapely[skillIndex, i] <- Amenity_shapely[skillIndex, i]/getWelfare_ShapelyDecomp(Consumption_data = Init_eq, Amenity_data = Init_eq, 
                                                                                                      skill = skill_to_pass, incomeType = i)
          }
          
        }
        
        #Consumption
        Consumption_shapely <- matrix(NA, length(skillVector), 7)
        skillIndex <- 0
        for (skill_to_pass in skillVector) {
          skillIndex <- skillIndex + 1
          
          for (i in 1:7) {         #Holding Amenities at counterfactual levels
            Consumption_shapely[skillIndex, i] <- (1/2)*( getWelfare_ShapelyDecomp(Consumption_data = Ct_Amenities, Amenity_data = Ct_Amenities, 
                                                                                   skill = skill_to_pass, incomeType = i) - 
                                                            getWelfare_ShapelyDecomp(Consumption_data = Init_eq, Amenity_data = Ct_Amenities, 
                                                                                     skill = skill_to_pass, incomeType = i) ) + 
              #Holding Amenities at initial equilibrium levels
              (1/2)*( getWelfare_ShapelyDecomp(Consumption_data = Ct_Amenities, Amenity_data = Init_eq, 
                                               skill = skill_to_pass, incomeType = i) - 
                        getWelfare_ShapelyDecomp(Consumption_data = Init_eq, Amenity_data = Init_eq, 
                                                 skill = skill_to_pass, incomeType = i) ) 
            
            #Making sure this is relative to initial welfare level (i.e. expressed as % change in utility)
            Consumption_shapely[skillIndex, i] <- Consumption_shapely[skillIndex, i]/getWelfare_ShapelyDecomp(Consumption_data = Init_eq, Amenity_data = Init_eq, 
                                                                                                              skill = skill_to_pass, incomeType = i)
            
          }
          
        }
        
        #Total % change in welfare by type (in utils; not $)
        tWelfare <- Amenity_shapely + Consumption_shapely
        
        
        #Normalize by total pooled effect (accounting for homeownership)
        Amenity_shapely_norm <- (Amenity_shapely/tWelfare)*(Eq_var[["pooled"]]) #comp_Var_store was created in welfare calculation above
        Consumption_shapely_norm <- (Consumption_shapely/tWelfare)*(Eq_var[["pooled"]])
        
        #Doing the same for social welfare by type
        total_Consumption_shapely <- (Consumption_shapely_norm%*%t(total_population))/(sum(total_population))
        total_Amenity_shapely <- (Amenity_shapely_norm%*%t(total_population))/(sum(total_population))
        
        #_______________Reporting welfare decomposition in bar chart__________________________________
        #Putting equivalent and compensating variation vectors into bar chart
        
        Welfare_barchart <- list()
        
        for (skill_to_pass in skillVector) {
          Welfare_barchart[[skill_to_pass]] <- rep(NA, 2*8)
          
        }
        
        #Putting in estimates into the list
        skillIndex <- 0
        for (skill_to_pass in skillVector) {
          skillIndex <- skillIndex + 1
          for (i in 1:7) {
            
            index <- 2*(i - 1) + 1
            
            Welfare_barchart[[skill_to_pass]][index] <- Consumption_shapely_norm[skillIndex, i]
            Welfare_barchart[[skill_to_pass]][index + 1] <- Amenity_shapely_norm[skillIndex, i] 
            
          }
          
          Welfare_barchart[[skill_to_pass]][index + 2] <- total_Consumption_shapely 
          Welfare_barchart[[skill_to_pass]][index + 3]  <- total_Amenity_shapely 
          
        }
        
        
        #BAR CHART FOR OUTPUT
        BarplotDF <- data.frame() #initializing data frame
        
        for (skill_to_pass in skillVector) {
          
          BarplotDF <- rbind(BarplotDF,  data.frame(c(rep("1. 0-25k", 2), rep("2. 25-50k", 2), rep("3. 50-75k", 2), 
                                                      rep("4. 75-100k", 2), rep("5. 100-150k", 2), rep("6. 150-200k", 2), 
                                                      rep("7. 200k+", 2), rep("Social Welfare", 2)), 
                                                    rep(c("Consumption", "Amenity"), 2),
                                                    Welfare_barchart[[skill_to_pass]], skill_to_pass)   )#end rbind
          
        }
        
        
        colnames(BarplotDF) <- c("Income", "Decomposition", "Value", "Education")
        
        
        if (BASELINE_SPECIFICATION$bySkill_to_pass == FALSE) {
          
          ggplot(BarplotDF, aes(fill = Decomposition, y = Value, x = factor(Income))) + 
            geom_bar(position = "dodge", stat = "identity") +
            xlab("Household type (income in average city, 2020 USD)") + 
            ylab("Equivalent Variation (% of income)") + theme_gray(base_size = 15) +
            theme(axis.text.x=element_text(size=rel(1), angle=90), 
                  legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 
          ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/OptimalPolicy/WelfareDecomp_Eq_var_bySkill_", 
                        BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                        BASELINE_SPECIFICATION$pref,"_F1_", F1, "_F2_", F2, ".png"), width = 25, height = 15, units = "cm") 
          
        }
        
        }
        
        
    
    } #End loop over grid search
  
  } #End loop over counterfactual types.
    
  
  
  #Analyze targeted socially targeted optimal regulation here via this optimization algorithm!
  
  
  