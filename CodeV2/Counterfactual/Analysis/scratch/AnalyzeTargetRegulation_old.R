# This file probes for socially optimal regulation.

library(dplyr)
library(haven)
library(labelled)
library(readr)
library(stringr)
library(rlang)
library(ggplot2)
library(collapse)
library(mgcv)
library(gratia) #for helping to plot these
library(patchwork) #combining plots


#PARAMETERS
source("CodeV2/Counterfactual/Parameters/GlobalParameters.R")
#FUNCTIONS
source("CodeV2/Counterfactual/Functions/Analysis_Functions.R")
#Equivalent variation import
source("CodeV2/Counterfactual/Functions/EquivalentVariation.R")


#Logs...
sink("DataV2/Counterfactuals/logs/AnalyzeTargetRegulation.txt")


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
uDR_df <- read_dta("DataV2/Counterfactuals/Counterfactual_Output/OptimalPolicy/Permuted_regulation.dta")



#____________________________________________________________________________________________________________
# Search for all files in solution for counterfactuals
#____________________________________________________________________________________________________________

#load grid search file to analyze 
totalGrid <- readRDS("DataV2/Counterfactuals/Counterfactual_Output/OptimalPolicy/grid_search.RData") 

for (model in 0:nrow(totalGrid) ) { #loop over all models   
    
    if (model == 0) { #if baseline model
      F1 <- 1
      F2 <- 1
      model_path <- "DataV2/Counterfactuals/Counterfactual_output/OptimalPolicy/Equilibrium_F1_1_F2_TargetFundAmenity.RData"
      
    }else{
      
      F1 <- totalGrid[model, ]$F1
      F2 <- totalGrid[model, ]$F2 #extract parameter from grid
      
      model_path <- paste0("DataV2/Counterfactuals/Counterfactual_output/OptimalPolicy/Equilibrium_F1_", F1 ,"_F2_", F2, ".RData")
      
    }

    #Importing equilibrium objects
    load(model_path)
    Ct_Amenities <- Equilibrium_objects_output
    rm(Equilibrium_objects_output)
    
    #Extracting scaling factor
    
    #Extracting counterfactual regulation vector
    Ct_Amenities["IncomeStringency_model_rents"] <- 
      F1*(uDR_df$IncomeStringency_counterfactual^(F2))*( mean(uDR_df$IncomeStringency_counterfactual)/mean(uDR_df$IncomeStringency_counterfactual^(F2)) ) 
    
    #START WELFARE CALCULATIONS HERE INCORPORATING CAPITAL GAINS
    
    #  #Calculate land values 
    #_________________________________________________________
    #Additional variables to feed into EquivalentVariation.R   
    
    #Replace housing price z1 and z2 equal to the other IF only regulated or unregulated land, this does not matter for calculations other to remove NaNs
    Ct_Amenities$housingPrice_z1[is.nan(Ct_Amenities$housingPrice_z1)] <- Ct_Amenities$housingPrice_z2[is.nan(Ct_Amenities$housingPrice_z1)] 
    Ct_Amenities$housingPrice_z2[is.nan(Ct_Amenities$housingPrice_z2)] <- Ct_Amenities$housingPrice_z1[is.nan(Ct_Amenities$housingPrice_z2)] 
    
    #Loading regulated and unregulated prices in memory...
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
    #Total expenditure on housing services in the neighborhood
    
    #total land value in initial neighborhood
    Init_eq["Total_land_values_initial"] <-  Init_eq$land_regulated*Init_eq$LandValInitEq_regulated + Init_eq$land_unregulated*Init_eq$LandValInitEq_unregulated #land type weighted land values per acre.
    #Total expenditure on housing services in the neighborhood
    
    #Calculating growth at neighborhood level (log differences in land-value-weighted growth)
    Init_eq["LandValGrowth"] <- log( ((Init_eq$land_regulated*Init_eq$LandValInitEq_regulated)/(Init_eq$Total_land_values_initial))* #weights
                                       (Init_eq$LandValCtEq_regulated/Init_eq$LandValInitEq_regulated) + 
                                       
                                       ((Init_eq$land_unregulated*Init_eq$LandValInitEq_unregulated)/(Init_eq$Total_land_values_initial))* #weights
                                       (Init_eq$LandValCtEq_unregulated/Init_eq$LandValInitEq_unregulated)  )
    
    
    
    #Storing average growth rate in land values
    growthRate_landval <- weighted.mean(exp(Init_eq$LandValGrowth), w = Init_eq$Total_land_values_initial) - 1 #Weight by value shares
    
    print(paste0("The national growth rate in land values is ", growthRate_landval*100, " percent.")) #large losses to landowners.
    
    #Calculate capital gains
    #___________________________
    #1. Create total spending measures and location based spending. 
    
    for (skill_to_pass in skillVector) {
      name_of_skill <- skillName[which(skill_to_pass == skillVector)]
      for (i in 1:7) {
        
        #Initiate spending by type
        Init_eq[[paste0("total_spending_type_", name_of_skill, i)]] <- rep(0, nrow(Init_eq))
        
        #Imputing Init_eq zonal population in locations where there are not multiple zones (these are NA in original data; does not matter)
        for (zone in c(1,2)) {
          
          Init_eq[[paste0("Population_type_", name_of_skill, i, "_z", zone)]][is.na(Init_eq[[paste0("Population_type_", name_of_skill, i, "_z", zone)]])] <-  
            (1/2)*Init_eq[[paste0("Population_type_", name_of_skill, i)]][ is.na(Init_eq[[paste0("Population_type_", name_of_skill, i, "_z", zone)]]) ] 
          
          Init_eq[[paste0("total_spending_type_", name_of_skill, i)]] <-      Init_eq[[paste0("total_spending_type_", name_of_skill, i)]] + 
            
            sum(Init_eq[[paste0(skill_to_pass, "Wage")]]*
                  Init_eq[[paste0("ability_grp", i)]]*
                  Init_eq[[paste0("hSpendShare_", name_of_skill, i, "_z", zone)]]*
                  (1/(1 + Init_eq$HS_Elasticity_imputed))*
                  Init_eq[[paste0("Population_type_", name_of_skill, i, "_z", zone)]]) #Population by zone
          
          #Note: Multiplied by fraction (1/(1 + \epsilon)) of spending on housing services paid to landowners.
          
        }
        
      }
    }
    
    #2. Rebating scheme goes here:
    # homeowners get their own housing expenditure + rebated income on rental properties proportional to the total spending by homeowners.
    
    #Create total aggregate spending measure for all homes and by renting households
    total_spending_renters <- 0
    total_spending_homeowners <- 0
    for (skill_to_pass in skillVector) {
      name_of_skill <- skillName[which(skill_to_pass == skillVector)]
      for (i in 1:7) {
        total_spending_renters  <- total_spending_renters + (1 - ownerOccupier_rate[i])*Init_eq[[paste0("total_spending_type_", name_of_skill, i)]][1]
        total_spending_homeowners  <- total_spending_homeowners + ownerOccupier_rate[i]*Init_eq[[paste0("total_spending_type_", name_of_skill, i)]][1]
      }
    }
    
    #Create housing wealth rebating income by type (i.e. what fraction of rental payments are rebated)
    for (skill_to_pass in skillVector) {
      name_of_skill <- skillName[which(skill_to_pass == skillVector)]
      for (i in 1:7) {
        Init_eq[[paste0("hWealth_fraction_", name_of_skill, i)]] <- ownerOccupier_rate[i]*Init_eq[[paste0("total_spending_type_", name_of_skill, i)]]/total_spending_homeowners
      }
    }
    
    
    for (skill_to_pass in skillVector) {
      name_of_skill <- skillName[which(skill_to_pass == skillVector)]
      for (i in 1:7) {
        
        #Initial equilibrium housing wealth, in levels 
        Init_eq[[paste0("Housing_wealth_change_Owner_", name_of_skill, i)]] <- (Init_eq[[paste0("hWealth_fraction_", name_of_skill, i)]]*(total_spending_homeowners + total_spending_renters))/ #Total land payments paid to homeowners
          (ownerOccupier_rate[i]*Init_eq[[paste0("Total_Population_type_", name_of_skill, i)]])                           #Divide by total population of homeowners for that type
        
        #Calculating capital loss--multiplying housing wealth by capital loss rate nationally
        Ct_Amenities[[paste0("Housing_wealth_change_Owner_", name_of_skill, i)]] <- (1 + growthRate_landval)*Init_eq[[paste0("Housing_wealth_change_Owner_", name_of_skill, i)]]  #capital losses in Ct_Amenities
        #This will be used to calculate the equivalent variation         #National capital loss rate calculated above. 
      }
    }
    #______________________________________
    
    ############################################################################
    # Calculate equivalent variation incorporating capital gains for each type
    ############################################################################
    
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
                                                               skill = skill_to_pass, #capital gains defaults to FALSE
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
    Welfare <- (Eq_var[["pooled"]]%*%t(total_population))/(sum(total_population))
    print(paste0("Social welfare for model ", F1, ", ", F2, " is ", round(Welfare, 7), "%.")) 
    
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
                    BASELINE_SPECIFICATION$pref, "_TargetFundAmenity_", F1, "_", F2, ".png"), width = 25, height = 15, units = "cm") 
      
      
      #Again, doing the same in dollar terms
      ability_grp_vec <- as.numeric(as.matrix(select(ungroup(Ct_Amenities), starts_with("ability_grp"))[1, ]))
      df_to_match <- data.frame(c("1. 0-25k", "2. 25-50k", "3. 50-75k", "4. 75-100k", "5. 100-150k", "6. 150-200k", "7. 200k+"),
                                ability_grp_vec) 
      colnames(df_to_match) <- c("Income", "ability")
      BarplotDF <- left_join(BarplotDF, df_to_match)
      BarplotDF["Value_no_percent"] <- BarplotDF$ability*BarplotDF$Value/100
      
      #Calculate social welfare
      Welfare <- (as.numeric(BarplotDF[BarplotDF$Households == "Owners and renters pooled" & !is.na(BarplotDF$Value_no_percent),]$Value_no_percent)%*%t(total_population))/sum(total_population)
      BarplotDF$Value_no_percent[BarplotDF$Income == "Social Welfare" & BarplotDF$Households == "Owners and renters pooled"] <- Welfare
      print(paste0("Social welfare for model ", F1, ", ", F2, "  under $ equivalent variation is ", round(Welfare, 7), "%.")) 
      
      
      BarplotDF$Value_no_percent[BarplotDF$Income == "Social Welfare" & BarplotDF$Households == "Owners only"] <- 
        (as.numeric(BarplotDF[BarplotDF$Households == "Owners only" & !is.na(BarplotDF$Value_no_percent),]$Value_no_percent)%*%t(total_population))/sum(total_population)
      
      
      ggplot(BarplotDF, aes(y = Value_no_percent, x = factor(Income), fill = Households,)) + 
        geom_bar(position = "dodge", stat = "identity") +
        xlab("Household type (income in average city, 2020 USD)") + 
        ylab("Equivalent Variation ($/year)") + 
        scale_fill_manual(values = c("royalblue", "red4")) +
        theme_gray(base_size = 15) +
        theme(axis.text.x=element_text(size=rel(1), angle=90), 
              legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 
      ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/OptimalPolicy/PooledWelfare_Eqnopercent_var_bySkill_", 
                    BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                    BASELINE_SPECIFICATION$pref, "_TargetFundAmenity_", F1, "_", F2,  ".png"), width = 25, height = 15, units = "cm") 
      
    } #Dont create graph for no bySkill
    
    #################################################################################
    # Next, do Shapely decomposition on renters, sum to pooled welfare values 
    #################################################################################
    
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
    
    
    #Normalize by renter effect to compare to Figure in paper after complete deregulation
    Amenity_shapely_norm <- (Amenity_shapely/tWelfare)*(Eq_var[["renter"]]) 
    Consumption_shapely_norm <- (Consumption_shapely/tWelfare)*(Eq_var[["renter"]]) #compare to renters only where this comparison as a Shapely decomposition is more valid
    
    #Doing the same for social welfare by type
    total_Consumption_shapely <- (Consumption_shapely_norm%*%t(total_population))/(sum(total_population))
    total_Amenity_shapely <- (Amenity_shapely_norm%*%t(total_population))/(sum(total_population))
    
    
    #What happens to amenity value exposure?
    
    
    
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
        xlab("Household type (income in average city, 2020 USD, renters only)") + 
        ylab("Equivalent Variation (% of income)") + theme_gray(base_size = 15) +
        theme(axis.text.x=element_text(size=rel(1), angle=90), 
              legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 
      ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/OptimalPolicy/WelfareDecomp_Eq_var_bySkill_", 
                    BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                    BASELINE_SPECIFICATION$pref,"_TargetFundAmenity_", F1, "_", F2,  ".png"), width = 25, height = 15, units = "cm") 
      
      #Do the same for no % terms (different welfare function)
      ability_grp_vec <- as.numeric(as.matrix(select(ungroup(Ct_Amenities), starts_with("ability_grp"))[1, ]))
      df_to_match <- data.frame(c("1. 0-25k", "2. 25-50k", "3. 50-75k", "4. 75-100k", "5. 100-150k", "6. 150-200k", "7. 200k+"),
                                ability_grp_vec) 
      colnames(df_to_match) <- c("Income", "ability")
      BarplotDF <- left_join(BarplotDF, df_to_match)
      BarplotDF["Value_no_percent"] <- BarplotDF$ability*BarplotDF$Value/100
      
      #Calculate social welfare
      BarplotDF$Value_no_percent[BarplotDF$Income == "Social Welfare" & BarplotDF$Decomposition == "Consumption"] <- 
        sum( sum(diag( (total_population%*%(as.numeric(t(BarplotDF[BarplotDF$Decomposition == "Consumption" & !is.na(BarplotDF$Value_no_percent),]$Value_no_percent)))) ))/sum(total_population) )
      
      BarplotDF$Value_no_percent[BarplotDF$Income == "Social Welfare" & BarplotDF$Decomposition == "Amenity"] <- 
        sum( sum(diag( (total_population%*%(as.numeric(t(BarplotDF[BarplotDF$Decomposition == "Amenity" & !is.na(BarplotDF$Value_no_percent),]$Value_no_percent)))) ))/sum(total_population) )
      
      
      ggplot(BarplotDF, aes(fill = Decomposition, y = Value_no_percent, x = factor(Income))) + 
        geom_bar(position = "dodge", stat = "identity") +
        xlab("Household type (income in average city, 2020 USD, renters only)") + 
        ylab("Equivalent Variation ($/year)") + theme_gray(base_size = 15) +
        theme(axis.text.x=element_text(size=rel(1), angle=90), 
              legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 
      ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/OptimalPolicy/WelfareDecomp_Eqnopercent_var_bySkill_", 
                    BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                    BASELINE_SPECIFICATION$pref,"_TargetFundAmenity_", F1, "_", F2,  ".png"), width = 25, height = 15, units = "cm") 
      
  
      
    }
    
    #######################################################################################
    # Income exposure metric: How population-weighted neighborhood income changes by type
    #######################################################################################
    
    #Population weighted average neighborhood income (weights taken at initial population distribution -- measure of average neighborhood amenity increase) 
    pop_weighted_avg_income_ctfl <- (sum(getNeighborhoodPop(Init_eq)*Ct_Amenities$Avg_income))/(sum(getNeighborhoodPop(Init_eq)))
    pop_weighted_avg_income_init <- (sum(getNeighborhoodPop(Init_eq)*Init_eq$Avg_income))/(sum(getNeighborhoodPop(Init_eq)))
    print(paste0("Average neighborhood income increases by ", 100*(pop_weighted_avg_income_ctfl/pop_weighted_avg_income_init - 1 ), "% using baseline population shares as weights."))
    
    pop_weighted_avg_income_ctfl <- (sum(getNeighborhoodPop(Ct_Amenities)*Ct_Amenities$Avg_income))/(sum(getNeighborhoodPop(Ct_Amenities)))
    pop_weighted_avg_income_init <- (sum(getNeighborhoodPop(Ct_Amenities)*Init_eq$Avg_income))/(sum(getNeighborhoodPop(Ct_Amenities)))
    print(paste0("Average neighborhood income increases by ", 100*(pop_weighted_avg_income_ctfl/pop_weighted_avg_income_init - 1 ), "% using ctfl population shares as weights."))
    
     
      #Shapely composition paints a (seemingly) different picture, as low income households are exposed to lower 
   
    
    #Next, take income exposure
    Income_exposure <- matrix(NA, length(skillVector), 7) #average income faced by income type i
    skillIndex <- 0
    for (skill_to_pass in skillVector) {
      name_of_skill <- skillName[which(skill_to_pass == skillVector)]
      skillIndex <- skillIndex + 1 
      for (i in 1:7) { 
        Income_exposure[skillIndex, i] <- sum(Ct_Amenities$Avg_income*Ct_Amenities[[paste0("Population_type_", name_of_skill, i)]]/Ct_Amenities[[paste0("Total_Population_type_", name_of_skill, i)]])/
                                          sum(Init_eq$Avg_income*Init_eq[[paste0("Population_type_", name_of_skill, i)]]/Init_eq[[paste0("Total_Population_type_", name_of_skill, i)]])
        
      }
    }
    
    #Alternative measure:
    print("Average income faced by type relative to baseline")
    print(Income_exposure) 
    
    
    #Manually input socially optimal to calculate density gradients as well
    #if (model == 0) {
    ########################################################################################################
    #    For F1 == 1, calculate income density gradients; + other facts to put into the bottom of the paper.
    #    Relating within + across city income distribution to this purely-permuted regulatory change. 
    ########################################################################################################
    
      
      #1. Categorize how this new policy changes the distribution of the regulatory stringency measure. 
        
        #1.1: calculate share of units in regulated structures in counterfactual, assuming initial regulated housing unit shares. 
          uDR_df["Regulation_difference"] <- uDR_df$IncomeStringency_counterfactual - uDR_df$IncomeStringency_model_rents
          
          #Getting city-wide regulation changes
          City_regChange <- collap(uDR_df, Regulation_difference ~ CBSA + CBSA_NAME, 
                                   w = getNeighborhoodPop(Init_eq))  
          # Note: initial population-weighted average stringency level barely changes. 
          #  This involves on average lowering regulation in ultra-stringent cities. 
          #  This makes sense -- less variation in fundamental amenities than what is prescribed by regulation.  
          
          #1.2: Show how this policy changes the neighborhood income distribution, both within and across cities. Talk about it! 
          #Load quantiles
          for (qtile in c("", "_dens", "_pop", "_wage")) {
            load(file = paste0("DataV2/US_Data/Output/CBSA_quantiles", qtile, ".Rdata"))
          }
          
          #
          Init_eq <- Init_eq %>% group_by(CBSA) %>% mutate(demeaned_log_Income = log(Avg_income) - mean(log(Avg_income), na.rm = TRUE))
          Ct_Amenities <- Ct_Amenities %>% group_by(CBSA) %>% mutate(demeaned_log_Income = log(Avg_income) - mean(log(Avg_income), na.rm = TRUE))
          
          #Retain same ranking for comparison
          Ct_Amenities["rank_density_CBSA"] <- Init_eq$rank_density_CBSA
          Ct_Amenities["rank_inv_D2CBD"] <- Init_eq$rank_inv_D2CBD
          
          #Create measure of change in regulation within-cities for comparison to previous facts. 
          top_init <- Init_eq[Init_eq$PooledWage > as.numeric(quantile_CBSA_wage["75.0%"]),]
          bot_init <- Init_eq[Init_eq$PooledWage < as.numeric(quantile_CBSA_wage["75.0%"]),]
          top_ct <- Ct_Amenities[Ct_Amenities$PooledWage > as.numeric(quantile_CBSA_wage["75.0%"]),]
          bot_ct <- Ct_Amenities[Ct_Amenities$PooledWage < as.numeric(quantile_CBSA_wage["75.0%"]),]
          
          
          #Formula for income
          excluded_controls_income_formula <- as.formula('demeaned_log_Income ~ s(rank_density_CBSA, k = 1, bs = "cr") + rank_inv_D2CBD') #Control for CBD distance in this
          
          
          #START REGRESSIONS HERE!
          reg_t25 <- gam(formula = excluded_controls_income_formula,
                         data = bot_init) #use high smoothing penalty gamma for this application, though it doesn't matter much. 
          
          reg_b25 <- gam(formula = excluded_controls_income_formula,
                         data = bot_ct) 
          
          #Plotting manually with GGPlot2 (Extracting partially linear plot using gratia package)
          t25_smooth <- smooth_estimates(reg_t25, n = 1000) %>%
            add_confint()  #from gratia, extracts gam estimates + add confidence intervals
          b25_smooth <- smooth_estimates(reg_b25, n = 1000) %>% #unconditional confidence intervals not working for some reason. i.e. arg unconditional = TRUE does not work
            add_confint()
          
          #plot bottom
          Income.plot.initial_forPres <- ggplot() +
            geom_ribbon(data = t25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
            geom_line(data = t25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Initial Equilibrium')) + 
            
            geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
            geom_line(data = b25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Counterfactual')) + 
            scale_colour_manual(name="Sample", values = c("yellow","purple")) + 
            xlab("Density quantile (Block Group level)") +
            ylab("Log Average Income (demeaned by MSA)") +
            ggtitle("Panel B: \n All other cities") + theme_gray(base_size = 15) & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
          
          #Repeating for high productivity cities
          reg_t25 <- gam(formula = excluded_controls_income_formula,
                         data = top_init) #use high smoothing penalty gamma for this application, though it doesn't matter much. 
          
          reg_b25 <- gam(formula = excluded_controls_income_formula,
                         data = top_ct) 
          
          #Plotting manually with GGPlot2 (Extracting partially linear plot using gratia package)
          t25_smooth <- smooth_estimates(reg_t25, n = 1000) %>%
            add_confint()  #from gratia, extracts gam estimates + add confidence intervals
          b25_smooth <- smooth_estimates(reg_b25, n = 1000) %>% #unconditional confidence intervals not working for some reason. i.e. arg unconditional = TRUE does not work
            add_confint()
          
          #Plot top
          Income.plot.new_forPres <- ggplot() +
            geom_ribbon(data = t25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA),alpha = 0.2) +
            geom_line(data = t25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Initial Equilibrium')) + 
            
            geom_ribbon(data = b25_smooth, aes(ymin = lower_ci, ymax = upper_ci, x = rank_density_CBSA), alpha = 0.2) +
            geom_line(data = b25_smooth, aes(x = rank_density_CBSA, y = est, color = 'Counterfactual')) + 
            scale_colour_manual(name="Sample", values = c("yellow","purple")) + 
            xlab("Density quantile (Block Group level)") +
            ylab("Log Average Income (demeaned by MSA)") +
            ggtitle("Panel A: \n Top 25% Productivity") + theme_gray(base_size = 15) & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
          
          #Plotting poster plot
          Income.plot.new_forPres + Income.plot.initial_forPres + plot_layout(guides = "collect") & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
          ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/OptimalPolicy/IncomeDensityGradCtfl_alternate_TargetedRegulation_", F1, "_", F2,
                        BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                        BASELINE_SPECIFICATION$pref, ".png"),
                 width = 24, height = 15, units = "cm") #Looks essentially the same! Wow!
          
          
          #Baseline Sample definitions of superstar cities based on productivity
          print("COUNTERFACTUAL AT BASELINE GRADIENTS")
          print("\n")
          print( summary(lm(demeaned_log_Income ~ rank_density_CBSA + rank_inv_D2CBD, data = top_ct) ))
          print( summary(lm(demeaned_log_Income ~ rank_density_CBSA + rank_inv_D2CBD, data = bot_ct) )) #Virtually the same average gradient now!
          print("INITIAL AT BASELINE GRADIENTS")
          print("\n")
          print( summary(lm(demeaned_log_Income ~ rank_density_CBSA + rank_inv_D2CBD, data = top_init) ))
          print( summary(lm(demeaned_log_Income ~ rank_density_CBSA + rank_inv_D2CBD, data = bot_init) )) 
          
    #}
  
  
    
    
    #Doing the same for other policy changes, after rescaling around this
    
  
} #End loop over models
  
  sink(NULL)
  
  
  rm(list = ls())



