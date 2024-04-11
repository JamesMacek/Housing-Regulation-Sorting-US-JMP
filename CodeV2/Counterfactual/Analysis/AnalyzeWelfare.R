#Date created: Feb 6th, 2023

#This file measures welfare for the full deregulation exercise. 

library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)
library(ggplot2)
library(forcats)


#PARAMETERS
source("CodeV2/Counterfactual/Parameters/GlobalParameters.R")

#FUNCTIONS
source("CodeV2/Counterfactual/Functions/Solve_Equilibrium_Functions_FullDereg_current.R")
source("CodeV2/Counterfactual/Functions/Analysis_Functions.R")


#_______________________________________________________________________________ PRELIMINARIES

#SPECIFY BASELINE COUNTERFACTUAL OUTPUT
BASELINE_SPECIFICATION <- list(pref = "SG", bySkill_to_pass = FALSE)
#Change specification here to get welfare statistics for different specifications.

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


#Importing all files from Counterfactual_Output (Endogenous amenities)
load(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", "Eq_Objects_FULL", 
            "_EndoAmen_", TRUE, 
            "_EndoProd_", FALSE,
            "_bySkill_", BASELINE_SPECIFICATION$bySkill_to_pass,
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
Ct_Amenities <- Equilibrium_objects
rm(Equilibrium_objects)

#Importing all files from Counterfactual_Output (Exogenous amenities)
load(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", "Eq_Objects_FULL", 
            "_EndoAmen_", FALSE, 
            "_EndoProd_", FALSE,
            "_bySkill_", BASELINE_SPECIFICATION$bySkill_to_pass,
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
Ct_NoAmenities <- Equilibrium_objects
rm(Equilibrium_objects)



#Importing master data for comparison to equilibrium
load(paste0("DataV2/Counterfactuals/Init_eq_", 
            BASELINE_SPECIFICATION$bySkill_to_pass, 
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
Init_eq <- Master
rm(Master)


#Consumption adjustment factors for baseline spec
load(paste0("DataV2/Counterfactuals/Calibration_output/consumption_AdjustmentFactor_bySkill",
              BASELINE_SPECIFICATION$bySkill_to_pass,
              "_pref_", BASELINE_SPECIFICATION$pref, ".Rdata"))
#Final land for residential use 
Init_eq["final_land_for_res"] <- Init_eq$land_regulated + Init_eq$land_unregulated
Ct_Amenities["ALAND"] <- Init_eq$ALAND #official landmass from census shapefiles
Ct_NoAmenities["ALAND"] <- Init_eq$ALAND

#___________________________________________________________________________________________________________________________________
#PART 1: analyzing the welfare of landlords by asking about the change in the average value of land across all locations.
#___________________________________________________________________________________________________________________________________

#Calculating change in land values given model counterpart

Init_eq["LandValAmenities"] <- (Ct_Amenities$housingPrice^(Ct_Amenities$HS_Elasticity_imputed + 1))*Ct_Amenities$lambda
Init_eq["LandValNoAmenities"] <- (Ct_NoAmenities$housingPrice^(Ct_Amenities$HS_Elasticity_imputed + 1))*Ct_Amenities$lambda

#Calculating land values in initial regulated vs. unregulated equilibrium
Init_eq["LandValInitEq_regulated"] <- (Init_eq$price_regulated^(Init_eq$HS_Elasticity_imputed + 1))*Init_eq$lambda 
Init_eq["LandValInitEq_unregulated"] <- (Init_eq$price_unregulated^(Init_eq$HS_Elasticity_imputed + 1))*Init_eq$lambda #land type weighted land values per acre.

#total land value in initial neighborhood
Init_eq["Total_land_values_initial"] <- Init_eq$land_regulated*Init_eq$LandValInitEq_regulated + Init_eq$land_unregulated*Init_eq$LandValInitEq_unregulated

#Calculating growth at neighborhood level (log differences in land value weighted growth)
Init_eq["LandValGrowth"] <- log( ((Init_eq$land_regulated*Init_eq$LandValInitEq_regulated)/(Init_eq$Total_land_values_initial))* #weights
                                    (Init_eq$LandValAmenities/Init_eq$LandValInitEq_regulated) + 
                                    
                                   ((Init_eq$land_unregulated*Init_eq$LandValInitEq_unregulated)/(Init_eq$Total_land_values_initial))* #weights
                                    (Init_eq$LandValAmenities/Init_eq$LandValInitEq_unregulated)  )

Init_eq["LandValGrowthNoAm"] <- log( ((Init_eq$land_regulated*Init_eq$LandValInitEq_regulated)/(Init_eq$Total_land_values_initial))* #weights
                                        (Init_eq$LandValNoAmenities/Init_eq$LandValInitEq_regulated) + 
                                        
                                     ((Init_eq$land_unregulated*Init_eq$LandValInitEq_unregulated)/(Init_eq$Total_land_values_initial))* #weights
                                        (Init_eq$LandValNoAmenities/Init_eq$LandValInitEq_unregulated)  )


#Storing average growth rate in land values, weighted by final_land_for_res
growthRate_landval <- weighted.mean(exp(Init_eq$LandValGrowth), w = Init_eq$Total_land_values_initial) - 1
growthRate_landval_NoAm <- weighted.mean(exp(Init_eq$LandValGrowthNoAm), w = Init_eq$Total_land_values_initial) - 1

#Printing changes in land value for an average block group
print(paste0("The national growth rate in land values is ", growthRate_landval*100, " percent.")) #large losses to landowners.
print(paste0("The national growth rate in land values is exogenous amenities at baseline is ", growthRate_landval_NoAm*100, " percent."))

#Plotting graph
ggplot() + 
  geom_point(data = Init_eq[Init_eq$IncomeStringency_cl > 0,], aes(x = log(IncomeStringency_cl), y = LandValGrowth), color = "red", size = 0.25, alpha = 0.05) +
  geom_point(data = Init_eq[Init_eq$IncomeStringency_cl > 0,], aes(x = log(IncomeStringency_cl), y = LandValGrowthNoAm), color = "blue", size = 0.25, alpha = 0.05) + #censor graph for locations with minimum lot sizes
  geom_smooth(data = Init_eq[Init_eq$IncomeStringency_cl > 0,], aes(x = log(IncomeStringency_cl), y = LandValGrowth, color = "Endogenous"), method = "lm") +
  geom_smooth(data = Init_eq[Init_eq$IncomeStringency_cl > 0,], aes(x = log(IncomeStringency_cl), y = LandValGrowthNoAm, color = "Exogenous"), method = "lm") +
  scale_colour_manual(name="Model Amenities", values = c("red4", "royalblue4")) + 
  xlab("log Lot Size Stringency in Initial Equilibrium (from Data, in 2020 USD)") + 
  ylab("Log Differences in Land Value") +
  coord_cartesian(xlim = c(min(log(Init_eq[Init_eq$IncomeStringency_cl > 0 & 
                                              Init_eq$IncomeStringency_cl > quantile(Init_eq[Init_eq$IncomeStringency_cl > 0,]$IncomeStringency_cl, 
                                                                                     probs = c(0.01), na.rm = TRUE),]$IncomeStringency_cl), na.rm = TRUE),
                           max(log(Init_eq[Init_eq$IncomeStringency_cl > 0 & 
                                       Init_eq$IncomeStringency_cl < quantile(Init_eq[Init_eq$IncomeStringency_cl > 0,]$IncomeStringency_cl, 
                                                                                probs = c(0.99), na.rm = TRUE),]$IncomeStringency_cl), na.rm = TRUE)),  #Cut off x axis at 99th percentile for plot
                  ylim = c(min(Init_eq[Init_eq$LandValGrowth > quantile(Init_eq$LandValGrowth, 
                                                                        probs = c(0.001), na.rm = TRUE),]$LandValGrowth),
                           max(Init_eq[Init_eq$LandValGrowth < quantile(Init_eq$LandValGrowth, 
                                                                              probs = c(0.999), na.rm = TRUE),]$LandValGrowth, na.rm = TRUE) ) ) #manual axes
#Note: plot does not include zeros but it does not matter for conclusions.
ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/StringencyChangeLandVal", 
              BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
              BASELINE_SPECIFICATION$pref, ".png"), width = 20, height = 12, units = "cm") 

#__________________________________________________________________________________________________
#PART 2: Finding % wage change to make person as worse off in new equilibrium at they were in old.

  #Start loop over different types of welfare measurements
  comp_Var_store <- list() #Storing aggregate welfare effects for decomposition later in the file. 
  
  for (variation in c("Compensating", "Equivalent")) {
  
    if (variation == "Compensating") {
      source("CodeV2/Counterfactual/Functions/CompensatingVariation.R") #Load compensating variation function, changes getVariation
      saveString <- "Comp"
      plotTitle <- "Inverse Compensating Variation (percent of income)"
    }
  
    if (variation == "Equivalent") {
      source("CodeV2/Counterfactual/Functions/EquivalentVariation.R")
      saveString <- "Eq"
      plotTitle <- "Equivalent Variation (percent of income)"
    }

    var_Amen <- matrix(NA, length(skillVector) , 7)
    var_NoAmen <- matrix(NA, length(skillVector), 7)

    skillIndex <- 0
    for (skill_to_pass in skillVector) {
      skillIndex <- skillIndex + 1
      for (i in 1:7) { 
      
        if (variation == "Compensating") {
          var_Amen[skillIndex, i] <- 1/(getVariation(Init = Init_eq, Ct = Ct_Amenities, incomeType = i, skill = skill_to_pass, demandParameters = demandParameters_to_pass)) 
          var_NoAmen[skillIndex, i] <- 1/(getVariation(Init = Init_eq, Ct = Ct_NoAmenities, incomeType = i, skill = skill_to_pass, demandParameters = demandParameters_to_pass)) 
        }
      
        if (variation == "Equivalent") {
          var_Amen[skillIndex, i] <- getVariation(Init = Init_eq, Ct = Ct_Amenities, incomeType = i, skill = skill_to_pass, demandParameters = demandParameters_to_pass)
          var_NoAmen[skillIndex, i] <- getVariation(Init = Init_eq, Ct = Ct_NoAmenities, incomeType = i, skill = skill_to_pass, demandParameters = demandParameters_to_pass)
        }
      
      }
    }

    #Solving for aggregate welfare changes
    #vector of total populations  
    total_population <- matrix(NA, length(skillVector), 7)
    skillIndex <- 0
    for (skill_to_pass in skillVector) {
      name_of_skill <- skillName[which(skill_to_pass == skillVector)]
      skillIndex <- skillIndex + 1
      for (i in 1:7) {
        total_population[skillIndex, i] <- Ct_Amenities[[paste0("Total_Population_type_", name_of_skill, i)]][1]
      }
    }

    #Calculating social welfare == population weighted average of variation across all types
    total_Welfare_Amen <- sum( sum(diag( (total_population%*%(t(var_Amen))) ))/sum(total_population) )
    total_Welfare_NoAmen <- sum( sum(diag( (total_population%*%(t(var_NoAmen))) ))/sum(total_population) )

    print(paste0("The ", saveString, " social welfare for renters at baseline is ", total_Welfare_Amen, " for endogenous amenities."))
    print(paste0("The ", saveString, " social welfare for renters at baseline is ", total_Welfare_NoAmen, " for exogenous amenities."))


    #_______________Reporting welfare in bar chart__________________________________
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
  
        Welfare_barchart[[skill_to_pass]][index] <- var_Amen[skillIndex, i] - 1
        Welfare_barchart[[skill_to_pass]][index + 1] <- var_NoAmen[skillIndex, i] - 1
    
      }
  
      Welfare_barchart[[skill_to_pass]][index + 2] <- total_Welfare_Amen - 1
      Welfare_barchart[[skill_to_pass]][index + 3]  <- total_Welfare_NoAmen - 1
  
    }

    #BAR CHART
    BarplotDF <- data.frame() #initializing data frame

    for (skill_to_pass in skillVector) {
      BarplotDF <- rbind(BarplotDF,  data.frame(c(rep("1: 0 - 25,000", 2), rep("2: 25,000 - 50,000", 2), rep("3: 50,000 - 75,000", 2), 
                                                  rep("4: 75,000 - 100,000", 2), rep("5: 100,000 - 150,000", 2), rep("6: 150,000 - 200,000", 2), 
                                                  rep("7: 200,000+", 2), rep("Social Welfare", 2)), 
                                                rep(c("Endogenous Amenities", "Exogenous Amenities"), 2),
                                                100*Welfare_barchart[[skill_to_pass]], skill_to_pass)   )#end rbind
  
    }

    colnames(BarplotDF) <- c("Income", "Amenities", "Value", "Education")
  
    
    if (BASELINE_SPECIFICATION$bySkill_to_pass == FALSE) {
  
      ggplot(BarplotDF, aes(fill = Amenities, y = Value, x = factor(Income))) + 
             geom_bar(position = "dodge", stat = "identity") +
             xlab("Household type (income in average city, 2020 USD)") + 
             ylab(plotTitle) + 
             theme(axis.text.x=element_text(size=rel(1), angle=90))
      ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/Welfare_", saveString, "_var_bySkill_", 
                   BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                   BASELINE_SPECIFICATION$pref, ".png"), width = 25, height = 15, units = "cm") 
 
  
    #__________________________________________
    # SAVE POSTER VERSION OF WELFARE OUTPUT
    #__________________________________________
    ggplot(BarplotDF, aes(fill = Amenities, y = Value, x = factor(Income))) + 
      geom_bar(position = "dodge", stat = "identity") +
      xlab("Household income (in an average city, 2020 USD)") + 
      ylab("Willingness to pay for deregulation (% of income)") + 
      theme(axis.title.y = element_text(size = rel(1.4), angle = 90),
            axis.title.x = element_text(size = rel(1.8), angle = 0),
            axis.text.x = element_text(size=rel(1), angle=90)) 
    ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/Welfare_forPoster_", saveString, "_var_bySkill_", 
                  BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                  BASELINE_SPECIFICATION$pref, ".png"), width = 24, height = 16, units = "cm") 
     rm(BarplotDF)
  
  
    }else{
    
      ggplot(BarplotDF, aes(fill = Amenities, y = Value, x = factor(Income))) + 
            geom_bar(position = "dodge", stat = "identity") + 
            facet_wrap(~Education) + 
            xlab("Household type (income in average city, 2020 USD)") + 
            ylab(plotTitle) + 
            theme(axis.text.x=element_text(size=rel(1), angle=90))
      ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/Welfare_", saveString, "_var_bySkill_", 
                    BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                    BASELINE_SPECIFICATION$pref, ".png"), width = 25, height = 15, units = "cm") 
      rm(BarplotDF)
    }

    rm(skillIndex, skill_to_pass)
  
    #Storing compensating variation for further use in this program
    if (variation == "Compensating") {
      comp_Var_store[[variation]] <- var_Amen - 1 #
    }
    
    if (variation == "Equivalent") {
      comp_Var_store[[variation]] <- var_Amen - 1
    }
    
  } #End loop over welfare estimates

#________________________________________________________________________________________________________________________
#PART 3: Consumption and Amenity value decomposition using Shapely values.
#________________________________________________________________________________________________________________________

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
    
  #Total % change in welfare by type (in utils, not $)
  tWelfare <- Amenity_shapely + Consumption_shapely

  #Income exposure metric: How population-weighted neighborhood income changes by type
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
  
  #Alternative measure: neighborhood income growth weighted by initial population (has origins as first order approx)
  
  print("Average income faced by type relative to baseline")
  print(Income_exposure) 
  #Above average income has less exposure!

  #_________________________________Putting this all in another barchart_________________________________________
  #(Rescaling Amenity_shapely and Consumption_shapely using $$ estimates)
  
  
  for (variation in c("Compensating", "Equivalent")) {
    
    if (variation == "Compensating") {
      saveString <- "Comp"
      plotTitle <- "Inverse Compensating Variation (percent of income)"
    }
    
    if (variation == "Equivalent") {
      saveString <- "Eq"
      plotTitle <- "Equivalent Variation (percent of income)"
    }
    
    Amenity_shapely_norm <- 100*(Amenity_shapely/tWelfare)*(comp_Var_store[[variation]]) #comp_Var_store was created in welfare calculation above
    Consumption_shapely_norm <- 100*(Consumption_shapely/tWelfare)*(comp_Var_store[[variation]]) 
    #These are now reported as a % of income.
    print("Amenity contribution (In % $) __________")
    print(Amenity_shapely_norm)
  
    print("Consumption contribution (In % $) __________")
    print(Consumption_shapely_norm)
  
    #Doing the same for social welfare by type
    total_Consumption_shapely <- (Consumption_shapely_norm%*%t(total_population))/(sum(total_population))
    total_Amenity_shapely <- (Amenity_shapely_norm%*%t(total_population))/(sum(total_population))
    print(total_Consumption_shapely)
    print(total_Amenity_shapely)
  
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
    
      BarplotDF <- rbind(BarplotDF,  data.frame(c(rep("1: 0 - 25,000", 2), rep("2: 25,000 - 50,000", 2), rep("3: 50,000 - 75,000", 2), 
                                                  rep("4: 75,000 - 100,000", 2), rep("5: 100,000 - 150,000", 2), rep("6: 150,000 - 200,000", 2), 
                                                  rep("7: 200,000+", 2), rep("Social Welfare", 2)), 
                                                  rep(c("Consumption", "Amenity"), 2),
                                                  Welfare_barchart[[skill_to_pass]], skill_to_pass)   )#end rbind
    
    }
  
  
    colnames(BarplotDF) <- c("Income", "Decomposition", "Value", "Education")
  
  
    if (BASELINE_SPECIFICATION$bySkill_to_pass == FALSE) {
    
      ggplot(BarplotDF, aes(fill = Decomposition, y = Value, x = factor(Income))) + 
        geom_bar(position = "dodge", stat = "identity") +
        xlab("Household type (income in average city, 2020 USD)") + 
        ylab(plotTitle) + 
        theme(axis.text.x=element_text(size=rel(1), angle=90))
      ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/WelfareDecomp_", saveString,"_var_bySkill_", 
                    BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                    BASELINE_SPECIFICATION$pref, ".png"), width = 25, height = 15, units = "cm") 
    
    }else{
    
      ggplot(BarplotDF, aes(fill = Decomposition, y = Value, x = factor(Income))) + 
        geom_bar(position = "dodge", stat = "identity") + 
        facet_wrap(~Education) + 
        xlab("Household type (income in average city, 2020 USD)") + 
        ylab(plotTitle) +  
        theme(axis.text.x=element_text(size=rel(1), angle=90))
      ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/WelfareDecomp_", saveString,"_var_bySkill_", 
                    BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                    BASELINE_SPECIFICATION$pref, ".png"), width = 25, height = 15, units = "cm") 
    
    }
  
    rm(BarplotDF)
  } #End loop over equivalent and compensating variations
#____________________________________________________________________________________________________________________
#PART 4: Welfare changes assuming home-ownership (i.e. accounting for capital losses on housing assets by income type)
#____________________________________________________________________________________________________________________
  
  # What are we assuming...? 
  #1) Homeowners own all land wealth across the country (do not own capital used to produce structures)
  #2) Initial Fraction of income received from the !net! rental income on housing services (not rental income they pay to themselves))
  #Note: If net rental income on housing services is precisely zero (not negative), then homeowners (with debt) only pay user cost of owning housing 
  #(which we assume is == rental rates by assuming away tax advantages of owner occupied housing-- mortgage interest deductions--
  # and a no arbitrage condition)
  #But they also get capital gains...
  
  #Note: we cannot apply this more rigorously in a baseline model because we do not observe housing wealth and debt.
  #Does not make any sense to ascribe to households any non-level of debt.
  
  
  #Method to calculate s(z) weights in the paper
  #1) Calculate aggregate expenditure on housing by income type.
  
  #National housing expenditure by income type -- used to build total portfolio size by income type
  Init_eq["total_spending"] <- rep(0, nrow(Init_eq))
  
  for (skill in skillVector) {
    name_of_skill <- skillName[which(skill_to_pass == skillVector)]
    for (i in 1:7) {
      Init_eq[[paste0("total_spending_type_", name_of_skill, i)]] <- sum(Init_eq[[paste0(skill, "Wage")]]*
                                                                                  Init_eq[[paste0("ability_grp", i)]]*
                                                                                  Init_eq[[paste0("Population_type_", name_of_skill, i)]]*
                                                                                  spendShares_targeted[i])
      
      Init_eq["total_spending"] <- Init_eq[["total_spending"]] + Init_eq[[paste0("total_spending_type_", name_of_skill, i)]]
      
    }
  }
  #note: total spendign at data almost equals total spending nationally in model 
  #(error is due to inexact calibration of spending shares)
  
  #Taking total spending as share of total housing wealth, dividing it by the 
  #population by type to arrive at imputed rents for homeowners
  for (skill in skillVector) {
    name_of_skill <- skillName[which(skill_to_pass == skillVector)]
    for (i in 1:7) {
      Init_eq[[paste0("Housing_wealth_Owner_", name_of_skill, i)]] <- Init_eq[[paste0("total_spending_type_", name_of_skill, i)]]/
                                                                      (sum(Init_eq[[paste0("Population_type_", name_of_skill, i)]])*ownerOccupier_rate[i])
      
    
    }
  }
  
  #Finally, calculating s(z) weights using ability_grp (income in an average city) as follows:
  # s(z) = (housing_wealth_Owner)/(housing_wealth_Owner + ability_grp)
  housing_exposure_weights <- matrix(NA, length(skillVector), 7)
  
  skillIndex <- 0
  for (skill in skillVector) {
    skillIndex <- skillIndex + 1
    name_of_skill <- skillName[which(skill_to_pass == skillVector)]
    
    for (i in 1:7) {
      housing_exposure_weights[skillIndex, i] <-     Init_eq[[paste0("Housing_wealth_Owner_", name_of_skill, i)]][1]/
                                                     (Init_eq[[paste0("ability_grp", i)]][1] +  Init_eq[[paste0("Housing_wealth_Owner_", name_of_skill, i)]][1])
      
      
    }
  }
  
  #Finally, calculating welfare effect for homeowners and renters; putting into chart after pooling
   comp_var_homeowner <- matrix(NA, length(skillVector), 7)
   
   skillIndex <- 0
   for (skill in skillVector) {
     skillIndex <- skillIndex + 1
     name_of_skill <- skillName[which(skill_to_pass == skillVector)]
     
     for (i in 1:7) {
        comp_var_homeowner[skillIndex, i] <- (1 - housing_exposure_weights[skillIndex, i])*comp_Var_store[["Compensating"]][skillIndex, i] + 
                                             housing_exposure_weights[skillIndex, i]*(growthRate_landval)   
       
       
     }
   }
   
   comp_var_renter <- comp_Var_store[["Compensating"]] #renters have identical welfare measurement as before...
   
   #final measure of compensating variation, pooled over renters and homeowners
    comp_Var_pooled_final <- matrix(NA, length(skillVector), 7)
    
    skillIndex <- 0
    for (skill in skillVector) {
      skillIndex <- skillIndex + 1
      name_of_skill <- skillName[which(skill_to_pass == skillVector)]
      
      for (i in 1:7) {
              comp_Var_pooled_final[skillIndex, i] <-  ownerOccupier_rate[i]*comp_var_homeowner[skillIndex, i] + 
                                                       (1-ownerOccupier_rate[i])*comp_var_renter[skillIndex, i]
        
        
      }
    }
    rm(comp_var_renter, comp_var_homeowner) #rming data we don't need. 
    comp_Var_pooled_final <- 100*comp_Var_pooled_final
    #__________________________________________________________________
    #Start putting this in to a welfare barchart.
    Welfare_barchart <- list()
    
    for (skill_to_pass in skillVector) {
      Welfare_barchart[[skill_to_pass]] <- rep(NA, 8)
      
    }
    
    #Putting in estimates into the list
    skillIndex <- 0
    for (skill_to_pass in skillVector) {
      skillIndex <- skillIndex + 1
      for (i in 1:7) {

        Welfare_barchart[[skill_to_pass]][i] <- comp_Var_pooled_final[skillIndex, i]
        
      }
      
      Welfare_barchart[[skill_to_pass]][i + 1] <- (comp_Var_pooled_final%*%t(total_population))/(sum(total_population))
      
    }
    
    
    #BAR CHART FOR OUTPUT
    BarplotDF <- data.frame() #initializing data frame
    
    for (skill_to_pass in skillVector) {
      
      BarplotDF <- rbind(BarplotDF,  data.frame(c(rep("1: 0 - 25,000", 1), rep("2: 25,000 - 50,000", 1), rep("3: 50,000 - 75,000", 1), 
                                                  rep("4: 75,000 - 100,000", 1), rep("5: 100,000 - 150,000", 1), rep("6: 150,000 - 200,000", 1), 
                                                  rep("7: 200,000+", 1), rep("Social Welfare", 1)), 
                                                rep(c("Welfare"), 1),
                                                Welfare_barchart[[skill_to_pass]], skill_to_pass)   )#end rbind
      
    }
    
    
    colnames(BarplotDF) <- c("Income", "Decomposition", "Value", "Education")
    
    
    if (BASELINE_SPECIFICATION$bySkill_to_pass == FALSE) {
      
      ggplot(BarplotDF, aes(y = Value, x = factor(Income))) + 
        geom_bar(position = "dodge", stat = "identity", color= "#F8761D", fill = "#00BFC4") +
        xlab("Household type (income in average city, 2020 USD)") + 
        ylab("Inverse Compensating variation (% of income)") + 
        theme(axis.text.x=element_text(size=rel(1), angle=90))
      ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/PooledWelfare_Comp_var_bySkill_", 
                    BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                    BASELINE_SPECIFICATION$pref, ".png"), width = 25, height = 15, units = "cm") 
      
    }else{
      
      ggplot(BarplotDF, aes(y = Value, x = factor(Income))) + 
        geom_bar(position = "dodge", stat = "identity", color="#F8761D", fill = "#00BFC4") + 
        facet_wrap(~Education) + 
        xlab("Household type (income in average city, 2020 USD)") + 
        ylab("Inverse Compensating variation (% of income)") + 
        theme(axis.text.x=element_text(size=rel(1), angle=90))
      ggsave(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/PooledWelfare_Comp_var_bySkill_", 
                    BASELINE_SPECIFICATION$bySkill_to_pass, "_pref_", 
                    BASELINE_SPECIFICATION$pref, ".png"), width = 25, height = 15, units = "cm") 
      
    }
    
    rm(BarplotDF)
#_____________________________________________________________________________________________________________________________________________
#____________ROBUSTNESS: _____________________________________________________________________________________________________________________
#How do welfare results compare to other specifications?


rm(list = ls())