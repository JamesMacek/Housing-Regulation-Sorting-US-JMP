#PACKAGES
library(haven) #Reading stata.dta files 
library(dplyr)
library(readr)
library(collapse)
library(stringr)
library(labelled)
library(rlang)
library(doParallel) #Parallelize for loops, solving housing market equilibria much faster. 
library(estimatr)
library(ggplot2)


#This file analyzes output from SolveEquilibrium.R for our main set of counterfactuals (preliminary results)
#Loading data frames
load("Data/Counterfactuals/CounterfactualDataOutput/initial_equilibrium.Rdata")
Init_eq <- equilibrium_objects

load("Data/Counterfactuals/CounterfactualDataOutput/Counterfactual_Amenities.Rdata")
Ct_Amenities <- equilibrium_objects

load("Data/Counterfactuals/CounterfactualDataOutput/Counterfactual_NoAmenities.Rdata")
Ct_NoAmenities <- equilibrium_objects

load("Data/Counterfactuals/CounterfactualDataOutput/Main.Rdata")

rm(equilibrium_objects)

#Across and within elasticities From CalibrateMigrationElasticityToLogit.R
Beta <- 0.174


#Functions to calculate welfare and other outcomes

#Functions to calculate consumption index to solve for equivalent variation__________________________________________________________________________
ConsumptionFactor <- function(WageFactor, College, Income_bin, eq_objectsDF, MainDF, house_spendsh) { #outputs consumption vector for each group in vector
  
  #Data frame to return
  Consumption <- rep(1, nrow(eq_objectsDF))
  
  #Income stringency of lot size regulation in each block group
  eq_objectsDF["Incstr_lot_size"] <- MainDF$DensityRestriction*MainDF$lambda*(eq_objectsDF$housePrice^(MainDF$converted_elasticities + 1))
  
   if (College == 1) { #COLLEGE WORKERS
    ability_grp <- paste("ability_grp", Income_bin, sep = "") #measure of center for each income group
    
    #COLLEGE WORKERS
    Consumption <- ifelse(house_spendsh*eq_objectsDF$CollegeWage*WageFactor*eq_objectsDF[[ability_grp]] > eq_objectsDF$Incstr_lot_size, 1, 0)* #If Unconstrained in that market
      (house_spendsh^(house_spendsh)*(1-house_spendsh)^(1-house_spendsh))*(eq_objectsDF$CollegeWage*WageFactor*eq_objectsDF[[ability_grp]])/(eq_objectsDF$housePrice^(house_spendsh)) +
      
      ifelse((house_spendsh*eq_objectsDF$CollegeWage*WageFactor*eq_objectsDF[[ability_grp]] <= eq_objectsDF$Incstr_lot_size) & #If constrained, but can afford
               (eq_objectsDF$CollegeWage*WageFactor*eq_objectsDF[[ability_grp]] > eq_objectsDF$Incstr_lot_size), 1, 0)*
      ((eq_objectsDF$Incstr_lot_size/eq_objectsDF$housePrice)^(house_spendsh))*((eq_objectsDF$CollegeWage*WageFactor*eq_objectsDF[[ability_grp]] - eq_objectsDF$Incstr_lot_size)^(1-house_spendsh))  
    
    
    Consumption[is.nan(Consumption)] <- 0 #Replacing priced out households with 0 (all other households)
   }  
    
   if (College == 0) {#NON-COLLEGE WORKERS
     
     ability_grp <- paste("ability_grp", Income_bin, sep = "") #measure of center for each income group
     
     Consumption <- ifelse(house_spendsh*eq_objectsDF$NoCollegeWage*WageFactor*eq_objectsDF[[ability_grp]] > eq_objectsDF$Incstr_lot_size, 1, 0)* #If Unconstrained in that market
        (house_spendsh^(house_spendsh)*(1-house_spendsh)^(1-house_spendsh))*(eq_objectsDF$NoCollegeWage*WageFactor*eq_objectsDF[[ability_grp]])/(eq_objectsDF$housePrice^(house_spendsh)) +
      
        ifelse((house_spendsh*eq_objectsDF$NoCollegeWage*WageFactor*eq_objectsDF[[ability_grp]] <= eq_objectsDF$Incstr_lot_size) & #If constrained, but can afford
               (eq_objectsDF$NoCollegeWage*WageFactor*eq_objectsDF[[ability_grp]] > eq_objectsDF$Incstr_lot_size), 1, 0)*
        ((eq_objectsDF$Incstr_lot_size/eq_objectsDF$housePrice)^(house_spendsh))*((eq_objectsDF$NoCollegeWage*WageFactor*eq_objectsDF[[ability_grp]] - eq_objectsDF$Incstr_lot_size)^(1-house_spendsh)) 
    
    
      Consumption[is.nan(Consumption)] <- 0 #Replacing priced out households with 0
  }
    
  
  return(Consumption)
  
}

max(abs(ConsumptionFactor(1, 1, 1, Init_eq, Main, Beta) - ConsumptionFactor(1, 1, 1, Ct_Amenities, Main, Beta)))




#Function to calculate welfare given objects and a pre-determined Consumption Vector
CalculateWelfare <- function(College, Income_bin, ConsumptionVector, #pass group and consumption vector associated with that group from ConsumptionFactor
                             eq_objectsDF, MainDF, elast_within, elast_across) { #Returns value of welfare given consumption index and amenities.
  
  eq_objectsDF["ConsumptionVector"] <- ConsumptionVector #Storing in eq objects so that it works with dplyr
  
  if (College == 1) { #college workers 
    #Calculating within-city welfare
    eq_objectsDF <- eq_objectsDF %>% group_by(CBSA) %>% mutate(wc_amenity := 
                                                                 log(sum(exp(elast_within*ConsumptionVector)*!!sym(paste("withincity_amenity_College", Income_bin, sep = "")))))
    
    #Calculating across-city welfare (doesn't matter if we take logs here, as the equivalent variation is invariant to all monotone transformations.)
    Welfare <- sum(exp(elast_across*eq_objectsDF$wc_amenity)*eq_objectsDF[[paste("acrosscity_amenity_College", Income_bin, sep = "")]]/eq_objectsDF$MSAcount)
    
  }
  
  if (College == 0) { #nocollege workers 
    #Calculating within-city welfare
    eq_objectsDF <- eq_objectsDF %>% group_by(CBSA) %>% mutate(wc_amenity := 
                                                                 log(sum(exp(elast_within*ConsumptionVector)*!!sym(paste("withincity_amenity_NoCollege", Income_bin, sep = "")))))
    
    #Calculating across-city welfare (doesn't matter if we take logs here)
    Welfare <- sum(exp(elast_across*eq_objectsDF$wc_amenity)*eq_objectsDF[[paste("acrosscity_amenity_NoCollege", Income_bin, sep = "")]]/eq_objectsDF$MSAcount)
    
  }
  
  #returning welfare  
  return(Welfare)
  
}

#Function 
CalculateWelfare(1, 1, ConsumptionFactor(1, 1, 1, Init_eq, Main, Beta), Init_eq, Main, me_within[1], me_across[1]) #Testing
CalculateWelfare(1, 1, ConsumptionFactor(1, 1, 1, Ct_Amenities, Main, Beta), Ct_Amenities, Main, me_within[1], me_across[1])
CalculateWelfare(1, 1, ConsumptionFactor(1, 1, 1, Ct_NoAmenities, Main, Beta), Ct_NoAmenities, Main, me_within[1], me_across[1])


#Question-- what is the equivalent variation? I.e. by group, what percentage would you have to increase income in order to be as well off as after counterfactual?
#If the counterfactual never happened? I.e. the reference point is the initial equilibrium. 

EqVariationToSolve <- function(WageFactor, #Wagefactor will be solved for with uniroot.
                               College, Income_bin, 
                               new_eqobjectsDF, old_eqobjectsDF, MainDF,
                               house_spendsh, elast_within, elast_across) { #defining function difference
  
  return(CalculateWelfare(College, Income_bin, ConsumptionFactor(WageFactor, College, Income_bin, old_eqobjectsDF, MainDF, house_spendsh), 
                          old_eqobjectsDF, MainDF, elast_within, elast_across) - #WELFARE OF OLD EQUILIBRIUM x Wage Factor
         CalculateWelfare(College, Income_bin, ConsumptionFactor(1, College, Income_bin, new_eqobjectsDF, MainDF, house_spendsh), 
                          new_eqobjectsDF, MainDF, elast_within, elast_across)) #WELFARE OF NEW EQUILIBRIUM (WAGE FACTOR = 1)
  
}


#Part one: welfare of migrants
Eq_Variation_College <- rep(0, 7)
Eq_Variation_NoCollege <- rep(0, 7)
#Solving for equivalent variation of each... 
for (i in 1:7) {
  
  solve <- uniroot(EqVariationToSolve, extendInt = c("upX"), tol = 10*.Machine$double.eps, interval = c(0, 5),
                   College = 1, Income_bin = i, new_eqobjectsDF = Ct_Amenities,
                   old_eqobjectsDF = Init_eq, MainDF = Main, house_spendsh = Beta, 
                   elast_within = me_within[i], elast_across = me_across[i])$root
  
  Eq_Variation_College[i] <- 100*(solve - 1)
  
  solve <- uniroot(EqVariationToSolve, extendInt = c("upX"), tol = 10*.Machine$double.eps, interval = c(0, 5),
                   College = 0, Income_bin = i, new_eqobjectsDF = Ct_Amenities,
                   old_eqobjectsDF = Init_eq, MainDF = Main, house_spendsh = Beta, 
                   elast_within = me_within[i], elast_across = me_across[i])$root
  
  Eq_Variation_NoCollege[i] <- 100*(solve - 1)
  
}

Eq_Variation_NoAmCollege <- rep(0, 7)
Eq_Variation_NoAmNoCollege <- rep(0, 7)
for (i in 1:7) {
  
  solve <- uniroot(EqVariationToSolve, extendInt = c("upX"), tol = 10*.Machine$double.eps, interval = c(0, 5),
                   College = 1, Income_bin = i, new_eqobjectsDF = Ct_NoAmenities,
                   old_eqobjectsDF = Init_eq, MainDF = Main, house_spendsh = Beta, 
                   elast_within = me_within[i], elast_across = me_across[i])$root
  
  Eq_Variation_NoAmCollege[i] <- 100*(solve - 1)
  
  solve <- uniroot(EqVariationToSolve, extendInt = c("upX"), tol = 10*.Machine$double.eps, interval = c(0, 5),
                   College = 0, Income_bin = i, new_eqobjectsDF = Ct_NoAmenities,
                   old_eqobjectsDF = Init_eq, MainDF = Main, house_spendsh = Beta, 
                   elast_within = me_within[i], elast_across = me_across[i])$root
  
  Eq_Variation_NoAmNoCollege[i] <- 100*(solve - 1)
  
}

#Constructing barplots
BarplotDF <- data.frame(rep(c("1: 0 - 25,000", "2: 25,000 - 50,000", "3: 50,000 - 75,000", "4: 75,000 - 100,000", "5: 100,000 - 150,000", "6: 150,000 - 200,000", "7: 200,000+"), 4), 
                        rep(c(rep("College", 7), rep("NoCollege", 7)), 2), 
                        c(rep("Endogenous Amenities", 14), rep("Exogenous Amenities", 14)), 
                        c(Eq_Variation_College, Eq_Variation_NoCollege, Eq_Variation_NoAmCollege, Eq_Variation_NoAmNoCollege))
colnames(BarplotDF) <- c("Income", "Education", "Amenities", "Value")

ggplot(BarplotDF, aes(fill = Education, y = Value, x = factor(Income))) + 
      geom_bar(position = "dodge", stat = "identity") + 
      facet_wrap(~Amenities) + 
      xlab("Household type (income in average city)") + 
      ylab("Equivalent Variation (percent)") + 
      theme(axis.text.x=element_text(size=rel(1), angle=90))
ggsave("Data/Counterfactuals/CounterfactualDataOutput/Welfare.png", width = 20, height = 12, units = "cm") 
rm(BarplotDF)

#Equivalent variation of an average household in the endogenous amenities channel
#College household vector
College_hh_vec <- c(Init_eq$tot_households_College1[1], Init_eq$tot_households_College2[1], 
                    Init_eq$tot_households_College3[1], Init_eq$tot_households_College4[1],
                    Init_eq$tot_households_College5[1], Init_eq$tot_households_College6[1],
                    Init_eq$tot_households_College7[1])

NoCollege_hh_vec <- c(Init_eq$tot_households_NoCollege1[1], Init_eq$tot_households_NoCollege2[1], 
                    Init_eq$tot_households_NoCollege3[1], Init_eq$tot_households_NoCollege4[1],
                    Init_eq$tot_households_NoCollege5[1], Init_eq$tot_households_NoCollege6[1],
                    Init_eq$tot_households_NoCollege7[1])

# Average welfare across all workers
avg_welfare <- (College_hh_vec%*%Eq_Variation_College + NoCollege_hh_vec%*%Eq_Variation_NoCollege)/sum(Main$tot_housing_units_cen_2010) #roughly 5%
avg_welfare_NoAm <- (College_hh_vec%*%Eq_Variation_NoAmCollege + NoCollege_hh_vec%*%Eq_Variation_NoAmNoCollege)/sum(Main$tot_housing_units_cen_2010) #roughly 5%
#___________________________________________________________________________________________________________
#Part two: Land Values in equilibrium (per acre)
landVal <- Main %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME)
landVal["LandValAmenities"] <- (Ct_Amenities$housePrice^(Main$converted_elasticities + 1))*Main$lambda
landVal["LandValNoAmenities"] <- (Ct_NoAmenities$housePrice^(Main$converted_elasticities + 1))*Main$lambda
landVal["LandValInitEq"] <- (Init_eq$housePrice^(Main$converted_elasticities + 1))*Main$lambda
landVal["InitialStringency"] <- (Main$DensityRestriction*
                                  ((Main$hedonicPrice)^(Main$converted_elasticities + rep(1, nrow(Main)))))*Main$lambda
landVal["LandValGrowth"] <- log(landVal$LandValAmenities/landVal$LandValInitEq)
landVal["LandValGrowthNoAm"] <- log(landVal$LandValNoAmenities/landVal$LandValInitEq)

#Average housing land rents
weighted.mean(landVal$LandValInitEq, w = Main$tot_housing_units_cen_2010)
weighted.mean(landVal$LandValAmenities, w = Main$tot_housing_units_cen_2010)/weighted.mean(landVal$LandValInitEq, w = Main$tot_housing_units_cen_2010) - 1
weighted.mean(landVal$LandValNoAmenities, w = Main$tot_housing_units_cen_2010)/weighted.mean(landVal$LandValInitEq, w = Main$tot_housing_units_cen_2010) - 1 #Should weight this by # of households (assuming landlords proportional to number of households)
#Increase in average land values driven by endogenous amenities

#Regressing Initial equilibrium values on new values (note: we choose custom axis limits to remove outliers from graph)
ggplot() + 
        geom_point(data = landVal, aes(x = LandValInitEq, y = LandValAmenities), color = "black", size = 0.5, alpha = 0.3) + 
        xlim(0, 1.25e+07) + ylim(0, 1.25e+07) +  #Set manual scales to remove outliers 
        geom_smooth(data = landVal, aes(x = LandValInitEq, y = LandValAmenities), method = "lm") + 
        geom_abline(intercept = 0, slope = 1, colour = "red") + 
        xlab("Land Value in Initial Equilibrium (Dollars per acre)") + 
        ylab("Land Value in Counterfactual Equilibrium (Dollars per acre)")
ggsave("Data/Counterfactuals/CounterfactualDataOutput/LandValueReg.png", width = 20, height = 12, units = "cm") 

summary(lm(data = landVal, LandValAmenities ~ LandValInitEq))

#
ggplot() + 
  geom_point(data = landVal[landVal$InitialStringency > 0,], aes(x = InitialStringency, y = LandValGrowth), color = "black", size = 0.5, alpha = 0.1) +  #Set manual scales to remove outliers 
  geom_smooth(data = landVal[landVal$InitialStringency > 0,], aes(x = InitialStringency, y = LandValGrowth, color = "Endogenous Amenities"), method = "lm") +
  geom_smooth(data = landVal[landVal$InitialStringency > 0,], aes(x = InitialStringency, y = LandValGrowthNoAm, color = "Exogenous Amenities"), method = "lm") +
  scale_colour_manual(name="Model", values = c("red", "blue")) + 
  xlab("Lot Size Stringency in Initial Equilibrium (Dollars)") + 
  ylab("Log Differences in Land Value (Dollar per acre)")
ggsave("Data/Counterfactuals/CounterfactualDataOutput/StringencyChangeLandVal.png", width = 20, height = 12, units = "cm") 
  
#Lot size stringency strongly predicts land value changes...
summary(lm(data = landVal[landVal$InitialStringency > 0,], LandValGrowth  ~ InitialStringency))
summary(lm(data = landVal[landVal$InitialStringency > 0,], LandValGrowthNoAm  ~ InitialStringency)) #Opposite occurs in non-endogenous amenities model. 
sd(landVal[landVal$InitialStringency > 0,]$InitialStringency)*
  (lm(data = landVal[landVal$InitialStringency > 0,], LandValGrowth  ~ InitialStringency)$coefficients[2]) #to compare estimates 

#Part three: the importance of the Hseih-Moretti channel (i.e. find what growth in aggregate output is)
#I.e. Find the changes in aggregate output per worker. I.e. the sum of wages of all workers/number of workers
#Find changes in average welfare per worker (i.e. income-group weighted share of equivalent variation)
#Find how much aggregate output would change if there was no income re-sorting (i.e. assuming population flows didn't affect average household type)


#3.1
#Freeing up memory
rm(list=(ls()[ls()!="me_within" & ls()!="me_across"]))
load("Data/Counterfactuals/CounterfactualDataOutput/initial_equilibrium.Rdata")
Init_eq <- equilibrium_objects

load("Data/Counterfactuals/CounterfactualDataOutput/Counterfactual_Amenities.Rdata")
Ct_Amenities <- equilibrium_objects

load("Data/Counterfactuals/CounterfactualDataOutput/Counterfactual_NoAmenities.Rdata")
Ct_NoAmenities <- equilibrium_objects

load("Data/Counterfactuals/CounterfactualDataOutput/Main.Rdata")
rm(equilibrium_objects)

#Loading US_BLOCK data to make comparison to expensive cities
load(file = "Data/US_Data/Output/CBSA_quantiles.Rdata")
load(file = "Data/US_Data/Output/Constructed_2010_Tract.Rdata") #For ranks
US_BLOCK_2010_JOINED <- US_BLOCK_2010_JOINED %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME,
                                                        CBSA_med_house_value, rank_density_CBSA)
US_BLOCK_2010_JOINED$CBSA <- as.double(US_BLOCK_2010_JOINED$CBSA)

#merging to init_equilibrium, etc
Ct_Amenities <- left_join(Ct_Amenities, US_BLOCK_2010_JOINED, by = c("State", "County", "Tract", "BlockGroup", "CBSA", "CBSA_NAME"))
Init_eq <- left_join(Init_eq, US_BLOCK_2010_JOINED, by = c("State", "County", "Tract", "BlockGroup", "CBSA", "CBSA_NAME"))
rm(US_BLOCK_2010_JOINED)


AggregateOutput <- function(eq_objectsDF, MainDF) { #function to calculate aggregate output for multiple data frames
  
  eq_objectsDF <- eq_objectsDF %>% 
                mutate(Ag_output = (households_College1*ability_grp1 + households_College2*ability_grp2 +
                                            households_College3*ability_grp3 + households_College4*ability_grp4 + 
                                            households_College5*ability_grp5 + households_College6*ability_grp6 + 
                                            households_College7*ability_grp7)*CollegeWage + 
                                    (households_NoCollege1*ability_grp1 + households_NoCollege2*ability_grp2 +
                                             households_NoCollege3*ability_grp3 + households_NoCollege4*ability_grp4 + 
                                             households_NoCollege5*ability_grp5 + households_NoCollege6*ability_grp6 + 
                                             households_NoCollege7*ability_grp7)*NoCollegeWage)
                                             
            
  
  
  return(sum(eq_objectsDF$Ag_output)/sum(MainDF$tot_housing_units_cen_2010))
  
}

AggregateOutput(Ct_Amenities, Main)
AggregateOutput(Init_eq, Main) #only about 50 dollars per year change! This channel doesn't matter too much because there's income sorting!

#How does across-city sorting work? Look at total labour supply in each city vs population


TotalHouseholds <- function(eq_objectsDF) {
  eq_objectsDF <- eq_objectsDF %>% group_by(CBSA) %>% mutate(tot_households_city = sum(households_College1 + households_College2 + 
                                                                                         households_College3 + households_College4 + 
                                                                                         households_College5 + households_College6 + 
                                                                                         households_College7 +
                                                                                      households_NoCollege1 + households_NoCollege2 + 
                                                                                         households_NoCollege3 + households_NoCollege4 + 
                                                                                         households_NoCollege5 + households_NoCollege6 + 
                                                                                         households_NoCollege7))
  
  to_return <- eq_objectsDF %>% select(CBSA, CBSA_NAME, tot_households_city)
  return(to_return)
}

test1 <- TotalHouseholds(Ct_Amenities)
test2 <- TotalHouseholds(Init_eq)
test1["Difference_hh_Am"] <- 100*(test1$tot_households_city - test2$tot_households_city)/test2$tot_households_city 

#Testing changes in household ability per worker

AverageMSAType <- function(eq_objectsDF) {
  eq_objectsDF <- eq_objectsDF %>% group_by(CBSA) %>% mutate(total_labsupply_city = sum(households_College1*ability_grp1 + households_College2*ability_grp2 + 
                                                                                         households_College3*ability_grp3 + households_College4*ability_grp4 + 
                                                                                         households_College5*ability_grp5 + households_College6*ability_grp6 + 
                                                                                         households_College7*ability_grp7 +
                                                                                         households_NoCollege1*ability_grp1 + households_NoCollege2*ability_grp2+ 
                                                                                         households_NoCollege3*ability_grp3 + households_NoCollege4*ability_grp4 + 
                                                                                         households_NoCollege5*ability_grp5 + households_NoCollege6*ability_grp6 + 
                                                                                         households_NoCollege7*ability_grp7))
  
  to_return <- eq_objectsDF %>% select(CBSA, CBSA_NAME, total_labsupply_city)
  to_return["Average_type_city"] <- to_return$total_labsupply_city/(TotalHouseholds(eq_objectsDF)$tot_households_city)
  return(to_return)
}

test4 <- AverageMSAType(Ct_Amenities)
test5 <- AverageMSAType(Init_eq)
test4["Difference_type_Am"] <- 100*((test4$Average_type_city - test5$Average_type_city)/test5$Average_type_city)
rm(test5)

#Joining, collapsing and regressing type growth on population growth
gc() #freeing up memory again

test1 <- collap(test1, Difference_hh_Am ~ CBSA + CBSA_NAME, FUN = "fmean")
test4 <- collap(test4, Difference_type_Am ~ CBSA + CBSA_NAME, FUN = "fmean")
test1 <- left_join(test1, test4, by = c("CBSA", "CBSA_NAME"))
rm(test4)
rownames(test1) <- test1$CBSA_NAME

#Merging with data on average wages in initial equilibrium, + CBSA med house values from observed data to make comparisons with expensive vs inexpensive cities
wagesToMerge <- collap(Init_eq, CollegeWage + NoCollegeWage + CBSA_med_house_value ~ CBSA + CBSA_NAME, FUN = "fmean")
#joining initial city populations
city_pop <- TotalHouseholds(Init_eq)
city_pop <- collap(city_pop, tot_households_city ~ CBSA + CBSA_NAME, fun = "fmean")

test1 <- left_join(test1, wagesToMerge, by = c("CBSA", "CBSA_NAME"))
test1 <- left_join(test1, city_pop, by = c("CBSA", "CBSA_NAME"))

rm(wagesToMerge, city_pop)

test1["average_Wage"] <- (1/2)*(test1$CollegeWage + test1$NoCollegeWage)

ggplot() +
  geom_point(data = test1, size = 3, alpha = 0.2, aes(x = Difference_hh_Am, y = Difference_type_Am, color = average_Wage)) + 
  geom_smooth(method = "lm") +
  geom_text(data = test1, check_overlap = T, size = 3, nudge_y = 1,
            aes(x = Difference_hh_Am, y = Difference_type_Am, label = CBSA_NAME)) + 
  scale_color_gradient(low = "red", high = "blue", name = "Productivity") +
  xlab("Growth rate in number of households (percent)") + 
  ylab("Growth rate in average household type (percent)")
ggsave("Data/Counterfactuals/CounterfactualDataOutput/IncomeSortingMovement.png", width = 30, height = 18, units = "cm") 

#Robustness: same with house values
ggplot() +
  geom_point(data = test1, size = 3, alpha = 0.7, aes(x = Difference_hh_Am, y = Difference_type_Am, color = CBSA_med_house_value)) + 
  geom_smooth(method = "lm") +
  geom_text(data = test1, check_overlap = T, size = 3, nudge_y = 1,
            aes(x = Difference_hh_Am, y = Difference_type_Am, label = CBSA_NAME)) + 
  scale_color_gradient(low = "red", high = "blue", name = "House values") +
  xlab("Growth rate in number of households (percent)") + 
  ylab("Growth rate in average household type (percent)")


#Correlation between wage, household growth, and expensiveness
cor(as.matrix(select(.data = test1, average_Wage, Difference_hh_Am, CBSA_med_house_value))) 


#Checking changes in household growth rates by superstar sample
weighted.mean(test1[test1$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$Difference_hh_Am, 
              w = test1[test1$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$tot_households_city)

weighted.mean(test1[test1$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$Difference_type_Am, 
              w = test1[test1$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$tot_households_city)

#yep... expensive cities get less affluent (in average type), 2.3% larger

weighted.mean(test1[test1$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$Difference_hh_Am, 
              w = test1[test1$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$tot_households_city)

weighted.mean(test1[test1$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$Difference_type_Am, 
              w = test1[test1$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$tot_households_city)

#Yep... inexpensive cities get 4% smaller, 3.3% more affluent 
#So selection into expensive cities changes 


#Part four: Outcomes for income sorting, distribution of households (missing middle) (remember-- these variables are fit almost exactly to the data we use, so comparable)
#(Also we have different samples for the facts based on data availability, so need to account for that)

#4.1 - within city population distributions...

#Creating housing unit density
Init_eq["land_area_acres"] <- Main$land_area_acres
Init_eq <- Init_eq %>% mutate(housing_unit_density  = (households_College1 + households_College2 +
                                                         households_College3 + households_College4 + 
                                                         households_College5 + households_College6 + 
                                                         households_College7 +
                                                         households_NoCollege1 + households_NoCollege2 +
                                                         households_NoCollege3 + households_NoCollege4 + 
                                                         households_NoCollege5 + households_NoCollege6 + 
                                                         households_NoCollege7)/land_area_acres)

Ct_Amenities["land_area_acres"] <- Main$land_area_acres
Ct_Amenities <- Ct_Amenities %>% mutate(housing_unit_density  = (households_College1 + households_College2 +
                                                         households_College3 + households_College4 + 
                                                         households_College5 + households_College6 + 
                                                         households_College7 +
                                                         households_NoCollege1 + households_NoCollege2 +
                                                         households_NoCollege3 + households_NoCollege4 + 
                                                         households_NoCollege5 + households_NoCollege6 + 
                                                         households_NoCollege7)/land_area_acres)

#Creating (new) ranking of housing unit density for each -- Note sample here is a subset of the sample from the initial facts. (UPDATED 08/09)
Ct_Amenities <- Ct_Amenities %>% group_by(CBSA) %>% mutate(rank_density_CBSA = order(order(housing_unit_density, decreasing = FALSE))/(max(order(order(housing_unit_density, decreasing = FALSE)))+1))
Init_eq <- Init_eq %>% group_by(CBSA) %>% mutate(rank_density_CBSA = order(order(housing_unit_density, decreasing = FALSE))/(max(order(order(housing_unit_density, decreasing = FALSE)))+1))


#demeaning housing unit density
Init_eq <- Init_eq %>% group_by(CBSA) %>% mutate(demeaned_hdensity = housing_unit_density/mean(housing_unit_density))
Ct_Amenities <- Ct_Amenities %>% group_by(CBSA) %>% mutate(demeaned_hdensity = housing_unit_density/mean(housing_unit_density))

#Plotting + Comparing to initial equilibrium (should look exactly the same as in our Fact1-- up to missing observations due to sample changes)-- and it does.
ggplot() + 
  geom_smooth(method = 'loess', span=0.5, data = 
                Ct_Amenities[Ct_Amenities$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], se = FALSE, #SET SE = FALSE FOR RAM ISSUES 
              aes(x=rank_density_CBSA, y=demeaned_hdensity, color = 'Top 25% Counterfactual')) +
  geom_smooth(method = 'loess', span=0.5, data = 
                Ct_Amenities[Ct_Amenities$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], se = FALSE, #SET SE = FALSE FOR RAM ISSUES
              aes(x=rank_density_CBSA, y=demeaned_hdensity, color = 'Bottom 25% Counterfactual')) +
  geom_smooth(method = 'loess', span=0.5, alpha = 0.3, data = 
                Init_eq[Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], se = FALSE, #SET SE = FALSE FOR RAM ISSUES 
              aes(x=rank_density_CBSA, y=demeaned_hdensity, color = 'Top 25% Observed')) +
  geom_smooth(method = 'loess', span=0.5, alpha = 0.3, data = 
                Init_eq[Init_eq$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], se = FALSE, #SET SE = FALSE FOR RAM ISSUES
              aes(x=rank_density_CBSA, y=demeaned_hdensity, color = 'Bottom 25% Observed')) +
  scale_colour_manual(name="Sample", values = c("red", "red", "blue", "blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Housing unit density (MSA Average == 1)")
ggsave("Data/Counterfactuals/CounterfactualDataOutput/Density_dist_ctfcl.png", width = 20, height = 12, units = "cm")

#Loess fit calculation for 100 evenly spaced points between 0 and 1
Ct_blockdens_fit_t25 <- predict(loess(demeaned_hdensity ~ rank_density_CBSA, 
                                      data = Ct_Amenities[Ct_Amenities$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),],
                                      span = 0.5),
                                seq(0, 1, length.out = 250))
Ct_blockdens_fit_b25 <- predict(loess(demeaned_hdensity ~ rank_density_CBSA, 
                                      data = Ct_Amenities[Ct_Amenities$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),],
                                      span = 0.5),
                                  seq(0, 1, length.out = 250))
distance_Ct <- sum((Ct_blockdens_fit_t25 - Ct_blockdens_fit_b25)^2, na.rm = TRUE) #l2 norm -- 13.86 sum of squared distances

Init_blockdens_fit_t25 <- predict(loess(demeaned_hdensity ~ rank_density_CBSA, 
                                      data = Init_eq[Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),],
                                      span = 0.5),
                                seq(0, 1, length.out = 250))
Init_blockdens_fit_b25 <- predict(loess(demeaned_hdensity ~ rank_density_CBSA, 
                                      data = Init_eq[Init_eq$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),],
                                      span = 0.5),
                                seq(0, 1, length.out = 250))
distance_Init <- sum((Init_blockdens_fit_t25 - Init_blockdens_fit_b25)^2, na.rm = TRUE) #l2 norm -- 25.24 sum of squared distances

#Change in distances across curves 
hdens_changeindist <- 100*(distance_Ct - distance_Init)/(distance_Init) #Difference in curves (in l2 norm) falls by approx. 50 percent!

#Switching in average density gradient. 


#4.2 - INCOME PER CAPITA to test within city income sorting
Init_eq <- Init_eq %>% mutate(implied_hhavgincome = ((households_College1*ability_grp1 + households_College2*ability_grp2 +
                                                      households_College3*ability_grp3 + households_College4*ability_grp4 + 
                                                      households_College5*ability_grp5 + households_College6*ability_grp6 + 
                                                      households_College7*ability_grp7)*CollegeWage +
                                                 
                                                     (households_NoCollege1*ability_grp1 + households_NoCollege2*ability_grp2 +
                                                      households_NoCollege3*ability_grp3 + households_NoCollege4*ability_grp4 + 
                                                      households_NoCollege5*ability_grp5 + households_NoCollege6*ability_grp6 + 
                                                      households_NoCollege7*ability_grp7)*NoCollegeWage)/
                          
                                                     (households_College1 + households_College2 +
                                                      households_College3 + households_College4 + 
                                                      households_College5 + households_College6 + 
                                                      households_College7 +
                                                      households_NoCollege1 + households_NoCollege2 +
                                                      households_NoCollege3 + households_NoCollege4 + 
                                                      households_NoCollege5 + households_NoCollege6 + 
                                                      households_NoCollege7))

Ct_Amenities <- Ct_Amenities %>% mutate(implied_hhavgincome = ((households_College1*ability_grp1 + households_College2*ability_grp2 +
                                                                households_College3*ability_grp3 + households_College4*ability_grp4 + 
                                                                households_College5*ability_grp5 + households_College6*ability_grp6 + 
                                                                households_College7*ability_grp7)*CollegeWage +
                                                       
                                                               (households_NoCollege1*ability_grp1 + households_NoCollege2*ability_grp2 +
                                                                households_NoCollege3*ability_grp3 + households_NoCollege4*ability_grp4 + 
                                                                households_NoCollege5*ability_grp5 + households_NoCollege6*ability_grp6 + 
                                                                households_NoCollege7*ability_grp7)*NoCollegeWage)/
                                
                                                               (households_College1 + households_College2 +
                                                                households_College3 + households_College4 + 
                                                                households_College5 + households_College6 + 
                                                                households_College7 +
                                                                households_NoCollege1 + households_NoCollege2 +
                                                                households_NoCollege3 + households_NoCollege4 + 
                                                                households_NoCollege5 + households_NoCollege6 + 
                                                                households_NoCollege7))

Ct_NoAmenities <- Ct_NoAmenities %>% mutate(implied_hhavgincome = ((households_College1*ability_grp1 + households_College2*ability_grp2 +
                                                                  households_College3*ability_grp3 + households_College4*ability_grp4 + 
                                                                  households_College5*ability_grp5 + households_College6*ability_grp6 + 
                                                                  households_College7*ability_grp7)*CollegeWage +
                                                                 
                                                                 (households_NoCollege1*ability_grp1 + households_NoCollege2*ability_grp2 +
                                                                    households_NoCollege3*ability_grp3 + households_NoCollege4*ability_grp4 + 
                                                                    households_NoCollege5*ability_grp5 + households_NoCollege6*ability_grp6 + 
                                                                    households_NoCollege7*ability_grp7)*NoCollegeWage)/
                                          
                                                                  (households_College1 + households_College2 +
                                                                   households_College3 + households_College4 + 
                                             households_College5 + households_College6 + 
                                             households_College7 +
                                             households_NoCollege1 + households_NoCollege2 +
                                             households_NoCollege3 + households_NoCollege4 + 
                                             households_NoCollege5 + households_NoCollege6 + 
                                             households_NoCollege7))

Init_eq <- Init_eq %>% group_by(CBSA) %>% mutate(demeaned_hhincome = log(implied_hhavgincome) - mean(log(implied_hhavgincome)))
Ct_Amenities <- Ct_Amenities %>% group_by(CBSA) %>% mutate(demeaned_hhincome = log(implied_hhavgincome) - mean(log(implied_hhavgincome)))

#Plotting
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = 
                Ct_Amenities[Ct_Amenities$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], se = FALSE, #SET SE = FALSE FOR RAM ISSUES 
              aes(x=rank_density_CBSA, y=demeaned_hhincome, color = 'Top 25% Counterfactual')) +
  geom_smooth(method = 'loess', span=1, data = 
                Ct_Amenities[Ct_Amenities$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], se = FALSE, #SET SE = FALSE FOR RAM ISSUES
              aes(x=rank_density_CBSA, y=demeaned_hhincome, color = 'Bottom 25% Counterfactual')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Log(Household income) (demeaned by MSA)")
ggsave("Data/Counterfactuals/CounterfactualDataOutput/Income_dist_ctfcl.png", width = 20, height = 12, units = "cm")


#Do linear regressions. 
summary(lm_robust(demeaned_hhincome ~ rank_density_CBSA, data = Ct_Amenities[Ct_Amenities$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]))
summary(lm_robust(demeaned_hhincome ~ rank_density_CBSA, data = Ct_Amenities[Ct_Amenities$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]))
summary(lm_robust(demeaned_hhincome ~ rank_density_CBSA, data = Init_eq[Ct_Amenities$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]))
summary(lm_robust(demeaned_hhincome ~ rank_density_CBSA, data = Init_eq[Ct_Amenities$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]))
#Differences in average slope cut in half. 


#Comparing to initial equilibrium (should look exactly the same as in our Fact1-- up to missing observations due to sample)-- and it does. 
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = 
                Init_eq[Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], se = FALSE, #SET SE = FALSE FOR RAM ISSUES 
              aes(x=rank_density_CBSA, y=demeaned_hhincome, color = 'Top 25%')) +
  geom_smooth(method = 'loess', span=1, data = 
                Init_eq[Init_eq$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], se = FALSE, #SET SE = FALSE FOR RAM ISSUES
              aes(x=rank_density_CBSA, y=demeaned_hhincome, color = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Log(Household income) (demeaned by MSA)")


#Margins of welfare effects

#Idea: check if all welfare gains are happening in big vs small cities (i.e. top quartile) -- do this welfare decomposition
#Idea: check if all welfare gains are happening in big downtowns or big suburbs 

#Calculating welfare effects for workers of all types and weighting (don't worry about endogenous amenities)


for (i in 1:7) {
  
  #Amenities
  Ct_Amenities[paste("Welfare_weight_College", i, sep = "")] <- (me_within[i]*Ct_Amenities[[paste("consumption_val_College", i, sep = "")]] + 
                                                      log(Ct_Amenities[[paste("withincity_amenity_College", i, sep = "")]]*
                                                      Ct_Amenities[[paste("acrosscity_amenity_College", i, sep = "")]]))*
                                                      Init_eq[[paste("households_College", i, sep = "")]] #weight with respect to initial equilibirum
  
  Ct_Amenities[[paste("Welfare_weight_College", i, sep = "")]][is.nan(Ct_Amenities[[paste("Welfare_weight_College", i, sep = "")]])] <- 0
  
  Ct_Amenities[paste("Welfare_weight_NoCollege", i, sep = "")] <- (me_within[i]*Ct_Amenities[[paste("consumption_val_NoCollege", i, sep = "")]] + 
                                                        log(Ct_Amenities[[paste("withincity_amenity_NoCollege", i, sep = "")]]*
                                                        Ct_Amenities[[paste("acrosscity_amenity_NoCollege", i, sep = "")]]))*
                                                        Init_eq[[paste("households_NoCollege", i, sep = "")]] #weight with respect to initial equilibirum
  
  Ct_Amenities[[paste("Welfare_weight_NoCollege", i, sep = "")]][is.nan(Ct_Amenities[[paste("Welfare_weight_NoCollege", i, sep = "")]])] <- 0
  
  #Ct_NoAmenities
  Ct_NoAmenities[paste("Welfare_weight_College", i, sep = "")] <- (me_within[i]*Ct_NoAmenities[[paste("consumption_val_College", i, sep = "")]] + 
                                                                   log(Ct_NoAmenities[[paste("withincity_amenity_College", i, sep = "")]]*
                                                                         Ct_NoAmenities[[paste("acrosscity_amenity_College", i, sep = "")]]))*
                                                                    Init_eq[[paste("households_College", i, sep = "")]] #weight with respect to initial equilibirum
  
  Ct_NoAmenities[[paste("Welfare_weight_College", i, sep = "")]][is.nan(Ct_NoAmenities[[paste("Welfare_weight_College", i, sep = "")]])] <- 0
  
  Ct_NoAmenities[paste("Welfare_weight_NoCollege", i, sep = "")] <- (me_within[i]*Ct_NoAmenities[[paste("consumption_val_NoCollege", i, sep = "")]] + 
                                                                     log(Ct_NoAmenities[[paste("withincity_amenity_NoCollege", i, sep = "")]]*
                                                                           Ct_NoAmenities[[paste("acrosscity_amenity_NoCollege", i, sep = "")]]))*
                                                                   Init_eq[[paste("households_NoCollege", i, sep = "")]] #weight with respect to initial equilibirum
  
  Ct_NoAmenities[[paste("Welfare_weight_NoCollege", i, sep = "")]][is.nan(Ct_NoAmenities[[paste("Welfare_weight_NoCollege", i, sep = "")]])] <- 0
  
  
  
  Init_eq[paste("Welfare_weight_College", i, sep = "")] <- (me_within[i]*Init_eq[[paste("consumption_val_College", i, sep = "")]] + 
                                                         log(Init_eq[[paste("withincity_amenity_College", i, sep = "")]]*
                                                               Init_eq[[paste("acrosscity_amenity_College", i, sep = "")]]))*
                                                        Init_eq[[paste("households_College", i, sep = "")]] #weight with respect to initial equilibirum
  
  
  Init_eq[[paste("Welfare_weight_College", i, sep = "")]][is.nan(Init_eq[[paste("Welfare_weight_College", i, sep = "")]])] <- 0
  
  Init_eq[paste("Welfare_weight_NoCollege", i, sep = "")] <- (me_within[i]*Init_eq[[paste("consumption_val_NoCollege", i, sep = "")]] + 
                                                           log(Init_eq[[paste("withincity_amenity_NoCollege", i, sep = "")]]*
                                                                 Init_eq[[paste("acrosscity_amenity_NoCollege", i, sep = "")]]))*
                                                        Init_eq[[paste("households_NoCollege", i, sep = "")]] #weight with respect to initial equilibirum
  
  Init_eq[[paste("Welfare_weight_NoCollege", i, sep = "")]][is.nan(Init_eq[[paste("Welfare_weight_NoCollege", i, sep = "")]])] <- 0
  
}


#Summing all welfare weights and differencing

Ct_Amenities["Welfare_effect"] <- rep(0, nrow(Ct_Amenities))
Ct_NoAmenities["Welfare_effect"] <- rep(0, nrow(Ct_NoAmenities))


for (i in 1:7) {
  Ct_Amenities$Welfare_effect <- Ct_Amenities[[paste("Welfare_weight_College", i, sep = "")]] + Ct_Amenities$Welfare_effect - (Init_eq[[paste("Welfare_weight_College", i, sep = "")]] )
  Ct_Amenities$Welfare_effect <- Ct_Amenities[[paste("Welfare_weight_NoCollege", i, sep = "")]] + Ct_Amenities$Welfare_effect - (Init_eq[[paste("Welfare_weight_NoCollege", i, sep = "")]])
  Ct_NoAmenities$Welfare_effect <- Ct_NoAmenities[[paste("Welfare_weight_College", i, sep = "")]] + Ct_NoAmenities$Welfare_effect - (Init_eq[[paste("Welfare_weight_College", i, sep = "")]] )
  Ct_NoAmenities$Welfare_effect <- Ct_NoAmenities[[paste("Welfare_weight_NoCollege", i, sep = "")]] + Ct_NoAmenities$Welfare_effect - (Init_eq[[paste("Welfare_weight_NoCollege", i, sep = "")]])
  
}

#Amenities total welfare
Tot_welfare <- sum(Ct_Amenities$Welfare_effect, na.rm = TRUE)
welfare_share_big <- sum(Ct_Amenities[Ct_Amenities$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$Welfare_effect, na.rm = TRUE)/Tot_welfare
welfare_share_small <- sum(Ct_Amenities[Ct_Amenities$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$Welfare_effect, na.rm = TRUE)/Tot_welfare

#more than half (58%) of welfare gains coming from action in high density neighborhoods == spillover effects 
welfare_share_bighighdens <- sum(Ct_Amenities[Ct_Amenities$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) & Init_eq$rank_density_CBSA >= 0.5,]$Welfare_effect, na.rm = TRUE)/Tot_welfare
welfare_share_smallhighdens <- sum(Ct_Amenities[Ct_Amenities$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]) & Init_eq$rank_density_CBSA >= 0.5,]$Welfare_effect, na.rm = TRUE)/Tot_welfare

#No Amenities total welfare
Tot_welfare <- sum(Ct_NoAmenities$Welfare_effect, na.rm = TRUE)
welfare_share_big <- sum(Ct_NoAmenities[Ct_Amenities$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$Welfare_effect, na.rm = TRUE)/Tot_welfare
welfare_share_small <- sum(Ct_NoAmenities[Ct_Amenities$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$Welfare_effect, na.rm = TRUE)/Tot_welfare

#Half (50%) of welfare gains coming from action in high density neighborhoods (slightly less than above because of exogenous amenities). Despite the fact that low density neighborhoods 3x more regulated. 
welfare_share_bighighdens <- sum(Ct_NoAmenities[Ct_Amenities$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) & Init_eq$rank_density_CBSA >= 0.5,]$Welfare_effect, na.rm = TRUE)/Tot_welfare
welfare_share_smallhighdens <- sum(Ct_NoAmenities[Ct_Amenities$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]) & Init_eq$rank_density_CBSA >= 0.5,]$Welfare_effect, na.rm = TRUE)/Tot_welfare



#Population share in each 
Ct_Amenities["tot_housing_units_cen_2010"] <- Main$tot_housing_units_cen_2010


pop_share_big <- sum(Ct_Amenities[Ct_Amenities$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$tot_housing_units_cen_2010, na.rm = TRUE)/sum(Ct_Amenities$tot_housing_units_cen_2010)
pop_share_big_hdens <- sum(Ct_Amenities[Ct_Amenities$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) & Init_eq$rank_density_CBSA >= 0.5,]$tot_housing_units_cen_2010, na.rm = TRUE)/sum(Ct_Amenities$tot_housing_units_cen_2010)

#almost exactly half of household share 

#checking average income stringency in low density neighborhoods
Main["InitialStringency"] <- (Main$DensityRestriction*
                                   ((Main$hedonicPrice)^(Main$converted_elasticities + rep(1, nrow(Main)))))*Main$lambda

Share_reg_big <- (sum(Main[Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$InitialStringency, na.rm = TRUE))/(sum(Main$InitialStringency))
Share_reg_big_hdens <- sum(Main[Init_eq$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) & Init_eq$rank_density_CBSA >= 0.5,]$InitialStringency, na.rm = TRUE)/sum(Main$InitialStringency)
#high density expensive neighborhoods are considerably less regulated, (i.e. 1/3 of regulation coming from higher density neighborhoods)
#but account for the bulk of aggregate welfare gains coming from downtowns => spillover effects from mobility.


#PART 2: analyzing aggregate welfare in equilibrium (using shares of different types)
load("Data/Counterfactuals/CounterfactualDataOutput/Counterfactual_Amenities_noMobility.Rdata")
Ct_Am_NoMobility <- equilibrium_objects

load("Data/Counterfactuals/CounterfactualDataOutput/Counterfactual_Amenities_withincMobility.Rdata")
Ct_Am_withincMobility <- equilibrium_objects

rm(equilibrium_objects)


#Checking if within-city population constant from Ct_Am_NoMobility
Ct_Am_withincMobility <- Ct_Am_withincMobility %>% group_by(CBSA) %>% mutate(test = sum(households_College1))
Init_eq <- Init_eq %>% group_by(CBSA) %>% mutate(test = sum(households_College1))
Ct_Am_withincMobility$test - Init_eq$test #Close to 0



#Constructing measures of aggregate welfare for no mobility and within city mobility cases
for (i in 1:7) {
  
  #No mobility
  Ct_Am_NoMobility[paste("Welfare_weight_College", i, sep = "")] <- (me_within[i]*Ct_Am_NoMobility[[paste("consumption_val_College", i, sep = "")]] + 
                                                                   log(Ct_Am_NoMobility[[paste("withincity_amenity_College", i, sep = "")]]*
                                                                    Ct_Am_NoMobility[[paste("acrosscity_amenity_College", i, sep = "")]]))*
                                                                    Init_eq[[paste("households_College", i, sep = "")]] #weight with respect to initial equilibirum
  
  Ct_Am_NoMobility[[paste("Welfare_weight_College", i, sep = "")]][is.nan(Ct_Am_NoMobility[[paste("Welfare_weight_College", i, sep = "")]])] <- 0
  
  Ct_Am_NoMobility[paste("Welfare_weight_NoCollege", i, sep = "")] <- (me_within[i]*Ct_Am_NoMobility[[paste("consumption_val_NoCollege", i, sep = "")]] + 
                                                                     log(Ct_Am_NoMobility[[paste("withincity_amenity_NoCollege", i, sep = "")]]*
                                                                     Ct_Am_NoMobility[[paste("acrosscity_amenity_NoCollege", i, sep = "")]]))*
                                                                     Init_eq[[paste("households_NoCollege", i, sep = "")]] #weight with respect to initial equilibirum
  
  Ct_Am_NoMobility[[paste("Welfare_weight_NoCollege", i, sep = "")]][is.nan(Ct_Am_NoMobility[[paste("Welfare_weight_NoCollege", i, sep = "")]])] <- 0 #doesnt matter
  
}

#Summing all welfare weights and differencing
Ct_Am_NoMobility["Welfare_effect"] <- rep(0, nrow(Ct_Am_NoMobility))


for (i in 1:7) {
  Ct_Am_NoMobility$Welfare_effect <- Ct_Am_NoMobility[[paste("Welfare_weight_College", i, sep = "")]] + Ct_Am_NoMobility$Welfare_effect - (Init_eq[[paste("Welfare_weight_College", i, sep = "")]] )
  Ct_Am_NoMobility$Welfare_effect <- Ct_Am_NoMobility[[paste("Welfare_weight_NoCollege", i, sep = "")]] + Ct_Am_NoMobility$Welfare_effect - (Init_eq[[paste("Welfare_weight_NoCollege", i, sep = "")]])
  
}

#Withincity mobility
for (i in 1:7) {
  
  #No mobility
  Ct_Am_withincMobility[paste("Welfare_weight_College", i, sep = "")] <- (me_within[i]*Ct_Am_withincMobility[[paste("consumption_val_College", i, sep = "")]] + 
                                                                       log(Ct_Am_withincMobility[[paste("withincity_amenity_College", i, sep = "")]]*
                                                                             Ct_Am_withincMobility[[paste("acrosscity_amenity_College", i, sep = "")]]))*
    Init_eq[[paste("households_College", i, sep = "")]] #weight with respect to initial equilibirum
  
  Ct_Am_withincMobility[[paste("Welfare_weight_College", i, sep = "")]][is.nan(Ct_Am_withincMobility[[paste("Welfare_weight_College", i, sep = "")]])] <- 0
  
  Ct_Am_withincMobility[paste("Welfare_weight_NoCollege", i, sep = "")] <- (me_within[i]*Ct_Am_withincMobility[[paste("consumption_val_NoCollege", i, sep = "")]] + 
                                                                         log(Ct_Am_withincMobility[[paste("withincity_amenity_NoCollege", i, sep = "")]]*
                                                                               Ct_Am_withincMobility[[paste("acrosscity_amenity_NoCollege", i, sep = "")]]))*
    Init_eq[[paste("households_NoCollege", i, sep = "")]] #weight with respect to initial equilibirum
  
  Ct_Am_withincMobility[[paste("Welfare_weight_NoCollege", i, sep = "")]][is.nan(Ct_Am_withincMobility[[paste("Welfare_weight_NoCollege", i, sep = "")]])] <- 0 #doesnt matter
  
}

#Summing all welfare weights and differencing
Ct_Am_withincMobility["Welfare_effect"] <- rep(0, nrow(Ct_Am_withincMobility))


for (i in 1:7) {
  Ct_Am_withincMobility$Welfare_effect <- Ct_Am_withincMobility[[paste("Welfare_weight_College", i, sep = "")]] + Ct_Am_withincMobility$Welfare_effect - (Init_eq[[paste("Welfare_weight_College", i, sep = "")]] )
  Ct_Am_withincMobility$Welfare_effect <- Ct_Am_withincMobility[[paste("Welfare_weight_NoCollege", i, sep = "")]] + Ct_Am_withincMobility$Welfare_effect - (Init_eq[[paste("Welfare_weight_NoCollege", i, sep = "")]])
  
}


#Total Welfare
Tot_welfare <- sum(Ct_Amenities$Welfare_effect, na.rm = TRUE)
NoMobility_welfare <- sum(Ct_Am_NoMobility$Welfare_effect, na.rm = TRUE)
withincMobility_welfare <- sum(Ct_Am_withincMobility$Welfare_effect, na.rm = TRUE)

NoMobility_welfare/Tot_welfare #81.8% of welfare gains come from being able to purchase smaller homes. Makes sense. 


#Withincity mobility equilibrium accounts for essentially 100% of welfare gains! That is, across city mobility does not matter for welfare at all! Big result. 
(withincMobility_welfare)/Tot_welfare


#Why do we observe these amenity changes?
Omega <- 1.31
Ct_Amenities["Aggregated_exo_amenities"] <- (Ct_Amenities$wc_Amenity_College1 + Ct_Amenities$wc_Amenity_College2 + 
                                             Ct_Amenities$wc_Amenity_College3 + Ct_Amenities$wc_Amenity_College4 + 
                                             Ct_Amenities$wc_Amenity_College5 + Ct_Amenities$wc_Amenity_College6 +
                                             Ct_Amenities$wc_Amenity_College7 + Ct_Amenities$wc_Amenity_NoCollege1 + Ct_Amenities$wc_Amenity_NoCollege2 + 
                                             Ct_Amenities$wc_Amenity_NoCollege3 + Ct_Amenities$wc_Amenity_NoCollege4 + 
                                             Ct_Amenities$wc_Amenity_NoCollege5 + Ct_Amenities$wc_Amenity_NoCollege6 +
                                             Ct_Amenities$wc_Amenity_NoCollege7)/(Ct_Amenities$implied_hhavgincome^(1.31)) #1.31

Ct_Amenities <- Ct_Amenities %>% group_by(CBSA) %>% mutate(dem_amenities = Aggregated_exo_amenities - mean(Aggregated_exo_amenities, na.rm = TRUE))

ggplot() + 
  geom_smooth(method = 'loess', span=1, data = 
                Ct_Amenities[Ct_Amenities$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], se = FALSE, #SET SE = FALSE FOR RAM ISSUES 
              aes(x=rank_density_CBSA, y=dem_amenities, color = 'Top 25% Counterfactual')) +
  geom_smooth(method = 'loess', span=1, data = 
                Ct_Amenities[Ct_Amenities$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], se = FALSE, #SET SE = FALSE FOR RAM ISSUES
              aes(x=rank_density_CBSA, y=dem_amenities, color = 'Bottom 25% Counterfactual')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Average Exogenous Amenities (demeaned by MSA)")
ggsave("Data/Counterfactuals/CounterfactualDataOutput/Exogenous_Amenities.png", width = 20, height = 12, units = "cm")


#Why do we get welfare gains within cities? What's driving this?
#Is it reduced sprawl in the aggregate? what is it?


  Ct_Amenities["Aggregated_amenity_exposure"] <- (Ct_Amenities$wc_Amenity_College1 + Ct_Amenities$wc_Amenity_College2 + 
                                                 Ct_Amenities$wc_Amenity_College3 + Ct_Amenities$wc_Amenity_College4 + 
                                                 Ct_Amenities$wc_Amenity_College5 + Ct_Amenities$wc_Amenity_College6 +
                                                 Ct_Amenities$wc_Amenity_College7 + Ct_Amenities$wc_Amenity_NoCollege1 + Ct_Amenities$wc_Amenity_NoCollege2 + 
                                                 Ct_Amenities$wc_Amenity_NoCollege3 + Ct_Amenities$wc_Amenity_NoCollege4 + 
                                                 Ct_Amenities$wc_Amenity_NoCollege5 + Ct_Amenities$wc_Amenity_NoCollege6 +
                                                 Ct_Amenities$wc_Amenity_NoCollege7)/(Ct_Amenities$implied_hhavgincome^(1.31))*
                                                 (Ct_Amenities$households_College1 + Ct_Amenities$households_College2 +
                                                  Ct_Amenities$households_College3 + Ct_Amenities$households_College4 + 
                                                  Ct_Amenities$households_College5 + Ct_Amenities$households_College6 + 
                                                  Ct_Amenities$households_College7 + Ct_Amenities$households_NoCollege1 + 
                                                  Ct_Amenities$households_NoCollege2 +
                                                  Ct_Amenities$households_NoCollege3 + Ct_Amenities$households_NoCollege4 + 
                                                  Ct_Amenities$households_NoCollege5 + Ct_Amenities$households_NoCollege6 + 
                                                  Ct_Amenities$households_NoCollege7)
  
  

  
  
Ct_Amenities <- Ct_Amenities %>% group_by(CBSA) %>% mutate(dem_amenities_exp = mean(Aggregated_amenity_exposure, na.rm = TRUE))
  
#Did within-city amenity exposure increase relative to Init_eq?
Init_eq["Aggregated_amenity_exposure"] <- ((Ct_Amenities$wc_Amenity_College1 + Ct_Amenities$wc_Amenity_College2 + 
                                                    Ct_Amenities$wc_Amenity_College3 + Ct_Amenities$wc_Amenity_College4 + 
                                                    Ct_Amenities$wc_Amenity_College5 + Ct_Amenities$wc_Amenity_College6 +
                                                    Ct_Amenities$wc_Amenity_College7 + Ct_Amenities$wc_Amenity_NoCollege1 + Ct_Amenities$wc_Amenity_NoCollege2 + 
                                                    Ct_Amenities$wc_Amenity_NoCollege3 + Ct_Amenities$wc_Amenity_NoCollege4 + 
                                                    Ct_Amenities$wc_Amenity_NoCollege5 + Ct_Amenities$wc_Amenity_NoCollege6 +
                                                    Ct_Amenities$wc_Amenity_NoCollege7)/(Ct_Amenities$implied_hhavgincome^(1.31)))*
                                                   (Main$tot_housing_units_cen_2010)
                                                  #1.31

Init_eq <- Init_eq %>% group_by(CBSA) %>% mutate(dem_amenities_exp = mean(Aggregated_amenity_exposure, na.rm = TRUE))

Ct_Amenities["Exposure_gain"] <- Ct_Amenities$dem_amenities_exp/Init_eq$dem_amenities_exp - 1
Ct_Amenities["Initial_population"] <- Main$tot_housing_units_cen_2010


exposure_df <- Ct_Amenities %>% select(CBSA, CBSA_NAME, Exposure_gain, Initial_population, CBSA_med_house_value)

exposure_df <- collap(exposure_df, Exposure_gain + Initial_population + CBSA_med_house_value ~ CBSA + CBSA_NAME, FUN = list(fsum, fmean))
#Most amenity exposure gains coming from big cities, there must be a net population gain!
popweighted_utility_gain_wc <- sum(exposure_df$fmean.Exposure_gain*exposure_df$fsum.Initial_population)/sum(exposure_df$fsum.Initial_population)

#8% aggregate population weighted gain in utilities!.
cor(exposure_df$fmean.Exposure_gain, exposure_df$fsum.Initial_population)
cor(exposure_df$fmean.Exposure_gain, exposure_df$fmean.CBSA_med_house_value) #37% positive correlation between population and amenity gains/house value 
                                                                          #holds true for prices as well!

#Most of the amenity value gains must be coming from within city movements. Replace this with within city movement equilibria.

Ct_Am_withincMobility <- Ct_Am_withincMobility %>% mutate(implied_hhavgincome = ((households_College1*ability_grp1 + households_College2*ability_grp2 +
                                                                      households_College3*ability_grp3 + households_College4*ability_grp4 + 
                                                                      households_College5*ability_grp5 + households_College6*ability_grp6 + 
                                                                      households_College7*ability_grp7)*CollegeWage +
                                                                     
                                                                     (households_NoCollege1*ability_grp1 + households_NoCollege2*ability_grp2 +
                                                                        households_NoCollege3*ability_grp3 + households_NoCollege4*ability_grp4 + 
                                                                        households_NoCollege5*ability_grp5 + households_NoCollege6*ability_grp6 + 
                                                                        households_NoCollege7*ability_grp7)*NoCollegeWage)/
                                              
                                                                      (households_College1 + households_College2 +
                                                 households_College3 + households_College4 + 
                                                 households_College5 + households_College6 + 
                                                 households_College7 +
                                                 households_NoCollege1 + households_NoCollege2 +
                                                 households_NoCollege3 + households_NoCollege4 + 
                                                 households_NoCollege5 + households_NoCollege6 + 
                                                 households_NoCollege7))



Ct_Am_withincMobility["Aggregated_amenity_exposure"] <- ((Ct_Am_withincMobility$wc_Amenity_College1 + Ct_Am_withincMobility$wc_Amenity_College2 + 
                                                  Ct_Am_withincMobility$wc_Amenity_College3 + Ct_Am_withincMobility$wc_Amenity_College4 + 
                                                  Ct_Am_withincMobility$wc_Amenity_College5 + Ct_Am_withincMobility$wc_Amenity_College6 +
                                                  Ct_Am_withincMobility$wc_Amenity_College7 + Ct_Am_withincMobility$wc_Amenity_NoCollege1 + Ct_Am_withincMobility$wc_Amenity_NoCollege2 + 
                                                  Ct_Am_withincMobility$wc_Amenity_NoCollege3 + Ct_Am_withincMobility$wc_Amenity_NoCollege4 + 
                                                  Ct_Am_withincMobility$wc_Amenity_NoCollege5 + Ct_Am_withincMobility$wc_Amenity_NoCollege6 +
                                                  Ct_Am_withincMobility$wc_Amenity_NoCollege7)/(Ct_Am_withincMobility$implied_hhavgincome^(1.31)))*
                                                  (Ct_Am_withincMobility$households_College1 + Ct_Am_withincMobility$households_College2 +
                                                   Ct_Am_withincMobility$households_College3 + Ct_Am_withincMobility$households_College4 + 
                                                   Ct_Am_withincMobility$households_College5 + Ct_Am_withincMobility$households_College6 + 
                                                   Ct_Am_withincMobility$households_College7 + Ct_Am_withincMobility$households_NoCollege1 + 
                                                   Ct_Am_withincMobility$households_NoCollege2 +
                                                   Ct_Am_withincMobility$households_NoCollege3 + Ct_Am_withincMobility$households_NoCollege4 + 
                                                   Ct_Am_withincMobility$households_NoCollege5 + Ct_Am_withincMobility$households_NoCollege6 + 
                                                   Ct_Am_withincMobility$households_NoCollege7)


Ct_Am_withincMobility <- Ct_Am_withincMobility %>% group_by(CBSA) %>% mutate(dem_amenities_exp = mean(Aggregated_amenity_exposure, na.rm = TRUE))
Ct_Am_withincMobility["Exposure_gain"] <- Ct_Am_withincMobility$dem_amenities_exp/Init_eq$dem_amenities_exp - 1
Ct_Am_withincMobility["Initial_population"] <- Main$tot_housing_units_cen_2010

exposure_df <- Ct_Am_withincMobility %>% select(CBSA, CBSA_NAME, Exposure_gain, Initial_population)

exposure_df <- collap(exposure_df, Exposure_gain + Initial_population ~ CBSA + CBSA_NAME, FUN = list(fsum, fmean))
#Most amenity exposure gains coming from small cities instead.. this is interesting? 
popweighted_utility_gain_wc_Nomobility <- sum(exposure_df$fmean.Exposure_gain*exposure_df$fsum.Initial_population)/sum(exposure_df$fsum.Initial_population)

#8% aggregate population weighted gain in utilities!
cor(exposure_df$fmean.Exposure_gain, exposure_df$fsum.Initial_population) #makes sense that most of the gains are coming from low population cities

#
