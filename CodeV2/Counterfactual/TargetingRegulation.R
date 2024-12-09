#This file figures out the welfare effects of policies that better target the neighborhood income distribution. 


#Packages
library(dplyr)
library(haven)
library(ggplot2)
library(collapse)
library(labelled)
library(readr)
library(rlang)
library(compiler)
library(doParallel)

searchPolicySpace <- FALSE

#FILEPATH FOR SOLUTION .R FILE
source("CodeV2/Counterfactual/Functions/Solve_Current_Equilibrium_allSpecs_vectorized.R")
source("CodeV2/Calibrate/Parameters/GlobalParameters.R") #Get regulation censoring parameter here. 

#PARAMETER GUI__________________________________________________________________
EquilibriumType <- list() #List to read which equilibrium to solve for 

#Specify partial deregulation exercise, uses slower algorithm
EquilibriumType["Partial_Dereg"] <- TRUE
EquilibriumType["SocialOpt_gridSearch"] <- TRUE
check_init_eq <- 0 #Must exist in memory, only for debugging

#MUTUALLY EXCLUSIVE PARAMETERS LIMITING MOBILITY
EquilibriumType["Full"] <- TRUE #Set to true/1 to solve for full counterfactual
EquilibriumType["WithinCityMobility"] <- FALSE
EquilibriumType["NoMobility"] <- FALSE


#Set to TRUE if we are to use bySkill version of equilibrium.
EquilibriumType["bySkill"] <- FALSE #BySkill
EquilibriumType["StoneGeary"] <- TRUE #SG preferences
EquilibriumType["EndogenousAmenities"] <- TRUE 
EquilibriumType["EndogenousProductivity"] <- FALSE #Baseline, no endogenous productivity
EquilibriumType["NoFundamentals"] <- FALSE #use observed fundamental amenities


if (EquilibriumType$StoneGeary == FALSE | EquilibriumType$bySkill == TRUE) {
  error("Won't work for non-baseline objects right now")
}


  #Name vectors for counterfactual
  F1 <- "1"


  #Import initial equilibrium to create vector of unit density restrictions
  uDR_df <- read_dta(paste0("DataV2/Counterfactuals/Master_post_calibration_bySkill", EquilibriumType$bySkill, "_pref_", "SG", "_amenities.dta")) %>%
            select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, IncomeStringency_model_rents, regulated_housingUnit_share, PooledWage,
                   land_regulated, land_unregulated, rank_density_CBSA)

  #Importing income data of neighborhoods absent regulation 
  load(paste0("DataV2/Counterfactuals/Counterfactual_Output/FullDeregulation/", "Eq_Objects_FULL", 
              "_EndoAmen_", TRUE, 
              "_EndoProd_", FALSE,
              "_bySkill_",  FALSE,
              "_pref_SG", ".RData"))
  uDR_df$Income_vector <- Equilibrium_objects_output$Avg_income #Average income after complete deregulation
  uDR_df$Skill_vector <- Equilibrium_objects_output$Avg_income/Equilibrium_objects_output$PooledWage

  # Create "Amenity score": which is just average neighborhood income assuming amenity value shares are population shares
  # Measures relative fundamental amenity value for rich, which we use in this file to target regulation
  
  #Total exogenous amenity values...
  Equilibrium_objects_output["tot_amenity_value"] <- rep(0, nrow(Equilibrium_objects_output))
  for (incomeType in 1:7) {
    Equilibrium_objects_output["tot_amenity_value"] <- Equilibrium_objects_output[["tot_amenity_value"]] + Equilibrium_objects_output[[paste0("exogenous_Amenity_", incomeType)]]
    
  }
  
  # Expected exogenous amenity score
  uDR_df["exogenousAmenity_score"] <- rep(0, nrow(Equilibrium_objects_output))
  for (incomeType in 1:7) {
    uDR_df["exogenousAmenity_score"] <-    uDR_df[["exogenousAmenity_score"]] + (Equilibrium_objects_output[[paste0("exogenous_Amenity_", incomeType)]]*
                                                                                                                    Equilibrium_objects_output[[paste0("ability_grp", incomeType)]]/
                                                                                                                    Equilibrium_objects_output[["tot_amenity_value"]])
  
  }
  rm(Equilibrium_objects_output)
  
  
  #Checking correlation between income and exogenous amenity score
  print(summary(lm(log(uDR_df$Income_vector) ~ log(uDR_df[["exogenousAmenity_score"]]))))
  
                #Note: this is because of non-homothetic preferences -- other reasons for income sorting absent regulation
  print(summary(lm(log(IncomeStringency_model_rents*regulated_housingUnit_share) ~ log(exogenousAmenity_score),
                   data = uDR_df[uDR_df$IncomeStringency_model_rents*uDR_df$regulated_housingUnit_share > 0,]))) 
  #only 7% R^2, pretty large, all things considered--but noisy.

  #Now, permute regulation across space so that it retains same spatial distribution
  
  #Sort uDR by exogenous amenity score
  uDR_df_sorted <- uDR_df %>% arrange(exogenousAmenity_score) 
  #can also do amenity score vector, slightly different results 
  
  #test amenity score variation against income variation at baseline
  print(paste0("Variation in the exogenous amenity score is ", sd(uDR_df$exogenousAmenity_score)))
  
  #Create regulation levels deflated by city productivity
  uDR_df<- uDR_df %>% mutate(Deflated_regulation = IncomeStringency_model_rents/PooledWage)
  
  #Next, permuting share of land for regulated structures
  uDR_df[["total_land"]] <- uDR_df$land_regulated + uDR_df$land_unregulated
  uDR_df[["land_share_reg"]] <- uDR_df$land_regulated/uDR_df$total_land
  
  #Now, sort by city-productivity deflated regulation:
  uDR_df_sorted["IncomeStringency_counterfactual"] <- as.vector( select(arrange(uDR_df, Deflated_regulation), IncomeStringency_model_rents) )
  
  #Creating a sorted share of land in regulated structures
  uDR_df_sorted["RegShare_counterfactual"] <- as.vector( select( arrange(uDR_df, land_share_reg), land_share_reg) )
  
  #Merging back to uDR_df to retain original order to pass to equilibrium solver
  uDR_df <- left_join(uDR_df, select(uDR_df_sorted, State, County, Tract, BlockGroup, IncomeStringency_counterfactual, RegShare_counterfactual),
                      by = c("State", "County", "Tract", "BlockGroup"))
  
  #Creating new land allocations holding total_land in block group fixed (cannot create new land from policy)
  uDR_df$land_regulated_counterfactual <- uDR_df$RegShare_counterfactual*uDR_df$total_land
  uDR_df$land_unregulated_counterfactual <- (1 - uDR_df$RegShare_counterfactual)*uDR_df$total_land
  rm(uDR_df_sorted)
  
  #Saving permuted regulation levels
  write_dta(uDR_df, "DataV2/Counterfactuals/Counterfactual_Output/OptimalPolicy/Permuted_regulation.dta")
 
   CityCollapse <- collap(uDR_df, IncomeStringency_model_rents + regulated_housingUnit_share + exogenousAmenity_score + 
                            IncomeStringency_counterfactual + RegShare_counterfactual + PooledWage + Income_vector ~ CBSA + CBSA_NAME, 
                          FUN = fmedian) #take fmedian for outliers in city; else results nonsensical due to outlier senstitivity of the mean
  
    #Create across + within city variation in this exogenous amenity score
     ggplot() + geom_smooth(data = CityCollapse, aes(x=PooledWage, y=(exogenousAmenity_score - mean(exogenousAmenity_score, na.rm = TRUE))/sd(exogenousAmenity_score, na.rm = TRUE)) ) + 
                geom_point(data = CityCollapse, size = 0.5, alpha = 0.45, 
                   aes(x = PooledWage, y = (exogenousAmenity_score - mean(exogenousAmenity_score, na.rm = TRUE))/sd(exogenousAmenity_score, na.rm = TRUE), label = CBSA_NAME)) +
                 geom_text(data = CityCollapse[CityCollapse$PooledWage > 1.1,], check_overlap = T, size = 3.5,
                           aes(x = PooledWage, y = (exogenousAmenity_score - mean(exogenousAmenity_score, na.rm = TRUE))/sd(exogenousAmenity_score, na.rm = TRUE), label = CBSA_NAME)) + 
       theme_gray(base_size = 15) + 
                ylab("Exogenous Amenity Score (standardized)") + 
                xlab("Productivity") 
     ggsave("DataV2/Counterfactuals/Counterfactual_Output/OptimalPolicy/Permuted_city_amenity_score.png", width = 25, height = 15, units = "cm")
     
    #Plus, create MSA demeaned amenity score, regress on density quantiles at initial equilibrium
     uDR_df <- uDR_df %>% group_by(CBSA) %>% mutate(demeaned_exo_score = exogenousAmenity_score - mean(exogenousAmenity_score, na.rm = TRUE))
     
    #Plot standardized scores 
     ggplot() + 
       
       geom_smooth(data = uDR_df[uDR_df$PooledWage > 1.05,], 
                            aes(x=rank_density_CBSA, 
                                y=(exogenousAmenity_score - mean(exogenousAmenity_score, na.rm = TRUE))/sd(exogenousAmenity_score, na.rm = TRUE), color = "Bottom 75% Productivity")) + 
       
       geom_smooth(data = uDR_df[uDR_df$PooledWage <= 1.05,], 
                   aes(x=rank_density_CBSA, 
                       y=(exogenousAmenity_score - mean(exogenousAmenity_score, na.rm = TRUE))/sd(exogenousAmenity_score, na.rm = TRUE), color = "Top 25% Productivity") ) + 
       scale_colour_manual(name="Sample", values = c("blue","red")) + 
       theme_gray(base_size = 15) + 
       ylab("Exogenous Amenity Score (standardized)") + 
       xlab("Density quantiles (block group level)") 
     
     #This is good enough, differences should not be exemplified
     
     ggplot() + geom_smooth(data = uDR_df, 
                           aes(x=rank_density_CBSA, 
                               y=(exogenousAmenity_score - mean(exogenousAmenity_score, na.rm = TRUE))/sd(exogenousAmenity_score, na.rm = TRUE))) +
       theme_gray(base_size = 15) + 
       ylab("Exogenous Amenity Score standardized, \n (demeaned by MSA)") + 
       xlab("Density quantiles (Block Group Level)") 
     ggsave("DataV2/Counterfactuals/Counterfactual_Output/OptimalPolicy/Within_city_amenity_score.png", width = 25, height = 15, units = "cm") 
     
   
    
   


# #################################################################################################################################
# ############## Solve equilibrium for permuted regulation ########################################################################
# #################################################################################################################################
    
  F2 <- "TargetFundAmenity"
  
  #Creating counterfactual parameter vector to pass to file, as well as new shares in regulated structures
  IncomeStringency_ctfl <- uDR_df$IncomeStringency_counterfactual
  
  # Creating object with new land amounts. If this object is in memory, new land is allocated
   land_regulated_ctfl <- uDR_df$land_regulated_counterfactual  #THIS CHANGES land in ctfl if in memory!
   land_unregulated_ctfl <- uDR_df$land_unregulated_counterfactual
  
  
  
  solveEquilibrium() #Call equilibrium solution
  
  
  #Doing the same with no endogenous amenities
  F2 <- "TargetFundAmenity_noEndoAmen"
  
  
  EquilibriumType["EndogenousAmenities"] <- FALSE
  
  solveEquilibrium() #Call equilibrium solution
    
  
#########################################################################################################################
### Search over space of policies as follows...
###   Set MLS to zero in different deciles untill 
#########################################################################################################################
  
if (searchPolicySpace == TRUE) {
    
  EquilibriumType["EndogenousAmenities"] <- TRUE
  land_regulated_ctfl <- uDR_df$land_regulated_counterfactual  #THIS CHANGES land in ctfl if in memory!
  land_unregulated_ctfl <- uDR_df$land_unregulated_counterfactual
  
  F1_grid = c(0.25, 0.5, 1) 
    
  F2_grid = c(1, 1.5, 1.75, 1.85, 1.95, 2, 2.1)   #increase dispersion of regulation to better target rich neighborhoods
  #No need to calculate F1 == 0 for all values of F2, as this is just complete deregulation
  
  totalGrid <- expand.grid(F1_grid, F2_grid) %>% setNames(c("F1", "F2"))
  totalGrid <- rbind(totalGrid, c(0, 1)) #add complete deregulation on at the end
  
  
  #Saving grid 
  saveRDS(totalGrid, file = "DataV2/Counterfactuals/Counterfactual_Output/OptimalPolicy/grid_search.RData")
  
  #Do this on multicore process
  ncores  <- min(nrow(totalGrid), 7, detectCores() - 1)
  registerDoParallel(ncores)
  
  foreach(row = 1:(nrow(totalGrid)) ) %dopar% { 
    
    F1 <- totalGrid$F1[row] #load for file names
    F2 <- totalGrid$F2[row]
    
    
    #Creating counterfactual parameter vector
    IncomeStringency_ctfl <- F1*(uDR_df$IncomeStringency_counterfactual^(F2))*( mean(uDR_df$IncomeStringency_counterfactual)/mean(uDR_df$IncomeStringency_counterfactual^(F2)) ) 
    
    #Control new distribution as isoelastic function of old one. 
    #Rescale by relative means so F2 does not affect mean regulation, just its dispersion.
    
    
    solveEquilibrium() #Call equilibrium solution
    
    return(NA)
    
    
    
  }

  stopImplicitCluster()
}
  
  
  rm(list = ls())
