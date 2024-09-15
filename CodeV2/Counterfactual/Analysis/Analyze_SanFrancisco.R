#This file analyzes halving the value of a minimal lot in San Francisco.
#Produces 1) Maps, 
#         2) Welfare calculations
#         3) Labour productivity calculations
#         4) Analyzing spatial distribution of income and land values on a map

library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)
library(ggplot2)
library(forcats)
library(sf)
library(haven) #Reading stata.dta files 
sf_use_s2(FALSE) #Switching off spherical geometry
library(here)
library(purrr)
library(collapse)
library(viridis) #for colors
library(patchwork) #combining plots

#PARAMETERS
source("CodeV2/Counterfactual/Parameters/GlobalParameters.R")

#FUNCTIONS
source("CodeV2/Counterfactual/Functions/Analysis_Functions.R")
source("CodeV2/Counterfactual/Functions/OpenStreetMapping_Functions.R")


#_________________ PRELIMINARIES _________________________#
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

#Importing all files from Counterfactual_Output 
load(paste0("DataV2/Counterfactuals/Counterfactual_Output/PartialDeregulation/", "SanFrancisco_ReZoning_Full", 
            "_EndoAmen_", TRUE, 
            "_EndoProd_", FALSE,
            "_bySkill_", BASELINE_SPECIFICATION$bySkill_to_pass,
            "_pref_", BASELINE_SPECIFICATION$pref, ".RData"))
Ct_Amenities <- Equilibrium_objects_output
rm(Equilibrium_objects_output)

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
Init_eq["final_land_for_res"] <- Init_eq$land_regulated + Init_eq$land_unregulated #calibrated land
Ct_Amenities["ALAND"] <- Init_eq$ALAND #official landmass from census shapefiles

#______________________________________________________________________________________________________
# PART 1: LAND VALUES. CALCULATE San Francisco changes in land values
#______________________________________________________________________________________________________
#Replace housing price z1 and z2 equal to the other IF only regulated or unregulated land, this does not matter for calculations other to remove NaNs
  Ct_Amenities$housingPrice_z1[is.nan(Ct_Amenities$housingPrice_z1)] <- Ct_Amenities$housingPrice_z2[is.nan(Ct_Amenities$housingPrice_z1)] 
  Ct_Amenities$housingPrice_z2[is.nan(Ct_Amenities$housingPrice_z2)] <- Ct_Amenities$housingPrice_z1[is.nan(Ct_Amenities$housingPrice_z2)] 

#Additional variables to feed into EquivalentVariation.R
  Ct_Amenities["price_regulated"] <- Ct_Amenities$housingPrice_z1
  Ct_Amenities["price_unregulated"] <- Ct_Amenities$housingPrice_z2
  Ct_Amenities["regulated_housingUnit_share"] <- Init_eq$regulated_housingUnit_share #shares at initial equilibrium
  Ct_Amenities["IncomeStringency_model_rents"] <- ifelse(test = (Ct_Amenities$CBSA_NAME == "San Francisco-Oakland-Hayward, CA"),       #Cutting value of minimal lot in 2 -- a bit more than eliminating SF zoning
                                                         yes = 1/2,                                                    # 
                                                         no = 1)*Init_eq$IncomeStringency_model_rents #NOTE: IF YOU CHANGE COUNTERFACTUAL YOU !MUST MUST MUST! CHANGE THIS

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
print(paste0("The change in land values in the US from the counterfactual is ", growthRate_landval*100, " percent.")) #US wide, land values increase by 0.44907 percent. 

#Only in San Francisco
growthRate_landval <- weighted.mean(exp(Init_eq[Init_eq$CBSA_NAME == "San Francisco-Oakland-Hayward, CA",]$LandValGrowth), 
                                    w = Init_eq[Init_eq$CBSA_NAME == "San Francisco-Oakland-Hayward, CA",]$Total_land_values_initial) - 1
print(paste0("The change in land values in SF only from the counterfactual is ", growthRate_landval*100, " percent.")) #In SF, land values drop by (only) 2 percent location-wide.
print("This masks significant spatial heterogeneity")

#Creating san francisco county dataframe
Init_eq["Avg_income_ctfl"] <- Ct_Amenities$Avg_income #counterfactual incomes
Init_eq["Init_nPop"] <- getNeighborhoodPop(Init_eq) #Initial neighborhood populations
Init_eq["Ct_nPop"] <- getNeighborhoodPop(Ct_Amenities) #Initial neighborhood populations 

SF_df <- Init_eq[Init_eq$CBSA_NAME == "San Francisco-Oakland-Hayward, CA",] %>% #Take dataframe for SF-Oakland-Hayward
         select(LandValGrowth, IncomeStringency_cl, starts_with("Avg_income"), ends_with("nPop"), UnitDensityRestriction_cl, land_regulated, final_land_for_res, starts_with("Amenity"), starts_with("consumption"),
               regulated_housingUnit_share, LandValueDensity_matched, IncomeStringency_model_rents, rank_density_CBSA, starts_with("Population_type"), State, County, Tract, BlockGroup, CBSA_NAME)


#_________________________________________________________________________________________________________




#_________________________________________________________________________________________________________
# PART 2: THE WELFARE OF RENTERS: 
#_________________________________________________________________________________________________________
#  #Equivalent variation
source("CodeV2/Counterfactual/Functions/EquivalentVariation.R")
var_Amen <- matrix(NA, length(skillVector) , 7)

  skillIndex <- 0
  for (skill_to_pass in skillVector) {
    skillIndex <- skillIndex + 1
    for (i in 1:7) { 
        
        new_welfare <- GetWelfareEqVar(Master_data = Ct_Amenities, EqVar = 1, FullDereg = FALSE, capitalGains = FALSE, #This call just calculates welfare in utils in ctfl
                                       skill = skill_to_pass, incomeType = i, demandParameters = demandParameters_to_pass)
        var_Amen[skillIndex, i] <- getVariation(Init = Init_eq, Welfare = new_welfare,
                                                incomeType = i, skill = skill_to_pass, #partial deregulation calculation...
                                                demandParameters = demandParameters_to_pass)
      
    
    }
  }
  
  print(paste0("Welfare by income type from this deregulation exercise is... "))
  print(var_Amen) 
  
  
  #Shapely decomposition
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
  
  print(Amenity_shapely)
  print(Consumption_shapely) #Consumption values AND amenity values decrease for pretty much everyone. (Segregation issues)
  
  
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
  
  #Total welfare effects by component
  total_population <- matrix(NA, length(skillVector), 7)
  skillIndex <- 0
  for (skill_to_pass in skillVector) {
    name_of_skill <- skillName[which(skill_to_pass == skillVector)]
    skillIndex <- skillIndex + 1
    for (i in 1:7) {
      total_population[skillIndex, i] <- Ct_Amenities[[paste0("Total_Population_type_", name_of_skill, i)]][1]
    }
  }
  total_Consumption_shapely <- (Consumption_shapely%*%t(total_population))/(sum(total_population))
  total_Amenity_shapely <- (Amenity_shapely%*%t(total_population))/(sum(total_population))
  
  total_var_amen <- (var_Amen%*%t(total_population))/(sum(total_population))
  
  #Printing aggregate outcomes to console
  print("Ag consumption component")
  print(total_Consumption_shapely)
  print("Ag Amenity component")
  print(total_Amenity_shapely)
  print("Ag equivalent variation")
  print(total_var_amen)
  
#_________________________________________________________________________________________________________

  
#_________________________________________________________________________________________________________
# PART 3: LABOUR PRODUCTIVITY AND COMPOSITION OF INFLOWERS________________________________________________
#_________________________________________________________________________________________________________
#Checking changes in aggregate labour productivity across equilibria
print(paste0("The difference in labour productivity after deregulating San Francisco is ",
             100*((getAggregateProductivity(Ct_Amenities) - getAggregateProductivity(Init_eq))/ 
                    getAggregateProductivity(Init_eq)), " percent.")) #only 0.03 percent!

#What would labour productivity be if no income sorting occured?
#Change in agg labour productivity = pop growth weighted by output shares (this is assuming everyone makes the same income)
#Setting 
  AcrossCityAnalysis <- Init_eq %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, IncomeStringency_cl, LandValGrowth) 
  #Delta Average types, populations
  AcrossCityAnalysis["pDelta_AvgType"] <- 100*(getCityAverageType(Ct_Amenities)/getCityAverageType(Init_eq) - 1)
  AcrossCityAnalysis["pDelta_Pop"] <- 100*(getCityTotalPop(Ct_Amenities)/getCityTotalPop(Init_eq) - 1)
  #collapsing at city level
  AcrossCityAnalysis <- collap(AcrossCityAnalysis, pDelta_AvgType + pDelta_Pop + IncomeStringency_cl + LandValGrowth ~ CBSA + CBSA_NAME)

  #Getting output shares in initial equilibrium
  output_shares <- data.frame(getCityOutputShares(Init_eq))
  colnames(output_shares) <- c("CBSA_NAME", "OutputShares")

  AcrossCityAnalysis <- left_join(AcrossCityAnalysis, output_shares, by = c("CBSA_NAME"))
  
  #Taking population flows holding income composition fixed, effect on agg productivity is PopFlow(%)*Output Share in initial equilibrium
  LabProdGrowth_noincomeSorting <- sum(((AcrossCityAnalysis$pDelta_Pop + 100)*as.numeric(AcrossCityAnalysis$OutputShares))) - 100 #4x higher 
  print(paste0("Aggregate productivity growth would have been ", LabProdGrowth_noincomeSorting, " percent if there was no income sorting.")) #would have been 0.171 percent if higher, san francisco grows by 12% in equilibrium, 
                                                                                                                                             #but average income falls by 6% in SF
#_________________________________________________________________________________________________________
  
  
#__________________________________________________________________________________________________________
# Part 4: Maps showing changes in income, initial regulation, etc__________________________________________
#__________________________________________________________________________________________________________
#   #Getting bounding box for the geometry of SF
  
  #Reading raw block geometry
  blkgeo <- st_read("DataV2/US_Data/Shapefiles/US_blck_grp_2020.shp") %>% 
                    mutate(State = as.numeric(STATEFP), County = as.numeric(COUNTYFP), Tract = as.numeric(TRACTCE),
                           BlockGroup = as.numeric(BLKGRPCE)) %>%
                           select(State, County, Tract, BlockGroup)
  
  SF_df <- inner_join(blkgeo, SF_df, by = c("State", "County", "Tract", "BlockGroup"))
  rm(blkgeo)
  SF_df <- st_transform(SF_df, 4326) #to lat/lon coordinates system
  
  SanFran_bbox <- st_bbox(SF_df) #getting bounding box for SF for plot
  
  #Setting IncomeStringency_cl to zero if NA (for graph, this was imputed missing for Merge_stringency.R)
  SF_df$IncomeStringency_cl[is.na(SF_df$IncomeStringency_cl)] <- 0
  
  #Getting arguments for map
  args <- createMappingArgs(boundingBox = SanFran_bbox,
                            additionalZoom = 2)
  
  #Bin each outcome by quantiles for better viewing of the map: 
  SF_df <-  SF_df %>% mutate(Avg_income_quartiles = cut(Avg_income/1000, breaks = unique(quantile(Avg_income/1000, seq(0, 1, 0.25))), 
                                                        label = FALSE),
                             IncomeStringency_cl_quartiles = cut(IncomeStringency_cl/1000000, breaks = unique(quantile(IncomeStringency_cl/1000000, seq(0, 1, 0.25))), 
                                                                 label = FALSE),
                             Income_change_quartiles = cut(100*((Avg_income_ctfl/Avg_income) - 1), breaks = unique(quantile(100*((Avg_income_ctfl/Avg_income) - 1), seq(0, 1, 0.1))), 
                                                           label = FALSE),
                             LandVal_change_quartiles = cut(100*(exp(LandValGrowth) - 1), breaks = unique(quantile(100*(exp(LandValGrowth) - 1), seq(0, 1, 0.1))), 
                                                            label = FALSE)
                        )
  
  SF_df$Avg_income_quartiles[is.na(SF_df$Avg_income_quartiles)] <- 1 #Set unregulated locations for first quartile.
  SF_df$IncomeStringency_cl_quartiles[is.na(SF_df$IncomeStringency_cl_quartiles)] <- 1 #Set unregulated locations for first quartile.
  SF_df$Income_change_quartiles[is.na(SF_df$Income_change_quartiles)] <- 1 #Set unregulated locations for first quartile.
  SF_df$LandVal_change_quartiles[is.na(SF_df$LandVal_change_quartiles)] <- 1 #Set unregulated locations for first quartile.
  
  
  
  #Creating bins
  SF_df <- SF_df %>% group_by(Avg_income_quartiles) %>% mutate(Avg_income_quartiles_center = round(mean(Avg_income/1000), 2)) %>%
                     group_by(IncomeStringency_cl_quartiles) %>% mutate(IncomeStringency_cl_quartiles_center = round(mean(IncomeStringency_cl/1000000), 2)) %>%
                     group_by(Income_change_quartiles) %>% mutate(Income_change_quartiles_center = round(mean(100*((Avg_income_ctfl/Avg_income) - 1)), 2)) %>%
                     group_by(LandVal_change_quartiles) %>% mutate(LandVal_change_quartiles_center = round(mean(100*(exp(LandValGrowth) - 1)), 2))
  
  
  #PLOTTING ALL OUTPUT MAPS
InitIncome_plot <-  ggplot() + coord_sf() +
    pmap(args, annotation_raster, interpolate = TRUE) + 
    geom_sf(data = SF_df,
            mapping = aes(fill = Avg_income_quartiles_center), 
            alpha = 0.6, lwd = 0) +
    coord_sf(expand = FALSE) + 
            scale_fill_gradientn(colours=c("orange", "steelblue"), name = "") +    
    theme_gray(base_size = 20) +
    theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + 
    ggtitle(paste0("Panel A:", "\n", "Income distribution (in 1000's, pre-deregulation)")) 
  ggsave("DataV2/Counterfactuals/Counterfactual_Output/PartialDeregulation/SF_NeighborhoodIncome_map.png", plot = InitIncome_plot,  
         width = 35, height = 21.875, units = "cm", type = "cairo") 
  
  #Change in incomes to counterfactual
ChIncome_plot <- ggplot() + coord_sf() +
    pmap(args, annotation_raster, interpolate = TRUE) + 
    geom_sf(data = SF_df,
            mapping = aes(fill = Income_change_quartiles_center), alpha = 0.6, lwd = 0) + 
    coord_sf(expand = FALSE) + 
    scale_fill_gradientn(colours= c("dodgerblue3", "firebrick1"), name = "") + 
    ggtitle(paste0("Panel C:", "\n", "Changes in income to counterfactual (%)")) + 
  theme_gray(base_size = 20) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 
  ggsave("DataV2/Counterfactuals/Counterfactual_Output/PartialDeregulation/SF_NeighborhoodIncome_change_map.png", plot = ChIncome_plot,  
         width = 35, height = 21.875, units = "cm", type = "cairo") 
  
  InitString_plot <-  ggplot() + coord_sf() +
    pmap(args, annotation_raster, interpolate = TRUE) + 
    geom_sf(data = SF_df, #censor for graph scale, one outlier where land values change a lot
            mapping = aes(fill = IncomeStringency_cl_quartiles_center), alpha = 0.6, lwd = 0) +
    coord_sf(expand = FALSE) + 
    scale_fill_gradientn(colours= c("orange", "steelblue"), name = "") + 
    ggtitle(paste0("Panel B:", "\n", "Regulatory stringency (in millions USD, pre-deregulation)")) + 
    theme_gray(base_size = 20) + 
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 12),
          legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 
  ggsave("DataV2/Counterfactuals/Counterfactual_Output/PartialDeregulation/SF_stringency_equilibrium.png", plot = InitString_plot, 
         width = 35, height = 21.875, units = "cm", type = "cairo") 
  
  
  #Change in Land Values to counterfactual
ChLandVal_plot <-  ggplot() + coord_sf() +
    pmap(args, annotation_raster, interpolate = TRUE) + 
    geom_sf(data = SF_df, #censor for graph scale, one outlier where land values change a lot (because its a high density neighborhood with very stringent regulation)
            mapping = aes(fill = LandVal_change_quartiles_center), alpha = 0.6, lwd = 0) + 
    coord_sf(expand = FALSE) + 
    scale_fill_gradientn(colours=c("dodgerblue3", "firebrick1"), name = "") + 
    ggtitle(paste0("Panel D:", "\n", "Changes in land values to counterfactual (%)")) + 
  theme_gray(base_size = 20) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 
  ggsave("DataV2/Counterfactuals/Counterfactual_Output/PartialDeregulation/SF_NeighborhoodLandVal_change_map.png", plot = ChLandVal_plot, 
         width = 35, height = 21.875, units = "cm", type = "cairo") 
  

  #Putting plots into patchwork graph, increasing size of font
  InitIncome_plot + InitString_plot + ChIncome_plot + ChLandVal_plot 
  ggsave("DataV2/Counterfactuals/Counterfactual_Output/PartialDeregulation/SF_combined.png",  
         width = 50, height = 50, units = "cm", type = "cairo") 
  
#_________________   _________________________________________________________________________________________  