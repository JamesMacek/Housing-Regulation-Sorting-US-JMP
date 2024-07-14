library(sf)
library(haven) #Reading stata.dta files 
library(dplyr)
library(readr)
library(collapse)
library(stringr)
library(labelled)
library(ggplot2)
library(estimatr)
library(rlang)


#Date created: December 17th, 2022
#Merges measures of lot size stringency for use with the facts and other uses in different programs. 
#Requires output from NaNDA_Construct.R and assign_Regulation.do

###_________PARAMETERS_________########
CleanLotSize <- 2 #parameter for which to censor minimum lot size calculation as being too large. 
                  #Facts are robust to large range of these parameters, except setting CleanLotSize == Inf.
                  #This is because sample will contain insanely large minimum lot sizes where the algorithm fails to capture regulation.

#PART 1:______________________________________________________________________ 
#Choose MAIN CLUSTERING DEFINITION from analysis__________(THIS IS A PARAMETER)
#This choice comes from the validate_Regulation.R results, 
#where we choose these hyperparameters to minimize error between hand-collected minimum lot size data and our estimates.
Main_alpha <- "0.75"
Main_minClust <- "5"
Main_municipality <- "CORELOGIC"
#______________________________________________________________________________

#Loading facts dataset.
load(file = "DataV2/US_Data/Output/Constructed_Block.Rdata")
US_BLOCK$State <- as.double(US_BLOCK$State)
US_BLOCK$County <- as.double(US_BLOCK$County)
US_BLOCK$Tract <- as.double(US_BLOCK$Tract)
US_BLOCK$BlockGroup <- as.double(US_BLOCK$BlockGroup) #converting to numeric


#Importing all clustering definitions in from our dataset. 
#Listing files
filelist <- list.files("DataV2/CoreLogic/Output")
e.filelist <- filelist[which(grepl("Regulation_measurements", filelist, fixed = TRUE) == TRUE)] #extracting desired files

#Importing all clustering definitions 
Regulation <- list() #list to contain all datasets.

for (file in e.filelist) {
  Regulation <- append(list(read_dta(paste0("DataV2/CoreLogic/Output/", file))), Regulation) #Note: clustering definition appears in order as in e.filelist.
}


 
#Loading: main (desired) clustering definition.

Main_regulation_index <- which(e.filelist == paste0("Regulation_measurements_alpha_",
                                                    Main_alpha, "_minClust", Main_minClust, "_", Main_municipality,
                                                    ".dta") ) 

Main_regulation <- Regulation[[Main_regulation_index]]

Main_regulation <- Main_regulation %>% dplyr::select(State, County, Tract, BlockGroup, CBSA, Assigned_municipality,
                                                     Assigned_geocoded_municipality, Assigned_zoningcode, ZoningDistrictID, UnitDensityRestriction, starts_with("LandValueDensity"), 
                                                     IncomeStringency, IncomeStringency_preCovid, regulated_housingUnit_share, MedLotSize_minimum_struct, ends_with("eLotSize"))



#Note: there are some selection issues here that come from the CoreLogic coverage vs the entire sample of 196,204 block groups. 
US_BLOCK <- left_join(US_BLOCK, Main_regulation, by = c("State", "County", "Tract", "BlockGroup", "CBSA"))

#setting some observations to 0 that have the measured mode above x% of the average lot size 
print(paste0("There are ", nrow(US_BLOCK[US_BLOCK$UnitDensityRestriction > CleanLotSize*US_BLOCK$MedLotSize_minimum_struct & !is.na(US_BLOCK$UnitDensityRestriction) & !is.na(US_BLOCK$MedLotSize_minimum_struct),]), " block groups that will be censored by cleaning of lot size"))
US_BLOCK["IncomeStringency_cl"] <- US_BLOCK$IncomeStringency
US_BLOCK$IncomeStringency_cl[US_BLOCK$UnitDensityRestriction > CleanLotSize*US_BLOCK$MedLotSize_minimum_struct] <- NA
US_BLOCK["IncomeStringency_preCovid_cl"] <- US_BLOCK$IncomeStringency_preCovid
US_BLOCK$IncomeStringency_preCovid_cl[US_BLOCK$UnitDensityRestriction > CleanLotSize*US_BLOCK$MedLotSize_minimum_struct] <- NA
US_BLOCK["UnitDensityRestriction_cl"] <- US_BLOCK$UnitDensityRestriction
US_BLOCK$UnitDensityRestriction_cl[US_BLOCK$UnitDensityRestriction > CleanLotSize*US_BLOCK$MedLotSize_minimum_struct] <- NA
#Results are robust to imputing these above with zeros, as in the model (not the baseline empirical facts exercise)

#Demeaning by CBSA for each type for facts
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_stringency = IncomeStringency_cl - mean(IncomeStringency_cl, na.rm = TRUE))
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_stringency_precovid = IncomeStringency_preCovid_cl - mean(IncomeStringency_preCovid_cl, na.rm = TRUE))
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_densrestriction = UnitDensityRestriction_cl - mean(UnitDensityRestriction_cl, na.rm = TRUE))
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_stringency_raw = IncomeStringency - mean(IncomeStringency, na.rm = TRUE)) #for robustness (not cleaned minimum lot sizes)

rm(Main_regulation)

#Constructing Lat/Lon coordinates for use with other programs
US_BLOCK <- st_as_sf(US_BLOCK)
US_BLOCK <- st_centroid(US_BLOCK)
US_BLOCK <- st_transform(US_BLOCK, crs = 4269)
US_BLOCK <- cbind(US_BLOCK, st_coordinates(US_BLOCK)) 
US_BLOCK <- rename(US_BLOCK, Lon = X, Lat = Y)

#Saving to STATA for use with other programs (namely the structural IV estimation + assigning data to the counterfactuals)
US_BLOCK <- US_BLOCK %>% st_drop_geometry() %>% rename(dm_car_transport_share= demeaned_car_transport_share,
                                                       dm_car_transport_share_hist= demeaned_car_transport_share_hist,
                                                       dm_public_transport_share= demeaned_public_transport_share,
                                                       dm_public_transport_share_hist= demeaned_public_transport_share_hist) #shortening variable names for stata output

write_dta(US_BLOCK, "DataV2/US_Data/Output/Constructed_Block_V2.dta") #Use this when you need the unit density restriction for other programs!

#Saving raw regulation by block group for use with other programs
Regulation_data <- US_BLOCK %>% dplyr::select(State, County, Tract, BlockGroup, CBSA, UnitDensityRestriction, IncomeStringency, 
                                              LandValueDensity_matched, LandValueDensity_hist) 
write_dta(Regulation_data, "DataV2/US_Data/Output/raw_Regulation_ByBlockGroup.dta")

#NEXT: LOOPING OVER ALL OTHER CLUSTERING DEFINITIONS AND CALCULATING demeaned income stringency. Merging to main dataset.
#These are only for robustness-- to check the facts, and select along clustering parameters to match observed data.  
index <- 0
for (file in e.filelist) {
  
  index <- index + 1
  
  Regulation.file <- Regulation[[index]] %>% select(State, County, Tract, BlockGroup, CBSA, IncomeStringency, 
                                                    UnitDensityRestriction, MedLotSize_minimum_struct, 
                                                    singlefamily_mode, duplex_mode, triplex_mode, quadriplex_mode)
  
  #Removing observations that have unreliable lot size measurements (i.e. minimum lot size > CleanLotSize*average lot size)
  Regulation.file["IncomeStringency_cl"] <- Regulation.file$IncomeStringency
  Regulation.file$IncomeStringency_cl[Regulation.file$UnitDensityRestriction > CleanLotSize*Regulation.file$MedLotSize_minimum_struct] <- NA
  Regulation.file["UnitDensityRestriction_cl"] <- Regulation.file$UnitDensityRestriction
  Regulation.file$UnitDensityRestriction_cl[Regulation.file$UnitDensityRestriction > CleanLotSize*Regulation.file$MedLotSize_minimum_struct] <- NA
  
  #Calculating demeaned stringency measure for use with the facts. 
  Regulation.file <- Regulation.file %>% group_by(CBSA) %>% mutate(demeaned_stringency = IncomeStringency_cl - mean(IncomeStringency_cl, na.rm = TRUE))
  
  names(Regulation.file)[names(Regulation.file) == 'demeaned_stringency'] <- paste0("IS", 
                                                                                sub("Regulation_measurements_", 
                                                                                    "", file))
  names(Regulation.file)[names(Regulation.file) == 'UnitDensityRestriction_cl'] <- paste0("uDR", 
                                                                                    sub("Regulation_measurements_", 
                                                                                        "", file))
  #Labeling modes for each type of structure to pass to validation program
  for (structures in c("singlefamily", "duplex", "triplex", "quadriplex")) {
    names(Regulation.file)[names(Regulation.file) == paste0(structures, "_mode")] <- paste0(paste0(structures, "_mode"), 
                                                                                      sub("Regulation_measurements_", 
                                                                                          "", file))
  }
  
  #cleaning and putting into one data frame
  Regulation.file <- Regulation.file %>% select(-IncomeStringency, -IncomeStringency_cl, -UnitDensityRestriction, -MedLotSize_minimum_struct)
  
  US_BLOCK <- left_join(US_BLOCK, Regulation.file, by = c("State", "County", "Tract", "BlockGroup", "CBSA"))
  
}
rm(index)


#Saving datasets in R format for use with other programs.
save(US_BLOCK, file = "DataV2/US_Data/Output/Constructed_Block_V2.Rdata")

#clear memory
remove(list = ls())