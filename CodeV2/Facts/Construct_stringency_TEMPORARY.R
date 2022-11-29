library(sf)
library(haven) #Reading stata.dta files 
library(dplyr)
library(readr)
library(collapse)
library(stringr)
library(labelled)
library(ggplot2)
library(estimatr)

#Temporary_facts_zoning_stringency
#Date created: October 12th, 2022.
#Use Zillow data on minimum lot sizes for each 2010 census block group, and overlay that onto 2020 census block group geographies.This is a temporary fix until CoreLogic comes in.

#Reading 2010/2020 geographies + income stringency of regulation

Block_geo_2020 <- st_read("DataV2/US_Data/Shapefiles/US_blck_grp_2020.shp")
Block_geo_2010 <- st_read("Data/US_Data/CensusBlockGroup2010/US_blck_grp_2010.shp")

Block_geo_2010 <- Block_geo_2010 %>% rename()

#Income stringency data for 2010 geographies
Main <- read_stata("Data/Counterfactuals/JoinedDataForCounterfactuals.dta", #From previous
                   col_select = c("State", "County", "Tract", "BlockGroup", "CBSA", "CBSA_NAME", 
                                  "DensityRestriction", "IncomeStringencyofRegulation"))

#Renaming variables for merge
names(Block_geo_2010)[names(Block_geo_2010) == 'STATEFP10'] <- 'State'
names(Block_geo_2010)[names(Block_geo_2010) == 'COUNTYFP10'] <- 'County'
names(Block_geo_2010)[names(Block_geo_2010) == 'TRACTCE10'] <- 'Tract' #Renaming these variables to join on State/County/Tract
names(Block_geo_2010)[names(Block_geo_2010) == 'BLKGRPCE10'] <- 'BlockGroup' #Renaming these variables to join on State/County/Tract

names(Block_geo_2020)[names(Block_geo_2020) == 'STATEFP'] <- 'State'
names(Block_geo_2020)[names(Block_geo_2020) == 'COUNTYFP'] <- 'County'
names(Block_geo_2020)[names(Block_geo_2020) == 'TRACTCE'] <- 'Tract' #Renaming these variables to join on State/County/Tract
names(Block_geo_2020)[names(Block_geo_2020) == 'BLKGRPCE'] <- 'BlockGroup' #Renaming these variables to join on State/County/Tract

#Destringing Code
Block_geo_2010$State <- as.double(Block_geo_2010$State)
Block_geo_2010$County <- as.double(Block_geo_2010$County)
Block_geo_2010$Tract <- as.double(Block_geo_2010$Tract)
Block_geo_2010$BlockGroup <- as.double(Block_geo_2010$BlockGroup)

Block_geo_2020$State <- as.double(Block_geo_2020$State)
Block_geo_2020$County <- as.double(Block_geo_2020$County)
Block_geo_2020$Tract <- as.double(Block_geo_2020$Tract)
Block_geo_2020$BlockGroup <- as.double(Block_geo_2020$BlockGroup)

Main$State <- as.double(Main$State)
Main$County <- as.double(Main$County)
Main$Tract <- as.double(Main$Tract)
Main$BlockGroup <- as.double(Main$BlockGroup)


#Merging main to 2010 geographies
Main <- left_join(Block_geo_2010, Main, by = c("State", "County", "Tract", "BlockGroup"))
#Removing unmatched
Main <- Main[!is.na(Main$IncomeStringencyofRegulation) | !is.na(Main$DensityRestriction), ]
rm(Block_geo_2010)


#To do: overlay geographies from Main, (how to aggregate? Take minimum? Match on centriod?)

#Matching to one location on centriod -- should be fine, if a lot of census tracts retain definitions.
Block_geo_2020 <- st_centroid(Block_geo_2020) #Taking centriods of 2020 block groups, matching them if they intersect with polygon from 2010 block groups

Block_geo_2020["Temp"] <- as.integer(st_intersects(Block_geo_2020, Main)) #Centroid matched to polygon in Main
Main <- mutate(Main, Temp = row_number()) #Temp used to join 2020 block groups with 2010 block groups

temp_tojoin <- Main %>% dplyr::select(Temp, IncomeStringencyofRegulation, DensityRestriction) %>% st_drop_geometry()
rm(Main)

Main <- left_join(Block_geo_2020, temp_tojoin, by = c("Temp"))
Main <- Main %>% dplyr::select(State, County, Tract, BlockGroup, IncomeStringencyofRegulation, DensityRestriction) %>% st_drop_geometry() #dropping Temp
Main <- Main[!is.na(Main$IncomeStringencyofRegulation) | !is.na(Main$DensityRestriction),] #Keeping 2020 block groups assigned regulation (190,000 assigned)
rm(Block_geo_2020, temp_tojoin)

#Merging to data
load(file = "DataV2/US_Data/Output/Constructed_Block.Rdata")
US_BLOCK$State <- as.double(US_BLOCK$State)
US_BLOCK$County <- as.double(US_BLOCK$County)
US_BLOCK$Tract <- as.double(US_BLOCK$Tract)
US_BLOCK$BlockGroup <- as.double(US_BLOCK$BlockGroup)

#merging and overwriting
US_BLOCK <- left_join(US_BLOCK, Main, by = c("State", "County", "Tract", "BlockGroup"))
  
#Demeaning by CBSA
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_stringency = IncomeStringencyofRegulation - mean(IncomeStringencyofRegulation, na.rm = TRUE))
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_densrestriction = DensityRestriction - mean(DensityRestriction, na.rm = TRUE))


#Constructing centroid in latitude and longitude coordinates
test <- st_coordinates(st_transform(st_centroid(US_BLOCK), 4269))
US_BLOCK["Lon"] <- test[,1]
US_BLOCK["Lat"] <- test[,2]

#Saving
save(US_BLOCK, file = "DataV2/US_Data/Output/Constructed_Block_V2.Rdata")

#Saving to STATA for use with other programs
US_BLOCK <- US_BLOCK %>% st_drop_geometry() %>% dplyr::select(-STATE.x, -COUNTY.x, -geometry)
write_dta(US_BLOCK, "DataV2/US_Data/Output/Constructed_Block_V2.dta")
