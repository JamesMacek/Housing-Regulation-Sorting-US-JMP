
#This file explores CBSA housing data by constructing spatial distributions of housing density within CBSAs for initial analysis
#SIMPLIFIED CODE FOR WHAT WE NEED to construct facts: Facts_construct.R
#Updated so that block groups are the unit of analysis.


#Date edited: June 22 2022
library(sf)
library(haven) #Reading stata.dta files 
library(dplyr)
library(labelled)
library(readr)
library(collapse)
library(stringr)


#Reading in US 2010 Census CBSA geography
US_CBSA_2010 <- st_read("Data/US_Data/cb_2013_us_cbsa_500k.shp") #2013 definitions from the US census bureau, better to use
US_CBSA_2010 <- US_CBSA_2010[US_CBSA_2010$LSAD == "M1",]
US_CBSA_2010 <- US_CBSA_2010 %>% rename(CBSA = CBSAFP)

#2010 census tracts
US_TRACT_2010 <- st_read("Data/US_Data/CensusTract2010/US_tract_2010.shp") #From IPUMS NHGIS website

#PART 1: MATCHING TRACTS TO CBSAs____________________________________________
#Extracting centriods, deleting US_Tract data otherwise
US_TRACT_2010_CENTRIOD <- st_centroid(US_TRACT_2010)
rm(US_TRACT_2010)
#Converting CBSA data to NAD83 reference system
US_TRACT_2010_CENTRIOD <- st_transform(US_TRACT_2010_CENTRIOD, 4269)

#Note: census tracts do not cross county lines and therefore to do not cross CBSA lines. 
#So matching centroids to CBSA polygons are sufficient for accuracy.  
US_TRACT_2010_CENTRIOD["Temp"] <- as.integer(st_intersects(US_TRACT_2010_CENTRIOD, US_CBSA_2010)) #Centroid matched to polygon by observation in US_CBSA_2010
US_CBSA_2010 <- mutate(US_CBSA_2010, Temp = row_number()) #Temp used to join MSA names with matched centroids. 

#Matching CBSA FIPS Code to US_TRACT_2010_CENTRIOD data.
#Data frame containing names of CBSA, unique CBSA code, etc. 
Temp_tojoin <- US_CBSA_2010 %>%
  select(Temp, NAME, CBSA)
Temp_tojoin <- as.data.frame(Temp_tojoin) #converting for left_join
Temp_tojoin <- Temp_tojoin[, !colnames(Temp_tojoin) %in% "geometry"] #deleting geometry data from sf object to join 
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_CENTRIOD, Temp_tojoin, by = "Temp")
rm(Temp_tojoin, US_TRACT_2010_CENTRIOD)
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED[, !colnames(US_TRACT_2010_JOINED) %in% "Temp"] #removing variable used solely for matching
names(US_TRACT_2010_JOINED)[names(US_TRACT_2010_JOINED) == 'NAME'] <- 'CBSA_NAME'
names(US_TRACT_2010_JOINED)[names(US_TRACT_2010_JOINED) == 'STATEFP10'] <- 'State'
names(US_TRACT_2010_JOINED)[names(US_TRACT_2010_JOINED) == 'COUNTYFP10'] <- 'County'
names(US_TRACT_2010_JOINED)[names(US_TRACT_2010_JOINED) == 'TRACTCE10'] <- 'Tract' #Renaming these variables to join on State/County/Tract


#Dropping tracts not in used CBSA
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED[!is.na(US_TRACT_2010_JOINED$CBSA),]

#Dropping Other -mainland states, Hawaii (15), Alaska (2) Puerto Rico (72), US Virgin Islands (already NOT IN SAMPLE)
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED[!(US_TRACT_2010_JOINED$State == 15 | US_TRACT_2010_JOINED$State == 72 | US_TRACT_2010_JOINED$State == 2),]
US_TRACT_2010_JOINED <- st_drop_geometry(US_TRACT_2010_JOINED) %>% select(State, County, Tract, CBSA, CBSA_NAME)

#Joining tracts to block group polygons 
US_BLOCK_2010 <- st_read("Data/US_Data/CensusBlockGroup2010/US_blck_grp_2010.shp") 
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'STATEFP10'] <- 'State'
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'COUNTYFP10'] <- 'County'
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'TRACTCE10'] <- 'Tract' #Renaming these variables to join on State/County/Tract
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'BLKGRPCE10'] <- 'BlockGroup'

#Joining CBSAs with US_TRACT_2010 polygons

US_BLOCK_2010 <- left_join(US_BLOCK_2010, US_TRACT_2010_JOINED, by = c("State", "County", "Tract"))
US_BLOCK_2010 <- US_BLOCK_2010[!is.na(US_BLOCK_2010$CBSA),] #Dropping tracts not in CBSA
rm(US_CBSA_2010, US_TRACT_2010_JOINED)

#converting to NAD83
US_BLOCK_2010 <- st_transform(US_BLOCK_2010, 4269)

#Merging with 2010 US Planning Database data. 
US_PLANNING_2010 <- read.csv("Data/US_Data/CensusTract2010/2010PlanningDatabase.csv") 
US_PLANNING_2010 <- US_PLANNING_2010 %>% rename(BlockGroup = Block_Group) %>% select(-GIDBG, -County_name, -State_name)

#First, removing leading zeros on US_BLOCK_2010 Tract codes, then joining to planning database
US_BLOCK_2010$State <- as.integer(str_remove(US_BLOCK_2010$State, "^0+"))
US_BLOCK_2010$County <- as.integer(str_remove(US_BLOCK_2010$County, "^0+"))
US_BLOCK_2010$Tract <- as.integer(str_remove(US_BLOCK_2010$Tract, "^0+"))
US_BLOCK_2010$BlockGroup <- as.integer(US_BLOCK_2010$BlockGroup)
US_BLOCK_2010_JOINED <- left_join(US_BLOCK_2010, US_PLANNING_2010, by = c("State", "County", "Tract", "BlockGroup"))

#Removing unneeded data
rm(US_BLOCK_2010, US_PLANNING_2010)

US_BLOCK_2010_JOINED$Med_house_val_tr_ACS_06_10 <- parse_number(US_BLOCK_2010_JOINED$Med_house_val_tr_ACS_06_10)
US_BLOCK_2010_JOINED$Med_HHD_Inc_TR_ACS_06_10 <- parse_number(US_BLOCK_2010_JOINED$Med_HHD_Inc_TR_ACS_06_10)


#Joining NHGIS Data
nhgis <- read_stata("Data/US_Data/NHGIS/BlockGroup/nhgis_blkgrp.dta")
US_BLOCK_2010_JOINED <- left_join(US_BLOCK_2010_JOINED, nhgis, by = c("State", "County", "Tract", "BlockGroup"))
#Some unmatched because of changes of census tracts between 2010 and 2012. These are minor and unlikely to unfluence results
rm(nhgis)


# PART 2: DATA CONSTRUCTION_________________________________________________________________________________________________________________

#Housing Density
#SUBSETTING MAIN SAMPLE TO HAVE POSITIVE HOUSEHOLDS AND LAND MASS
US_BLOCK_2010_JOINED <- US_BLOCK_2010_JOINED[!(US_BLOCK_2010_JOINED$Tot_Housing_Units_CEN_2010 == 0) & !(US_BLOCK_2010_JOINED$LAND_AREA == 0), ] #Taking BLOCKs with coded positive land area and housing units. 
US_BLOCK_2010_JOINED["log_Housing_density"] <- log(US_BLOCK_2010_JOINED$Tot_Housing_Units_CEN_2010/US_BLOCK_2010_JOINED$LAND_AREA) #log density of housing units
US_BLOCK_2010_JOINED["Housing_density"] <- US_BLOCK_2010_JOINED$Tot_Housing_Units_CEN_2010/US_BLOCK_2010_JOINED$LAND_AREA # Non-logs
US_BLOCK_2010_JOINED["Population_density"] <- US_BLOCK_2010_JOINED$Tot_Population_CEN_2010/US_BLOCK_2010_JOINED$LAND_AREA

#constructing MSA housing price subsets
US_CBSA_2010_c <- as.data.frame(collap(US_BLOCK_2010_JOINED, Med_house_val_tr_ACS_06_10 ~ CBSA + CBSA_NAME, FUN = c("fmedian")))

names(US_CBSA_2010_c)[names(US_CBSA_2010_c) == 'Med_house_val_tr_ACS_06_10'] <- 'CBSA_med_house_value'
US_BLOCK_2010_JOINED <- left_join(US_BLOCK_2010_JOINED, US_CBSA_2010_c, by = c("CBSA"), suffix = c("", ".y")) %>% select(-ends_with(".y"))

#Population density ranks (to compare with French exercise in robustness)
US_CBSA_2010_cpop <- as.data.frame(collap(US_BLOCK_2010_JOINED, Population_density ~ CBSA + CBSA_NAME, FUN = c("fmean")))
names(US_CBSA_2010_cpop)[names(US_CBSA_2010_cpop) == 'Population_density'] <- 'CBSA_popdens'
US_BLOCK_2010_JOINED <- left_join(US_BLOCK_2010_JOINED, US_CBSA_2010_cpop, by = c("CBSA"), suffix = c("", ".y")) %>% select(-ends_with(".y"))

#Centiles pf CBSAs in terms of median-of-median house prices: look at density distributions
quantile_CBSA_houseval <- quantile(US_CBSA_2010_c$CBSA_med_house_value, probs = seq(0, 1, 0.005), na.rm = TRUE)
quantile_CBSA_popdens <- quantile(US_CBSA_2010_cpop$CBSA_popdens, probs = seq(0, 1, 0.005), na.rm = TRUE)

#White share of population at tract level, from census
US_BLOCK_2010_JOINED["White_share"] <- US_BLOCK_2010_JOINED$NH_White_alone_CEN_2010/US_BLOCK_2010_JOINED$Tot_Population_CEN_2010
US_BLOCK_2010_JOINED["Black_share"] <- US_BLOCK_2010_JOINED$NH_Blk_alone_CEN_2010/US_BLOCK_2010_JOINED$Tot_Population_CEN_2010

#Average household size (For thinking about family effects)
US_BLOCK_2010_JOINED["Average_HH_size"] <- US_BLOCK_2010_JOINED$qyne001

#Constructing ranking of densities
US_BLOCK_2010_JOINED <- US_BLOCK_2010_JOINED %>% group_by(CBSA) %>% mutate(rank_density_CBSA = order(order(Housing_density, decreasing = FALSE))/(max(order(order(Housing_density, decreasing = FALSE)))+1))
US_BLOCK_2010_JOINED <- US_BLOCK_2010_JOINED %>% group_by(CBSA) %>% mutate(rank_popdensity_CBSA = order(order(Population_density, decreasing = FALSE))/(max(order(order(Population_density, decreasing = FALSE)))+1))


##################DENSITY COMPONENTS_____________________######################################
#1) Single family homes
#2) 2-19 unit structures
#3) 20+ unit structures 
#4) 2-4 unit structures as a sub-component of 2) (Aradhya's request)
#5) 2+ unit structures (only splitting by half)
#USE ACS TO CONSTRUCT SHARES in structure type, Census (Planning Database) to construct number of units in levels.
#NOTE qyye010 and qyye011 are boat and mobile homes, respectively. 

US_BLOCK_2010_JOINED["Housing_density"] <- US_BLOCK_2010_JOINED$Tot_Housing_Units_CEN_2010/US_BLOCK_2010_JOINED$LAND_AREA
US_BLOCK_2010_JOINED <- US_BLOCK_2010_JOINED  %>% group_by(CBSA) %>% mutate(tract_land_fraction = LAND_AREA/sum(LAND_AREA))
US_BLOCK_2010_JOINED <- US_BLOCK_2010_JOINED  %>% group_by(CBSA) %>% mutate(demeaned_Housing_density = Housing_density/mean(Housing_density))


US_BLOCK_2010_JOINED_D <- US_BLOCK_2010_JOINED[!is.na(US_BLOCK_2010_JOINED$qyye008) & !is.na(US_BLOCK_2010_JOINED$qyye009) &
                                                 !is.na(US_BLOCK_2010_JOINED$qyye002) & !is.na(US_BLOCK_2010_JOINED$qyye003) & !is.na(US_BLOCK_2010_JOINED$qyye001) &
                                                 !is.na(US_BLOCK_2010_JOINED$qyye010) & !is.na(US_BLOCK_2010_JOINED$qyye011), ] 
US_BLOCK_2010_JOINED_D["Single_family_density"] <- ((US_BLOCK_2010_JOINED_D$qyye002 + US_BLOCK_2010_JOINED_D$qyye003)*(US_BLOCK_2010_JOINED_D$Tot_Housing_Units_CEN_2010/US_BLOCK_2010_JOINED_D$qyye001))/US_BLOCK_2010_JOINED_D$LAND_AREA
US_BLOCK_2010_JOINED_D <- US_BLOCK_2010_JOINED_D  %>% group_by(CBSA) %>% mutate(demeaned_Single_family_density = Single_family_density/mean(Housing_density)) #Demean with total housing density (as the sums add up to total demeaned density!)
US_BLOCK_2010_JOINED <- left_join(US_BLOCK_2010_JOINED, US_BLOCK_2010_JOINED_D, by = c("State", "County", "Tract", "BlockGroup"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_BLOCK_2010_JOINED_D)

US_BLOCK_2010_JOINED_D <- US_BLOCK_2010_JOINED[!is.na(US_BLOCK_2010_JOINED$qyye008) & !is.na(US_BLOCK_2010_JOINED$qyye009) &
                                                 !is.na(US_BLOCK_2010_JOINED$qyye002) & !is.na(US_BLOCK_2010_JOINED$qyye003) & !is.na(US_BLOCK_2010_JOINED$qyye001) &
                                                 !is.na(US_BLOCK_2010_JOINED$qyye010) & !is.na(US_BLOCK_2010_JOINED$qyye011), ]
US_BLOCK_2010_JOINED_D["Building_20_density"] <- ((US_BLOCK_2010_JOINED_D$qyye008 + US_BLOCK_2010_JOINED_D$qyye009)*(US_BLOCK_2010_JOINED_D$Tot_Housing_Units_CEN_2010/US_BLOCK_2010_JOINED_D$qyye001))/US_BLOCK_2010_JOINED_D$LAND_AREA
US_BLOCK_2010_JOINED_D <- US_BLOCK_2010_JOINED_D  %>% group_by(CBSA) %>% mutate(demeaned_Building_20_density = Building_20_density/mean(Housing_density))
US_BLOCK_2010_JOINED <- left_join(US_BLOCK_2010_JOINED, US_BLOCK_2010_JOINED_D, by = c("State", "County", "Tract", "BlockGroup"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_BLOCK_2010_JOINED_D)

US_BLOCK_2010_JOINED_D <- US_BLOCK_2010_JOINED[!is.na(US_BLOCK_2010_JOINED$qyye008) & !is.na(US_BLOCK_2010_JOINED$qyye009) &
                                                 !is.na(US_BLOCK_2010_JOINED$qyye002) & !is.na(US_BLOCK_2010_JOINED$qyye003) & !is.na(US_BLOCK_2010_JOINED$qyye001) &
                                                 !is.na(US_BLOCK_2010_JOINED$qyye010) & !is.na(US_BLOCK_2010_JOINED$qyye011),]

US_BLOCK_2010_JOINED_D["Middle_density"] <- ((US_BLOCK_2010_JOINED_D$qyye001 - (US_BLOCK_2010_JOINED_D$qyye002 + US_BLOCK_2010_JOINED_D$qyye003 + 
                                                                                  US_BLOCK_2010_JOINED_D$qyye008 + US_BLOCK_2010_JOINED_D$qyye009 + US_BLOCK_2010_JOINED_D$qyye010 +
                                                                                  US_BLOCK_2010_JOINED_D$qyye011))*(US_BLOCK_2010_JOINED_D$Tot_Housing_Units_CEN_2010/US_BLOCK_2010_JOINED_D$qyye001))/US_BLOCK_2010_JOINED_D$LAND_AREA
US_BLOCK_2010_JOINED_D <- US_BLOCK_2010_JOINED_D  %>% group_by(CBSA) %>% mutate(demeaned_Middle_density = Middle_density/mean(Housing_density))
US_BLOCK_2010_JOINED <- left_join(US_BLOCK_2010_JOINED, US_BLOCK_2010_JOINED_D, by = c("State", "County", "Tract", "BlockGroup"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_BLOCK_2010_JOINED_D)

#Only duplexes, triplexes and quadplexes.
US_BLOCK_2010_JOINED_D <- US_BLOCK_2010_JOINED[!is.na(US_BLOCK_2010_JOINED$qyye008) & !is.na(US_BLOCK_2010_JOINED$qyye009) &
                                                 !is.na(US_BLOCK_2010_JOINED$qyye002) & !is.na(US_BLOCK_2010_JOINED$qyye003) & !is.na(US_BLOCK_2010_JOINED$qyye001) &
                                                 !is.na(US_BLOCK_2010_JOINED$qyye010) & !is.na(US_BLOCK_2010_JOINED$qyye011) & 
                                                 !is.na(US_BLOCK_2010_JOINED$qyye004) & !is.na(US_BLOCK_2010_JOINED$qyye005),]
US_BLOCK_2010_JOINED_D["Middle_density_spl1"] <- ((US_BLOCK_2010_JOINED_D$qyye004 + US_BLOCK_2010_JOINED_D$qyye005)*(US_BLOCK_2010_JOINED_D$Tot_Housing_Units_CEN_2010/US_BLOCK_2010_JOINED_D$qyye001))/US_BLOCK_2010_JOINED_D$LAND_AREA
US_BLOCK_2010_JOINED_D <- US_BLOCK_2010_JOINED_D  %>% group_by(CBSA) %>% mutate(demeaned_Middlesp1_density = Middle_density_spl1/mean(Housing_density))
US_BLOCK_2010_JOINED <- left_join(US_BLOCK_2010_JOINED, US_BLOCK_2010_JOINED_D, by = c("State", "County", "Tract", "BlockGroup"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_BLOCK_2010_JOINED_D)


US_BLOCK_2010_JOINED_D <- US_BLOCK_2010_JOINED[!is.na(US_BLOCK_2010_JOINED$qyye008) & !is.na(US_BLOCK_2010_JOINED$qyye009) &
                                                 !is.na(US_BLOCK_2010_JOINED$qyye002) & !is.na(US_BLOCK_2010_JOINED$qyye003) & !is.na(US_BLOCK_2010_JOINED$qyye001) &
                                                 !is.na(US_BLOCK_2010_JOINED$qyye010) & !is.na(US_BLOCK_2010_JOINED$qyye011),]
US_BLOCK_2010_JOINED_D["Building_2_density"] <- ((US_BLOCK_2010_JOINED_D$qyye001 - 
                                                    (US_BLOCK_2010_JOINED_D$qyye002 + US_BLOCK_2010_JOINED_D$qyye003 + 
                                                       US_BLOCK_2010_JOINED_D$qyye010 + US_BLOCK_2010_JOINED_D$qyye011))*(US_BLOCK_2010_JOINED_D$Tot_Housing_Units_CEN_2010/US_BLOCK_2010_JOINED_D$qyye001))/US_BLOCK_2010_JOINED_D$LAND_AREA
US_BLOCK_2010_JOINED_D <- US_BLOCK_2010_JOINED_D  %>% group_by(CBSA) %>% mutate(demeaned_Building_2_density = Building_2_density/mean(Housing_density))
US_BLOCK_2010_JOINED <- left_join(US_BLOCK_2010_JOINED, US_BLOCK_2010_JOINED_D, by = c("State", "County", "Tract", "BlockGroup"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_BLOCK_2010_JOINED_D)

#####################################################################################################################_____________________
#Further variable construction

#demeaned population density at 2010 values (fsum.TOT_POPULATION_CEN_2010)
US_BLOCK_2010_JOINED <- US_BLOCK_2010_JOINED  %>% group_by(CBSA) %>% mutate(demeaned_Pop_density = Population_density/mean(Population_density, na.rm= TRUE))

#Average income from the NHGIS
US_BLOCK_2010_JOINED["Average_income"] <- US_BLOCK_2010_JOINED$qvbe001/US_BLOCK_2010_JOINED$qx6e001 #Aggregate income from 2008-2012 ACS/ACS number of households

#MSA demeaned variables
#White subset
US_BLOCK_2010_P <- US_BLOCK_2010_JOINED[US_BLOCK_2010_JOINED$White_share > 0,]  #Subsetting data where at least some white people are
US_BLOCK_2010_P <- US_BLOCK_2010_P[!(is.na(US_BLOCK_2010_P$White_share)),]
US_BLOCK_2010_P <- US_BLOCK_2010_P %>% group_by(CBSA) %>% mutate(demeaned_log_White_share = log(White_share) - mean(log(White_share)))
US_BLOCK_2010_JOINED <- left_join(US_BLOCK_2010_JOINED, US_BLOCK_2010_P, by = c("State", "County", "Tract", "BlockGroup"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_BLOCK_2010_P)

#Income subset, average income (not median income)
US_BLOCK_2010_JOINED_INC <- US_BLOCK_2010_JOINED[!(is.na(US_BLOCK_2010_JOINED$Average_income)),] 
US_BLOCK_2010_JOINED_INC <- US_BLOCK_2010_JOINED_INC %>% group_by(CBSA) %>% mutate(demeaned_log_Income = log(Average_income) - mean(log(Average_income)))
US_BLOCK_2010_JOINED_INC <- US_BLOCK_2010_JOINED_INC %>% group_by(CBSA) %>% mutate(demeaned_Avg_Income = Average_income/mean(Average_income)) #Another way to measure income differences
US_BLOCK_2010_JOINED <- left_join(US_BLOCK_2010_JOINED, US_BLOCK_2010_JOINED_INC, by = c("State", "County", "Tract", "BlockGroup"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_BLOCK_2010_JOINED_INC)

#Average household size
US_BLOCK_2010_HH <- US_BLOCK_2010_JOINED[!(is.na(US_BLOCK_2010_JOINED$Average_HH_size)),] 
US_BLOCK_2010_HH <- US_BLOCK_2010_HH %>% group_by(CBSA) %>% mutate(demeaned_log_Average_HH_size = log(Average_HH_size) - mean(log(Average_HH_size)))
US_BLOCK_2010_JOINED <- left_join(US_BLOCK_2010_JOINED, US_BLOCK_2010_HH, by = c("State", "County", "Tract", "BlockGroup"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_BLOCK_2010_HH)

#____________PART3_____________Constructing single family margin from Assessment data, getting margins of single detached housing______________________________
#Requires SingleFamilyLandConstructed.dta, output from ConstructTractBlockLotSizeAggregates.do

SingleFamilyLand <- read_stata("Data/US_Data/Output/SingleFamilyLandConstructed.dta") #available for 167371 tracts
SingleFamilyLand$State <- as.double(SingleFamilyLand$State)
SingleFamilyLand$County <- as.double(SingleFamilyLand$County)
SingleFamilyLand$Tract <- as.double(SingleFamilyLand$Tract)
SingleFamilyLand$BlockGroup <- as.double(SingleFamilyLand$BlockGroup)

#Joining lot sizes with main dataset
US_BLOCK_2010_JOINED <- left_join(US_BLOCK_2010_JOINED, SingleFamilyLand, by = c("State", "County", "Tract", "BlockGroup"))
rm(SingleFamilyLand)

#converting to square miles for use with tract land measurements
US_BLOCK_2010_JOINED["LotSizeSquareMiles"] <- US_BLOCK_2010_JOINED$LotSizeAcres/640

#Constructing measures of total land used. I.e. measures of number of single family homes from ACS/Census x Average lot size from assessments
#Remember: qyye002 and qyye003 are both detached and semi-detached houses. 
US_BLOCK_2010_JOINED["LandSingleFamily"] <- US_BLOCK_2010_JOINED$LotSizeSquareMiles*((US_BLOCK_2010_JOINED$qyye002 + US_BLOCK_2010_JOINED$qyye003)*(US_BLOCK_2010_JOINED$Tot_Housing_Units_CEN_2010/US_BLOCK_2010_JOINED$qyye001))
US_BLOCK_2010_JOINED["LandShareSingleFamily"] <- US_BLOCK_2010_JOINED$LandSingleFamily/US_BLOCK_2010_JOINED$LAND_AREA  

#Some assessments are outliers, largely above the census tract. This might be because these homes are large and may cross tract boundaries. 
length(US_BLOCK_2010_JOINED$LandShareSingleFamily[US_BLOCK_2010_JOINED$LandShareSingleFamily > 1])
US_BLOCK_2010_JOINED$LandShareSingleFamily[US_BLOCK_2010_JOINED$LandShareSingleFamily > 1] <- NA #Only a few thousand follow under this category, could be randomness. Imperfect measure.

US_BLOCK_2010_JOINED_D <- US_BLOCK_2010_JOINED[!is.na(US_BLOCK_2010_JOINED$qyye008) & !is.na(US_BLOCK_2010_JOINED$qyye009) &
                                                 !is.na(US_BLOCK_2010_JOINED$qyye002) & !is.na(US_BLOCK_2010_JOINED$qyye003) & !is.na(US_BLOCK_2010_JOINED$qyye001) &
                                                 !is.na(US_BLOCK_2010_JOINED$qyye010) & !is.na(US_BLOCK_2010_JOINED$qyye011) & !is.na(US_BLOCK_2010_JOINED$LandShareSingleFamily), ]
US_BLOCK_2010_JOINED_D <- US_BLOCK_2010_JOINED_D %>% group_by(CBSA) %>% mutate(demeaned_SingleF_Density_Margin = (demeaned_Single_family_density*sqrt(mean(Housing_density)))/(LandShareSingleFamily)) #Multiplying by sqrt because this value was already divided by housing density before)
US_BLOCK_2010_JOINED_D <- US_BLOCK_2010_JOINED_D %>% group_by(CBSA) %>% mutate(demeaned_SingleF_Land_Margin = (LandShareSingleFamily)/(sqrt(mean(Housing_density)))) #dividing by sqrt to carry over normalization component.

#Check: demeaned_SingleF_Density_Margin should equal this quantity
US_BLOCK_2010_JOINED_D <- US_BLOCK_2010_JOINED_D %>% group_by(CBSA) %>% mutate(demeaned_SingleF_Density_Margin2 = (1/LotSizeSquareMiles)*(1/sqrt(mean(Housing_density)))) #Multiplying by sqrt because this value was already divided by housing density before)
#Equals approximately,(very close, since we computed mean with a different dataframe that didn't have the land share missing they are not exactly equal.)
US_BLOCK_2010_JOINED_D <- US_BLOCK_2010_JOINED_D %>% select(-demeaned_SingleF_Density_Margin2)

US_BLOCK_2010_JOINED <- left_join(US_BLOCK_2010_JOINED, US_BLOCK_2010_JOINED_D, by = c("State", "County", "Tract", "BlockGroup"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_BLOCK_2010_JOINED_D)

#removing geometry to save disk space
US_BLOCK_2010_JOINED <- US_BLOCK_2010_JOINED %>% select(-geometry)

#Saving...
save(US_BLOCK_2010_JOINED, file = "Data/US_Data/Output/Constructed_2010_Tract.Rdata")
save(US_CBSA_2010_c, file = "Data/US_Data/Output/CBSA_med_house_price.Rdata")
save(quantile_CBSA_houseval, file = "Data/US_Data/Output/CBSA_quantiles.Rdata")
save(quantile_CBSA_popdens, file = "Data/US_Data/Output/CBSA_quantiles_popdens.Rdata")
rm(US_BLOCK_2010_JOINED, US_CBSA_2010_c, US_CBSA_2010_cpop, quantile_CBSA_houseval, quantile_CBSA_popdens)