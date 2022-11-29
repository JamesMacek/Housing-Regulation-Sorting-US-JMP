#Date Created: March 29 2022
#Date Edited: June 23th 2022

library(dplyr)
library(haven)
library(labelled)
library(sf)

#Module_1: 
#This code reads the Zillow current assessment files, 
#matches each household to a 2010 definition census block group, and retains lot sizes/land use. 

#Output:

#Set to 1 if prototyping code
proto <- 0

nmax = Inf

if (proto == 1) {
  nmax = 100000

}

sf::sf_use_s2(FALSE) 

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

#Dropping Non-mainland states: Alaska (already NOT IN SAMPLE), Hawaii (15), Puerto Rico (72), US Virgin Islands (already NOT IN SAMPLE)
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED[!(US_TRACT_2010_JOINED$State == 15 | US_TRACT_2010_JOINED$State == 72),]

#Joining tracts to block group polygons 
US_BLOCK_2010 <- st_read("Data/US_Data/CensusBlockGroup2010/US_blck_grp_2010.shp") 
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'STATEFP10'] <- 'State'
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'COUNTYFP10'] <- 'County'
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'TRACTCE10'] <- 'Tract' #Renaming these variables to join on State/County/Tract
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'BLKGRPCE10'] <- 'BlockGroup'

#Joining CBSAs with US_TRACT_2010 polygons
US_TRACT_2010_JOINED <- st_drop_geometry(US_TRACT_2010_JOINED)
US_BLOCK_2010 <- left_join(US_BLOCK_2010, US_TRACT_2010_JOINED, by = c("State", "County", "Tract"))
US_BLOCK_2010 <- US_BLOCK_2010[!is.na(US_BLOCK_2010$CBSA),] #Dropping tracts not in CBSA
rm(US_CBSA_2010, US_TRACT_2010_JOINED)

#converting to NAD83
US_BLOCK_2010 <- st_transform(US_BLOCK_2010, 4269)


#Temporary row number for matching
US_BLOCK_2010 <- mutate(US_BLOCK_2010, Temp = row_number())

#Creating data frame with only row number and state/county/tract
US_BLOCK_2010_red <- st_drop_geometry(US_BLOCK_2010 %>% select(State, County, Tract, BlockGroup, GEOID10.x, CBSA, CBSA_NAME, Temp))



#________________________________________________________________________________
#PART2: MATCHING PROPERTIES TO TRACTS + BLOCK GROUPS

#Columns to import that will be used in hedonic regression
columns <- c("ImportParcelID", "LotSizeAcres", "LotSizeSquareFeet", "_Y", "_X", 
             "PropertyLandUseStndCode", "YearBuilt", #These are the minimum standard variables to append in CurrentAssess_append.do
             "YearRemodeled", "BuildingQualityStndCode", "RoofCoverStndCode",
             "RoofStructureTypeStndCode", "FoundationTypeStndCode", "FireplaceNumber",
             "HeatingTypeorSystemStndCode", "ACTypeorSystemStndCode",
             "NoOfStories", "TotalRooms", "TotalBedrooms",
             "RoofCoverStndCode", "RoofStructureTypeStndCode",
             "max_area_sqft", "GarageAreaSqFt", "TypeConstructionStndCode",
             "FullBath", "HalfBath", "BathSourceStndCode",
             "WaterStndCode", "SewerStndCode",
             "TotalMarketValue", "MarketValueYear", "NoOfUnits") #Set of desired columns to import 


#Region0 dataset contains all current assessments outside BSH (2021) geography
#Sample contains all assessments inside the same geography

#REGION_0
#Takes very long to import
Region0 <- read_dta("Data/ZillowData/currentassess_sample/currentassess_zillow_region0.dta",
                    col_select = columns, n_max = nmax)  

names(Region0) <- gsub("\\_", "", names(Region0)) # deleting underscores from column names

Region0 <- Region0[!(is.na(Region0$LotSizeAcres) & is.na(Region0$LotSizeSquareFeet)) & 
                     Region0$PropertyLandUseStndCode != "" & !is.na(Region0$X) &
                     !is.na(Region0$Y), ] #Main sample requires lot sizes and a land use description. 

#transforming Region0 into sf file
Region0["Lat"] <- Region0$Y
Region0["Lon"] <- Region0$X
Region0 <- st_as_sf(Region0, coords = c("X", "Y"))
Region0 <- st_set_crs(Region0, 4269) #

#Matching to CBSAs, 
Region0["Temp"] <- as.integer(st_intersects(Region0, US_BLOCK_2010))

Region0 <- left_join(Region0, US_BLOCK_2010_red, by = c("Temp")) #Takes very, very long to run
Region0 <- Region0[!(is.na(Region0$GEOID10.x)), ]
Region0 <- Region0 %>% select(-Temp) %>% st_drop_geometry() %>% rename(GEOID10 = GEOID10.x)
Region0 <- as.data.frame(Region0)

#Write output to region0

if (proto != 1) { #Dont save if prototyping
  write_dta(Region0, path = "Data/ZillowData/Output/Region0_matched.dta") #OUTPUTS 8million observations in a 1GB file
  rm(Region0)
}



#PART 2.5: CURRENT ASSESSMENT FILES

#Can't import the entire dataset at once, as there are memory issues on a 32GB machine. 

#Prototyping
if (proto == 1) {
  
  for (i in 1:1) { #loop for which to import fraction of data at a time
    i_min = i*nmax - (nmax)
    
    CurrentAssess <- read_dta("Data/ZillowData/currentassess_sample/currentassess_zillow_sample.dta",
                              col_select = columns, n_max = i_max, skip = i_min)  
    
    names(CurrentAssess) <- gsub("\\_", "", names(CurrentAssess)) # deleting underscores from column names
    
    CurrentAssess <- CurrentAssess[!(is.na(CurrentAssess$LotSizeAcres) & is.na(CurrentAssess$LotSizeSquareFeet)) & 
                         CurrentAssess$PropertyLandUseStndCode != "" & !is.na(CurrentAssess$X) &
                         !is.na(CurrentAssess$Y), ] #Main sample requires lot sizes and a land use description. 
    
    CurrentAssess["Lat"] <- CurrentAssess$Y
    CurrentAssess["Lon"] <- CurrentAssess$X
    CurrentAssess <- st_as_sf(CurrentAssess, coords = c("X", "Y"))
    CurrentAssess <- st_set_crs(CurrentAssess, 4269) #
    
    CurrentAssess["Temp"] <- as.integer(st_intersects(CurrentAssess, US_BLOCK_2010))
    
    CurrentAssess <- left_join(CurrentAssess, US_BLOCK_2010_red, by = c("Temp")) #Takes very, very long to run
    CurrentAssess <- CurrentAssess[!(is.na(CurrentAssess$GEOID10.x)), ]
    CurrentAssess <- CurrentAssess %>% select(-Temp) %>% st_drop_geometry() %>% rename(GEOID10 = GEOID10.x)
    
    #write_dta(CurrentAssess, path = paste(paste("Data/ZillowData/Output/CurrentAssess_matched", toString(i), sep = ""), ".dta", sep = "")) Dont save if prototyping
    
  }
  
}

#Not prototyping
if (proto != 1) {
  nmax = 5000000 #5 million observations at a time (I think there are approx 100million in the current assessment files)
  
  for (i in 1:20) { #loop for which to import fraction of data at a time
    i_min = i*nmax - (nmax)
    
    CurrentAssess <- read_dta("Data/ZillowData/currentassess_sample/currentassess_zillow_sample.dta",
                              col_select = columns, n_max = nmax, skip = i_min)  
    
    names(CurrentAssess) <- gsub("\\_", "", names(CurrentAssess)) # deleting underscores from column names
    
    CurrentAssess <- CurrentAssess[!(is.na(CurrentAssess$LotSizeAcres) & is.na(CurrentAssess$LotSizeSquareFeet)) & 
                                     CurrentAssess$PropertyLandUseStndCode != "" & !is.na(CurrentAssess$X) &
                                     !is.na(CurrentAssess$Y), ] #Main sample requires lot sizes and a land use description. 
    
    CurrentAssess["Lat"] <- CurrentAssess$Y
    CurrentAssess["Lon"] <- CurrentAssess$X
    CurrentAssess <- st_as_sf(CurrentAssess, coords = c("X", "Y"))
    CurrentAssess <- st_set_crs(CurrentAssess, 4269) #
    
    CurrentAssess["Temp"] <- as.integer(st_intersects(CurrentAssess, US_BLOCK_2010))
    
    CurrentAssess <- left_join(CurrentAssess, US_BLOCK_2010_red, by = c("Temp")) #Takes very, very long to run
    CurrentAssess <- CurrentAssess[!(is.na(CurrentAssess$GEOID10.x)), ]
    CurrentAssess <- CurrentAssess %>% select(-Temp) %>% st_drop_geometry() %>% rename(GEOID10 = GEOID10.x)
    
    write_dta(CurrentAssess, path = paste(paste("Data/ZillowData/Output/CurrentAssess_matched", toString(i), sep = ""), ".dta", sep = ""))
    rm(CurrentAssess)
    
  }
  
  
}


