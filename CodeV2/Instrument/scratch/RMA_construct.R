#This file constructs RMA (temp using Nates dataset), merges from 2000 tracts to 2020 block groups
library(dplyr)
library(haven)
library(labelled)
library(sf)
library(collapse)

US_CBSA_2010 <- st_read("DataV2/US_Data/Shapefiles/cb_2013_us_cbsa_500k.shp") #2013 MSAs
US_CBSA_2010 <- US_CBSA_2010[US_CBSA_2010$LSAD == "M1",]
US_CBSA_2010 <- US_CBSA_2010 %>% rename(CBSA = CBSAFP)


US_BLOCK <- st_read("DataV2/US_Data/Shapefiles/US_blck_grp_2020.shp") #From IPUMS NHGIS
US_BLOCK_CENTRIOD <- st_centroid(US_BLOCK)
US_BLOCK_CENTRIOD <- st_transform(US_BLOCK_CENTRIOD, crs = 4269)


#Matching block group centroids to CBSA polygons are sufficient for accuracy.  
US_BLOCK["Temp"] <- as.integer(st_intersects(US_BLOCK_CENTRIOD, US_CBSA_2010)) #Centroid matched to polygon by observation in US_CBSA_2010
US_CBSA_2010 <- mutate(US_CBSA_2010, Temp = row_number()) #Temp used to join MSA names with matched centroids

#Matching CBSA FIPS Code to US_TRACT_2010_CENTRIOD data.
#Data frame containing names of CBSA, unique CBSA code, etc. 
Temp_tojoin <- US_CBSA_2010 %>%
  dplyr::select(Temp, NAME, CBSA)
Temp_tojoin <- as.data.frame(Temp_tojoin) #converting for left_join
Temp_tojoin <- Temp_tojoin[, !colnames(Temp_tojoin) %in% "geometry"] #deleting geometry data from sf object to join 
US_BLOCK <- left_join(US_BLOCK, Temp_tojoin, by = "Temp")
rm(Temp_tojoin, US_BLOCK_CENTRIOD)

#Renaming
US_BLOCK <- US_BLOCK[, !colnames(US_BLOCK) %in% "Temp"] #removing variable used solely for matching
names(US_BLOCK)[names(US_BLOCK) == 'NAME'] <- 'CBSA_NAME'
names(US_BLOCK)[names(US_BLOCK) == 'STATEFP'] <- 'State'
names(US_BLOCK)[names(US_BLOCK) == 'COUNTYFP'] <- 'County'
names(US_BLOCK)[names(US_BLOCK) == 'TRACTCE'] <- 'Tract' #Renaming these variables to join on State/County/Tract
names(US_BLOCK)[names(US_BLOCK) == 'BLKGRPCE'] <- 'BlockGroup' #Renaming these variables to join on State/County/Tract
names(US_BLOCK)[names(US_BLOCK) == 'ALAND'] <- 'LAND_AREA' 

#Dropping tracts not in used CBSA
US_BLOCK<- US_BLOCK[!is.na(US_BLOCK$CBSA),] #200,670 block groups remain.

US_BLOCK <- US_BLOCK[!(US_BLOCK$State == 15 | US_BLOCK$State == 72 | US_BLOCK$State == '02'),]
US_BLOCK <- US_BLOCK %>% dplyr::select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, LAND_AREA) #197,062 block groups remain.


#PART 2: Merging with 2000 census tract geography
US_TRACT_2000 <- st_read("DataV2/US_Data/Shapefiles/US_tract_2000.shp")

#Creating overlap shapefile (st intersects)
US_JOINED <- st_join(US_BLOCK, US_TRACT_2000)
rm(US_BLOCK, US_TRACT_2000)

#Creating geoid codes to join with housing supply elasticity estimates
US_JOINED["ctracts2000"] <- paste0(substr(US_JOINED$GISJOIN2, 1, 2), substr(US_JOINED$GISJOIN2, 4, 6),
                                   substr(US_JOINED$GISJOIN2, 8, 13)) 
US_JOINED <- US_JOINED %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME,
                                  ctracts2000)

#Importing RMA at 2000 ctract level
RMA <- read_dta("DataV2/US_Data/BSH_HousingElasticities/RMA_raw.dta")
RMA <- collap(RMA, RMA00_0 + RMA10_0 ~ ctracts2000) #collapsing to unique tract

#Merging to our crosswalk
US_JOINED <- left_join(US_JOINED, RMA, by = ("ctracts2000"))
US_JOINED["land_mass"] <- st_area(US_JOINED)

#Collapsing to State County Tract BlockGroup
US_JOINED <- US_JOINED %>% st_drop_geometry()
US_BLOCK <- collap(US_JOINED, RMA00_0 + RMA10_0 ~ State + County + Tract + BlockGroup + CBSA + CBSA_NAME, FUN = fmean, w = US_JOINED$land_mass)

US_BLOCK <- US_BLOCK %>% select(-land_mass)
#Only 1000 block groups are imputed with national average. 

#coercing to numeric to write dta
for (colNames in colnames(US_BLOCK)) {
  if (colNames != "CBSA_NAME") {
    US_BLOCK[[colNames]] <- as.numeric(US_BLOCK[[colNames]])
  }
}


#Saving in .dta format.
write_dta(US_BLOCK, "DataV2/US_Data/Instrument/RMA_ctrl.dta")


