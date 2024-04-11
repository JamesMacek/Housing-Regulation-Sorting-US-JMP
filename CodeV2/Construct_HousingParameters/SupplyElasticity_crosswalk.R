
#This file transforms 2020 census tract estimates of housing supply elasticities to 2020 census tracts. 
library(dplyr)
library(haven)
library(labelled)
library(sf)
library(collapse)

#Creating sample geography shapefile

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
                                   
#Reading in housing supply elasticity estimates
BSH <- read_dta("DataV2/US_Data/BSH_HousingElasticities/gammas_hat_all.dta")

BSH <- BSH %>% select(ctracts2000, gamma01b_space_IV, gamma01b_devl_IV) %>% rename(HS_Elasticity = gamma01b_space_IV, Land_Elasticity = gamma01b_devl_IV)


#Taking average within census tracts (census tracts are split across regions because of LODES set up in the paper)
#We want one elasticity for a given census tract...
BSH <- collap(BSH, HS_Elasticity + Land_Elasticity ~ ctracts2000)

#Merging to our crosswalk
US_JOINED <- left_join(US_JOINED, BSH, by = ("ctracts2000"))

#Assinging land area 
US_JOINED["land_mass"] <- st_area(US_JOINED)
US_JOINED["MissingElast_indicator"] <- ifelse(is.na(US_JOINED$HS_Elasticity), 1, 0) #Missing elasticities... 
US_JOINED["MissingLandElast_indicator"] <- ifelse(is.na(US_JOINED$Land_Elasticity), 1, 0) #Missing elasticities..
US_JOINED$land_mass[US_JOINED$MissingElast_indicator == 1] <- NA #setting NA's for weights that correspond to observations with missing elasticities.
US_JOINED$land_mass[US_JOINED$MissingLandElast_indicator == 1] <- NA #setting NA's for weights that correspond to observations with missing elasticities.

US_JOINED <- US_JOINED %>% st_drop_geometry()


#Aggregating up using land mass as weights to the 2020 block group level 
#Collapsing by State County Tract BlockGroup
US_BLOCK <- collap(US_JOINED, HS_Elasticity + Land_Elasticity ~ State + County + Tract + BlockGroup + CBSA + CBSA_NAME, FUN = fmean, w = US_JOINED$land_mass)

#Collapsing again by CBSA to get CBSA average
US_CBSA <-  collap(US_BLOCK, HS_Elasticity + Land_Elasticity ~ CBSA + CBSA_NAME, FUN = fmean, w = US_BLOCK$land_mass)
US_CBSA <- US_CBSA %>% select(CBSA, CBSA_NAME, HS_Elasticity, Land_Elasticity) %>% rename(CBSA_HS_Elasticity = HS_Elasticity, CBSA_Land_Elasticity = Land_Elasticity)

US_BLOCK <- left_join(US_BLOCK, US_CBSA, by = c("CBSA", "CBSA_NAME"))

#Censoring all negative supply elasticities to zero
US_BLOCK$HS_Elasticity[US_BLOCK$HS_Elasticity <= 0 & !is.na(US_BLOCK$HS_Elasticity)] <- 0
US_BLOCK$Land_Elasticity[US_BLOCK$Land_Elasticity <= 0 & !is.na(US_BLOCK$Land_Elasticity)] <- 0

#Imputing missing HS_elasticities with MSA average
#Final floorspace supply elasticity (holding land supply variable and land supply fixed)
US_BLOCK["HS_Elasticity_imputed"] <- US_BLOCK$HS_Elasticity
US_BLOCK["HS_Elasticity_imputed_noland"] <- US_BLOCK$HS_Elasticity - US_BLOCK$Land_Elasticity


US_BLOCK$HS_Elasticity_imputed[is.na(US_BLOCK$HS_Elasticity_imputed)] <- 0
US_BLOCK$HS_Elasticity_imputed_noland[is.na(US_BLOCK$HS_Elasticity_imputed_noland)] <- 0
US_BLOCK$HS_Elasticity_imputed <- US_BLOCK$HS_Elasticity_imputed + ifelse(is.na(US_BLOCK$HS_Elasticity), 1, 0)*(US_BLOCK$CBSA_HS_Elasticity)
US_BLOCK$HS_Elasticity_imputed_noland <- US_BLOCK$HS_Elasticity_imputed_noland + ifelse(is.na(US_BLOCK$HS_Elasticity), 1, 0)*(US_BLOCK$CBSA_HS_Elasticity - US_BLOCK$CBSA_Land_Elasticity)

#Imputing remaining by national average (weighting all block groups evenly)
#Removing remaining 1000 block groups without supply elasticity
US_BLOCK <- US_BLOCK[!is.na(US_BLOCK$HS_Elasticity_imputed),] #Only 1000 block groups -- st george, Utah

US_BLOCK <- US_BLOCK %>% select(-land_mass)
#Only 1000 block groups are imputed with national average. 

#coercing to numeric to write dta
for (colNames in colnames(US_BLOCK)) {
  if (colNames != "CBSA_NAME") {
    US_BLOCK[[colNames]] <- as.numeric(US_BLOCK[[colNames]])
  }
}


#Saving in .dta format.
write_dta(US_BLOCK, "DataV2/US_Data/Output/HS_Elasticities.dta")

