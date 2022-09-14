#DATE CREATED:  August 26th, 2022
#Constructing facts such that

library(sf)
library(haven) #Reading stata.dta files 
library(dplyr)
library(labelled)
library(readr)
library(collapse)
library(stringr)

US_CBSA_2010 <- st_read("DataV2/US_Data/Shapefiles/cb_2013_us_cbsa_500k.shp") #2013 MSAs
US_CBSA_2010 <- US_CBSA_2010[US_CBSA_2010$LSAD == "M1",]
US_CBSA_2010 <- US_CBSA_2010 %>% rename(CBSA = CBSAFP)


US_BLOCK <- st_read("DataV2/US_Data/Shapefiles/US_blck_grp_2020.shp") #From IPUMS NHGIS
US_BLOCK_CENTRIOD <- st_centroid(US_BLOCK)
rm(US_BLOCK)
US_BLOCK_CENTRIOD <- st_transform(US_BLOCK_CENTRIOD, 4269)

#Matching block group centroids to CBSA polygons are sufficient for accuracy.  
US_BLOCK_CENTRIOD["Temp"] <- as.integer(st_intersects(US_BLOCK_CENTRIOD, US_CBSA_2010)) #Centroid matched to polygon by observation in US_CBSA_2010
US_CBSA_2010 <- mutate(US_CBSA_2010, Temp = row_number()) #Temp used to join MSA names with matched centroids

#Matching CBSA FIPS Code to US_TRACT_2010_CENTRIOD data.
#Data frame containing names of CBSA, unique CBSA code, etc. 
Temp_tojoin <- US_CBSA_2010 %>%
  select(Temp, NAME, CBSA)
Temp_tojoin <- as.data.frame(Temp_tojoin) #converting for left_join
Temp_tojoin <- Temp_tojoin[, !colnames(Temp_tojoin) %in% "geometry"] #deleting geometry data from sf object to join 
US_BLOCK <- left_join(US_BLOCK_CENTRIOD, Temp_tojoin, by = "Temp")
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
US_BLOCK <- st_drop_geometry(US_BLOCK) %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, LAND_AREA) #197,385 block groups remain. 

#Joining NHGIS Data 
US_NHGIS <- read_csv("DataV2/US_Data/NHGIS/nhgis_20205_blck_grp.csv")
names(US_NHGIS)[names(US_NHGIS) == 'STATEA'] <- 'State'
names(US_NHGIS)[names(US_NHGIS) == 'COUNTYA'] <- 'County'
names(US_NHGIS)[names(US_NHGIS) == 'TRACTA'] <- 'Tract' #Renaming these variables to join on State/County/Tract
names(US_NHGIS)[names(US_NHGIS) == 'BLCK_GRPA'] <- 'BlockGroup' 

#Converting to double
US_BLOCK$BlockGroup <- as.double(US_BLOCK$BlockGroup)


#Joining Data
US_BLOCK <- left_join(US_BLOCK, US_NHGIS, by = c("State", "County", "Tract", "BlockGroup"))

rm(US_NHGIS)
rm(US_CBSA_2010)

#__________________________________________________
#PART 2: DATA CONSTRUCTION
US_BLOCK <- US_BLOCK[US_BLOCK$LAND_AREA > 0 & US_BLOCK$AMUDE001 > 0,] #Taking those with positive land mass and housing units

US_BLOCK$Housing_density <- US_BLOCK$AMUDE001/US_BLOCK$LAND_AREA #Housing unit density
#Demeaning housing unit density by MSA
US_BLOCK<- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_Housing_density = Housing_density/mean(Housing_density))

#Median housing value subsets to classify cities 
US_CBSA_2010_c <- as.data.frame(collap(US_BLOCK, AMWBE001 ~ CBSA + CBSA_NAME, FUN = c("fmedian")))
names(US_CBSA_2010_c)[names(US_CBSA_2010_c) == 'AMWBE001'] <- 'CBSA_med_house_value'
US_BLOCK <- left_join(US_BLOCK, US_CBSA_2010_c, by = c("CBSA"), suffix = c("", ".y")) %>% select(-ends_with(".y"))

#Constructing ranking of densities
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(rank_density_CBSA = order(order(Housing_density, decreasing = FALSE))/(max(order(order(Housing_density, decreasing = FALSE)))+1))

#Average income from the NHGIS
US_BLOCK["Average_income"] <- US_BLOCK$AMR9E001/US_BLOCK$AMUDE001 #Aggregate income from 2016-2020 ACS/ACS number of households

#Demeaning by MSA, plus calculating average by MSA
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_log_Income = log(Average_income) - mean(log(Average_income), na.rm = TRUE))
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_Avg_Income = Average_income/mean(Average_income, na.rm = TRUE))

#Centiles pf CBSAs in terms of median-of-median house prices: look at density distributions
quantile_CBSA_houseval <- quantile(US_CBSA_2010_c$CBSA_med_house_value, probs = seq(0, 1, 0.005), na.rm = TRUE)



#Saving Data for use with other programs
save(US_BLOCK, file = "DataV2/US_Data/Output/Constructed_Block.Rdata")
save(US_CBSA_2010_c, file = "DataV2/US_Data/Output/CBSA_med_house_price.Rdata")
save(quantile_CBSA_houseval, file = "DataV2/US_Data/Output/CBSA_quantiles.Rdata")