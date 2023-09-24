#DATE CREATED:  August 26th, 2022
#Constructing facts for contemporary (2020) data

library(sf)
library(haven) #Reading stata.dta files 
library(dplyr)
library(labelled)
library(readr)
library(collapse)
library(stringr)
library(rlang)

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

#Joining NHGIS Data (ACS 2016-2020)
US_NHGIS <- read_csv("DataV2/US_Data/NHGIS/nhgis_20205_blck_grp.csv")
names(US_NHGIS)[names(US_NHGIS) == 'STATEA'] <- 'State'
names(US_NHGIS)[names(US_NHGIS) == 'COUNTYA'] <- 'County'
names(US_NHGIS)[names(US_NHGIS) == 'TRACTA'] <- 'Tract' #Renaming these variables to join on State/County/Tract
names(US_NHGIS)[names(US_NHGIS) == 'BLCK_GRPA'] <- 'BlockGroup' 
US_NHGIS <- dplyr::select(US_NHGIS, contains(c("State", "County", "Tract", "BlockGroup", "AM")))

#Joining census population and housing counts (2020)
US_NHGIS_cen <- read_csv("DataV2/US_Data/NHGIS/nhgis_2020_blck_grp.csv")
names(US_NHGIS_cen)[names(US_NHGIS_cen) == 'STATEA'] <- 'State'
names(US_NHGIS_cen)[names(US_NHGIS_cen) == 'COUNTYA'] <- 'County'
names(US_NHGIS_cen)[names(US_NHGIS_cen) == 'TRACTA'] <- 'Tract' #Renaming these variables to join on State/County/Tract
names(US_NHGIS_cen)[names(US_NHGIS_cen) == 'BLKGRPA'] <- 'BlockGroup'
US_NHGIS_cen <- dplyr::select(US_NHGIS_cen, contains(c("State", "County", "Tract", "BlockGroup", "U7")))

#Converting to double
US_BLOCK$BlockGroup <- as.double(US_BLOCK$BlockGroup)


#Joining Data
US_BLOCK <- left_join(US_BLOCK, US_NHGIS, by = c("State", "County", "Tract", "BlockGroup"))
US_BLOCK <- left_join(US_BLOCK, US_NHGIS_cen, by = c("State", "County", "Tract", "BlockGroup"))

rm(US_NHGIS, US_NHGIS_cen, US_CBSA_2010)

#__________________________________________________
#PART 2: DATA CONSTRUCTION
US_BLOCK <- US_BLOCK[US_BLOCK$LAND_AREA > 0 & US_BLOCK$U7G001 > 0,] #Taking those with positive land mass and housing units. U7G001 is from the Census.

US_BLOCK$Housing_density <- US_BLOCK$U7G001/US_BLOCK$LAND_AREA #Housing unit density

#Constructing CBSA level housing unit density 
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(City_housing_density = sum(U7G001, na.rm = TRUE)/sum(LAND_AREA, na.rm = TRUE))
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(City_housing_pop = sum(U7G001, na.rm = TRUE))

#Demeaning housing unit density by MSA
US_BLOCK<- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_Housing_density = Housing_density/mean(Housing_density))

#Median housing value subsets to classify cities 
US_CBSA_2010_c <- as.data.frame(collap(US_BLOCK, AMWBE001 + City_housing_density + City_housing_pop ~ CBSA + CBSA_NAME, FUN = c("fmedian")))
names(US_CBSA_2010_c)[names(US_CBSA_2010_c) == 'AMWBE001'] <- 'CBSA_med_house_value'
US_BLOCK <- left_join(US_BLOCK, US_CBSA_2010_c, by = c("CBSA"), suffix = c("", ".y")) %>% select(-ends_with(".y"))

#Constructing ranking of densities
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(rank_density_CBSA = order(order(Housing_density, decreasing = FALSE))/(max(order(order(Housing_density, decreasing = FALSE)))+1))

#Average income from the NHGIS
US_BLOCK["Average_income"] <- US_BLOCK$AMR9E001/US_BLOCK$AMUDE001 #Aggregate income from 2016-2020 ACS/ACS number of households

#Demeaning by MSA, plus calculating average by MSA
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_log_Income = log(Average_income) - mean(log(Average_income), na.rm = TRUE))

#Census white share
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_white_share = U7B003/U7B001 - mean(U7B003/U7B001, na.rm = TRUE))

#ACS college share 
US_BLOCK <- US_BLOCK %>% mutate(college_share = (AMRZE021 + AMRZE022 + AMRZE023 + AMRZE024 + AMRZE025)/AMRZE001)
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_college_share = college_share - mean(college_share, na.rm = TRUE))

#Building age (gentrification cycles-- Bruecker and Rosenthaal (2008) )
US_BLOCK$AMU8E001[US_BLOCK$AMU8E001 == 18] <- NA #18 appears to be some type of error code. removing these observations (only a few hundred)
US_BLOCK$AMU8E001[US_BLOCK$AMU8E001 == 0 & US_BLOCK$AMU7E001 > 0] <- 1939 #if at least one structure is observed built before 1939, and coded 0 -means median is below bottomcode.
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_median_bage = AMU8E001 - mean(AMU8E001, na.rm = TRUE))

#Household size
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_household_size = AMUUE001 - mean(AMUUE001, na.rm = TRUE))

#Family household shares
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_family_share = AMQSE002/AMQSE001 - mean(AMQSE002/AMQSE001, na.rm = TRUE))

#No Car share
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_car_share = (AMVHE003 + AMVHE010)/AMVHE001 - 
                                                    mean((AMVHE003 + AMVHE010)/AMVHE001, na.rm = TRUE))
#Car work transport share
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_car_transport_share = (AMQKE002)/AMQKE001 -
                                                     mean((AMQKE002)/AMQKE001, na.rm = TRUE))
#Public transport share
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_public_transport_share = (AMQKE010)/AMQKE001 -
                                                     mean((AMQKE010)/AMQKE001, na.rm = TRUE))
#Breaking it out into different types
#i.e. bus share of public transport 
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_bus_share = (AMQKE011)/AMQKE001 -
                                                     mean((AMQKE011)/AMQKE001, na.rm = TRUE))


#Average travel time

for (i in 2:13) {
  
  if (i < 10) {
  US_BLOCK[paste("Travel_time_share", i, sep = "")] <- 
    US_BLOCK[[paste("AMQME00", i, sep  = "")]]/US_BLOCK[["AMQME001"]] #left hand side of variable definition
  } 
  
  if (i >= 10) {
    US_BLOCK[paste("Travel_time_share", i, sep = "")] <- 
      US_BLOCK[[paste("AMQME0", i, sep  = "")]]/US_BLOCK[["AMQME001"]] #left hand side of variable definition
  }
  
}

US_BLOCK["avg_travel_time"] <- 2.5*US_BLOCK$Travel_time_share2 + 7.5*US_BLOCK$Travel_time_share3 + 
                               12.5*US_BLOCK$Travel_time_share4 + 17.5*US_BLOCK$Travel_time_share5 +
                               22.5*US_BLOCK$Travel_time_share6 + 27.5*US_BLOCK$Travel_time_share7 + 
                               32.5*US_BLOCK$Travel_time_share8 + 37.5*US_BLOCK$Travel_time_share8 + 
                               42.5*US_BLOCK$Travel_time_share10 + 52.5*US_BLOCK$Travel_time_share11 +
                               75*US_BLOCK$Travel_time_share12 + 90*US_BLOCK$Travel_time_share13

US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_avg_travel_time = avg_travel_time - mean(avg_travel_time, na.rm = TRUE))


#Regulated housing share (note this does not capture a notion of stringency)
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_regulated_housing_share = (AMU5E003 + AMU5E004 + AMU5E005 + AMU5E006 +
                                                                                       AMU5E014 + AMU5E015 + AMU5E016 + AMU5E017)/AMU5E001 - 
                                                                                       mean((AMU5E003 + AMU5E004 + AMU5E005 + AMU5E006 +
                                                                                               AMU5E014 + AMU5E015 + AMU5E016 + AMU5E017)/AMU5E001, na.rm = TRUE))



#Centiles of CBSAs in terms of median-of-median house prices
quantile_CBSA_houseval <- quantile(US_CBSA_2010_c$CBSA_med_house_value, probs = seq(0, 1, 0.005), na.rm = TRUE)
quantile_CBSA_dens <- quantile(US_CBSA_2010_c$City_housing_density, probs = seq(0, 1, 0.005), na.rm = TRUE)
quantile_CBSA_pop <-quantile(US_CBSA_2010_c$City_housing_pop, probs = seq(0, 1, 0.005), na.rm = TRUE)

US_BLOCK <- st_set_geometry(US_BLOCK, US_BLOCK$geometry) #for some reason geometries were dropped before, resetting
US_BLOCK <- select(US_BLOCK, contains(c("State", "County", "Tract", "BlockGroup", "CBSA", "CBSA_NAME", "NAME",
                                      "rank", "density", "pop", "Income", "demeaned", "geometry")))



#Saving Data for use with other programs
save(US_BLOCK, file = "DataV2/US_Data/Output/Constructed_Block.Rdata")
save(US_CBSA_2010_c, file = "DataV2/US_Data/Output/CBSA_med_house_price.Rdata")
save(quantile_CBSA_houseval, file = "DataV2/US_Data/Output/CBSA_quantiles.Rdata")
save(quantile_CBSA_dens, file = "DataV2/US_Data/Output/CBSA_quantiles_dens.Rdata")
save(quantile_CBSA_pop, file = "DataV2/US_Data/Output/CBSA_quantiles_pop.Rdata")

rm(list = ls())