#DATE CREATED:  August 26th, 2022
#Constructing facts for contemporary (2020) data.
#Requires output from city productivity files

library(sf)
library(haven) #Reading stata.dta files 
library(dplyr)
library(labelled)
library(readr)
library(collapse)
library(stringr)
library(rlang)

#Importing sample geography (see construction of current assessments)
US_BLOCK <- read_dta("DataV2/US_Data/Output/SampleGeography.dta") %>% rename(LAND_AREA = ALAND) #Note: Land area measured in acres, see currentAssess_construct.R

#Joining all NHGIS Data (ACS 2016-2020)
US_NHGIS <- read_csv("DataV2/US_Data/NHGIS/nhgis_20205_blck_grp.csv") %>% mutate(State = as.numeric(STATEA),
                                                                                 County = as.numeric(COUNTYA),
                                                                                 Tract = as.numeric(TRACTA),
                                                                                 BlockGroup = as.numeric(BLKGRPA))
US_NHGIS <- dplyr::select(US_NHGIS, State, County, Tract, BlockGroup, contains(c("AM"))) %>% select(-NAME_E)

#Joining census population and housing counts (2020)
US_NHGIS_cen <- read_csv("DataV2/US_Data/NHGIS/nhgis_2020_blck_grp.csv") %>% mutate(State = as.numeric(STATEA),
                                                                                    County = as.numeric(COUNTYA),
                                                                                    Tract = as.numeric(TRACTA),
                                                                                    BlockGroup = as.numeric(BLKGRPA))
US_NHGIS_cen <- dplyr::select(US_NHGIS_cen, State, County, Tract, BlockGroup, contains(c("U7")))

#Joining geometry 
US_BLOCK_GEO <- st_read("DataV2/US_Data/Shapefiles/US_blck_grp_2020.shp") %>% mutate(State = as.numeric(STATEFP),
                                                                                     County = as.numeric(COUNTYFP),
                                                                                     Tract = as.numeric(TRACTCE),
                                                                                     BlockGroup = as.numeric(BLKGRPCE)) %>%
                                                                              select(State, County, Tract, BlockGroup, geometry)#From IPUMS NHGIS 

#Joining Data
US_BLOCK <- left_join(US_BLOCK, US_NHGIS, by = c("State", "County", "Tract", "BlockGroup"))
US_BLOCK <- left_join(US_BLOCK, US_NHGIS_cen, by = c("State", "County", "Tract", "BlockGroup"))
US_BLOCK <- left_join(US_BLOCK_GEO, US_BLOCK, by = c("State", "County", "Tract", "BlockGroup"))
US_BLOCK <- US_BLOCK[!is.na(US_BLOCK$LAND_AREA),] #Keeping sample block groups

rm(US_NHGIS, US_NHGIS_cen, US_BLOCK_GEO)

#__________________________________________________
#PART 2: DATA CONSTRUCTION
US_BLOCK <- US_BLOCK[US_BLOCK$LAND_AREA > 0 & US_BLOCK$U7G001 > 0,] #Taking those with positive land mass and housing units. U7G001 is from the Census.

US_BLOCK$Housing_density <- US_BLOCK$U7G001/US_BLOCK$LAND_AREA #Housing unit density

#Constructing CBSA level housing unit density 
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(City_housing_density = sum(U7G001, na.rm = TRUE)/sum(LAND_AREA, na.rm = TRUE))
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(City_housing_pop = sum(U7G001, na.rm = TRUE))

#Matching city productivity (via mincer regression in ) 
prod <- read_dta("DataV2/US_Data/Output/CityProd_individual.dta") %>% select(CBSA, PooledWage)
US_BLOCK <- left_join(US_BLOCK, prod, by = c("CBSA"))


#Demeaning housing unit density by MSA
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_Housing_density = Housing_density/mean(Housing_density))

#Median housing value subsets to classify cities 
US_CBSA_2010_c <- as.data.frame(collap(US_BLOCK, AMWBE001 + City_housing_density + City_housing_pop + PooledWage ~ CBSA + CBSA_NAME, FUN = c("fmedian")))
US_BLOCK$PooledWage <- NULL
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



#Centiles of CBSAs in terms of median-of-median house prices + other variables
quantile_CBSA_houseval <- quantile(US_CBSA_2010_c$CBSA_med_house_value, probs = seq(0, 1, 0.005), na.rm = TRUE)
quantile_CBSA_dens <- quantile(US_CBSA_2010_c$City_housing_density, probs = seq(0, 1, 0.005), na.rm = TRUE)
quantile_CBSA_pop <-quantile(US_CBSA_2010_c$City_housing_pop, probs = seq(0, 1, 0.005), na.rm = TRUE)
quantile_CBSA_wage <- quantile(US_CBSA_2010_c$PooledWage, probs = seq(0, 1, 0.005), na.rm = TRUE)

US_BLOCK <- st_set_geometry(US_BLOCK, US_BLOCK$geometry) #for some reason geometries were dropped before, resetting
US_BLOCK <- select(US_BLOCK, contains(c("State", "County", "Tract", "BlockGroup", "CBSA", "CBSA_NAME", "NAME", "LAND_AREA",
                                        "rank", "density", "pop", "Income", "demeaned", "PooledWage", "geometry")))

#__________________________2008-2012 ACS HISTORICAL VARIABLES___________________________________________________
#Doing the same procedure for historical variables to aid in structural estimation. 


#Joining all NHGIS Data (ACS 2008-2012)
US_NHGIS <- read_csv("DataV2/US_Data/NHGIS/nhgis_20105_blck_grp.csv") %>% select(GISJOIN, contains("Q"))

#Joining census population and housing counts (2010)
US_NHGIS_cen <- read_csv("DataV2/US_Data/NHGIS/nhgis_2010_blck_grp.csv") %>% select(GISJOIN, contains("IFC"))


US_BLOCK_hist <- left_join(US_NHGIS, US_NHGIS_cen, by = c("GISJOIN"))
rm(US_NHGIS, US_NHGIS_cen)

#Reformatting GISJOIN for block group crosswalk to 2020 geography
US_BLOCK_hist["GEOID_2012"] <- paste0(substr(US_BLOCK_hist$GISJOIN, 2, 3), substr(US_BLOCK_hist$GISJOIN, 5, 7),
                                      substr(US_BLOCK_hist$GISJOIN, 9, 14), substr(US_BLOCK_hist$GISJOIN, 15, 15))
US_BLOCK_hist <- US_BLOCK_hist %>% select(-GISJOIN)

#Importing crosswalk to 2020 geography for merge
crosswalk <- read_dta("DataV2/US_Data/Shapefiles/blkgrp_2010_2020_crosswalk.dta")
US_BLOCK_hist <- left_join(US_BLOCK_hist, crosswalk, by = c("GEOID_2012"))
rm(crosswalk)

#Collapsing all data down to 2020 geography -- note: we are taking means of count data but that is OK, w
#Will be partialed away in long differences as an intercept in any regression that uses long differences 2010-2020
#Making sure weights sum to one within each GEOID_2020
US_BLOCK_hist <- US_BLOCK_hist %>% group_by(GEOID_2020) %>% mutate(norm_wt_hh = wt_hh/sum(wt_hh, na.rm = TRUE))
US_BLOCK_hist <- US_BLOCK_hist %>% select(-GEOID_2012, - wt_hh)
US_BLOCK_hist <- collap(US_BLOCK_hist, by = ~ GEOID_2020,  w = US_BLOCK_hist$norm_wt_hh, keep.w = FALSE) %>% ungroup()

#Reformatting State/County/Tract/BlockGroup
US_BLOCK_hist <- US_BLOCK_hist %>% mutate(State = as.numeric(substr(GEOID_2020, 1, 2)),
                                          County = as.numeric(substr(GEOID_2020, 3, 5)),
                                          Tract = as.numeric(substr(GEOID_2020, 6, 11)),
                                          BlockGroup = as.numeric(substr(GEOID_2020, 12, 12)))
US_BLOCK_hist <- US_BLOCK_hist %>% select(-GEOID_2020)

#Merging to US_BLOCK
US_BLOCK <- left_join(US_BLOCK, US_BLOCK_hist, by = c("State", "County", "Tract", "BlockGroup"))
rm(US_BLOCK_hist)

#________________________________________
#Finally, commence variable construction:

#Historical income
US_BLOCK["Average_income_hist"] <- US_BLOCK$QVBE001/US_BLOCK$QX6E001 #Aggregate income from 2016-2020 ACS/ACS number of households
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_log_Income_hist = log(Average_income_hist) - mean(log(Average_income_hist), na.rm = TRUE))

#Housing density
US_BLOCK["Housing_density_hist"] <- US_BLOCK$IFC001/US_BLOCK$LAND_AREA #Housing unit density
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_Housing_density_hist = Housing_density_hist/mean(Housing_density_hist))

#Census white share
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_white_share_hist = QYJE002/QYJE001 - mean(QYJE003/QYJE001, na.rm = TRUE))

#ACS college share 
US_BLOCK <- US_BLOCK %>% mutate(college_share_hist = (QUSE021 + QUSE022 + QUSE023 + QUSE024 + QUSE025)/QUSE001)
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_college_share_hist = college_share_hist - mean(college_share_hist, na.rm = TRUE))

#Building age (gentrification cycles-- Bruecker and Rosenthaal (2008) )
US_BLOCK$QY2E001[US_BLOCK$QY2E001 == 18] <- NA #18 appears to be some type of error code. removing these observations (only a few hundred)
US_BLOCK$QY2E001[US_BLOCK$QY2E001 == 0 & US_BLOCK$QY1E001 > 0] <- 1939 #if at least one structure is observed built before 1939, and coded 0 -means median is below bottomcode.
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_median_bage_hist = QY2E001 - mean(QY2E001, na.rm = TRUE))

#Family household shares
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_family_share_hist = QTME002/QTME001 - mean(QTME002/QTME001, na.rm = TRUE))

#Car work transport share
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_car_transport_share_hist = (QTFE002)/QTFE001 -
                                                     mean((QTFE002)/QTFE001, na.rm = TRUE))
#Public transport share
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_public_transport_share_hist = (QTFE010)/QTFE001 -
                                                     mean((QTFE010)/QTFE001, na.rm = TRUE))
#Breaking it out into different types
#i.e. bus share of public transport 
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_bus_share_hist = (QTFE011)/QTFE001 -
                                                     mean((QTFE011)/QTFE001, na.rm = TRUE))
#Average travel time

for (i in 2:13) {
  
  if (i < 10) {
    US_BLOCK[paste("Travel_time_share", i, sep = "")] <- 
      US_BLOCK[[paste("QTHE00", i, sep  = "")]]/US_BLOCK[["QTHE001"]] #left hand side of variable definition
  } 
  
  if (i >= 10) {
    US_BLOCK[paste("Travel_time_share", i, sep = "")] <- 
      US_BLOCK[[paste("QTHE0", i, sep  = "")]]/US_BLOCK[["QTHE001"]] #left hand side of variable definition
  }
  
}

US_BLOCK["avg_travel_time_hist"] <- 2.5*US_BLOCK$Travel_time_share2 + 7.5*US_BLOCK$Travel_time_share3 + 
                                   12.5*US_BLOCK$Travel_time_share4 + 17.5*US_BLOCK$Travel_time_share5 +
                                   22.5*US_BLOCK$Travel_time_share6 + 27.5*US_BLOCK$Travel_time_share7 + 
                                   32.5*US_BLOCK$Travel_time_share8 + 37.5*US_BLOCK$Travel_time_share8 + 
                                   42.5*US_BLOCK$Travel_time_share10 + 52.5*US_BLOCK$Travel_time_share11 +
                                   75*US_BLOCK$Travel_time_share12 + 90*US_BLOCK$Travel_time_share13

US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(demeaned_avg_travel_time_hist = avg_travel_time_hist - mean(avg_travel_time_hist, na.rm = TRUE))



#Selecting variables we want
US_BLOCK <- select(US_BLOCK, contains(c("State", "County", "Tract", "BlockGroup", "CBSA", "CBSA_NAME", "NAME", "LAND_AREA",
                                        "rank", "density", "pop", "Income", "demeaned", "PooledWage", "geometry")))



#Saving Data for use with other programs
save(US_BLOCK, file = "DataV2/US_Data/Output/Constructed_Block.Rdata")
save(US_CBSA_2010_c, file = "DataV2/US_Data/Output/CBSA_med_house_price.Rdata")
save(quantile_CBSA_houseval, file = "DataV2/US_Data/Output/CBSA_quantiles.Rdata")
save(quantile_CBSA_dens, file = "DataV2/US_Data/Output/CBSA_quantiles_dens.Rdata")
save(quantile_CBSA_pop, file = "DataV2/US_Data/Output/CBSA_quantiles_pop.Rdata")
save(quantile_CBSA_wage, file = "DataV2/US_Data/Output/CBSA_quantiles_wage.Rdata")

rm(list = ls())