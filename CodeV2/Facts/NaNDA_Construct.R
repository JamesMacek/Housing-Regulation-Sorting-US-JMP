#DATE CREATED:  October 21st 2022
#Constructing facts for contemporary (2020) data from the NaNDa Database

library(sf)
library(haven) #Reading stata.dta files 
library(dplyr)
library(labelled)
library(readr)
library(collapse)
library(stringr)
library(rlang)


#Importing NaNDa datasets (vary at the 2010 tract level)
nanda_arts <- read_dta("DataV2/US_Data/NANDA/nanda_artsentrec_tract_2003-2017_01P.dta")
nanda_arts <- nanda_arts[nanda_arts$year == 2017,] #keeping latest date (2017)
nanda_arts <- nanda_arts %>% select(tract_fips10, aden_7111,
                     aden_7112, aden_7132, aden_7139) %>% rename(perf_arts_dens = aden_7111, 
                                                                 spec_sports_dens = aden_7112,
                                                                 casino_dens = aden_7132,
                                                                 rec_act_dens = aden_7139)

nanda_parks <- read_dta("DataV2/US_Data/NANDA/nanda_parks_tract_2018_01P.dta")
nanda_parks <- nanda_parks %>% select(tract_fips10, prop_park_area_tract)

nanda_pollution <- read_dta("DataV2/US_Data/NANDA/nanda_pollutst_tract_2000-2018_01P.dta")
nanda_pollution <- nanda_pollution[nanda_pollution$year == 2018,] %>% select(-year) #extracting 2018 data

nanda_transit <- read_dta("DataV2/US_Data/NANDA/nanda_transit_tract_2016-2018_05P.dta")
nanda_transit <- nanda_transit %>% rename(tract_fips10 = tract_fips) %>% select(tract_fips10, stops_per_sqmile)

nanda_eat <- read_dta("DataV2/US_Data/NANDA/nanda_eatdrink_tract_2003-2017_01P.dta")
nanda_eat <- nanda_eat[nanda_eat$year == 2017,] %>% rename(frestaurant_dens = aden_sales_722511,
                                                           fastfood_dens = aden_sales_722513,
                                                           coffee_dens = aden_sales_722515,
                                                           bar_dens = aden_sales_722410) %>% 
                                                    select(tract_fips10, contains("dens"))
nanda_land <- read_dta("DataV2/US_Data/NANDA/nanda_landcover_tract_2001-2016_02P.dta")
nanda_land <- nanda_land[nanda_land$year_intp == 2016,] %>% 
                                                          select(prop_value_12, prop_value_41,
                                                                 prop_value_42, prop_value_43,
                                                                 prop_value_52, prop_value_71,
                                                                 prop_value_90, prop_value_95,
                                                                 tract_fips) %>%
                                                          rename(perennial_snow = prop_value_12,
                                                                   deciduous_forest = prop_value_41,
                                                                   evergreen_forest = prop_value_42,
                                                                   mixed_forest = prop_value_43,
                                                                   shrubs = prop_value_52,
                                                                   herbaceous = prop_value_71,
                                                                   woody_wetlands = prop_value_90,
                                                                   herbaceous_wetlands = prop_value_95,
                                                                   tract_fips10 = tract_fips)                                                                                                    


#Parsing state_county_tracts from this definition. 
main <- full_join(nanda_arts, nanda_parks, by = c("tract_fips10"))
main <- full_join(main, nanda_pollution, by = c("tract_fips10"))
main <- full_join(main, nanda_transit, by = c("tract_fips10"))
main <- full_join(main, nanda_eat, by = c("tract_fips10"))
main <- full_join(main, nanda_land, by = c("tract_fips10"))

rm(nanda_arts, nanda_parks, nanda_pollution, nanda_transit, nanda_eat, nanda_land)
main["State"] <- substring(main$tract_fips10, 1, 2)
main["County"] <- substring(main$tract_fips10, 3, 5)
main["Tract"] <- substring(main$tract_fips10, 6, 12)

main$State <- as.double(main$State)
main$County <- as.double(main$County)
main$Tract <- as.double(main$Tract)

main <- main %>% select(-tract_fips10)

#Importing relevant geographies for crosswalk
Block_geo_2020 <- st_read("DataV2/US_Data/Shapefiles/US_blck_grp_2020.shp") #2020 block groups
Block_geo_2020 <- st_centroid(Block_geo_2020) #Extracting centroids for merge
Tract_geo_2010 <- st_read("Data/US_Data/CensusTract2010/US_tract_2010.shp") #2010 Tracts

#Renaming variables for merge
names(Tract_geo_2010)[names(Tract_geo_2010) == 'STATEFP10'] <- 'State'
names(Tract_geo_2010)[names(Tract_geo_2010) == 'COUNTYFP10'] <- 'County'
names(Tract_geo_2010)[names(Tract_geo_2010) == 'TRACTCE10'] <- 'Tract' #Renaming these variables to join on State/County/Tract

names(Block_geo_2020)[names(Block_geo_2020) == 'STATEFP'] <- 'State'
names(Block_geo_2020)[names(Block_geo_2020) == 'COUNTYFP'] <- 'County'
names(Block_geo_2020)[names(Block_geo_2020) == 'TRACTCE'] <- 'Tract' #Renaming these variables to join on State/County/Tract
names(Block_geo_2020)[names(Block_geo_2020) == 'BLKGRPCE'] <- 'BlockGroup' #Renaming these variables to join on State/County/Tract

Tract_geo_2010$State <- as.double(Tract_geo_2010$State)
Tract_geo_2010$County <- as.double(Tract_geo_2010$County)
Tract_geo_2010$Tract <- as.double(Tract_geo_2010$Tract)

Block_geo_2020$BlockGroup <- as.double(Block_geo_2020$BlockGroup)


#Joining tract data to the 2010 geography on 2020 block group centroids
Tract_geo_2010 <- left_join(Tract_geo_2010, main, by = c("State", "County", "Tract")) #for some reason, some are not joined because of weird FIPS codes in the NaNDA sample. NanDA has more tracts than the official 2010 shapefiles?

vars_to_merge <- c("perf_arts_dens", "spec_sports_dens", 
                   "casino_dens", "rec_act_dens", "prop_park_area_tract",
                   "count_tri_facilities", "stops_per_sqmile", "frestaurant_dens",
                   "fastfood_dens", "coffee_dens", "bar_dens",
                   "perennial_snow", "deciduous_forest", "evergreen_forest", "mixed_forest",
                   "shrubs", "herbaceous", "woody_wetlands", "herbaceous_wetlands")
Tract_geo_2010 <- Tract_geo_2010 %>% select(all_of(vars_to_merge))


Block_geo_2020["Temp"] <- as.integer(st_intersects(Block_geo_2020, Tract_geo_2010))
Tract_geo_2010 <- mutate(Tract_geo_2010, Temp = row_number()) %>% st_drop_geometry() #Temp used to join 2020 block groups with 2010 block groups
Block_geo_2020 <- left_join(Block_geo_2020, Tract_geo_2010, by = c("Temp")) %>% select(-Temp) %>% st_drop_geometry()
rm(Tract_geo_2010, main)

#Part 2: joining to main dataset with facts + demeaning by CBSA

#Merging to data
load(file = "DataV2/US_Data/Output/Constructed_Block.Rdata")
#merging and overwriting
US_BLOCK <- left_join(US_BLOCK, Block_geo_2020, by = c("State", "County", "Tract", "BlockGroup"))

#Demeaning by MSA
for (variable in vars_to_merge) {
  var_sym <- sym(paste("demeaned", variable, sep = "_"))
  
  US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(!!var_sym := !!sym(variable) - mean(!!sym(variable), na.rm = TRUE))
  
}

#Setting CBSA to numeric
US_BLOCK$CBSA <- as.numeric(US_BLOCK$CBSA)

save(US_BLOCK, file = "DataV2/US_Data/Output/Constructed_Block.Rdata")
