
#This file explores CBSA housing data by constructing spatial distributions of housing density within CBSAs for initial analysis

#PACKAGES_____________
library(sf)
library(collapse)
library(plm)
library(haven)
library(pacman)
library(dplyr)
library(readr)
library(tmap)
library(ggplot2)
library(readxl)
library(stringr)
library(geosphere)
library(ggrepel)
library(data.table)
library(estimatr)
library(np)
#_____________________

#WORKING DIRECTORY
setwd("Z:/Dropbox/SchoolFolder/Projects/Zoning/Us_Data")


#Importing data
#_____________________________________________________________________
#2013 CBSA boundaries
US_CBSA_2013 <- st_read("gz_2010_us_310_m1_500k.shp")
US_CBSA_2013 <- US_CBSA_2013[US_CBSA_2013$LSAD == "Metro",] #Keeping Metropolitan statistical areas


#2010 census tracts
US_TRACT_2010 <- st_read("CensusTract2010/US_tract_2010.shp")


#PART 1: MATCHING TRACTS TO CBSAs____________________________________________
#Extracting centriods, deleting US_Tract data otherwise
US_TRACT_2010_CENTRIOD <- st_centroid(US_TRACT_2010)
rm(US_TRACT_2010)
#Converting CBSA data to NAD83 reference system
US_TRACT_2010_CENTRIOD <- st_transform(US_TRACT_2010_CENTRIOD, 4269)

#Note: census tracts do not cross county lines and therefore to do not cross CBSA lines. 
#So matching centroids to CBSA polygons are sufficient for accuracy.  
US_TRACT_2010_CENTRIOD["Temp"] <- as.integer(st_intersects(US_TRACT_2010_CENTRIOD, US_CBSA_2013))
US_CBSA_2013 <- mutate(US_CBSA_2013, Temp = row_number())  

#Matching CBSA FIPS Code to US_TRACT_2010_CENTRIOD data.
#Data frame containing names of CBSA, unique CBSA code, etc. 
Temp_tojoin <- US_CBSA_2013 %>%
              select(Temp, NAME, CBSA)
Temp_tojoin <- as.data.frame(Temp_tojoin) #converting for left_join
Temp_tojoin <- Temp_tojoin[, !colnames(Temp_tojoin) %in% "geometry"] #deleting geometry data from sf object
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_CENTRIOD, Temp_tojoin, by = "Temp")
rm(Temp_tojoin, US_CBSA_2013, US_TRACT_2010_CENTRIOD)
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED[, !colnames(US_TRACT_2010_JOINED) %in% "Temp"]
names(US_TRACT_2010_JOINED)[names(US_TRACT_2010_JOINED) == 'NAME'] <- 'CBSA_NAME'
names(US_TRACT_2010_JOINED)[names(US_TRACT_2010_JOINED) == 'STATEFP10'] <- 'State'
names(US_TRACT_2010_JOINED)[names(US_TRACT_2010_JOINED) == 'COUNTYFP10'] <- 'County'
names(US_TRACT_2010_JOINED)[names(US_TRACT_2010_JOINED) == 'TRACTCE10'] <- 'Tract'

#PART2: Joining geometry with more detailed dataset from the US Census Planning Database
US_PLANNING_2010 <- read.csv("CensusTract2010/2010PlanningDatabase.csv")
#Aggregating dataset to tract level (from block level)
temp <- US_PLANNING_2010 %>% count(State, County, Tract)
names(temp)[names(temp) == "n"] <- 'No_blocks'
US_PLANNING_2010 <- left_join(US_PLANNING_2010, temp, by = c("State", "County", "Tract"))
rm(temp)

#Collapsing data over blocks using sum() function. Means will be
US_PLANNING_2010_c <- collap(US_PLANNING_2010, Tot_Population_CEN_2010 + No_blocks + NH_White_alone_CEN_2010 + 
                             NH_Blk_alone_CEN_2010 + Tot_Housing_Units_CEN_2010 + Med_house_val_tr_ACS_06_10 +
                             Med_HHD_Inc_TR_ACS_06_10 + LAND_AREA ~ 
                             State + County + Tract, FUN = list(fmean, fsum))
rm(US_PLANNING_2010)

#Merging Tract and US_Planning Data we need
#First, removing zeros on US_TRACT_2010 Tract codes
US_TRACT_2010_JOINED$State <- as.integer(str_remove(US_TRACT_2010_JOINED$State, "^0+"))
US_TRACT_2010_JOINED$County <- as.integer(str_remove(US_TRACT_2010_JOINED$County, "^0+"))
US_TRACT_2010_JOINED$Tract <- as.integer(str_remove(US_TRACT_2010_JOINED$Tract, "^0+"))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_PLANNING_2010_c, by = c("State", "County", "Tract"))
rm(US_PLANNING_2010_c)

#2.5 extra cleaning of monetary values from csv file. parse_number removes all non-character elements
US_TRACT_2010_JOINED$fmode.Med_house_val_tr_ACS_06_10 <- parse_number(US_TRACT_2010_JOINED$fmode.Med_house_val_tr_ACS_06_10)
US_TRACT_2010_JOINED$fmode.Med_HHD_Inc_TR_ACS_06_10 <- parse_number(US_TRACT_2010_JOINED$fmode.Med_HHD_Inc_TR_ACS_06_10)

#Dropping tracts not in used CBSA
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED[!is.na(US_TRACT_2010_JOINED$CBSA),]

#Dropping Non-mainland states: Alaska (already NOT IN SAMPLE), Hawaii (15), Puerto Rico (72), US Virgin Islands (already NOT IN SAMPLE)
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED[!(US_TRACT_2010_JOINED$State == 15 | US_TRACT_2010_JOINED$State == 72),]

#Joining MSA CBDS. 
CBDs <- read_excel("geocode_comparison_V5.xlsx", sheet = "Holian") 
CBDs <- CBDs %>% select(CBSA_code, GoogleEarthLat, GoogleEarthLon)
names(CBDs)[names(CBDs) == 'CBSA_code'] <- 'CBSA'
US_TRACT_2010_JOINED$CBSA <- as.integer(US_TRACT_2010_JOINED$CBSA)
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, CBDs, by = c("CBSA"))
names(US_TRACT_2010_JOINED)[names(US_TRACT_2010_JOINED) == 'GoogleEarthLat'] <- 'CBD_Lat'
names(US_TRACT_2010_JOINED)[names(US_TRACT_2010_JOINED) == 'GoogleEarthLon'] <- 'CBD_Lon'


#Temporary file to apply distance function quickly (a headache, this code is unreadable but works)
temp <- US_TRACT_2010_JOINED %>% select(State, County, Tract, CBSA, CBD_Lat, CBD_Lon, geometry)
test <-  as.data.frame(st_coordinates(temp))
temp["Tract_Lon"] <- test$X
temp["Tract_Lat"] <- test$Y
temp <- st_drop_geometry(temp)
temp1 <- as.data.frame(apply(temp, 1, function(x)distm(c(x[6], x[5]), c(x[7], x[8]), fun = distGeo)))
colnames(temp1) <- c("distance")
#Merging back to temp and to US_TRACT_2010_JOINED
US_TRACT_2010_JOINED["Dist_to_CBD"] <- temp1$distance/1000 #In km, Google Earth defined CBD
rm(temp, temp1, test, CBDs)


#Joining estimates from Baum-Snow and Han (2020)-- NOTE THESE ARE 2000 TRACT DEFINITIONS (some tracts retain code and definition. Not perfect but no fast way to do this properly without Geolytics)
H_elas <- read_stata("BaumSnowHan_elasticities/gammas_hat_all.dta")
H_elas <- H_elas %>% select(ctracts2000, gamma11b_units_FMM) #Note: only 50410 unique tract estimates. Collapse by unique tract (mean)
H_elas <- collap(H_elas, gamma11b_units_FMM ~ ctracts2000, FUN = fmean)

colnames(H_elas) <- c("GEOID10", "BSH_2020_Elasticity")
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, H_elas, by = c("GEOID10"))
rm(H_elas)

#____More data construction____
#White share of population at tract level
US_TRACT_2010_JOINED["White_share"] <- US_TRACT_2010_JOINED$fsum.NH_White_alone_CEN_2010/US_TRACT_2010_JOINED$fsum.Tot_Population_CEN_2010
US_TRACT_2010_JOINED["Black_share"] <- US_TRACT_2010_JOINED$fsum.NH_Black_alone_CEN_2010/US_TRACT_2010_JOINED$fsum.Tot_Population_CEN_2010

#Housing Density
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED[!(US_TRACT_2010_JOINED$fsum.Tot_Housing_Units_CEN_2010 == 0) & !(US_TRACT_2010_JOINED$fsum.LAND_AREA == 0), ]
US_TRACT_2010_JOINED["log_Housing_density"] <- log(US_TRACT_2010_JOINED$fsum.Tot_Housing_Units_CEN_2010/US_TRACT_2010_JOINED$fsum.LAND_AREA) #density of housing units

#Merging variables from IPUMS NHGIS Dataset (I.e. structure types, for quality adjustment, etc)

#Household family structure by race
#Median household income by White/Black/Aggregate
#Housing units
#Tenure by black/white -- not terribly important 
#Race of householder
#Tenure by household size
#Average household size by tenure status
#Units by number of rooms
#Median number of rooms
#Units in structure
#Units in structure for occupied housing
#Year structure built (vintage)
#Median year structure built
#Units by number of bedrooms
#Gross rent
#Median gross rent
#Bedrooms by Gross Rent
#Gross rent as a percentage of HH income in the past 12 months
#Aggregate Gross Rent By Units in Structure
#Units in structure by Gross Rent as a Percentage of HH income 
#Self reported value of owner occupied units
#Median value owner occupied
#Aggregate value by units in structure (DIVIDE BY units in structure)
#Household type (family versus nonfamily) by Units in Structure 
#Units in Structure by race
#Tenure by Household Size by Units in Structure *Tells us join distribution for household size and structure type
nhgis_191 <- read_stata("NHGIS/nhgis_191.dta")
nhgis_192 <- read_stata("NHGIS/nhgis_192.dta")

US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, nhgis_191, by = c("State", "County", "Tract"))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, nhgis_192, by = c("State", "County", "Tract")) #About 20 unmatched tracts. Weird but likely insignificant.  
rm(nhgis_191)
rm(nhgis_192)

#constructing MSA housing price subsets
US_CBSA_2010_c <- as.data.frame(collap(US_TRACT_2010_JOINED, fmode.Med_house_val_tr_ACS_06_10 ~ CBSA + CBSA_NAME, FUN = c("fmedian")))
names(US_CBSA_2010_c)[names(US_CBSA_2010_c) == 'fmode.Med_house_val_tr_ACS_06_10'] <- 'CBSA_med_house_value'
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_CBSA_2010_c, by = c("CBSA")) %>% select(-ends_with(".y"))

#Centiles pf CBSAs in terms of median-of-median house prices: look at density distributions
quantile_CBSA_houseval <- quantile(US_CBSA_2010_c$CBSA_med_house_value, probs = seq(0, 1, 0.005), na.rm = TRUE)
rm(US_CBSA_2010_c)


#__________Creating variables from codebook_______
#Median income from NHGIS
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED %>% rename(Median_Income_NHGIS = qu1e001)
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED %>% rename(Median_Income_White_NHGIS = qu2e001)
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED %>% rename(Median_Income_Black_NHGIS = qu3e001)

#Median gross rent
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED %>% rename(median_gross_rent = qzte001)

#Average household income 
US_TRACT_2010_JOINED["Average_income"] <- US_TRACT_2010_JOINED$qvbe001/US_TRACT_2010_JOINED$fsum.Tot_Housing_Units_CEN_2010 #Aggregate income from ACS/census no. households

#Rent to income
US_TRACT_2010_JOINED["log_income_to_rent"] <- -log(US_TRACT_2010_JOINED$qz0e001/100) #inverse spending shares on rent
US_TRACT_2010_JOINED["log_income_to_value"] <-  log(US_TRACT_2010_JOINED$fmode.Med_HHD_Inc_TR_ACS_06_10/US_TRACT_2010_JOINED$fmode.Med_house_val_tr_ACS_06_10) #Wage/House price

#Check rent data against evidence in Davis and Ortalo-Magne. Turns out superstars in 2010 are indeed more housing constrained-- at odds. 
mean(US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$log_income_to_rent, na.rm = TRUE)
mean(US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$log_income_to_rent, na.rm = TRUE)
mean(US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$log_income_to_value, na.rm = TRUE)
mean(US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$log_income_to_value, na.rm = TRUE)

#Average household size (For thinking about family effects)
US_TRACT_2010_JOINED["Average_HH_size"] <- US_TRACT_2010_JOINED$qyne001

#____________________________________
#Shares of number of rooms, by tenure
#Owner-occupied
for (i in 4:9) {
temp <- st_drop_geometry(US_TRACT_2010_JOINED[paste("qyue00", i, sep = "")])/US_TRACT_2010_JOINED$qyue002
US_TRACT_2010_JOINED[paste("Share_room_occupant", i, sep="")] <- temp #have to separate these for some reason 
rm(temp)
}
US_TRACT_2010_JOINED["Share_room_occupant10"] <- US_TRACT_2010_JOINED$qyue010/US_TRACT_2010_JOINED$qyue002
US_TRACT_2010_JOINED["Share_room_occupant11"] <- US_TRACT_2010_JOINED$qyue011/US_TRACT_2010_JOINED$qyue002
#Renters
for (i in 14:21) {
  i2 = i-10
  temp <- st_drop_geometry(US_TRACT_2010_JOINED[paste("qyue0", i, sep = "")])/US_TRACT_2010_JOINED$qyue012
  US_TRACT_2010_JOINED[paste("Share_room_rent", i2, sep="")] <- temp #have to separate these for some reason 
  rm(temp)
}


#________________________
#Share Units in structure 
#Occupants
for (i in 4:9) {
  temp <- st_drop_geometry(US_TRACT_2010_JOINED[paste("qyze00", i, sep="")])/US_TRACT_2010_JOINED$qyze002
  US_TRACT_2010_JOINED[paste("Share_units_in_structure_occupant", i, sep="")] <- temp
}

for (i in 10:12) {
  temp <- st_drop_geometry(US_TRACT_2010_JOINED[paste("qyze0", i, sep="")])/US_TRACT_2010_JOINED$qyze002
  US_TRACT_2010_JOINED[paste("Share_units_in_structure_occupant", i, sep="")] <- temp
  rm(temp)
}
#Renters
for (i in 15:23) {
  i2 = i - 11
  temp <- st_drop_geometry(US_TRACT_2010_JOINED[paste("qyze0", i, sep="")])/US_TRACT_2010_JOINED$qyze013
  US_TRACT_2010_JOINED[paste("Share_units_in_structure_rent", i2, sep="")] <- temp
  rm(temp)
}

#________________________________________
#Share of vintages (Years structure built)
#Occupant
for (i in 4:9) {
  temp <- st_drop_geometry(US_TRACT_2010_JOINED[paste("qy3e00", i, sep="")])/US_TRACT_2010_JOINED$qy3e002
  US_TRACT_2010_JOINED[paste("Share_year_built_occupant", i, sep="")] <- temp
  rm(temp)
}
US_TRACT_2010_JOINED["Share_year_built_occupant10"] <- US_TRACT_2010_JOINED$qy3e010/US_TRACT_2010_JOINED$qy3e002
US_TRACT_2010_JOINED["Share_year_built_occupant11"] <- US_TRACT_2010_JOINED$qy3e011/US_TRACT_2010_JOINED$qy3e002

#Renters
for (i in 14:21) {
  i2 <- i-10
  temp <- st_drop_geometry(US_TRACT_2010_JOINED[paste("qy3e0", i, sep="")])/US_TRACT_2010_JOINED$qy3e012
  US_TRACT_2010_JOINED[paste("Share_year_built_rent", i2, sep="")] <- temp
  rm(temp)
}

#________________________
#Share number of bedrooms 
#owner-occupied
for (i in 4:8) {
  temp <- st_drop_geometry(US_TRACT_2010_JOINED[paste("qy9e00", i, sep="")])/US_TRACT_2010_JOINED$qy9e002
  US_TRACT_2010_JOINED[paste("Share_bedrooms_occupant", i, sep="")] <- temp
  rm(temp)
}
#Renters
for (i in 11:15) {
  i2 <- i-7
  temp <- st_drop_geometry(US_TRACT_2010_JOINED[paste("qy9e0", i, sep="")])/US_TRACT_2010_JOINED$qy9e009
  US_TRACT_2010_JOINED[paste("Share_bedrooms_rent", i2, sep="")] <- temp
  rm(temp)
}


#Share of complete plumbing facilities
US_TRACT_2010_JOINED["Share_plumbing_facilities_occupant"] <- US_TRACT_2010_JOINED$qzfe003/US_TRACT_2010_JOINED$qzfe002
US_TRACT_2010_JOINED["Share_plumbing_facilities_rent"] <- US_TRACT_2010_JOINED$qzfe006/US_TRACT_2010_JOINED$qzfe005

#Share of complete kitchen facilities
US_TRACT_2010_JOINED["Share_kitchen_facilities_occupant"] <- US_TRACT_2010_JOINED$qzie003/US_TRACT_2010_JOINED$qzie002
US_TRACT_2010_JOINED["Share_kitchen_facilities_rent"] <- US_TRACT_2010_JOINED$qzie006/US_TRACT_2010_JOINED$qzie005

#Share in various power sources (Utility gas, gas tank, Electricity, Fuel oil, coal or coke, wood, solar, etc)
for (i in 2:9) {
  temp <- st_drop_geometry(US_TRACT_2010_JOINED[paste("qy7e00", i, sep="")])/US_TRACT_2010_JOINED$qy7e001
  US_TRACT_2010_JOINED[paste("Share_fuel_source", i, sep="")] <- temp
  rm(temp)
}

for (i in 2:6) {
  temp <- st_drop_geometry(US_TRACT_2010_JOINED[paste("qtme00", i, sep="")])/US_TRACT_2010_JOINED$qtme001
  US_TRACT_2010_JOINED[paste("Share_household_type", i, sep="")] <- temp
  rm(temp)
}
US_TRACT_2010_JOINED["Share_household_type8"] <- US_TRACT_2010_JOINED$qtme008/US_TRACT_2010_JOINED$qtme001

rm(i, i2)


#Ranking within MSA's and allowing for MSA fixed effects. 
#Ranking tracts within CBSAs according to normalized ordering of housing densities-- tract ordering evenly spaced between 0 and 1. I.e. calculating rank-size distributions==inverse CDFs within each MSA.  
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED %>% group_by(CBSA) %>% mutate(rank_density_CBSA = order(order(log_Housing_density, decreasing = FALSE))/max(order(order(log_Housing_density, decreasing = FALSE))))#Have to run order twice for some reason for it to work. Dividing by max_order to make plot manageable

#Demeaning housing density by CBSA (taking out MSA fixed effects)
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED  %>% group_by(CBSA) %>% mutate(demeaned_log_Housing_density = log_Housing_density - mean(log_Housing_density))

#Detached/Attached housing stats 
US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED[!is.na(US_TRACT_2010_JOINED$qyye001) & !is.na(US_TRACT_2010_JOINED$qyye002) & US_TRACT_2010_JOINED$qyye001 > 0 & US_TRACT_2010_JOINED$qyye002 > 0,] %>% st_drop_geometry()
US_TRACT_2010_JOINED_D["log_detached_housing_share"] <- log(US_TRACT_2010_JOINED_D$qyye002/US_TRACT_2010_JOINED_D$qyye001)
US_TRACT_2010_JOINED_D["log_detached_housing_density"] <- log(US_TRACT_2010_JOINED_D$qyye002/US_TRACT_2010_JOINED_D$fsum.LAND_AREA)
US_TRACT_2010_JOINED_D["log_NHGIS_housing_density"] <- log(US_TRACT_2010_JOINED_D$qyye001/US_TRACT_2010_JOINED_D$fsum.LAND_AREA)
US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED_D  %>% group_by(CBSA) %>% mutate(demeaned_log_detached_share = log_detached_housing_share - mean(log_detached_housing_share))
US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED_D  %>% group_by(CBSA) %>% mutate(demeaned_log_detached_density = log_detached_housing_density - mean(log_detached_housing_density))
US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED_D  %>% group_by(CBSA) %>% mutate(demeaned_log_NHGIS_density = log_NHGIS_housing_density - mean(log_NHGIS_housing_density))
US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED_D %>% group_by(CBSA) %>% mutate(rank_NHGIS_density_CBSA = order(order(log_NHGIS_housing_density, decreasing = FALSE))/max(order(order(log_NHGIS_housing_density, decreasing = FALSE))))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_JOINED_D, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_JOINED_D)

US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED[!is.na(US_TRACT_2010_JOINED$qyye001) & !is.na(US_TRACT_2010_JOINED$qyye002) & US_TRACT_2010_JOINED$qyye001 > 0 & US_TRACT_2010_JOINED$qyye002 < US_TRACT_2010_JOINED$qyye001,] %>% st_drop_geometry()
US_TRACT_2010_JOINED_D["log_attached_housing_density"] <- log((US_TRACT_2010_JOINED_D$qyye001 - US_TRACT_2010_JOINED_D$qyye002)/US_TRACT_2010_JOINED_D$fsum.LAND_AREA)
US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED_D  %>% group_by(CBSA) %>% mutate(demeaned_log_attached_density = log_attached_housing_density - mean(log_attached_housing_density))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_JOINED_D, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_JOINED_D)


#Dist to CBD (for later)
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED %>% group_by(CBSA) %>% mutate(norm_Dist_to_CBD = Dist_to_CBD/max(Dist_to_CBD))

#What about quality adjustment for housing prices (using log transform to keep value positive)
#Regressing self reported median house value on various housing characteristics from NHGIS including MSA fixed effects 
formula_occupant <- as.formula(paste("log(fmode.Med_house_val_tr_ACS_06_10) ~ ", paste(paste0("Share_room_occupant", 4:11, collapse = "+"), paste0("Share_units_in_structure_occupant", 4:12, collapse = "+"), 
                            paste0("Share_year_built_occupant", 4:11, collapse = "+"),  paste0("Share_bedrooms_occupant", 4:8, collapse = "+"), "Share_plumbing_facilities_occupant", "Share_kitchen_facilities_occupant",
                            paste0("Share_fuel_source", 2:9, collapse = "+"), "factor(CBSA)", sep = " + "), sep = "")) #Succinct formula for regression 
Housing_price_reg <- lm(formula_occupant, data = US_TRACT_2010_JOINED, na.action = na.exclude)


rm(formula_occupant)
#Taking MSA fixed effects + residuals to get adjusted housing prices. 
#Extracting Fixed Effects
test <- as.data.frame(Housing_price_reg$coefficients)
coeff <- as.data.frame(test[!rownames(test) %like% "Share",]) #keeping only MSA fixed effects and intercept
test <- US_TRACT_2010_JOINED[order(US_TRACT_2010_JOINED$CBSA),]
test <- distinct(as.data.frame(test$CBSA))
coeff["CBSA"] <- test$`test$CBSA`
rm(test) 
colnames(coeff) <- c("MSA_Houseprice_FE", "CBSA")
#Matching fixed effects to data
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, coeff, by = "CBSA")
rm(coeff)
US_TRACT_2010_JOINED["Residualized_log_houseprice"] <- US_TRACT_2010_JOINED$MSA_Houseprice_FE + resid(Housing_price_reg)

#Doing the same for rents 
formula_rent <- as.formula(paste("log(median_gross_rent) ~ ", paste(paste0("Share_room_rent", 4:11, collapse = "+"), paste0("Share_units_in_structure_rent", 4:12, collapse = "+"), 
                                                                                       paste0("Share_year_built_rent", 4:11, collapse = "+"),  paste0("Share_bedrooms_rent", 4:8, collapse = "+"), "Share_plumbing_facilities_rent", "Share_kitchen_facilities_rent",
                                                                                       paste0("Share_fuel_source", 2:9, collapse = "+"), "factor(CBSA)", sep = " + "), sep = ""))
Housing_rent_reg <- lm(formula_rent, data = US_TRACT_2010_JOINED, na.action = na.exclude)

rm(formula_rent)
#Taking MSA fixed effects + residuals to get adjusted housing prices. 
#Extracting Fixed Effects
test <- as.data.frame(Housing_rent_reg$coefficients)
coeff <- as.data.frame(test[!rownames(test) %like% "Share",]) #keeping only MSA fixed effects and intercept
test <- US_TRACT_2010_JOINED[order(US_TRACT_2010_JOINED$CBSA),]
test <- distinct(as.data.frame(test$CBSA))
coeff["CBSA"] <- test$`test$CBSA`
rm(test) 
colnames(coeff) <- c("MSA_rent_FE", "CBSA")
#Matching fixed effects to data
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, coeff, by = "CBSA")
rm(coeff)
US_TRACT_2010_JOINED["Residualized_log_rent"] <- US_TRACT_2010_JOINED$MSA_rent_FE + resid(Housing_rent_reg)


#For some reason the sf geometry was lost?
US_TRACT_2010_JOINED <- st_as_sf(US_TRACT_2010_JOINED)
rm(Housing_price_reg, Housing_rent_reg)

#Housing prices subset (censored at 1m+ prices)
US_TRACT_2010_JOINED_PRICE <- US_TRACT_2010_JOINED[!is.na(US_TRACT_2010_JOINED$fmode.Med_house_val_tr_ACS_06_10),] %>% st_drop_geometry()
US_TRACT_2010_JOINED_PRICE <- US_TRACT_2010_JOINED_PRICE  %>% group_by(CBSA) %>% mutate(demeaned_log_housevalue = log(fmode.Med_house_val_tr_ACS_06_10) - mean(log(fmode.Med_house_val_tr_ACS_06_10)))#using median because topcensored
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_JOINED_PRICE, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_JOINED_PRICE)

US_TRACT_2010_JOINED_PRICE <- US_TRACT_2010_JOINED[!is.na(US_TRACT_2010_JOINED$Residualized_log_houseprice),] %>% st_drop_geometry()
US_TRACT_2010_JOINED_PRICE <- US_TRACT_2010_JOINED_PRICE %>% group_by(CBSA) %>% mutate(demeaned_log_adjusted_housevalue = Residualized_log_houseprice - mean(Residualized_log_houseprice))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_JOINED_PRICE, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_JOINED_PRICE)

US_TRACT_2010_JOINED_PRICE <- US_TRACT_2010_JOINED[!is.na(US_TRACT_2010_JOINED$Residualized_log_rent),] %>% st_drop_geometry()
US_TRACT_2010_JOINED_PRICE <- US_TRACT_2010_JOINED_PRICE %>% group_by(CBSA) %>% mutate(demeaned_log_adjusted_rent = Residualized_log_rent - mean(Residualized_log_rent))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_JOINED_PRICE, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_JOINED_PRICE)

#Gross rent subset
US_TRACT_2010_JOINED_PRICE <- US_TRACT_2010_JOINED[!is.na(US_TRACT_2010_JOINED$median_gross_rent),] %>% st_drop_geometry()
US_TRACT_2010_JOINED_PRICE <- US_TRACT_2010_JOINED_PRICE %>% group_by(CBSA) %>% mutate(demeaned_log_gross_rent = log(median_gross_rent) - mean(log(median_gross_rent)))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_JOINED_PRICE, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_JOINED_PRICE)

#Income subset
US_TRACT_2010_JOINED_INC <- US_TRACT_2010_JOINED[!(is.na(US_TRACT_2010_JOINED$fmode.Med_HHD_Inc_TR_ACS_06_10)),] %>% st_drop_geometry()
US_TRACT_2010_JOINED_INC <- US_TRACT_2010_JOINED_INC %>% group_by(CBSA) %>% mutate(demeaned_log_Income = log(fmode.Med_HHD_Inc_TR_ACS_06_10) - mean(log(fmode.Med_HHD_Inc_TR_ACS_06_10)))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_JOINED_INC, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_JOINED_INC)

#Price to income
US_TRACT_2010_JOINED_INC <- US_TRACT_2010_JOINED[!(is.na(US_TRACT_2010_JOINED$fmode.Med_HHD_Inc_TR_ACS_06_10)) & !is.na(US_TRACT_2010_JOINED$fmode.Med_house_val_tr_ACS_06_10),] %>% st_drop_geometry()
US_TRACT_2010_JOINED_INC <- US_TRACT_2010_JOINED_INC %>% group_by(CBSA) %>% mutate(demeaned_log_Income_to_value = log(fmode.Med_HHD_Inc_TR_ACS_06_10/fmode.Med_house_val_tr_ACS_06_10) - mean(log(fmode.Med_HHD_Inc_TR_ACS_06_10/fmode.Med_house_val_tr_ACS_06_10)))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_JOINED_INC, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_JOINED_INC)

#White subset
US_TRACT_2010_P <- US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$White_share > 0,] %>% st_drop_geometry() #Subsetting data where at least some white people are
US_TRACT_2010_P <- US_TRACT_2010_P[!(is.na(US_TRACT_2010_P$White_share)),]
US_TRACT_2010_P <- US_TRACT_2010_P %>% group_by(CBSA) %>% mutate(demeaned_log_White_share = log(White_share) - mean(log(White_share)))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_P, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_P)

#Black subset 
US_TRACT_2010_P <- US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$Black_share > 0,] %>% st_drop_geometry() #Subsetting data where at least some black people are
US_TRACT_2010_P <- US_TRACT_2010_P[!(is.na(US_TRACT_2010_P$Black_share)),]
US_TRACT_2010_P <- US_TRACT_2010_P %>% group_by(CBSA) %>% mutate(demeaned_log_Black_share = log(Black_share) - mean(log(Black_share)))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_P, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_P)

#Rent to income ratio subset
US_TRACT_2010_JOINED_INC <- US_TRACT_2010_JOINED[!(is.na(US_TRACT_2010_JOINED$qz0e001)),] %>% st_drop_geometry()
US_TRACT_2010_JOINED_INC <- US_TRACT_2010_JOINED_INC %>% group_by(CBSA) %>% mutate(demeaned_log_Income_to_rent = log_income_to_rent - mean(log_income_to_rent))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_JOINED_INC, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_JOINED_INC)

#Housing Supply Elasticity Subset
US_TRACT_2010_SUP <- US_TRACT_2010_JOINED[!(is.na(US_TRACT_2010_JOINED$BSH_2020_Elasticity)),] %>% st_drop_geometry()
US_TRACT_2010_SUP <- US_TRACT_2010_SUP %>% group_by(CBSA) %>% mutate(demeaned_BSH_Elasticity = BSH_2020_Elasticity - mean(BSH_2020_Elasticity))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_SUP, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_SUP)

#Average household size
US_TRACT_2010_HH <- US_TRACT_2010_JOINED[!(is.na(US_TRACT_2010_JOINED$Average_HH_size)),] %>% st_drop_geometry()
US_TRACT_2010_HH <- US_TRACT_2010_HH %>% group_by(CBSA) %>% mutate(demeaned_log_Average_HH_size = log(Average_HH_size) - mean(log(Average_HH_size)))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_HH, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_HH)

#Share of family households
US_TRACT_2010_HH <- US_TRACT_2010_JOINED[!(is.na(US_TRACT_2010_JOINED$Share_household_type2)),] %>% st_drop_geometry()
US_TRACT_2010_HH <- US_TRACT_2010_HH %>% group_by(CBSA) %>% mutate(demeaned_FamilyShare = Share_household_type2 - mean(Share_household_type2)) #No zeros because very small numbers
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_HH, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_HH)

#_______________________________________________________________________________________________________________________________________________________________________________________________________________________
#FACT 1: Superstar cities have more "sprawl" relative to cheap cities -- that is-- high density housing at the bottom of the distribution. 
#This is driven by the detached housing margin. 
#_______________________________________________________________________________________________________________________________________________________________________________________________________________________

#Top 10% of MSAs
ggplot() + 
  geom_smooth(method = 'loess', span= 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Housing_density), color = 'red') +
  geom_smooth(method = 'loess', span = 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Housing_density), color = 'blue')

#75vs bot 25?
ggplot() + 
  geom_smooth(method = 'loess', span= 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Housing_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Housing_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))
ggsave("tractdens_dist.png", width = 16, height = 10, units = "cm")

#Is the density gradient driven by single detached housing? 
#This "flatter" geography manifests itself in a flatter density gradient of detached housing units. This is where the Missing Middle comes in! 
ggplot() + 
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_detached_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_detached_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))
ggsave("detached_dens_dist.png", width = 16, height = 10, units = "cm")

#Is the density gradient also driven by attached housing units? NO! -- i.e. semi detached, row, apartments. 
ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_attached_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_attached_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))
ggsave("attached_dens_dist.png", width = 16, height = 10, units = "cm")
#________________________________________________________________________________________________________________________________________________________________________________________
#FACT 2: In the lower densities (where "sprawl" occurs), superstar cities have  a relatively 
#1) higher share of whites, and 
#2) higher income when compared with cheap cities.
#3) Inverse relationships between quality adjusted housing price (self reported)
#4) 
#________________________________________________________________________________________________________________________________________________________________________________________
#Who is occupying the large stretch of uniform density in superstars?

#White: Top 10% of MSAs
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_White_share), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_White_share), color = 'blue')

#Top 75% of MSAs
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_White_share, color = 'Top 25%'))  +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_White_share, color = 'Bottom 25%')) + 
  scale_colour_manual(name="legend", values = c("blue", "red"))
ggsave("white_share.png", width = 16, height = 10, units = "cm")

#Income
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Income), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Income), color = 'blue')

ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Income, color = 'Top 25%')) +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Income, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))
ggsave("income.png", width = 16, height = 10, units = "cm")

#Question: repeat distributional exercise with white share distributions, income distributions, etc to see if there is actually more income/white sorting.  
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED %>% group_by(CBSA) %>% mutate(rank_Whiteshare_CBSA = order(order(demeaned_log_White_share, decreasing = FALSE))/max(order(order(demeaned_log_White_share, decreasing = FALSE))))#Have to run order twice for some reason for it to work. Dividing by max_order to make plot manageable
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED %>% group_by(CBSA) %>% mutate(rank_Income_CBSA = order(order(demeaned_log_Income, decreasing = FALSE))/max(order(order(demeaned_log_Income, decreasing = FALSE))))#Have to run order twice for some reason for it to work. Dividing by max_order to make plot manageable

ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_Whiteshare_CBSA, y=demeaned_log_White_share), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_Whiteshare_CBSA, y=demeaned_log_White_share), color = 'blue')
#Same Idea!

ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_Income_CBSA, y=demeaned_log_Income), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_Income_CBSA, y=demeaned_log_Income), color = 'blue')
#Same idea (slightly more income segregation) -- I.e. these cities have only slightly higher degree of income sorting.  

#For house prices (these are top-censored at 1m--this an issue?)
ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_housevalue), color = 'red') +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_housevalue), color = 'blue')

ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_housevalue,  colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_housevalue, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))
ggsave("housingprice.png")

#For residualized housing prices
#Rising real estate density gradient in superstars after adjustment.
ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_adjusted_housevalue,  colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_adjusted_housevalue, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))
  ggsave("adjusted_housevalues.png", width = 16, height = 10, units = "cm")
  
#Rent? Sheds light on a different story!
ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_gross_rent,  colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_gross_rent, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))

#Quality adjusted rents follow same idea (though standard errors are large that we can't rule it out)
ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_adjusted_rent,  colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_adjusted_rent, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))
  ggsave("adjusted_rents.png", width = 16, height = 10, units = "cm")

#________________________________________________________________________________________________________________
#FACT 3: Housing consumption at all tracts in superstars appear to be more income constrained!
#
#Superstar sample FE regression
US_TRACT_2010_JOINED_naf3 <- US_TRACT_2010_JOINED[!(is.na(US_TRACT_2010_JOINED$log_income_to_rent)) &
                             !(is.na(US_TRACT_2010_JOINED$Average_HH_size)) &
                             !(is.na(US_TRACT_2010_JOINED$Share_household_type2)), ]

#Regressing y on x1, x2
constrained_reg_rent_top25_y <- lm_robust(log_income_to_rent ~ rank_density_CBSA + Average_HH_size + Share_household_type2, 
                                  data = US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], fixed_effects = CBSA)

y_top25_resid <- as.vector(US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$log_income_to_rent -
                 constrained_reg_rent_top25_y$fitted.values + constrained_reg_rent_top25_y$coefficients[1]*US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$rank_density_CBSA)

#adjusting y upward by average fixed effect in sample
y_top25_resid <- y_top25_resid + (mean(as.vector(constrained_reg_rent_top25_y$fixed_effects)) - mean(y_top25_resid))*rep(1, length(y_top25_resid))

#For bottom
constrained_reg_rent_bot25_y <- lm_robust(log_income_to_rent ~ rank_density_CBSA + Average_HH_size + Share_household_type2, 
                                          data = US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], fixed_effects = CBSA)

y_bot25_resid <- as.vector(US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$log_income_to_rent -
                             constrained_reg_rent_bot25_y$fitted.values + constrained_reg_rent_bot25_y$coefficients[1]*US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$rank_density_CBSA)

#adjusting y upward by average fixed effect in sample
y_bot25_resid <- y_bot25_resid + (mean(as.vector(constrained_reg_rent_bot25_y$fixed_effects)) - mean(y_bot25_resid))*rep(1, length(y_bot25_resid))

#Time to plot both!

#We Got em!!!!
ggplot() +
  geom_smooth(method = 'lm', aes(x = US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$rank_density_CBSA, y = y_top25_resid, color = 'Top 25%')) +
  geom_smooth(method = 'lm', aes(x = US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$rank_density_CBSA, y = y_bot25_resid, color = 'Bottom 25%')) +
  scale_colour_manual(name = "legend", values = c("blue", "red")) +
  labs(x = "Ranked CBSA density", y = "-log(spending share on rent)")
ggsave("rent_stringency.png", width = 16, height = 10, units = "cm")

#Do the same for housing prices
US_TRACT_2010_JOINED_naf3 <- US_TRACT_2010_JOINED[!(is.na(US_TRACT_2010_JOINED$log_income_to_value)) &
                                                    !(is.na(US_TRACT_2010_JOINED$Average_HH_size)) &
                                                    !(is.na(US_TRACT_2010_JOINED$Share_household_type2)), ]

#Regressing y on x1, x2
constrained_reg_rent_top25_y <- lm_robust(log_income_to_value ~ rank_density_CBSA + Average_HH_size + Share_household_type2, 
                                          data = US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], fixed_effects = CBSA)

y_top25_resid <- as.vector(US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$log_income_to_value -
                             constrained_reg_rent_top25_y$fitted.values + constrained_reg_rent_top25_y$coefficients[1]*US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$rank_density_CBSA)

#adjusting y upward by average fixed effect in sample
y_top25_resid <- y_top25_resid + (mean(as.vector(constrained_reg_rent_top25_y$fixed_effects)) - mean(y_top25_resid))*rep(1, length(y_top25_resid))

#For bottom
constrained_reg_rent_bot25_y <- lm_robust(log_income_to_value ~ rank_density_CBSA + Average_HH_size + Share_household_type2, 
                                          data = US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], fixed_effects = CBSA)

y_bot25_resid <- as.vector(US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$log_income_to_value -
                             constrained_reg_rent_bot25_y$fitted.values + constrained_reg_rent_bot25_y$coefficients[1]*US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$rank_density_CBSA)

#adjusting y upward by average fixed effect in sample
y_bot25_resid <- y_bot25_resid + (mean(as.vector(constrained_reg_rent_bot25_y$fixed_effects)) - mean(y_bot25_resid))*rep(1, length(y_bot25_resid))

#Time to plot both!

#We Got em!!!!
ggplot() +
  geom_smooth(method = 'lm', aes(x = US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),]$rank_density_CBSA, y = y_top25_resid, color = 'Top 25%')) +
  geom_smooth(method = 'lm', aes(x = US_TRACT_2010_JOINED_naf3[US_TRACT_2010_JOINED_naf3$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),]$rank_density_CBSA, y = y_bot25_resid, color = 'Bottom 25%')) +
  scale_colour_manual(name = "legend", values = c("blue", "red")) + 
  labs(x = "Ranked CBSA density", y = "log(Income / HouseValue)")
ggsave("value_stringency.png", width = 16, height = 10, units = "cm")

#_____________________________________________________________________________________________________________________________

#_______________________________________________________________________________________________________________________
#Fact 4: Superstars have flatter housing supply elasticity gradients. Low density housing has lower supply elasticities.
#_______________________________________________________________________________________________________________________
#Housing supply elasticities?

ggplot() + 
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_BSH_Elasticity, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_BSH_Elasticity, colour = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))
ggsave("supplyelasticity.png", width = 16, height = 10, units = "cm")


#__________________________________________________________________________________________________
#PART 3###_________________________________________________________________________________________
#Q: Do facts replicate with distance to CBD instead of housing density rank? 
#__________________________________________________________________________________________________
#Interesting... same conclusion!

#Housing density
ggplot() + 
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_Housing_density), color = 'red') +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_Housing_density), color = 'blue')

#Prices
ggplot() + 
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_housevalue), color = 'red') +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_housevalue), color = 'blue') 


#income + white (not really... less of a clear cut phenomenon)
#income 90-10
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_Income), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_Income), color = 'blue')

#income 75-25 (not clear it follows!)
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_Income), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_Income), color = 'blue')

#White 90-10 (follows same pattern!)
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_White_share), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_White_share), color = 'blue')

ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_White_share), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_White_share), color = 'blue')

##################PART 4: ROBUSTNESS#####################################################
#1) Sorting replicates after removing california? Yes!! California does not drive the results 

ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) & US_TRACT_2010_JOINED_INC$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_Income, color = 'Top 25%')) +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]) & US_TRACT_2010_JOINED_INC$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_Income, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))

ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) & US_TRACT_2010_P$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_White_share, color = 'Top 25%'))  +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]) & US_TRACT_2010_P$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_White_share, color = 'Bottom 25%')) + 
  scale_colour_manual(name="legend", values = c("blue", "red"))

#What about density and BSH elasticities? Yes

ggplot() + 
  geom_smooth(method = 'loess', span= 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) & US_TRACT_2010_JOINED$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_Housing_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]) & US_TRACT_2010_JOINED$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_Housing_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))

ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) & US_TRACT_2010_JOINED_PRICE$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_housevalue,  colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]) & US_TRACT_2010_JOINED_PRICE$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_housevalue, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))

#Do housing density units replicate across census and ACS from NHGIS and Planning database? Yes

ggplot() +
  geom_smooth(method = 'loess', span = 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_NHGIS_density_CBSA, y=demeaned_log_NHGIS_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_NHGIS_density_CBSA, y=demeaned_log_NHGIS_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red")) #SAME RESULTS.


