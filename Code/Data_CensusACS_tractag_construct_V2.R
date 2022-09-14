
#This file explores CBSA housing data by constructing spatial distributions of housing density within CBSAs for initial analysis
#SIMPLIFIED CODE FOR WHAT WE NEED to construct facts: Facts_construct.R


#Date edited: June 22 2022
library(sf)
library(haven) #Reading stata.dta files 
library(dplyr)
library(readr)
library(collapse)
library(stringr)

#Date created: April 27th


#PART 1: JOINING TRACTS TO CBSAs__________________________________________________________________________________________________

#Importing CBSA data
US_CBSA_2010 <- st_read("Data/US_Data/cb_2013_us_cbsa_500k.shp") #2013 definitions from the US census bureau, better to use
US_CBSA_2010 <- US_CBSA_2010[US_CBSA_2010$LSAD == "M1",]
US_CBSA_2010 <- US_CBSA_2010 %>% rename(CBSA = CBSAFP)

US_TRACT_2010 <- st_read("Data/US_Data/CensusTract2010/US_tract_2010.shp") #From IPUMS NHGIS website, our census tract definitions


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

#PART2: Joining geometry with more detailed dataset from the US Census Planning Database
US_PLANNING_2010 <- read.csv("Data/US_Data/CensusTract2010/2010PlanningDatabase.csv")
#Aggregating dataset to tract level (from block level)
temp <- US_PLANNING_2010 %>% count(State, County, Tract) #Counting number of census block groups. 
names(temp)[names(temp) == "n"] <- 'No_blocks'
US_PLANNING_2010 <- left_join(US_PLANNING_2010, temp, by = c("State", "County", "Tract"))
rm(temp)

#Collapsing data over blocks using sum() function. Means will be
US_PLANNING_2010_c <- collap(US_PLANNING_2010, Tot_Population_CEN_2010 + No_blocks + NH_White_alone_CEN_2010 + 
                               NH_Blk_alone_CEN_2010 + Tot_Housing_Units_CEN_2010 + Med_house_val_tr_ACS_06_10 +
                               Med_HHD_Inc_TR_ACS_06_10 + LAND_AREA ~ 
                               State + County + Tract, FUN = list(fmean, fsum)) #Aggregating dataset with sums/averages over blocks within tracts. 
#Note: tract level aggregates are under 'fmode' because no variation within tracts.
rm(US_PLANNING_2010)

#Merging Tract and US_Planning Data we need
#First, removing leading zeros on US_TRACT_2010 Tract codes
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

#Extracting latitude of of tract centriod
US_TRACT_2010_JOINED["Lat"] <- st_coordinates(US_TRACT_2010_JOINED)[, 2]
US_TRACT_2010_JOINED["Lon"] <- st_coordinates(US_TRACT_2010_JOINED)[, 1]


# PART 2: DATA CONSTRUCTION_________________________________________________________________________________________________________________

#BSH elasticities
H_elas <- read_stata("Data/Us_Data/BaumSnowHan_elasticities/gammas_hat_all.dta")
H_elas <- H_elas %>% select(ctracts2000, gamma11b_space_FMM) #Note: only 50410 unique tract estimates. Collapse by unique tract (mean)
H_elas <- collap(H_elas, gamma11b_space_FMM ~ ctracts2000, FUN = fmean)

colnames(H_elas) <- c("GEOID10", "BSH_2020_Elasticity_Units")
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, H_elas, by = c("GEOID10"))
rm(H_elas)


#White share of population at tract level
US_TRACT_2010_JOINED["White_share"] <- US_TRACT_2010_JOINED$fsum.NH_White_alone_CEN_2010/US_TRACT_2010_JOINED$fsum.Tot_Population_CEN_2010
US_TRACT_2010_JOINED["Black_share"] <- US_TRACT_2010_JOINED$fsum.NH_Black_alone_CEN_2010/US_TRACT_2010_JOINED$fsum.Tot_Population_CEN_2010

#Housing Density
#SUBSETTING MAIN SAMPLE TO HAVE POSITIVE HOUSEHOLDS AND LAND MASS
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED[!(US_TRACT_2010_JOINED$fsum.Tot_Housing_Units_CEN_2010 == 0) & !(US_TRACT_2010_JOINED$fsum.LAND_AREA == 0), ] #Taking tracts with coded positive land area and housing units. 
US_TRACT_2010_JOINED["log_Housing_density"] <- log(US_TRACT_2010_JOINED$fsum.Tot_Housing_Units_CEN_2010/US_TRACT_2010_JOINED$fsum.LAND_AREA) #log density of housing units
US_TRACT_2010_JOINED["Housing_density"] <- US_TRACT_2010_JOINED$fsum.Tot_Housing_Units_CEN_2010/US_TRACT_2010_JOINED$fsum.LAND_AREA # Non-logs
US_TRACT_2010_JOINED["Population_density"] <- US_TRACT_2010_JOINED$fsum.Tot_Population_CEN_2010/US_TRACT_2010_JOINED$fsum.LAND_AREA

#constructing MSA housing price subsets
US_CBSA_2010_c <- as.data.frame(collap(US_TRACT_2010_JOINED, fmode.Med_house_val_tr_ACS_06_10 ~ CBSA + CBSA_NAME, FUN = c("fmedian")))
names(US_CBSA_2010_c)[names(US_CBSA_2010_c) == 'fmode.Med_house_val_tr_ACS_06_10'] <- 'CBSA_med_house_value'
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_CBSA_2010_c, by = c("CBSA"), suffix = c("", ".y")) %>% select(-ends_with(".y"))

#Population density ranks (to compare with French exercise in robustness)
US_CBSA_2010_cpop <- as.data.frame(collap(US_TRACT_2010_JOINED, Population_density ~ CBSA + CBSA_NAME, FUN = c("fmean")))
names(US_CBSA_2010_cpop)[names(US_CBSA_2010_cpop) == 'Population_density'] <- 'CBSA_popdens'
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_CBSA_2010_cpop, by = c("CBSA"), suffix = c("", ".y")) %>% select(-ends_with(".y"))

#Centiles pf CBSAs in terms of median-of-median house prices: look at density distributions
quantile_CBSA_houseval <- quantile(US_CBSA_2010_c$CBSA_med_house_value, probs = seq(0, 1, 0.005), na.rm = TRUE)
quantile_CBSA_popdens <- quantile(US_CBSA_2010_cpop$CBSA_popdens, probs = seq(0, 1, 0.005), na.rm = TRUE)

#Joining NHGIS Data
nhgis_191 <- read_stata("Data/US_Data/NHGIS/Tract/nhgis_191.dta")
nhgis_192 <- read_stata("Data/US_Data/NHGIS/Tract/nhgis_192.dta")

US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, nhgis_191, by = c("State", "County", "Tract"))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, nhgis_192, by = c("State", "County", "Tract")) #About 20 unmatched tracts. Weird but likely insignificant. Due to some (minor) changes in tract defns between 2010 and 2012 
rm(nhgis_191)
rm(nhgis_192)

#Income by race 
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED %>% rename(Median_Income_NHGIS = qu1e001)
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED %>% rename(Median_Income_White_NHGIS = qu2e001)
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED %>% rename(Median_Income_Black_NHGIS = qu3e001)
US_TRACT_2010_JOINED["Average_income"] <- US_TRACT_2010_JOINED$qvbe001/US_TRACT_2010_JOINED$fsum.Tot_Housing_Units_CEN_2010 #Aggregate income from 2008-2012 ACS/census no. households

#Average household size (For thinking about family effects)
US_TRACT_2010_JOINED["Average_HH_size"] <- US_TRACT_2010_JOINED$qyne001

#Constructing ranking of densities
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED %>% group_by(CBSA) %>% mutate(rank_density_CBSA = order(order(Housing_density, decreasing = FALSE))/(max(order(order(Housing_density, decreasing = FALSE)))+1))
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED %>% group_by(CBSA) %>% mutate(rank_popdensity_CBSA = order(order(Population_density, decreasing = FALSE))/(max(order(order(Population_density, decreasing = FALSE)))+1))

#ROBUSTNESS: ALTERNATIVE DEFINITIONS OF RANKINGS


##################DENSITY COMPONENTS_____________________######################################
#1) Single family homes
#2) 2-19 unit structures
#3) 20+ unit structures 
#4) 2-4 unit structures as a sub-component of 2) (Aradhya's request)
#5) 2+ unit structures (only splitting by half)
#USE ACS TO CONSTRUCT SHARES in structure type, Census (Planning Database) to construct number of units in levels.
#NOTE qyye010 and qyye011 are boat and mobile homes, respectively. 

US_TRACT_2010_JOINED["Housing_density"] <- US_TRACT_2010_JOINED$fsum.Tot_Housing_Units_CEN_2010/US_TRACT_2010_JOINED$fsum.LAND_AREA
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED  %>% group_by(CBSA) %>% mutate(tract_land_fraction = fsum.LAND_AREA/sum(fsum.LAND_AREA))
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED  %>% group_by(CBSA) %>% mutate(demeaned_Housing_density = Housing_density/mean(Housing_density))


US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED[!is.na(US_TRACT_2010_JOINED$qyye008) & !is.na(US_TRACT_2010_JOINED$qyye009) &
                                                 !is.na(US_TRACT_2010_JOINED$qyye002) & !is.na(US_TRACT_2010_JOINED$qyye003) & !is.na(US_TRACT_2010_JOINED$qyye001) &
                                                 !is.na(US_TRACT_2010_JOINED$qyye010) & !is.na(US_TRACT_2010_JOINED$qyye011), ] 
US_TRACT_2010_JOINED_D["Single_family_density"] <- ((US_TRACT_2010_JOINED_D$qyye002 + US_TRACT_2010_JOINED_D$qyye003)*(US_TRACT_2010_JOINED_D$fsum.Tot_Housing_Units_CEN_2010/US_TRACT_2010_JOINED_D$qyye001))/US_TRACT_2010_JOINED_D$fsum.LAND_AREA
US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED_D  %>% group_by(CBSA) %>% mutate(demeaned_Single_family_density = Single_family_density/mean(Housing_density)) #Demean with total housing density (as the sums add up to total demeaned density!)
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_JOINED_D, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_JOINED_D)

US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED[!is.na(US_TRACT_2010_JOINED$qyye008) & !is.na(US_TRACT_2010_JOINED$qyye009) &
                                                 !is.na(US_TRACT_2010_JOINED$qyye002) & !is.na(US_TRACT_2010_JOINED$qyye003) & !is.na(US_TRACT_2010_JOINED$qyye001) &
                                                 !is.na(US_TRACT_2010_JOINED$qyye010) & !is.na(US_TRACT_2010_JOINED$qyye011), ]
US_TRACT_2010_JOINED_D["Building_20_density"] <- ((US_TRACT_2010_JOINED_D$qyye008 + US_TRACT_2010_JOINED_D$qyye009)*(US_TRACT_2010_JOINED_D$fsum.Tot_Housing_Units_CEN_2010/US_TRACT_2010_JOINED_D$qyye001))/US_TRACT_2010_JOINED_D$fsum.LAND_AREA
US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED_D  %>% group_by(CBSA) %>% mutate(demeaned_Building_20_density = Building_20_density/mean(Housing_density))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_JOINED_D, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_JOINED_D)

US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED[!is.na(US_TRACT_2010_JOINED$qyye008) & !is.na(US_TRACT_2010_JOINED$qyye009) &
                                                 !is.na(US_TRACT_2010_JOINED$qyye002) & !is.na(US_TRACT_2010_JOINED$qyye003) & !is.na(US_TRACT_2010_JOINED$qyye001) &
                                                 !is.na(US_TRACT_2010_JOINED$qyye010) & !is.na(US_TRACT_2010_JOINED$qyye011),]

US_TRACT_2010_JOINED_D["Middle_density"] <- ((US_TRACT_2010_JOINED_D$qyye001 - (US_TRACT_2010_JOINED_D$qyye002 + US_TRACT_2010_JOINED_D$qyye003 + 
                                                                                  US_TRACT_2010_JOINED_D$qyye008 + US_TRACT_2010_JOINED_D$qyye009 + US_TRACT_2010_JOINED_D$qyye010 +
                                                                                  US_TRACT_2010_JOINED_D$qyye011))*(US_TRACT_2010_JOINED_D$fsum.Tot_Housing_Units_CEN_2010/US_TRACT_2010_JOINED_D$qyye001))/US_TRACT_2010_JOINED_D$fsum.LAND_AREA
US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED_D  %>% group_by(CBSA) %>% mutate(demeaned_Middle_density = Middle_density/mean(Housing_density))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_JOINED_D, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_JOINED_D)

#Only duplexes, triplexes and quadplexes.
US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED[!is.na(US_TRACT_2010_JOINED$qyye008) & !is.na(US_TRACT_2010_JOINED$qyye009) &
                                                 !is.na(US_TRACT_2010_JOINED$qyye002) & !is.na(US_TRACT_2010_JOINED$qyye003) & !is.na(US_TRACT_2010_JOINED$qyye001) &
                                                 !is.na(US_TRACT_2010_JOINED$qyye010) & !is.na(US_TRACT_2010_JOINED$qyye011) & 
                                                 !is.na(US_TRACT_2010_JOINED$qyye004) & !is.na(US_TRACT_2010_JOINED$qyye005),]
US_TRACT_2010_JOINED_D["Middle_density_spl1"] <- ((US_TRACT_2010_JOINED_D$qyye004 + US_TRACT_2010_JOINED_D$qyye005)*(US_TRACT_2010_JOINED_D$fsum.Tot_Housing_Units_CEN_2010/US_TRACT_2010_JOINED_D$qyye001))/US_TRACT_2010_JOINED_D$fsum.LAND_AREA
US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED_D  %>% group_by(CBSA) %>% mutate(demeaned_Middlesp1_density = Middle_density_spl1/mean(Housing_density))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_JOINED_D, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_JOINED_D)


US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED[!is.na(US_TRACT_2010_JOINED$qyye008) & !is.na(US_TRACT_2010_JOINED$qyye009) &
                                                 !is.na(US_TRACT_2010_JOINED$qyye002) & !is.na(US_TRACT_2010_JOINED$qyye003) & !is.na(US_TRACT_2010_JOINED$qyye001) &
                                                 !is.na(US_TRACT_2010_JOINED$qyye010) & !is.na(US_TRACT_2010_JOINED$qyye011),]
US_TRACT_2010_JOINED_D["Building_2_density"] <- ((US_TRACT_2010_JOINED_D$qyye001 - 
                                                    (US_TRACT_2010_JOINED_D$qyye002 + US_TRACT_2010_JOINED_D$qyye003 + 
                                                     US_TRACT_2010_JOINED_D$qyye010 + US_TRACT_2010_JOINED_D$qyye011))*(US_TRACT_2010_JOINED_D$fsum.Tot_Housing_Units_CEN_2010/US_TRACT_2010_JOINED_D$qyye001))/US_TRACT_2010_JOINED_D$fsum.LAND_AREA
US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED_D  %>% group_by(CBSA) %>% mutate(demeaned_Building_2_density = Building_2_density/mean(Housing_density))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_JOINED_D, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_JOINED_D)

#####################################################################################################################_____________________

#demeaned population density at 2010 values (fsum.TOT_POPULATION_CEN_2010)
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED  %>% group_by(CBSA) %>% mutate(demeaned_Pop_density = Population_density/mean(Population_density, na.rm= TRUE))

#MSA demeaned variables
#White subset
US_TRACT_2010_P <- US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$White_share > 0,]  #Subsetting data where at least some white people are
US_TRACT_2010_P <- US_TRACT_2010_P[!(is.na(US_TRACT_2010_P$White_share)),]
US_TRACT_2010_P <- US_TRACT_2010_P %>% group_by(CBSA) %>% mutate(demeaned_log_White_share = log(White_share) - mean(log(White_share)))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_P, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_P)

#Income subset, average income (not median income)
US_TRACT_2010_JOINED_INC <- US_TRACT_2010_JOINED[!(is.na(US_TRACT_2010_JOINED$Average_income)),] 
US_TRACT_2010_JOINED_INC <- US_TRACT_2010_JOINED_INC %>% group_by(CBSA) %>% mutate(demeaned_log_Income = log(Average_income) - mean(log(Average_income)))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_JOINED_INC, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_JOINED_INC)

#Housing Supply Elasticity Subset
US_TRACT_2010_SUP <- US_TRACT_2010_JOINED[!(is.na(US_TRACT_2010_JOINED$BSH_2020_Elasticity_Units)),] 
US_TRACT_2010_SUP <- US_TRACT_2010_SUP %>% group_by(CBSA) %>% mutate(demeaned_BSH_Elasticity = BSH_2020_Elasticity_Units - mean(BSH_2020_Elasticity_Units))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_SUP, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_SUP)

#Average household size
US_TRACT_2010_HH <- US_TRACT_2010_JOINED[!(is.na(US_TRACT_2010_JOINED$Average_HH_size)),] 
US_TRACT_2010_HH <- US_TRACT_2010_HH %>% group_by(CBSA) %>% mutate(demeaned_log_Average_HH_size = log(Average_HH_size) - mean(log(Average_HH_size)))
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_HH, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_HH)

#_________________________Constructing single family margin from Assessment data, getting margins of single detached housing______________________________
#Requires SingleFamilyLandConstructed.dta, output from ConstructTractBlockLotSizeAggregates.do

SingleFamilyLand <- read_stata("Data/US_Data/Output/SingleFamilyLandConstructed.dta")
SingleFamilyLand$State <- as.double(SingleFamilyLand$State)
SingleFamilyLand$County <- as.double(SingleFamilyLand$County)
SingleFamilyLand$Tract <- as.double(SingleFamilyLand$Tract)

#Joining lot sizes with main dataset
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, SingleFamilyLand, by = c("State", "County", "Tract"))
rm(SingleFamilyLand)

#converting to square miles
US_TRACT_2010_JOINED["LotSizeSquareMiles"] <- US_TRACT_2010_JOINED$LotSizeAcres/640

#Constructing measures of total land used. I.e. measures of number of single family homes from ACS/Census x Average lot size from assessments
#Remember: qyye002 and qyye003 are both detached and semi-detached houses. 
US_TRACT_2010_JOINED["LandSingleFamily"] <- US_TRACT_2010_JOINED$LotSizeSquareMiles*((US_TRACT_2010_JOINED$qyye002 + US_TRACT_2010_JOINED$qyye003)*(US_TRACT_2010_JOINED$fsum.Tot_Housing_Units_CEN_2010/US_TRACT_2010_JOINED$qyye001))
US_TRACT_2010_JOINED["LandShareSingleFamily"] <- US_TRACT_2010_JOINED$LandSingleFamily/US_TRACT_2010_JOINED$fsum.LAND_AREA  

#Some assessments are outliers, largely above the census tract. This might be because these homes are large and may cross tract boundaries. 

US_TRACT_2010_JOINED$LandShareSingleFamily[US_TRACT_2010_JOINED$LandShareSingleFamily > 1] <- NA #Only a few thousand follow under this category, still strange. Need to check this.

US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED[!is.na(US_TRACT_2010_JOINED$qyye008) & !is.na(US_TRACT_2010_JOINED$qyye009) &
                                                 !is.na(US_TRACT_2010_JOINED$qyye002) & !is.na(US_TRACT_2010_JOINED$qyye003) & !is.na(US_TRACT_2010_JOINED$qyye001) &
                                                 !is.na(US_TRACT_2010_JOINED$qyye010) & !is.na(US_TRACT_2010_JOINED$qyye011) & !is.na(US_TRACT_2010_JOINED$LandShareSingleFamily), ]
US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED_D %>% group_by(CBSA) %>% mutate(demeaned_SingleF_Density_Margin = (demeaned_Single_family_density*sqrt(mean(Housing_density)))/(LandShareSingleFamily)) #Multiplying by sqrt because this value was already divided by housing density before)
US_TRACT_2010_JOINED_D <- US_TRACT_2010_JOINED_D %>% group_by(CBSA) %>% mutate(demeaned_SingleF_Land_Margin = (LandShareSingleFamily)/(sqrt(mean(Housing_density)))) #dividing by sqrt to carry over normalization component.

US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_TRACT_2010_JOINED_D, by = c("State", "County", "Tract"), suffix = c("", ".y")) %>% select(-ends_with(".y"))
rm(US_TRACT_2010_JOINED_D)

#_____________________________Lin and Lee Data construct__________________________
#I.e. merge these natural amenities variables with log income per capita to think about the first stage.

US_FirstStage_2010 <- US_TRACT_2010_JOINED %>% select(Average_income, rank_density_CBSA, 
                                                      State, County, Tract, 
                                                      CBSA, CBSA_NAME, GEOID10, Lat, Lon)

LeeLin <- read_dta("Data/US_Data/LinLee/Supplementary/data/Lee_Lin_data.dta")
LeeLin <- LeeLin %>% rename(GEOID10 = geo2010)
#Merging to first stage database
US_FirstStage_2010 <- full_join(US_FirstStage_2010, LeeLin, by = c("GEOID10"))

#Merging with NANDA land-cover database
NANDA <- read_dta("Data/US_Data/NANDA landcover/nanda_landcover_tract_2001-2016_02P.dta")
NANDA <- NANDA %>%  rename(GEOID10 = tract_fips)
NANDA <- NANDA[NANDA$year_intp == 2011,] #2011 is an official data release closest to our target sample period

US_FirstStage_2010 <- left_join(US_FirstStage_2010, NANDA, by = c("GEOID10"))



#Lastly, saving dataset
save(US_TRACT_2010_JOINED, file = "Data/US_Data/Output/Constructed_2010_Tract.Rdata")
save(US_CBSA_2010_c, file = "Data/US_Data/Output/CBSA_med_house_price.Rdata")
save(quantile_CBSA_houseval, file = "Data/US_Data/Output/CBSA_quantiles.Rdata")
save(quantile_CBSA_popdens, file = "Data/US_Data/Output/CBSA_quantiles_popdens.Rdata")
write_dta(as.data.frame(US_FirstStage_2010), path = "Data/US_Data/Output/First_stage.dta") #Do first stage regressions in STATA for ease. 

rm(US_TRACT_2010_JOINED, US_CBSA_2010_c, quantile_CBSA_houseval, US_CBSA_2010,
   US_CBSA_2010_cpop, quantile_CBSA_popdens, US_FirstStage_2010, LeeLin, NANDA)
