
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
#_____________________

#WORKING DIRECTORY
setwd("C:/Users/James/Dropbox/SchoolFolder/Projects/Zoning/Us_Data")


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
#So matching centroids to CBSA polygons are sufficient. 
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


#Temporary file to apply distance function quickly (a headache)
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


#Joining estimates from Baum-Snow and Han (2020)-- NOTE THESE ARE 2000 TRACT DEFINITIONS (some tracts retain code and definition. not perfect but no fast way to do this properly)
H_elas <- read_stata("BaumSnowHan_elasticities/gammas_hat_all.dta")
H_elas <- H_elas %>% select(ctracts2000, gamma11b_units_FMM) #Note: only 50410 unique tract estimates. Collapse by unique tract (mean)
H_elas <- collap(H_elas, gamma11b_units_FMM ~ ctracts2000, FUN = fmean)

colnames(H_elas) <- c("GEOID10", "BSH_2020_Elasticity")
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, H_elas, by = c("GEOID10"))
rm(H_elas)

#____More data construction____
#White share of population at tract level
US_TRACT_2010_JOINED["White_share"] <- US_TRACT_2010_JOINED$fsum.NH_White_alone_CEN_2010/US_TRACT_2010_JOINED$fsum.Tot_Population_CEN_2010


#______________________________________________________________________________________
#_______________________PART 3: Analysis-- Do we have a "missing middle"?______________
#______________________________________________________________________________________

#Density estimation of median housing values (pareto ish-- with top censoring)
ggplot(US_TRACT_2010_JOINED, aes(fmode.Med_house_val_tr_ACS_06_10)) + geom_density(adjust = 2.5, na.rm = TRUE) #looks quite lognormal 

#Density estimation of housing density
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED[!(US_TRACT_2010_JOINED$fsum.Tot_Housing_Units_CEN_2010 == 0) & !(US_TRACT_2010_JOINED$fsum.LAND_AREA == 0), ]
US_TRACT_2010_JOINED["log_Housing_density"] <- log(US_TRACT_2010_JOINED$fsum.Tot_Housing_Units_CEN_2010/US_TRACT_2010_JOINED$fsum.LAND_AREA) #density of housing units

ggplot(US_TRACT_2010_JOINED, aes(log_Housing_density)) + geom_density(adjust=0.5, na.rm=TRUE) #Extremely fat tailed distribution

#By top 10% CBSAs in housing prices. Constructing
US_CBSA_2010_c <- as.data.frame(collap(US_TRACT_2010_JOINED, fmode.Med_house_val_tr_ACS_06_10 ~ CBSA + CBSA_NAME, FUN = c("fmedian")))
names(US_CBSA_2010_c)[names(US_CBSA_2010_c) == 'fmode.Med_house_val_tr_ACS_06_10'] <- 'CBSA_med_house_value'
US_TRACT_2010_JOINED <- left_join(US_TRACT_2010_JOINED, US_CBSA_2010_c, by = c("CBSA"))

#Centiles pf CBSAs in terms of median-of-median house prices: look at density distributions
quantile_CBSA_houseval <- quantile(US_CBSA_2010_c$CBSA_med_house_value, probs = seq(0, 1, 0.005), na.rm = TRUE)

#Looking at Tract level median housing price distribution
ggplot(US_TRACT_2010_JOINED, aes(fmode.Med_house_val_tr_ACS_06_10)) +
      geom_histogram(na.rm=TRUE) #clearly censored at top of distribution but probably follows lognormal/power law 

#TOP 10%
ggplot(US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),], aes(log_Housing_density)) + 
      geom_histogram(na.rm=TRUE)
#BOTTOM 10%
ggplot(US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),], aes(log_Housing_density)) + 
      geom_histogram(na.rm=TRUE)

#_______________________________________________________________________________________________________________________________________________________________________________________________________________________
#FACT 1: Superstar cities have more "sprawl" relative to cheap cities -- that is-- high density housing at the bottom of the distribution. 
#_______________________________________________________________________________________________________________________________________________________________________________________________________________________
#Ranking within MSA's and allowing for MSA fixed effects. 

#Ranking tracts within CBSAs according to normalized ordering of housing densities-- tract ordering evenly spaced between 0 and 1. I.e. calculating rank-size distributions==inverse CDFs within each MSA.  
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED %>% group_by(CBSA) %>% mutate(rank_density_CBSA = order(order(log_Housing_density, decreasing = FALSE))/max(order(order(log_Housing_density, decreasing = FALSE))))#Have to run order twice for some reason for it to work. Dividing by max_order to make plot manageable

#Demeaning housing density by CBSA (taking out MSA fixed effects)
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED  %>% group_by(CBSA) %>% mutate(demeaned_log_Housing_density = log_Housing_density - mean(log_Housing_density))

#Dist to CBD (for later)
US_TRACT_2010_JOINED <- US_TRACT_2010_JOINED %>% group_by(CBSA) %>% mutate(norm_Dist_to_CBD = Dist_to_CBD/max(Dist_to_CBD))

#Housing prices subset (censored at 1m+ prices)
US_TRACT_2010_JOINED_PRICE <- US_TRACT_2010_JOINED[!is.na(US_TRACT_2010_JOINED$fmode.Med_house_val_tr_ACS_06_10),]
US_TRACT_2010_JOINED_PRICE <- US_TRACT_2010_JOINED_PRICE  %>% group_by(CBSA) %>% mutate(demeaned_log_housevalue = log(fmode.Med_house_val_tr_ACS_06_10) - mean(log(fmode.Med_house_val_tr_ACS_06_10))) #using median because topcensored


#Top 10% of MSAs
ggplot() + 
  geom_smooth(method = 'loess', span= 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Housing_density), color = 'red') +
  geom_smooth(method = 'loess', span = 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Housing_density), color = 'blue')

#75vs bot 25?
ggplot() + 
  geom_smooth(method = 'loess', span= 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Housing_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Housing_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))
ggsave("tractdens_dist.png")

#For house prices (these are top-censored at 1m--this an issue?)
ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED_PRICE[US_TRACT_2010_JOINED_PRICE$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_housevalue), color = 'red') +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED_PRICE[US_TRACT_2010_JOINED_PRICE$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_housevalue), color = 'blue')

ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED_PRICE[US_TRACT_2010_JOINED_PRICE$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_housevalue,  colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED_PRICE[US_TRACT_2010_JOINED_PRICE$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_housevalue, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))
ggsave("housingprice.png")

#________________________________________________________________________________________________________________________________________________________________________________________
#FACT 2: In the lower densities (where "sprawl" occurs), superstar cities have  a relatively 1) higher share of whites, and 2) higher income when compared with cheap cities.______________
#________________________________________________________________________________________________________________________________________________________________________________________
#Who is occupying the large stretch of uniform density in superstars?
US_TRACT_2010_P <- US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$White_share > 0,] #Subsetting data where at least some white people are
US_TRACT_2010_P <- US_TRACT_2010_P[!(is.na(US_TRACT_2010_P$White_share)),]
#Demeaning 
US_TRACT_2010_P <- US_TRACT_2010_P %>% group_by(CBSA) %>% mutate(demeaned_log_White_share = log(White_share) - mean(log(White_share)))

#Income? Income-housing-density gradient more stringent in superstars. 
US_TRACT_2010_JOINED_INC <- US_TRACT_2010_JOINED[!(is.na(US_TRACT_2010_JOINED$fmode.Med_HHD_Inc_TR_ACS_06_10)),]
US_TRACT_2010_JOINED_INC <- US_TRACT_2010_JOINED_INC %>% group_by(CBSA) %>% mutate(demeaned_log_Income = log(fmode.Med_HHD_Inc_TR_ACS_06_10) - mean(log(fmode.Med_HHD_Inc_TR_ACS_06_10)))

#White: Top 10% of MSAs
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_P[US_TRACT_2010_P$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_White_share), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_P[US_TRACT_2010_P$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_White_share), color = 'blue')

#Top 75% of MSAs
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_P[US_TRACT_2010_P$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_White_share, color = 'Top 25%'))  +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_P[US_TRACT_2010_P$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_White_share, color = 'Bottom 25%')) + 
  scale_colour_manual(name="legend", values = c("blue", "red"))
ggsave("white_share.png")

#Income
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED_INC[US_TRACT_2010_JOINED_INC$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Income), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED_INC[US_TRACT_2010_JOINED_INC$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Income), color = 'blue')

ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED_INC[US_TRACT_2010_JOINED_INC$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Income, color = 'Top 25%')) +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED_INC[US_TRACT_2010_JOINED_INC$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_log_Income, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))
ggsave("income.png")

#Question: repeat distributional exercise with white share distributions, income distributions, etc to see if there is actually more income/white sorting.  
US_TRACT_2010_P <- US_TRACT_2010_P %>% group_by(CBSA) %>% mutate(rank_Whiteshare_CBSA = order(order(demeaned_log_White_share, decreasing = FALSE))/max(order(order(demeaned_log_White_share, decreasing = FALSE))))#Have to run order twice for some reason for it to work. Dividing by max_order to make plot manageable
US_TRACT_2010_JOINED_INC <- US_TRACT_2010_JOINED_INC %>% group_by(CBSA) %>% mutate(rank_Income_CBSA = order(order(demeaned_log_Income, decreasing = FALSE))/max(order(order(demeaned_log_Income, decreasing = FALSE))))#Have to run order twice for some reason for it to work. Dividing by max_order to make plot manageable

ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_P[US_TRACT_2010_P$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_Whiteshare_CBSA, y=demeaned_log_White_share), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_P[US_TRACT_2010_P$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_Whiteshare_CBSA, y=demeaned_log_White_share), color = 'blue')
#Same Idea!

ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED_INC[US_TRACT_2010_JOINED_INC$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_Income_CBSA, y=demeaned_log_Income), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED_INC[US_TRACT_2010_JOINED_INC$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_Income_CBSA, y=demeaned_log_Income), color = 'blue')
#Same idea (slightly more income segregation) -- I.e. these cities have only slightly higher degree of income sorting.  

#_______________________________________________________________________________________________________________________________
#Q: Could-- reg cross-tract income inequality on MSA housing prices as well/ same with white share. 
#Q: Do facts replicate with distance to CBD instead of housing density rank? 

#Interesting... same conclusion!

#Housing density
ggplot() + 
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_Housing_density), color = 'red') +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_Housing_density), color = 'blue')

#Prices
ggplot() + 
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED_PRICE[US_TRACT_2010_JOINED_PRICE$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_housevalue), color = 'red') +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED_PRICE[US_TRACT_2010_JOINED_PRICE$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_housevalue), color = 'blue') 


#income + white (not really... less of a clear cut phenomenon)
#income 90-10
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED_INC[US_TRACT_2010_JOINED_INC$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_Income), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED_INC[US_TRACT_2010_JOINED_INC$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_Income), color = 'blue')

#income 75-25 (not clear it follows!)
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED_INC[US_TRACT_2010_JOINED_INC$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_Income), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED_INC[US_TRACT_2010_JOINED_INC$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_Income), color = 'blue')

#White 90-10 (follows same pattern!)
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_P[US_TRACT_2010_P$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["90.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_White_share), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_P[US_TRACT_2010_P$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["10.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_White_share), color = 'blue')

ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_P[US_TRACT_2010_P$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_White_share), color = 'red') +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_P[US_TRACT_2010_P$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=norm_Dist_to_CBD, y=demeaned_log_White_share), color = 'blue')

#Housing supply elasticities?
US_TRACT_2010_SUP <- US_TRACT_2010_JOINED[!(is.na(US_TRACT_2010_JOINED$BSH_2020_Elasticity)),]
US_TRACT_2010_SUP <- US_TRACT_2010_SUP %>% group_by(CBSA) %>% mutate(demeaned_BSH_Elasticity = BSH_2020_Elasticity - mean(BSH_2020_Elasticity))

ggplot() + 
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_SUP[US_TRACT_2010_SUP$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], aes(x=rank_density_CBSA, y=demeaned_BSH_Elasticity, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_SUP[US_TRACT_2010_SUP$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], aes(x=rank_density_CBSA, y=demeaned_BSH_Elasticity, colour = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))
ggsave("supplyelasticity.png")



##################PART 4: ROBUSTNESS#####################################################
#Sorting replicates after removing california? Yes!! California does not drive the results 

ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED_INC[US_TRACT_2010_JOINED_INC$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) & US_TRACT_2010_JOINED_INC$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_Income, color = 'Top 25%')) +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_JOINED_INC[US_TRACT_2010_JOINED_INC$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]) & US_TRACT_2010_JOINED_INC$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_Income, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))

ggplot() + 
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_P[US_TRACT_2010_P$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) & US_TRACT_2010_P$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_White_share, color = 'Top 25%'))  +
  geom_smooth(method = 'loess', span=1, data = US_TRACT_2010_P[US_TRACT_2010_P$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]) & US_TRACT_2010_P$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_White_share, color = 'Bottom 25%')) + 
  scale_colour_manual(name="legend", values = c("blue", "red"))

#What about density and BSH elasticities? Yes

ggplot() + 
  geom_smooth(method = 'loess', span= 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) & US_TRACT_2010_JOINED$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_Housing_density, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.1, data = US_TRACT_2010_JOINED[US_TRACT_2010_JOINED$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]) & US_TRACT_2010_JOINED$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_Housing_density, colour = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))

ggplot() +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED_PRICE[US_TRACT_2010_JOINED_PRICE$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]) & US_TRACT_2010_JOINED_PRICE$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_housevalue,  colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 1, data = US_TRACT_2010_JOINED_PRICE[US_TRACT_2010_JOINED_PRICE$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]) & US_TRACT_2010_JOINED_PRICE$State != 6,], aes(x=rank_density_CBSA, y=demeaned_log_housevalue, color = 'Bottom 25%')) +
  scale_colour_manual(name="legend", values = c("blue", "red"))