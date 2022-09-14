library(dplyr)
library(haven)
library(labelled)
library(sf)
library(spdep)
library(cluster) #calculate silhouette score
library(ggplot2)
library(stringr)

#Performs some preliminary validation of lot sizes in San Francisco using a land coverage scheme
#Requires JoinedDataForCounterfactuals.dta. 

LotSizeReg <- read_dta("Data/Counterfactuals/JoinedDataForCounterfactuals.dta",
                       col_select = c("State", "County", "Tract", "BlockGroup", 
                                      "CBSA", "CBSA_NAME", "DensityRestriction"))
LotSizeReg["temp"] <- rep(1, nrow(LotSizeReg))


#Importing shapefile for block groups
US_BLOCK_2010 <- st_read("Data/US_Data/CensusBlockGroup2010/US_blck_grp_2010.shp") 
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'STATEFP10'] <- 'State'
US_BLOCK_2010["State"] <- as.double(US_BLOCK_2010$State)
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'COUNTYFP10'] <- 'County'
US_BLOCK_2010["County"] <- as.double(US_BLOCK_2010$County)
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'TRACTCE10'] <- 'Tract'
US_BLOCK_2010["Tract"] <- as.double(US_BLOCK_2010$Tract) #Renaming these variables to join on State/County/Tract/BlockGroup
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'BLKGRPCE10'] <- 'BlockGroup' #Renaming these variables to join on State/County/Tract/BlockGroup
US_BLOCK_2010["BlockGroup"] <- as.double(US_BLOCK_2010$BlockGroup)

LotSizeReg <- left_join(US_BLOCK_2010, LotSizeReg,  by = c("State", "County", "Tract", "BlockGroup"))
rm(US_BLOCK_2010)
LotSizeReg <- LotSizeReg[!is.na(LotSizeReg$temp),]
LotSizeReg <- LotSizeReg %>% select(-temp)

#Retaining SF
LotSizeReg <- LotSizeReg[LotSizeReg$CBSA == 41860 & LotSizeReg$County == 75,]

#Importing San Francisco shapefile data
SFZoning <- st_read("Data/Zoning_Maps/SF_ZONING/2010/OfficialZoning2010/geo_export_11769668-c4b0-4d97-9134-88d21733fe34.shp")
#Imputing minimum lot sizes using simple SF rule

SFZoning["DensityRestrictionDirect"] <- rep(0, nrow(SFZoning))
#Setting RH-1 (D) districts to have 4000 square foot minimum lots
# All other RH districts have 2500 square foot lots
SFZoning$DensityRestrictionDirect[str_detect(SFZoning$zoning_sim, "RH-1")] <- 2500/43560
SFZoning$DensityRestrictionDirect[SFZoning$zoning_sim == "RH-1(D)"] <- 4000/43560

SFZoning$DensityRestrictionDirect[str_detect(SFZoning$zoning_sim, "RH-2")] <- (2500/43560)/2 #two family
SFZoning$DensityRestrictionDirect[str_detect(SFZoning$zoning_sim, "RH-3")] <- (2500/43560)/3 #three family

SFZoning <- st_transform(SFZoning, crs = st_crs(LotSizeReg))
SFZoning["OfficialZoningDistID"] <- seq(1, nrow(SFZoning))

#Combining into transformed geography
Union <- st_intersection(LotSizeReg, SFZoning) %>%
            mutate(intersect_area = st_area(.)) 


#Calculating mean absolute error (as a percentage of within-city variation)
mean(abs(Union$DensityRestriction - Union$DensityRestrictionDirect))/mean(abs(Union$DensityRestrictionDirect))


#mean absolute error of 0.021 acres or approximately 900 square feet. 
#Most of this error comes from underestimation in neighborhoods. 

ggplot() + geom_sf(data = LotSizeReg,
                   mapping = aes(fill = 43560*DensityRestriction)) +
  coord_sf() + scale_fill_gradient(name = "Effective Minimum Lot Size, Square Feet")
  ggsave("Data/US_Data/Output/SF_EFFMINLOTSIZE.png",  width = 20, height = 12, units = "cm")
  

ggplot() + geom_sf(data = Union,
                   mapping = aes(fill = DensityRestrictionDirect)) +
  coord_sf()

