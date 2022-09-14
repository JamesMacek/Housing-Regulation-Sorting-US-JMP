#Date created: July 19th, 2022
#Date edited: Julu 20th, 2022

#Constructs zoning district maps for some MSAs using otuput from AlgoirthmConstructZoningDistrictsV3.R

library(dplyr)
library(haven)
library(labelled)
library(sf)
library(spdep)
library(ClustGeo) #for spatially constrained clustering (parameterized by alpha). See CRAN documentation
library(cluster) #calculate silhouette score
library(ggplot2)


#174,000 block groups that have assessment data, 177,000 block groups assigned to MSAs
ZoningDist <- read_dta("Data/ZillowData/Output/ConstructedZoningDistricts.dta")
ZoningDist["temp"] <- rep(1, nrow(ZoningDist))

US_BLOCK_2010 <- st_read("Data/US_Data/CensusBlockGroup2010/US_blck_grp_2010.shp") 
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'STATEFP10'] <- 'State'
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'COUNTYFP10'] <- 'County'
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'TRACTCE10'] <- 'Tract' #Renaming these variables to join on State/County/Tract/BlockGroup
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'BLKGRPCE10'] <- 'BlockGroup' #Renaming these variables to join on State/County/Tract/BlockGroup

ZoningDist <- left_join(US_BLOCK_2010, ZoningDist,  by = c("State", "County", "Tract", "BlockGroup"))
rm(US_BLOCK_2010)
ZoningDist <- ZoningDist[!is.na(ZoningDist$temp),]
ZoningDist <- ZoningDist %>% select(-temp)


#Plotting for SF

#San francisco MSA

ggplot() + geom_sf(data = ZoningDist[ZoningDist$CBSA == 41860,], mapping = aes(fill = as.factor(ZoningDistrictID)), show.legend = FALSE) +
  coord_sf()
ggsave("Data/ZillowData/Output/SFCBSA_MAP.png", width = 20, height = 12, units = "cm")

#San Francisco
ggplot() + geom_sf(data = ZoningDist[ZoningDist$CBSA == 41860 & ZoningDist$County == "075",],
                              mapping = aes(fill = as.factor(ZoningDistrictID)), show.legend = FALSE) +
  coord_sf()
ggsave("Data/ZillowData/Output/SF_MAP.png", width = 20, height = 12, units = "cm")
