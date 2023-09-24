#Date created: June 26th, 2023

#This file constructs some maps for regulation

library(sf)
library(haven) #Reading stata.dta files 
library(dplyr)
library(readr)
library(collapse)
library(stringr)
library(labelled)
library(ggplot2)
library(estimatr)
library(rlang)

#Requires output from Merge_stringency.R, validate_Regulation.R and hayward_assessments_for_figure.do

#Constructed_Block_V2.data at current algorithm from Merge_stringency.R
US_BLOCK <- read_dta("DataV2/US_Data/Output/Constructed_Block_V2.dta")

#PART 1: ECDF OF Minimum LOT SIZE ESTIMATES
#Censoring at 5 acres for output on plot
US_BLOCK_temp <- US_BLOCK
US_BLOCK_temp$UnitDensityRestriction_cl[US_BLOCK_temp$UnitDensityRestriction > 5 & !is.na(US_BLOCK_temp$UnitDensityRestriction)] <- 5 

#PART 1.5: Average cluster size 


#empirical CDF of unit density restrictions after cleaning
ggplot(data = US_BLOCK_temp, aes(UnitDensityRestriction_cl)) + stat_ecdf() +
  ylab("Empirical CDF") + xlab("Minimum Lot Size (Acres), Censored at 5 Acres")
ggsave("DataV2/US_Data/Output/UnitDensityRestriction_ECDF.png", width = 24, height = 15, units = "cm")
rm(US_BLOCK_temp)

#PART 2: ECDF OF Hayward California, censored at 1 acre to make figure more visible. Import assessment level dataset
Hayward_assess <- read_dta("DataV2/CoreLogic/output/currentAssess_hayward.dta") %>% filter(acres <= 1) #filtering for properties below one acre to make the plot nice
ggplot(data = Hayward_assess, aes(landsquarefootage)) + stat_ecdf() + geom_vline(xintercept = 5000, linetype = 2) +  #5000 square foot minimum lot size
       ylab("Empirical CDF") + xlab("Lot Size (Square Feet)") + ggtitle("Hayward, CA Lot Size Distribution")
ggsave("DataV2/US_Data/Output/HaywardLotSizeDist.png", width = 24, height = 15, units = "cm")

#PART 3: Hayward, CA Zoning Districts (example) at optimal solution
  Hayward_block <- US_BLOCK[US_BLOCK$Assigned_geocoded_municipality == "Hayward city",]

  #Reading raw block geometry
  blkgeo <- st_read("DataV2/US_Data/Shapefiles/US_blck_grp_2020.shp") %>% 
            mutate(State = as.numeric(STATEFP), County = as.numeric(COUNTYFP), Tract = as.numeric(TRACTCE),
            BlockGroup = as.numeric(BLKGRPCE)) %>%
            select(State, County, Tract, BlockGroup)

  Hayward_block <- inner_join(blkgeo, Hayward_block, by = c("State", "County", "Tract", "BlockGroup"))
  rm(blkgeo)
  sf_use_s2(FALSE) #switching spherical geometry off-- this causes issues with st_intersects. No big deal given small locations. 
  
  #multicolor...
  Hayward_block$ZoningDistrictID <- as.character(Hayward_block$ZoningDistrictID)
  
  ggplot() + geom_sf(data = Hayward_block,
                     mapping = aes(fill = ZoningDistrictID),
                     show.legend = FALSE) +
    coord_sf() + ggtitle("Hayward, CA Optimal Zoning Districts")
  ggsave("DataV2/US_Data/Output/HaywardZoningDistricts.png",  
         width = 35, height = 21.875, units = "cm", type = "cairo") 
  
#PART 4: Hayward, CA Minimum Lot Sizes
  ggplot() + geom_sf(data = Hayward_block,
                     mapping = aes(fill = 43560*UnitDensityRestriction)) + #Note: this is the uncleaned version -- remember we censor lot sizes if mode > 2*average lot size
    coord_sf() + scale_fill_gradient(name = "Official Housing Unit Density Restriction, Square Feet") +
    ggtitle("Hayward, CA Unit Density Restrictions")
  ggsave("DataV2/US_Data/Output/HaywardMinimumLotSizes.png",  
         width = 35, height = 21.875, units = "cm", type = "cairo") 

rm(list = ls())
       
