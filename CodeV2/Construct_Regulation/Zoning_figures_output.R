#Date created: June 26th, 2023

#This file constructs some maps to display regulation in the paper + summary statistics on zoning district IDs
library(dplyr)
library(readr)
library(collapse)
library(stringr)
library(labelled)
library(ggplot2)
library(estimatr)
library(rlang)
library(sf)
library(haven) #Reading stata.dta files 
sf_use_s2(FALSE) #Switching off spherical geometry
library(here)
library(purrr)
library(viridis) #for colors

#___________________________________________________________
#Functions to create maps
source("CodeV2/Counterfactual/Functions/OpenStreetMapping_Functions.R")
#______________________________________________________________


#This file requires output from Merge_stringency.R, validate_Regulation.R and extract_Hayward_example.do
#Municipality definition of optimal algorithm HERE.
Main_municipality <- "CORELOGIC"

if (Main_municipality == "CORELOGIC") {
  munic_def <- "Assigned_municipality"
}else{
  munic_def <- "Assigned_geocoded_municipality"
}



#Constructed_Block_V2.data at current algorithm from Merge_stringency.R
US_BLOCK <- read_dta("DataV2/US_Data/Output/Constructed_Block_V2.dta")

#PART 1: ECDF OF Minimum LOT SIZE ESTIMATES
#Censoring at 5 acres for output on plot
US_BLOCK_temp <- US_BLOCK
US_BLOCK_temp$UnitDensityRestriction_cl[US_BLOCK_temp$UnitDensityRestriction > 5 & !is.na(US_BLOCK_temp$UnitDensityRestriction)] <- 5 

#PART 1.5: Average cluster size 
US_BLOCK <- US_BLOCK[!is.na(US_BLOCK$ZoningDistrictID),] #Taking all block groups assigned to cluster (i.e. have data in CoreLogic when doing collapse_stats_for_Clustering.do)
US_BLOCK <- US_BLOCK %>% group_by(CBSA, !!sym(munic_def), Assigned_zoningcode, ZoningDistrictID) %>% mutate(DistrictCount = n())
US_BLOCK_collap <- unique(select(US_BLOCK, CBSA, !!sym(munic_def), Assigned_zoningcode, ZoningDistrictID, DistrictCount))
print(paste0("The mean zoning district size is ", mean(US_BLOCK_collap$DistrictCount), " with a standard deviation of ", sd(US_BLOCK_collap$DistrictCount)))
print(paste0("The largest zoning district size is ", max(US_BLOCK_collap$DistrictCount)))
print(paste0("The median zoning district size is ", median(US_BLOCK_collap$DistrictCount))) #50% of all zonign districts are of size 1. 


rm(US_BLOCK_collap)

#________________________________________________________
#FIGURES_________________________________________________
#________________________________________________________
#empirical CDF of unit density restrictions after cleaning
ggplot(data = US_BLOCK_temp, aes(UnitDensityRestriction_cl)) + stat_ecdf() +
  ylab("Empirical CDF") + xlab("Minimum Lot Size (Acres), Censored at 5 Acres") + theme_gray(base_size = 18) 
ggsave("DataV2/US_Data/Output/UnitDensityRestriction_ECDF.png", width = 24, height = 15, units = "cm") 
  
rm(US_BLOCK_temp)

#PART 2: ECDF OF Hayward California, censored at 1 acre to make figure more visible. Import assessment level dataset
Hayward_assess <- read_dta("DataV2/CoreLogic/output/currentAssess_hayward.dta") %>% filter(acres <= 1) #filtering for properties below one acre to make the plot nice
ggplot(data = Hayward_assess, aes(landsquarefootage)) + stat_ecdf() + geom_vline(xintercept = 5000, linetype = 2) +  #5000 square foot minimum lot size
       ylab("Empirical CDF") + xlab("Lot Size (Square Feet)") + ggtitle("Hayward, CA Lot Size Distribution") +
       theme_gray(base_size = 18) 
ggsave("DataV2/US_Data/Output/HaywardLotSizeDist.png", width = 24, height = 15, units = "cm")




#Zoning map figures
#_____________________________________________________________________________________
#Constructing hayward block group dataset
Hayward_block <- US_BLOCK[US_BLOCK$Assigned_geocoded_municipality == "Hayward city",]

#Reading raw block geometry
blkgeo <- st_read("DataV2/US_Data/Shapefiles/US_blck_grp_2020.shp") %>% 
  mutate(State = as.numeric(STATEFP), County = as.numeric(COUNTYFP), Tract = as.numeric(TRACTCE),
         BlockGroup = as.numeric(BLKGRPCE)) %>%
  select(State, County, Tract, BlockGroup)

Hayward_block <- inner_join(blkgeo, Hayward_block, by = c("State", "County", "Tract", "BlockGroup"))
rm(blkgeo)
Hayward_block <- st_transform(Hayward_block, 4326) #to lat/lon coordinates system
#Creating temporary zoning district ID joined with municipality given definition
Hayward_block["Zoning_Dist_ID_unique"] <- paste0(Hayward_block[[munic_def]], "_", as.character(Hayward_block$ZoningDistrictID))

  #Getting bounding box for the geometry
  hayward_bbox <- st_bbox(Hayward_block)
  
  #Extracting open street map tiles to overlay zoning districts
  # see https://yutani.rbind.io/post/2018-06-09-plot-osm-tiles/ for code
  x_len <- hayward_bbox["xmax"] - hayward_bbox["xmin"]
  y_len <- hayward_bbox["ymax"] - hayward_bbox["ymin"]

  # calculate the minimum zoom level that is smaller than the lengths
  x_zoom <- sum(x_len < 360 / 2^(0:19)) - 1
  y_zoom <- sum(y_len < 170.1022 / 2^(0:19)) - 1
  zoom <- min(x_zoom, y_zoom)
  
  #add additional zoom
  zoom <- zoom + 2
  rm(x_zoom, y_zoom)
  
  #TESTING on zoning districts
  ggplot() +
    geom_sf(data = Hayward_block,
            mapping = aes(fill = Zoning_Dist_ID_unique, alpha = 0.15),
            show.legend = FALSE) + 
    annotate("rect", xmin = hayward_bbox["xmin"], ymin = hayward_bbox["ymin"], 
                     xmax = hayward_bbox["xmax"], ymax = hayward_bbox["ymax"], 
             colour = alpha("red", 0.4), linetype = "dashed",
             fill = "transparent", linetype = "dashed", linewidth = 1.2) #bounding box looks correct

  
  #Create tiles
  xy <- lonlat2xy(hayward_bbox[c("xmin", "xmax")], hayward_bbox[c("ymin", "ymax")], zoom)
  tiles <- expand.grid(x = seq(xy$x["xmin"], xy$x["xmax"]),
                       y = seq(xy$y["ymin"], xy$y["ymax"]))
  
  #Open street map api tiles
  urls <- sprintf("https://a.tile.openstreetmap.org/%d/%d/%d.png", zoom, tiles$x, tiles$y)

  #Using get tiles function for pngs, read them directly into R
  pngs <- map(urls, get_tile)

  #Getting tile positions
  nw_corners <- pmap_dfr(tiles, xy2lonlat, zoom = zoom)
  # add 1 to x and y to get the south-east corners
  se_corners <- pmap_dfr(mutate_all(tiles, `+`, 1), xy2lonlat, zoom = zoom)
  
  names(nw_corners) <- c("xmin", "ymax")
  names(se_corners) <- c("xmax", "ymin")
  
  tile_positions <- bind_cols(nw_corners, se_corners)
  rm(se_corners, nw_corners)
  
  #Setting up data to use pmap for plotting
  args <- tile_positions %>% mutate(raster = pngs)
  

#CREATING ZONING MAPS
  
#PART 3: Hayward, CA Zoning Districts (example) at optimal solution
  
  #PLOTTING
  ggplot() + coord_sf() +
    pmap(args, annotation_raster, interpolate = TRUE) + 
    geom_sf(data = Hayward_block,
                     mapping = aes(fill = Zoning_Dist_ID_unique, alpha = 0.15),
                     show.legend = FALSE) + coord_sf(expand = FALSE) +
    theme_gray(base_size = 18) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 12)) +
    ggtitle("Hayward, CA Optimal Zoning Districts")
  ggsave("DataV2/US_Data/Output/HaywardZoningDistricts.png",  
         width = 35, height = 21.875, units = "cm", type = "cairo") 
  
  #PART 4: Hayward, CA Minimum Lot Sizes
  ggplot() + coord_sf() + 
      pmap(args, annotation_raster, interpolate = TRUE) + 
      geom_sf(data = Hayward_block,
                     mapping = aes(fill = 43560*UnitDensityRestriction_cl), alpha = 0.65) + #Note: this is the cleaned version
     scale_fill_gradientn(colours=rev(magma(6)), name = paste0("Assigned Density Restriction, \n Square Feet / Household")) +
    coord_sf(expand = FALSE) +
    theme_gray(base_size = 18) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 12)) 
    ggtitle("Hayward, CA Unit Density Restrictions")
  ggsave("DataV2/US_Data/Output/HaywardMinimumLotSizes.png",  
         width = 35, height = 21.875, units = "cm", type = "cairo") 
  
  
 #Part 5: Income stringency measure
  ggplot() + coord_sf() + 
    pmap(args, annotation_raster, interpolate = TRUE) + 
    geom_sf(data = Hayward_block,
            mapping = aes(fill = IncomeStringency_cl/1000000), alpha = 0.65) + #Note: this is the cleaned version
    scale_fill_gradientn(colours=rev(magma(6)), name = paste0("Income Stringency measure, \n in millions USD")) +
    coord_sf(expand = FALSE) +
    theme_gray(base_size = 18) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 12)) 
    ggtitle("Hayward, CA Income Stringency measure")
  ggsave("DataV2/US_Data/Output/HaywardIncomeStringency.png",  
         width = 35, height = 21.875, units = "cm", type = "cairo") 
  
  
  #Part 6: Hayward observed income
  ggplot() + coord_sf() + 
    pmap(args, annotation_raster, interpolate = TRUE) + 
    geom_sf(data = Hayward_block,
            mapping = aes(fill = Average_income/1000), alpha = 0.65) + #Note: this is the cleaned version
    scale_fill_gradientn(colours=rev(magma(6)), name = paste0("Average income, \n in thousands USD")) +
    coord_sf(expand = FALSE) +
    theme_gray(base_size = 18) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 12)) 
    ggtitle("Hayward, CA Neighborhood income distribution")
  ggsave("DataV2/US_Data/Output/HaywardIncome.png",  
         width = 35, height = 21.875, units = "cm", type = "cairo") 
  
  
  

rm(list = ls())
       
