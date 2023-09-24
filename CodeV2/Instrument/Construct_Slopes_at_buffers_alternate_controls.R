#PACKAGES
library(haven) #Reading stata.dta files 
library(dplyr)
library(readr)
library(collapse)
library(ggplot2)

#Raster library
library(raster)
library(sf)
library(terra) #for extract
library(rgdal)
library(exactextractr)

#This file calculates the exact slopes in given set of buffers, 
#rather than using block group aggregates and matching blocks to donuts based on centroids.
#Used in main specification for robustness, to see if this centroid matching procedure introduces any bias. 

#Vector of buffers we want to try (in meters)
Buffers <- c(200, 500, 750, 1000) 

#Parrallel computing
library(doParallel) #Parallelize for loops

#Import any adf file from the slope folder. 40GB is the total size of the raster
slopes <- raster("Data/US_Data/Instrument/ElevationMaps/us_slope/slope/w001001.adf") #Change to DataV2 folder when migrating

#Import shapefile for census tracts (we do one by one)
#Importing CBSA data
US_CBSA <- st_read("Data/US_Data/cb_2013_us_cbsa_500k.shp") #2013 definitions from the US census bureau
US_CBSA <- US_CBSA[US_CBSA$LSAD == "M1",]
US_CBSA <- US_CBSA %>% rename(CBSA = CBSAFP)

US_BLOCK <- st_read("DataV2/US_Data/Shapefiles/US_blck_grp_2020.shp") #From IPUMS NHGIS website, our census BLOCK definitions

#Merging BLOCKs on centriods
US_BLOCK_CENTROID <- st_centroid(US_BLOCK)
US_BLOCK_CENTROID <- st_transform(US_BLOCK_CENTROID, 4269)

US_BLOCK["Temp"] <- as.integer(st_intersects(US_BLOCK_CENTROID, US_CBSA))
US_CBSA <- mutate(US_CBSA, Temp = row_number()) #Temp index used to join MSA names with matched centroids. 

Temp_tojoin <- US_CBSA %>% dplyr::select(Temp, NAME, CBSA) 
Temp_tojoin <- as.data.frame(Temp_tojoin) #converting for left_join
Temp_tojoin <- Temp_tojoin[, !colnames(Temp_tojoin) %in% "geometry"] #deleting geometry data from sf object to join 
US_BLOCK <- left_join(US_BLOCK, Temp_tojoin, by = "Temp")

#renaming
names(US_BLOCK)[names(US_BLOCK) == 'NAME'] <- 'CBSA_NAME'
names(US_BLOCK)[names(US_BLOCK) == 'STATEFP'] <- 'State'
names(US_BLOCK)[names(US_BLOCK) == 'COUNTYFP'] <- 'County'
names(US_BLOCK)[names(US_BLOCK) == 'TRACTCE'] <- 'Tract' #Renaming these variables to join on State/County/Tract
names(US_BLOCK)[names(US_BLOCK) == 'BLKGRPCE'] <- 'BlockGroup' #Renaming these variables to join on State/County/Tract


#Dropping BLOCKs not in used CBSA
US_BLOCK <- US_BLOCK[!is.na(US_BLOCK$CBSA),]

#Dropping Non-mainland states: Alaska (2), Hawaii (15), Puerto Rico (72), US Virgin Islands (already NOT IN SAMPLE)
US_BLOCK <- US_BLOCK[!(US_BLOCK$State == "15" | US_BLOCK$State == "72" | US_BLOCK$State == "02"),]
rm(US_BLOCK_CENTROID, Temp_tojoin, US_CBSA)

#Putting US_BLOCK in same projection as slope
US_BLOCK <- st_transform(US_BLOCK, st_crs(slopes))

#_______EXTRACTING FEATURES FROM RASTER___________
#Function to extract raster for each block group to pass to dopar
Extract <- function(raster, polygonDF, i) { #i are rows to be looped over with for parrallel computing
  
  output <- exactextractr::exact_extract(raster, polygonDF[i, ], fun  = "mean")
  return(output)
  
}

#Loop over multiple buffer sizes
for (buffer_dist in Buffers) {

  #constructing buffer geometry
  US_BLOCK_POLY <- US_BLOCK %>% dplyr::select(State, County, Tract, BlockGroup, geometry) %>% st_buffer(dist = buffer_dist)
  
  US_BLOCK[paste0("buffer_area_", buffer_dist, "m")] <- st_area(US_BLOCK_POLY) #Calculating buffer area 
  
  #Registering doParallel
  ncores  <- detectCores() - 1
  
  #Setting up number of cores (we want to conserve ram as this program is reasonably ram intensive)
  registerDoParallel(ncores)
  
  US_BLOCK[paste0("Avg_Slope_", buffer_dist, "m")] <- foreach (row = 1:nrow(US_BLOCK), .combine = 'c') %dopar% {
                                                                Extract(raster = slopes, polygonDF = US_BLOCK_POLY,
                                                                        i = row)
    
  }
  
  #closing cluster on this loop
  stopImplicitCluster()
  
}

#Converting buffer area from "units" object
index <- 1
for (buffer_dist in Buffers) {
  
  US_BLOCK[paste0("buffer_area_", buffer_dist, "m")] <- as.numeric(US_BLOCK[[paste0("buffer_area_", buffer_dist, "m")]])
  US_BLOCK[paste0("control_Avg_Slope_spec_", index, "_alt")] <- US_BLOCK[[paste0("Avg_Slope_", buffer_dist, "m")]]
  
  index <- index + 1
}

#Destringing variables for merge
US_BLOCK["State"] <- as.double(US_BLOCK$State)
US_BLOCK["County"] <- as.double(US_BLOCK$County)
US_BLOCK["Tract"] <- as.double(US_BLOCK$Tract)
US_BLOCK["BlockGroup"] <- as.double(US_BLOCK$BlockGroup)
US_BLOCK["CBSA"] <- as.double(US_BLOCK$CBSA)

US_BLOCK <- US_BLOCK %>% dplyr::select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME,
                                       starts_with("control_Avg_Slope"), starts_with("instrument_Avg_Slope"), 
                                       starts_with("Avg_Slope_"))

#Dropping geometry and saving as .dta file
US_BLOCK <- US_BLOCK %>% st_drop_geometry()
write_dta(US_BLOCK, "DataV2/US_Data/Instrument/Instrument_constructed_alternate.dta")

