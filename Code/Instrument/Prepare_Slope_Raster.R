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

#Parrallel computing
library(doParallel) #Parallelize for loops

#Date created: September 27th 2022
#Constructs slope rasters for merge and construction with the instrument


#Import any adf file from the slope folder. 40GB is the total size of the raster
slopes <- raster("Data/US_Data/Instrument/ElevationMaps/us_slope/slope/w001001.adf")
elevation <- raster("Data/US_Data/Instrument/ElevationMaps/us_orig_dem/orig_dem/w001001.adf")

#Import shapefile for census tracts (we do one by one)
#Importing CBSA data
US_CBSA <- st_read("Data/US_Data/cb_2013_us_cbsa_500k.shp") #2013 definitions from the US census bureau, better to use
US_CBSA <- US_CBSA[US_CBSA$LSAD == "M1",]
US_CBSA <- US_CBSA %>% rename(CBSA = CBSAFP)

US_BLOCK <- st_read("Data/US_Data/CensusBlockGroup2010/US_blck_grp_2010.shp") #From IPUMS NHGIS website, our census BLOCK definitions

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
names(US_BLOCK)[names(US_BLOCK) == 'STATEFP10'] <- 'State'
names(US_BLOCK)[names(US_BLOCK) == 'COUNTYFP10'] <- 'County'
names(US_BLOCK)[names(US_BLOCK) == 'TRACTCE10'] <- 'Tract' #Renaming these variables to join on State/County/Tract
names(US_BLOCK)[names(US_BLOCK) == 'BLKGRPCE10'] <- 'BlockGroup' #Renaming these variables to join on State/County/Tract


#Dropping BLOCKs not in used CBSA
US_BLOCK <- US_BLOCK[!is.na(US_BLOCK$CBSA),]

#Dropping Non-mainland states: Alaska (2), Hawaii (15), Puerto Rico (72), US Virgin Islands (already NOT IN SAMPLE)
US_BLOCK <- US_BLOCK[!(US_BLOCK$State == "15" | US_BLOCK$State == "72" | US_BLOCK$State == "02"),]
rm(US_BLOCK_CENTROID, Temp_tojoin, US_CBSA)

#Putting US_BLOCK in same projection as slope
US_BLOCK <- st_transform(US_BLOCK, st_crs(slopes))

#_______EXTRACTING FEATURES FROM RASTER___________
#Initiating doParrallel 
n.cores <- detectCores() - 1 #11 threads (cores?) on my main machine.
my.cluster <- makeCluster(n.cores, type = "PSOCK") #needs to allow firewall.

#registering cluster
registerDoParallel(cl = my.cluster)

#checking to see if this is registered
getDoParRegistered() #TRUE.
getDoParWorkers()

#Function to extract raster for each block group
Extract <- function(raster, polygonDF, i) { #i are rows to be looped over with for parrallel computing
  
  output <- exactextractr::exact_extract(raster, polygonDF[i, ], fun  = "mean")
  return(output)
  
}

#testing function
Extract(raster = slopes, polygonDF = US_BLOCK, i = 1)

#TESTING_____________________________________________________
test <- 0 #set to any number except 1 to run full program

if (test == 1) {
  US_BLOCK <- US_BLOCK[1:1000,] #extract first 1000 observations
}
#_____________________________________________________________

#Initiate parrallelized for loop

Sys.time()

#Parrallel computing doesn't a lot because most of the time is reading raster data from disk
US_BLOCK["Avg_Slope"] <- foreach (row = 1:nrow(US_BLOCK), .combine = 'c') %dopar% {
  Extract(raster = slopes, polygonDF = US_BLOCK,
                i = row)
  
}

US_BLOCK["Avg_Elev"] <- foreach (row = 1:nrow(US_BLOCK), .combine = 'c') %dopar% {
  Extract(raster = elevation, polygonDF = US_BLOCK,
          i = row)
  
}

Sys.time() #1000 slopes extracted in <15 seconds. #Extrapolating to ~ 45 minutes for entire dataset. 
#Only 4 observations could not be assigned slopes in the data (out of 177,261 block groups)


#Do the same with altitude raster?


#Writing and storing as shapefile
st_write(US_BLOCK, "Data/US_Data/Instrument/Slope_Extracted.shp", append = FALSE)