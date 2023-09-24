#PACKAGES
library(sf)
library(haven) #Reading stata.dta files 
library(dplyr)
library(stringr)
library(ggplot2)
library(readxl) #read excel files

#Date created: October 10, 2022

library(googleway) #pinging google maps
library(geosphere) #pairwise distances

#Parrallel computing
library(doParallel) #Parallelize for loops


GoogleMaps_Ping <- 0 #SET TO 0 AFTER AUGUST 2023, TAKE CBD LOCATION AS GIVEN!


#This file constructs CBD distances by pinging google API
load(file = "DataV2/US_Data/Output/Constructed_Block.Rdata")
load(file = "DataV2/US_Data/Output/CBSA_med_house_price.Rdata")
load(file = "DataV2/US_Data/Output/CBSA_quantiles.Rdata")

#Loading Holian (2019) CBD definitions + cleaning for merge
CBD <- read_excel("DataV2/US_Data/Shapefiles/holian2019CBDdefs.xlsx", sheet = "copy_of_merged_data2") #Note: these are pre-2013 definition MSAs. Some will need to be filled in. 

CBD <- CBD %>% rename(CBSA = CBSA_code, CBSA_NAME = CBSA_name) %>% select(starts_with(c("CBSA", "Google")))

#Merging
US_CBSA_2010_c$CBSA <- as.double(US_CBSA_2010_c$CBSA)
US_CBSA_2010_c <- left_join(US_CBSA_2010_c, CBD, by = c("CBSA"), suffix = c("", ".join1")) #missing handful of CBDs unmatched, match manually

#Storing unmatched obs
US_CBSA_unmatched <- US_CBSA_2010_c[is.na(US_CBSA_2010_c$CBSA_NAME.join1),]
US_CBSA_2010_c <- US_CBSA_2010_c[!is.na(US_CBSA_2010_c$CBSA_NAME.join1),]

#Taking unmatched CBSAs and finding a way to match them
#Manually pinging google earth
CBSA_NAME_toMatch <- US_CBSA_unmatched$CBSA_NAME

#Start google search
gmaps_key <- "AIzaSyDlKczwjeiIG83YCgdodjkl2C7VapyBKOI" #our API key (DO NOT SHARE!!!) DO NOT RUN AFTER AUGUST 2023!


if (GoogleMaps_Ping == 1) {

  #Starting queries
  for (row in 1:nrow(US_CBSA_unmatched)) {
  
  
      search_query <- paste(US_CBSA_unmatched$CBSA_NAME[row], "City hall", sep = " ")
  
      if (US_CBSA_unmatched$CBSA_NAME[row] == "Los Angeles-Long Beach-Anaheim, CA") { #alteration to only include LA and not anaheim
        search_query <- "Los Angeles City hall" 
      }
  
      place_results <- google_places(search_string = search_query, 
                                 key = gmaps_key)
    
      US_CBSA_unmatched$GoogleEarthLat[row] <- place_results$results$geometry$location[[1]][1] #latitude of first search result
      US_CBSA_unmatched$GoogleEarthLon[row] <- place_results$results$geometry$location[[2]][1] #longitude
  
  }
    
  #SAVING GOOGLE MAPS PING + DAY OF EXTRACTION 
   date_full <- date()
   date <- paste0(word(date_full, start = 2, end = 2), "_",
                word(date_full, start = 4, end = 4), "_",
                word(date_full, start = 6, end = 6)) 
  save(US_CBSA_unmatched, file = paste0("DataV2/US_Data/Output/Unmatched_CBD_locations_fromGoogleMaps_", date, ".Rdata"))  
  
}

#loading matched filename (note: there should be only one in output folder!)
filelist <- list.files("DataV2/US_Data/Output")
load_GoogleMaps_CBD <- filelist[which(grepl("Unmatched_CBD_locations_fromGoogleMaps", filelist, fixed = TRUE) == TRUE)] #extracting desired files
#loading data
load(paste0("DataV2/US_Data/Output/", load_GoogleMaps_CBD))

US_CBSA_2010_c <- rbind(US_CBSA_2010_c, US_CBSA_unmatched)

#Merging dist_to_CBD with US_BLOCK
US_CBSA_toJoin <- US_CBSA_2010_c %>% select(CBSA, GoogleEarthLat, GoogleEarthLon) %>%
                                     rename(CBDLat = GoogleEarthLat, CBDLon = GoogleEarthLon)
US_BLOCK$CBSA <- as.double(US_BLOCK$CBSA) #changing to double for merge

US_BLOCK <- left_join(US_BLOCK, US_CBSA_toJoin, by = c("CBSA"))

#Calculating physical distance to CBD for each object


#Calculating centriods
US_BLOCK <- st_set_geometry(US_BLOCK, US_BLOCK$geometry) #setting geometry which was dropped after rowwise join
US_BLOCK_CENTRIOD <- st_centroid(US_BLOCK)
US_BLOCK_CENTRIOD <- st_transform(US_BLOCK_CENTRIOD, 4269) #setting correct crs to talk to lat/lon


#loop over rows and calculate distance between CBD and block group
#Initializing column
US_BLOCK["Dist_to_CBD"] <- foreach(i=1:nrow(US_BLOCK), .combine = c) %do% {
  
                           return(geosphere::distm(c(US_BLOCK_CENTRIOD$geometry[i][[1]][1], US_BLOCK_CENTRIOD$geometry[i][[1]][2]),
                                                                                          c(US_BLOCK$CBDLon[i], US_BLOCK$CBDLat[i]), fun = distHaversine))
  
}


US_BLOCK["inv_D2CBD"] <- 1/US_BLOCK$Dist_to_CBD

#Creating ordering of block groups (closest to CBD --> 1, farthest is 0)
US_BLOCK <- US_BLOCK %>% group_by(CBSA) %>% mutate(rank_inv_D2CBD = order(order(inv_D2CBD, decreasing = FALSE))/(max(order(order(inv_D2CBD, decreasing = FALSE)))+1)) %>%
                                            st_set_geometry(US_BLOCK$geometry)

#Saving
save(US_BLOCK, file = "DataV2/US_Data/Output/Constructed_Block.Rdata")

rm(list = ls())
