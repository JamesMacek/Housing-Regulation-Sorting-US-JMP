#This do file bulk downloads some 2020 shapefiles from the US Census Bureau.

#This is to get US places (towns, cities, etc) to compare with official 
#municipalities recored in the CoreLogic assessments. Unzips and merges them into one shapefile

library(sf)

stateIndex <- c("02", "01", "05", "04", "06", "08", "09", "11",
                "10", "12", "13", "15", "19", "16", "17", "18",
                "20", "21", "22", "25", "24", "23", "26", "27",
                "29", "28", "30", "37", "38", "31", "33", "34",
                "35", "32", "36", "39", "40", "41", "42", "44",
                "45", "46", "47", "48", "49", "51", "78", "50",
                "53", "55", "54", "56") #vector of FIPS codes, not including Puerto Rico with same index as stateList

for (i in stateIndex) {
  url_toDownload <- paste0("https://www2.census.gov/geo/tiger/TIGER2020/PLACE/tl_2020_",
                            paste0(i, "_place.zip"))
  
  download.file(url = url_toDownload,
                destfile = paste0("DataV2/US_Data/Shapefiles/2020_Places/", 
                                  paste0(i, ".zip")))
  
}

#Unzipping all files

for (i in stateIndex) {
  unzip(paste0("DataV2/US_Data/Shapefiles/2020_Places/", 
               paste0(i, ".zip")),
        exdir = "DataV2/US_Data/Shapefiles/2020_Places")
  
}


#Rbinding all these shapefiles 
shapefile_forOut <- data.frame() #initializing empty dataframe

for (i in stateIndex) {
 toAppend <-  st_read(paste0("DataV2/US_Data/Shapefiles/2020_Places/", 
                 paste0("tl_2020_", paste0(i, "_place.shp")))) #reading shapefile
 
 shapefile_forOut <- rbind(shapefile_forOut, toAppend)
  
}

#Writing
st_write(shapefile_forOut, "DataV2/US_Data/Shapefiles/2020Places.shp", append = FALSE)


rm(list = ls())