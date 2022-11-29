#PACKAGES
library(haven) #Reading stata.dta files 
library(dplyr)
library(readr)
library(collapse)
library(ggplot2)

#Sf library
library(sf)


#Parrallel computing
library(doParallel) #Parallelize for loops

#date created: October 4th, 2022

#PARAMETERS

EXCLUSION_BUFFER <- c(200, 500, 750, 1000) 
INCLUSION_BUFFER <- c(1000, 1000, 1250, 1500) #vector of buffers you want to try
#Index of vectors above determine the specification-- i.e. Spec 1 is 200m exclusion, 1000m inclusion
#Each vector above should have the same length

#Load data
US_BLOCK <- st_read("DataV2/US_Data/Instrument/Slope_Extracted.shp") 

#Values of instruments/other stuff
donut_instrument_list <- c("Avg_Slope", "Avg_Elev") 

for (varname in donut_instrument_list) {
  for (s in 1:length(EXCLUSION_BUFFER)) {
  
    US_BLOCK[paste(paste("instrument", varname, sep = "_"), s, sep = "_spec_")] <- rep(NA, nrow(US_BLOCK))
  
  }
}

US_BLOCK_APPENDED <- data.frame() #final output (appended dataset)

#Check if data are in meters
st_crs(US_BLOCK)$units #'m'


#Loop over individual MSAs (start loop here)
for (loop_MSA in unique(US_BLOCK$CBSA)) {  
  
  US_BLOCK_MSA <- US_BLOCK[US_BLOCK$CBSA == loop_MSA,]
  
  US_BLOCK_MSA <- US_BLOCK_MSA %>% mutate(row_index = row_number())
  
  #Centroid geography
  US_BLOCK_MSA_CENTROID <- st_centroid(US_BLOCK_MSA)
  
  #Start specification loop here
  for (s in 1:length(EXCLUSION_BUFFER)) {
  
  #Included and excluded polygons
  Excluded_polygon <- st_buffer(US_BLOCK_MSA, dist = EXCLUSION_BUFFER[s]) #Buffer1 -- exclusion region
  Included_polygon <- st_buffer(US_BLOCK_MSA, dist = INCLUSION_BUFFER[s]) #Buffer2 -- inclusion region

  #Take all pairwise polygons whose centriods intersect the inclusion buffer but not the exclusion buffer. 
  Excluded <- st_intersects(Excluded_polygon, US_BLOCK_MSA_CENTROID)
  Included <- st_intersects(Included_polygon, US_BLOCK_MSA_CENTROID)
  
  #Loop over rows in the data 
  for (loop_row in 1:nrow(US_BLOCK_MSA)) {
    #Take list of intersecting rows 
      included_row <- Included[[loop_row]] 
      excluded_row <- Excluded[[loop_row]]
      
      included_row <- included_row[!included_row %in% excluded_row]#Delete intersection of included and excluded rows (including own row)
      
      #Take average of variables in list
      neighbors <- US_BLOCK_MSA_CENTROID[US_BLOCK_MSA_CENTROID$row_index %in% included_row,]
      excl_neighbors <- US_BLOCK_MSA_CENTROID[US_BLOCK_MSA_CENTROID$row_index %in% excluded_row,]
      
      for (varname in donut_instrument_list) {
        US_BLOCK_MSA[[paste(paste("instrument", varname, sep = "_"), s, sep = "_spec_")]][loop_row] <- mean(neighbors[[varname]])
        US_BLOCK_MSA[[paste(paste("control", varname, sep = "_"), s, sep = "_spec_")]][loop_row] <- mean(excl_neighbors[[varname]])
      } 
  } #NaNs that appear from this procedure must come from MSA's with no neighbors. 
}
  
  US_BLOCK_APPENDED <- rbind(US_BLOCK_APPENDED, US_BLOCK_MSA) #Appending to dataset
}

#Saving to stata format
US_BLOCK_APPENDED <- US_BLOCK_APPENDED %>% dplyr::select(starts_with(c("instrument", "control", "Avg", "State", "County",
                                                                       "Tract", "BlockGroup", "CBSA"))) %>% st_drop_geometry()

#Destringing variables for merge
US_BLOCK_APPENDED["State"] <- as.double(US_BLOCK_APPENDED$State)
US_BLOCK_APPENDED["County"] <- as.double(US_BLOCK_APPENDED$County)
US_BLOCK_APPENDED["Tract"] <- as.double(US_BLOCK_APPENDED$Tract)
US_BLOCK_APPENDED["BlockGroup"] <- as.double(US_BLOCK_APPENDED$BlockGroup)
US_BLOCK_APPENDED["CBSA"] <- as.double(US_BLOCK_APPENDED$CBSA)

#outputting to STATA.
write_dta(US_BLOCK_APPENDED, "Datav2/US_Data/Instrument/Instrument_constructed.dta")
